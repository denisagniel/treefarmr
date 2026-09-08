// Extract exactly ONE optimal tree, chosen by a fixed deterministic tie-break rule.
// Work is proportional to the size of the returned tree, not to the number of
// tied-optimal trees.
//
// Needed because models_inner() -- inherited from the Rashomon-set lineage -- takes
// the cross-product of all optimal splits and all optimal left/right subtrees at
// every node. T(d) = d * T(d-1)^2 distinct trees realize the same full product
// partition over d binary features (T(3)=12, T(4)=576, T(5)=1658880), so that
// cross-product exhausts Configuration::model_limit and returns nothing. Those trees
// are partition-identical, because binarization is global: construct_bitmasks() fixes
// one binary feature matrix per fit and Dataset::subset() only masks against it. Any
// one of them is therefore the same fitted function.
//
// Tie-break rules, applied in order at every node:
//   1. Take the leaf when it attains the subproblem optimum. base_objective() <=
//      upperbound() implies equality (the leaf is always feasible, so upperbound()
//      cannot exceed it), so the leaf is genuinely optimal and is the most
//      parsimonious optimum.
//   2. Otherwise take the optimum-attaining split with the lowest feature index whose
//      children both resolve. Ordering by feature index rather than by bound_list
//      position makes the choice independent of worker thread scheduling.
//
// @param identifier: vertex of the dependency graph to extract from
// @returns one optimal model rooted at identifier, or nullptr if none is extractable
std::shared_ptr<Model> Optimizer::single_model(key_type const & identifier) {
    // Lock graph for all accesses in this function (vertices, bounds, children, translations)
    std::lock_guard<std::recursive_mutex> lock(this->state.graph.graph_mutex);

    auto task_accessor = this->state.graph.vertices.find(identifier);
    if (task_accessor == this->state.graph.vertices.end()) { return nullptr; }
    Task & task = task_accessor -> second;

    float const bound = task.upperbound();
    float const tolerance = std::numeric_limits<float>::epsilon();

    // Tie-break rule 1: prefer the leaf whenever it attains the subproblem optimum.
    if (task.base_objective() <= bound + tolerance) {
        std::shared_ptr<Model> leaf(new Model(std::shared_ptr<Bitmask>(new Bitmask(task.capture_set())), this->state, task.worker_id()));
        leaf -> identify(identifier);
        leaf -> translate_self(task.order());
        return leaf;
    }

    auto bounds = this->state.graph.bounds.find(identifier);
    if (bounds == this->state.graph.bounds.end()) { return nullptr; }

    // Collect the splits that attain the optimum, then visit them in increasing
    // feature order so the selection does not depend on bound_list insertion order.
    // Held as int, like models_inner(), so the -(feature + 1) child keys below stay
    // signed rather than relying on unsigned wraparound.
    std::vector<int> candidates;
    for (bound_iterator iterator = bounds -> second.begin(); iterator != bounds -> second.end(); ++iterator) {
        if (std::get<2>(* iterator) > bound + tolerance) { continue; }
        candidates.push_back((int) std::get<0>(* iterator));
    }
    std::sort(candidates.begin(), candidates.end());

    // Tie-break rule 2: first candidate feature whose children both resolve.
    for (auto candidate = candidates.begin(); candidate != candidates.end(); ++candidate) {
        int feature = * candidate;

        std::shared_ptr<Model> negative;
        auto left_key = this->state.graph.children.find(std::make_pair(identifier, -(feature + 1)));
        if (left_key == this->state.graph.children.end()) {
            // No edge recorded: the negative side is a leaf over the induced subset
            Bitmask subset(task.capture_set());
            this->state.dataset.subset(feature, false, subset);
            negative = std::shared_ptr<Model>(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state, task.worker_id()));
        } else if (this->state.graph.vertices.find(left_key -> second) != this->state.graph.vertices.end()) {
            negative = single_model(left_key -> second);
        }
        if (!negative) { continue; }

        std::shared_ptr<Model> positive;
        auto right_key = this->state.graph.children.find(std::make_pair(identifier, feature + 1));
        if (right_key == this->state.graph.children.end()) {
            Bitmask subset(task.capture_set());
            this->state.dataset.subset(feature, true, subset);
            positive = std::shared_ptr<Model>(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state, task.worker_id()));
        } else if (this->state.graph.vertices.find(right_key -> second) != this->state.graph.vertices.end()) {
            positive = single_model(right_key -> second);
        }
        if (!positive) { continue; }

        std::shared_ptr<Model> model(new Model(feature, negative, positive, this->state, task.worker_id()));
        model -> identify(identifier);
        model -> translate_self(task.order());
        if (negative -> identified()) {
            auto negative_translation = this->state.graph.translations.find(std::make_pair(identifier, -(feature + 1)));
            if (negative_translation != this->state.graph.translations.end()) {
                model -> translate_negatives(negative_translation -> second);
            }
        }
        if (positive -> identified()) {
            auto positive_translation = this->state.graph.translations.find(std::make_pair(identifier, feature + 1));
            if (positive_translation != this->state.graph.translations.end()) {
                model -> translate_positives(positive_translation -> second);
            }
        }
        return model;
    }

    return nullptr;
}

void Optimizer::models(std::unordered_set< Model > & results) {
    assert(!rashomon_flag);

    // Single-tree extraction: walk the graph once and take one optimal tree.
    // Deliberately does NOT go through models()/models_inner(), whose
    // enumerate-all-ties semantics are only needed by extract_rashomon_models().
    std::shared_ptr<Model> model = single_model(this -> root);

    if (Configuration::verbose) {
        std::cout << "Memory usage: " << getCurrentRSS() / 1000000 << std::endl;
    }

    if (!model) { return; }
    max_result_size = std::max(max_result_size, (std::size_t) 1);
    results.insert(* model);
}

void Optimizer::models(key_type const & identifier, std::unordered_set< std::shared_ptr<Model>, std::hash< std::shared_ptr<Model> >, std::equal_to< std::shared_ptr<Model> > > & results, float scope) {
    // Shortcircuit model extraction if number of models exceeds given amount
    if (model_limit_exceeded) {
        return;
    }

    models_inner(identifier, results, scope);
}

void Optimizer::models_inner(key_type const & identifier, std::unordered_set< std::shared_ptr<Model>, std::hash< std::shared_ptr<Model> >, std::equal_to< std::shared_ptr<Model> > > & results, float scope) {
    // Lock graph for all accesses in this function (vertices, bounds, children, translations)
    std::lock_guard<std::recursive_mutex> lock(this->state.graph.graph_mutex);

    auto task_accessor = this->state.graph.vertices.find(identifier);
    if (task_accessor == this->state.graph.vertices.end()) { return; }
    Task & task = task_accessor -> second;
    //std::cout << "Base Condition: " << task.base_objective() << " <= " << task.upperbound() << " = " << (int)(task.base_objective() <= task.upperbound()) << std::endl;

    // std::cout << "Capture: " << task.capture_set().to_string() << std::endl;

    // Use scope for filtering when provided (Rashomon extraction), otherwise fall back to task.upperbound()
    float effective_bound = (scope > 0) ? scope : task.upperbound();

    if (task.base_objective() <= effective_bound + std::numeric_limits<float>::epsilon()) {
        // || (Configuration::rule_list && task.capture_set().count() != task.capture_set().size())) {
        // std::cout << "Stump" << std::endl;
        // std::shared_ptr<key_type> stump(new Tile(set));
        // Model stump_key(stump_set); // shallow variant
        // Model * stump_address = new Model(stump_set);
        std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(task.capture_set())), this->state, task.worker_id()));
        model -> identify(identifier);
        
        model -> translate_self(task.order());
        results.insert(model);
    }   
    auto bounds = this->state.graph.bounds.find(identifier);
    if (bounds == this->state.graph.bounds.end()) { return; }
    for (bound_iterator iterator = bounds -> second.begin(); iterator != bounds -> second.end(); ++iterator) {

        // When scope > 0 (Rashomon), filter by lowerbound (index 1) like rash_models_inner
        // When scope <= 0 (optimal), filter by upperbound (index 2) as before
        float split_bound = (scope > 0) ? std::get<1>(* iterator) : std::get<2>(* iterator);
        if (split_bound > effective_bound + std::numeric_limits<float>::epsilon()) { continue; }
        int feature = std::get<0>(* iterator);
        //std::cout << "Feature: " << feature << std::endl;
        std::unordered_set< std::shared_ptr<Model> > negatives;
        std::unordered_set< std::shared_ptr<Model> > positives;
        bool ready = true;

        float left_lowerbound = 0, right_lowerbound = 0;

        auto left_key = this->state.graph.children.find(std::make_pair(identifier, -(feature + 1)));
        bool left_has_key = (left_key != this->state.graph.children.end());
        auto left_child = left_has_key ? this->state.graph.vertices.find(left_key->second) : this->state.graph.vertices.end();
        bool left_has_child = (left_child != this->state.graph.vertices.end());
        if (left_has_child) {
            left_lowerbound = left_child->second.lowerbound();
        } else if (!left_has_key) {
            Bitmask subset(task.capture_set());
            this->state.dataset.subset(feature, false, subset);
            unsigned int count = subset.count();
            std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state, task.worker_id()));
            float leaf_objective = model->loss() + model->complexity();
            left_lowerbound = leaf_objective;
            negatives.insert(model);
        } else {
            continue;
        }

        auto right_key = this->state.graph.children.find(std::make_pair(identifier, feature + 1));
        bool right_has_key = (right_key != this->state.graph.children.end());
        auto right_child = right_has_key ? this->state.graph.vertices.find(right_key->second) : this->state.graph.vertices.end();
        bool right_has_child = (right_child != this->state.graph.vertices.end());
        if (right_has_child) {
            right_lowerbound = right_child->second.lowerbound();
        } else if (!right_has_key) {
            Bitmask subset(task.capture_set());
            this->state.dataset.subset(feature, true, subset);
            unsigned int count = subset.count();
            std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state, task.worker_id()));
            float leaf_objective = model->loss() + model->complexity();
            right_lowerbound = leaf_objective;
            positives.insert(model);
        } else {
            // might never reach here? 
            continue;
        }

        // When using scope (Rashomon), skip if scope would go negative for either child
        if (scope > 0 && (scope - right_lowerbound < 0 || scope - left_lowerbound < 0)) { continue; }

        if (left_has_child) {
            models(left_key -> second, negatives, scope - right_lowerbound);
        }

        if (negatives.size() == 0) { continue; }

        if (right_has_child) {
            models(right_key -> second, positives, scope - left_lowerbound);
        }

        if (positives.size() == 0) { continue; }
        
        if (Configuration::rule_list) {
            throw std::invalid_argument("Does not support rule lists");
        } else {

            for (auto negative_it = negatives.begin(); negative_it != negatives.end(); ++negative_it) {
                for (auto positive_it = positives.begin(); positive_it != positives.end(); ++positive_it) {

                    if (Configuration::model_limit > 0 && results.size() > Configuration::model_limit) { 
                        model_limit_exceeded = true;
                        return;
                    }
                    
                    std::shared_ptr<Model> negative(* negative_it);
                    std::shared_ptr<Model> positive(* positive_it);

                    // When using scope (Rashomon), prune combinations exceeding bound
                    if (scope > 0) {
                        float combined = negative->loss() + negative->complexity() + positive->loss() + positive->complexity();
                        if (combined > effective_bound + std::numeric_limits<float>::epsilon()) { continue; }
                    }

                    std::shared_ptr<Model> model(new Model(feature, negative, positive, this->state, task.worker_id()));
                    model -> identify(identifier);
                    model -> translate_self(task.order());
                    if ((** negative_it).identified()) {
                        auto negative_translation = this->state.graph.translations.find(std::make_pair(identifier, -(feature + 1)));
                        if (negative_translation != this->state.graph.translations.end()) {
                            model -> translate_negatives(negative_translation -> second);
                        }
                    }
                    if ((** positive_it).identified()) {
                        auto positive_translation = this->state.graph.translations.find(std::make_pair(identifier, feature + 1));
                        if (positive_translation != this->state.graph.translations.end()) {
                            model -> translate_positives(positive_translation -> second);
                        }
                    }
            
                    results.insert(model); 
                    
                }
            }

        }
    }

    max_result_size = std::max(max_result_size, results.size());
    return;
}


