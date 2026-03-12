void Optimizer::models(std::unordered_set< Model > & results) {
    // Note: model_limit == 0 means unlimited (not zero models)
    // The actual limit is checked inside models_inner() at line 153
    std::unordered_set< std::shared_ptr<Model>, std::hash< std::shared_ptr<Model> >, std::equal_to< std::shared_ptr<Model> > > local_results;
    assert(!rashomon_flag);
    models(this -> root, local_results);
    
    // std::cout << "Local Size: " << local_results.size() << std::endl;
    // std::cout << "Result Size: " void Optimizer::models(std::unordered_set< Model > & results) {<< results.size() << std::endl;
    
    if (Configuration::verbose) {
        std::cout << "Memory usage: " << getCurrentRSS() / 1000000 << std::endl;
    }

    // Copy into final results
    if (model_limit_exceeded) {
        std::cout << "Model limit exceeded. Will not produce any model." << std::endl;
        results.clear();
        return;
    }
    int count = 0;
    for (auto iterator = local_results.begin(); iterator != local_results.end(); ++iterator) {

        // std::pair< std::unordered_set<Model>::iterator, bool > insertion = results.insert(Model());
        // * (insertion.first) = (** iterator);
         Model * model = new Model(**iterator);
         count++;
        
        // Serialization happens in gosdt.cpp for R interface - no need to print here
        // (Printing causes massive log files in simulations)
        results.insert(**iterator);
        delete model;
    }
    //std::cout << "Local Size: " << local_results.size() << std::endl;
    //std::cout << "Result Size: " << results.size() << std::endl;

    //std::cout << "Local Size: " << local_results.size() << std::endl;
    //std::cout << "Result Size: " << results.size() << std::endl;
}

void Optimizer::models(key_type const & identifier, std::unordered_set< std::shared_ptr<Model>, std::hash< std::shared_ptr<Model> >, std::equal_to< std::shared_ptr<Model> > > & results, float scope) {
    // Shortcircuit model extraction if number of models exceeds given amount
    if (model_limit_exceeded) {
        return;
    }

    models_inner(identifier, results, scope);
}

void Optimizer::models_inner(key_type const & identifier, std::unordered_set< std::shared_ptr<Model>, std::hash< std::shared_ptr<Model> >, std::equal_to< std::shared_ptr<Model> > > & results, float scope) {
    auto task_accessor = this->state.graph.vertices.find(identifier);
    if (task_accessor == this->state.graph.vertices.end()) { return; }
    Task & task = task_accessor -> second;
    //std::cout << "Base Condition: " << task.base_objective() << " <= " << task.upperbound() << " = " << (int)(task.base_objective() <= task.upperbound()) << std::endl;

    // std::cout << "Capture: " << task.capture_set().to_string() << std::endl;

    if (task.base_objective() <= task.upperbound() + std::numeric_limits<float>::epsilon()) {
        // || (Configuration::rule_list && task.capture_set().count() != task.capture_set().size())) {
        // std::cout << "Stump" << std::endl;
        // std::shared_ptr<key_type> stump(new Tile(set));
        // Model stump_key(stump_set); // shallow variant
        // Model * stump_address = new Model(stump_set);
        std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(task.capture_set())), this->state));
        model -> identify(identifier);
        
        model -> translate_self(task.order());
        results.insert(model);
    }   
    auto bounds = this->state.graph.bounds.find(identifier);
    if (bounds == this->state.graph.bounds.end()) { return; }
    for (bound_iterator iterator = bounds -> second.begin(); iterator != bounds -> second.end(); ++iterator) {

        if (std::get<2>(* iterator) > task.upperbound() + std::numeric_limits<float>::epsilon()) { continue; }
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
            std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state));
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
            std::shared_ptr<Model> model(new Model(std::shared_ptr<Bitmask>(new Bitmask(subset)), this->state));
            float leaf_objective = model->loss() + model->complexity();
            right_lowerbound = leaf_objective;
            positives.insert(model);
        } else {
            // might never reach here? 
            continue;
        }

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
                    std::shared_ptr<Model> model(new Model(feature, negative, positive, this->state));
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


