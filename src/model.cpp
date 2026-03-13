#include "model.hpp"
#include <iomanip>
#include <cmath>

Model::Model(void) {}

Model::Model(std::shared_ptr<Bitmask> capture_set, State & state, unsigned int worker_id) {
    std::string prediction_name, prediction_type, prediction_value;
    float info, potential, min_loss, max_loss;
    unsigned int target_index;
    state.dataset.summary(* capture_set, info, potential, min_loss, max_loss, target_index, worker_id, state);

    this -> _loss = max_loss;
    this -> _complexity = Configuration::regularization;
    this -> capture_set = capture_set;
    this -> terminal = true;

    if (Configuration::loss_function == SQUARED_ERROR) {
        unsigned int n = capture_set -> count();
        if (n == 0) {
            this -> prediction = "0";
        } else {
            double sum_y = 0.0;  // Use double precision for accumulation
            unsigned int count = 0;
            for (unsigned int i = 0; i < state.dataset.height(); ++i) {
                if (capture_set -> get(i)) {
                    double val = state.dataset.get_target_values()[i];
                    if (!std::isfinite(val)) {
                        throw std::runtime_error("Non-finite target value at index " + std::to_string(i));
                    }
                    sum_y += val;
                    count++;
                }
            }
            if (count == 0) {
                throw std::runtime_error("Empty capture set - cannot compute prediction");
            }
            float pred = static_cast<float>(sum_y / count);
            if (!std::isfinite(pred)) {
                throw std::runtime_error("Non-finite prediction: sum=" + std::to_string(sum_y) + ", n=" + std::to_string(count));
            }
            this -> prediction = std::to_string(pred);
        }
        state.dataset.encoder.header(prediction_name);
        this -> name = prediction_name;
        this -> type = "Rational";
        this -> binary_target = 0;
        return;
    }

    state.dataset.encoder.target_value(target_index, prediction_value);
    state.dataset.encoder.header(prediction_name);
    state.dataset.encoder.target_type(prediction_type);

    this -> binary_target = target_index;
    this -> name = prediction_name;
    this -> type = prediction_type;
    this -> prediction = prediction_value;

    unsigned int dataset_depth = state.dataset.depth();
    assert(dataset_depth > 0 && "Dataset depth must be > 0");
    this -> class_distribution.resize(dataset_depth, 0.0);

    try {
        if (state.locals.size() > 0) {
            LocalState& local0 = state.get_local(0);
            state.dataset.get_class_distribution(*capture_set, this -> class_distribution, 0, state);
            float sum = 0.0;
            for (float prob : this -> class_distribution) {
                sum += prob;
                if (prob < 0.0 || prob > 1.0) {
                    throw std::runtime_error("Invalid probability value");
                }
            }
            if (sum < 0.99 || sum > 1.01) {
                throw std::runtime_error("Probabilities do not sum to ~1.0");
            }
        } else {
            throw std::runtime_error("state.locals is empty");
        }
    } catch (const std::exception& e) {
        std::cerr << "Warning: Exception in Model constructor class distribution computation: " << e.what() << std::endl;
        std::cerr << "  Falling back to uniform class distribution." << std::endl;
        float uniform_prob = 1.0 / dataset_depth;
        for (size_t i = 0; i < this -> class_distribution.size(); ++i) {
            this -> class_distribution[i] = uniform_prob;
        }
    } catch (...) {
        std::cerr << "Warning: Unknown exception in Model constructor class distribution computation." << std::endl;
        std::cerr << "  Falling back to uniform class distribution." << std::endl;
        float uniform_prob = 1.0 / dataset_depth;
        for (size_t i = 0; i < this -> class_distribution.size(); ++i) {
            this -> class_distribution[i] = uniform_prob;
        }
    }
}

Model::Model(unsigned int binary_feature_index, std::shared_ptr<Model> negative, std::shared_ptr<Model> positive, State & state, unsigned int worker_id) {
    unsigned int feature_index;
    std::string feature_name, feature_type, relation, reference;
    state.dataset.encoder.decode(binary_feature_index, & feature_index);
    state.dataset.encoder.encoding(binary_feature_index, feature_type, relation, reference);
    state.dataset.encoder.header(feature_index, feature_name);

    this -> binary_feature = binary_feature_index;
    this -> feature = feature_index;
    this -> name = feature_name;
    this -> type = feature_type;
    this -> relation = relation;
    this -> reference = reference;
    this -> negative = negative;
    this -> positive = positive;
    this -> terminal = false;
}

Model::~Model(void) {}

void Model::identify(Tile const & identifier) {
    this -> identifier = identifier;
}

bool Model::identified(void) { return this -> identifier.content().size() > 0; }

void Model::translate_self(translation_type const & translation) {
    this -> self_translator = translation;
}

void Model::translate_negatives(translation_type const & translation) {
    this -> negative_translator = translation;
}

void Model::translate_positives(translation_type const & translation) {
    this -> positive_translator = translation;
}

void Model::_partitions(std::vector< Bitmask * > & addresses) const {
    if (this -> terminal) {
        addresses.push_back(this -> capture_set.get());
    } else {
        this -> negative -> _partitions(addresses);
        this -> positive -> _partitions(addresses);
    }

    return;
};

void Model::partitions(std::vector< Bitmask * > & sorted_addresses) const {
    std::vector< Bitmask * > addresses;
    _partitions(addresses);
    // std::cout << "_partition size: " << addresses.size() << std::endl;
    std::map< unsigned int, Bitmask * > sorted;
    // for (auto it = addresses.begin(); it != addresses.end(); ++it) {
    //     Bitmask * address = * it;
    //     std::cout << "Address: " << address << std::endl;

    //     unsigned int size = address -> size();
    //     for (unsigned int rank = 0; rank < size; ++rank) {
    //         if (address -> get(rank) == 1) {
    //             sorted[rank] = address;
    //             break;
    //         }
    //     }
    // }
    // for (auto it = sorted.begin(); it != sorted.end(); ++it) {
    //     sorted_addresses.push_back(it -> second);
    // }
    for (auto it = addresses.begin(); it != addresses.end(); ++it) {
        sorted_addresses.push_back(* it);
    }
    // std::cout << "partition size: " << sorted_addresses.size() << std::endl;
    return;
};

size_t Model::hash(void) const {
    if (_hash) return _hash;

    size_t seed = 0;
    if (this -> terminal) {
        seed = capture_set.get()->hash();
    } else {
        seed ^= get_feature() * 0x9e3779b9 + (seed<<6) + (seed>>2);
        seed ^= positive->hash() + 0x9e3779b9 + (seed<<6) + (seed>>2);
        seed ^= negative->hash() + 0x9e3779b9 + (seed<<6) + (seed>>2);
    }
    _hash = seed;
    return seed;
}

bool Model::operator==(Model const & other) const {
    if (hash() != other.hash()) {
        return false;
    } else {
        if (terminal != other.terminal) {
            return false;
        } else if (terminal) {
            return get_binary_target() == other.get_binary_target();
        } else if (get_feature() != other.get_feature()) {
            return false;
        } else {
            return (*negative == *other.negative) && (*positive == *other.positive);
        }
    }
}

float Model::loss(void) const {
    if (cached_loss >= 0) {
        return cached_loss;
    }
    float loss;
    // Currently a stub, need to implement
    if (this -> terminal) {
        loss = this -> _loss;
    } else {
        loss = this -> negative -> loss() + this -> positive -> loss();
    }

    // Validate before caching
    if (!std::isfinite(loss)) {
        throw std::runtime_error("Non-finite loss computed in Model::loss()");
    }

    cached_loss = loss;
    return loss;
}

float Model::complexity(void) const {
    if (cached_complexity >= 0) {
        return cached_complexity;
    }
    float complexity;
    // Currently a stub, need to implement
    if (this -> terminal) {
        complexity = this -> _complexity;
    } else {
        complexity = this -> negative -> complexity() + this -> positive -> complexity();
    }

    // Validate before caching
    if (!std::isfinite(complexity)) {
        throw std::runtime_error("Non-finite complexity computed in Model::complexity()");
    }

    cached_complexity = complexity;
    return complexity;
}

void Model::predict(Bitmask const & sample, std::string & prediction) const {
    if (this -> terminal) {
        prediction = this -> prediction;
    } else {
        if (sample.get(this -> binary_feature)) {
            this -> positive -> predict(sample, prediction);
        } else {
            this -> negative -> predict(sample, prediction);
        }
    }
}

void Model::predict_proba(Bitmask const & sample, std::vector<float> & probabilities) const {
    if (this -> terminal) {
        // Use the stored class distribution
        probabilities = this -> class_distribution;
    } else {
        if (sample.get(this -> binary_feature)) {
            this -> positive -> predict_proba(sample, probabilities);
        } else {
            this -> negative -> predict_proba(sample, probabilities);
        }
    }
}

void Model::serialize(std::string & serialization, int const spacing, State & state) const {
    json node = json::object();
    to_json(node, state);
    serialization = spacing == 0 ? node.dump() : node.dump(spacing);
    return;
}

void Model::print_readable(std::ostream & os, int indent) const {
    std::string indent_str(indent, ' ');
    
    if (this -> terminal) {
        // Leaf node
        os << indent_str << "Leaf: ";
        os << "prediction=" << this -> prediction;
        os << ", loss=" << std::fixed << std::setprecision(3) << this -> _loss;
        os << ", complexity=" << std::fixed << std::setprecision(3) << this -> _complexity;
        if (!this -> class_distribution.empty()) {
            os << ", probabilities=[";
            for (size_t i = 0; i < this -> class_distribution.size(); ++i) {
                if (i > 0) os << ", ";
                os << std::fixed << std::setprecision(3) << this -> class_distribution[i];
            }
            os << "]";
        }
        os << std::endl;
    } else {
        // Internal node
        os << indent_str << "Split on feature " << this -> feature;
        if (!this -> name.empty()) {
            os << " (" << this -> name << ")";
        }
        os << " " << this -> relation << " " << this -> reference;
        os << std::endl;
        
        // Print false branch
        os << indent_str << "  If FALSE:" << std::endl;
        if (this -> negative) {
            this -> negative -> print_readable(os, indent + 4);
        }
        
        // Print true branch
        os << indent_str << "  If TRUE:" << std::endl;
        if (this -> positive) {
            this -> positive -> print_readable(os, indent + 4);
        }
    }
}

void Model::intersect(json & src, json & dest) const {

    if (!src[0].is_null() && !dest[0].is_null()) {
        dest[0] = std::max(src[0], dest[0]);
    } else if (!src[0].is_null() && dest[0].is_null()) {
        dest[0] = src[0];
    }
    if (!src[1].is_null() && !dest[1].is_null()) {
        dest[1] = std::min(src[1], dest[1]);
    } else if (!src[1].is_null() && dest[1].is_null()) {
        dest[1] = src[1];
    }
}


void Model::summarize(json & node) const {
    if (node.contains("feature")) {
        summarize(node["true"]);
        summarize(node["false"]);

        // Check feature domain type
        bool integral = node["type"] == "integral";
        bool rational = node["type"] == "rational";
        bool categorical = node ["type"] == "categorical";

        node["children"] = {json::object(), json::object()};
        node["children"][0]["then"] = node["true"];
        node["children"][1]["then"] = node["false"];
        if (integral) {
            node["children"][0]["in"] = { node["reference"], nullptr };
            node["children"][1]["in"] = { nullptr, node["reference"]  };
        } else if (rational) {
            node["children"][0]["in"] = { node["reference"], nullptr };
            node["children"][1]["in"] = { nullptr, node["reference"]  };
        } else if (categorical) {
            node["children"][0]["in"] = node["reference"];
            node["children"][1]["in"] = "default";
        }
        node.erase("reference");
        node.erase("relation");
        node.erase("true");
        node.erase("false");

        json new_children = json::array();
        for (json::iterator it = node["children"].begin(); it != node["children"].end(); ++it) {
            json & condition = (* it)["in"];
            json & child = (* it)["then"];
            if (child.contains("feature") && child["feature"] == node["feature"]) {
                // Child has grand children and child feature matches parent feature
                for (json::iterator sub_it = child["children"].begin(); sub_it != child["children"].end(); ++sub_it) {
                    json & subcondition = (* sub_it)["in"];
                    json & grandchild = (* sub_it)["then"];
                    if (integral || rational) {
                        // Promote grandchild into child
                        json promoted_condition = { subcondition[0], subcondition[1] };
                        intersect(condition, promoted_condition);
                        json promoted_child = { { "in", promoted_condition }, { "then", grandchild } };
                        new_children.push_back(promoted_child);
                    } else if (categorical) {
                        json promoted_child = { { "in", subcondition }, { "then", grandchild } };
                        new_children.push_back(promoted_child);
                    }
                }
            } else { //re-insert
                json unpromoted_child = { { "in", condition }, { "then", child } };
                new_children.push_back(unpromoted_child);
            }
        }
        node["children"] = new_children; // Overwrite previous list fo children
    } else {
        // Is a leaf node
        // No transformation
    }
}

void Model::to_json(json & node, State & state) const {
    _to_json(node);
    node["model_objective"] = loss() + complexity();
    decode_json(node, state);
    // Convert to N-ary
    if (Configuration::non_binary) { summarize(node); }
}

void Model::_to_json(json & node) const {
    if (this -> terminal) {
        if (Configuration::loss_function == SQUARED_ERROR) {
            // For regression: convert string prediction to double
            try {
                double pred_value = std::stod(this -> prediction);
                node["prediction"] = pred_value;
            } catch (...) {
                // Fallback if conversion fails
                node["prediction"] = 0.0;
            }
        } else {
            node["prediction"] = this -> binary_target;
        }
        node["loss"] = this -> _loss;
        node["complexity"] = Configuration::regularization;
        // Only serialize class_distribution for classification models
        // For regression, class_distribution is not initialized
        if (Configuration::loss_function != SQUARED_ERROR && !this -> class_distribution.empty()) {
            json prob_array = json::array();
            for (float prob : this -> class_distribution) {
                prob_array.push_back(prob);
            }
            node["probabilities"] = prob_array;
        }
    } else {
        node["feature"] = this -> binary_feature;
        node["false"] = json::object();
        node["true"] = json::object();
        this -> negative -> _to_json(node["false"]);
        this -> positive -> _to_json(node["true"]);

        // Note: translate_json needs State, but _to_json is called during serialization
        // when State might not be available. For now, we'll skip translation in _to_json
        // and do it in to_json() after decode_json() if needed.
        // TODO: Refactor to pass State through or store necessary info in Model
    }
    return;
}

void Model::translate_json(json & node, translation_type const & main, translation_type const & alternative, State & state) const {
    if (node.contains("prediction")) {
        // index translation to undo any reordering from tile normalization
        int cannonical_index = (int)(node["prediction"]) + state.dataset.width();
        int normal_index = std::distance(main.begin(), std::find(main.begin(), main.end(), cannonical_index));
        int alternative_index = (int)(alternative.at(normal_index)) - state.dataset.width();

        node["prediction"] = alternative_index;
    } else if (node.contains("feature")) {
        // index translation to undo any reordering from tile normalization
        bool flip = false;
        int cannonical_index = node["feature"];
        int normal_index;
        if (std::find(main.begin(), main.end(), cannonical_index) != main.end()) {
            normal_index = std::distance(main.begin(), std::find(main.begin(), main.end(), cannonical_index));
        } else if (std::find(main.begin(), main.end(), -cannonical_index) != main.end()) {
            normal_index = std::distance(main.begin(), std::find(main.begin(), main.end(), -cannonical_index));
            flip = !flip;
        }
        int alternative_index = alternative.at(normal_index);
        if (alternative_index < 0) { flip = !flip; }

        node["feature"] = std::abs(alternative_index);
        translate_json(node["false"], main, alternative, state);
        translate_json(node["true"], main, alternative, state);
        if (flip) {
            node["swap"] = node["true"];
            node["true"] = node["false"];
            node["false"] = node["swap"];
            node.erase("swap");
        }
    }
    return;
}


void Model::decode_json(json & node, State & state) const {
    if (node.contains("prediction")) {
        // For regression (SQUARED_ERROR), prediction is already a double value, not an index
        // Skip encoder lookup and just add the name
        if (Configuration::loss_function == SQUARED_ERROR) {
            std::string prediction_name;
            state.dataset.encoder.header(prediction_name);
            node["name"] = prediction_name;
        } else {
            // For classification: decode prediction index to actual value
            std::string prediction_name, prediction_value;
            state.dataset.encoder.target_value(node["prediction"], prediction_value);
            state.dataset.encoder.header(prediction_name);

            if (Encoder::test_integral(prediction_value)) {
                node["prediction"] = atoi(prediction_value.c_str());
            } else if (Encoder::test_rational(prediction_value)) {
                node["prediction"] = atof(prediction_value.c_str());
            } else {
                node["prediction"] = prediction_value;
            }
            node["name"] = prediction_name;
        }
    } else if (node.contains("feature")) {
        // index decoding from binary feature to original feature space
        unsigned int binary_feature_index = node["feature"];
        unsigned int feature_index;
        std::string feature_name, feature_type, relation, reference;
        state.dataset.encoder.decode(binary_feature_index, & feature_index);
        state.dataset.encoder.encoding(binary_feature_index, feature_type, relation, reference);
        state.dataset.encoder.header(feature_index, feature_name);

        node["feature"] = feature_index;
        node["name"] = feature_name;
        node["relation"] = relation;
        if (Encoder::test_integral(reference)) {
            node["type"] = "integral";
            node["reference"] = atoi(reference.c_str());
        } else if (Encoder::test_rational(reference)) {
            node["type"] = "rational";
            node["reference"] = atof(reference.c_str());
        } else {
            node["type"] = "categorical";
            node["reference"] = reference;
        }

        decode_json(node["false"], state);
        decode_json(node["true"], state);
    }

    return;
}