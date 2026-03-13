#include "task.hpp"
#include "graph.hpp"
#include "state.hpp"

Task::Task(void) {}

Task::Task(Bitmask const & capture_set, Bitmask const & feature_set, unsigned int id, State & state, bool rashomon_flag) {
    this -> _capture_set = capture_set;
    this -> _feature_set = feature_set;
    this -> _worker_id = id;

    if (!capture_set.valid() || !feature_set.valid()) {
        this -> _support = 0.0f;
        this -> _information = 0.0f;
        this -> _base_objective = std::numeric_limits<float>::max();
        this -> _lowerbound = std::numeric_limits<float>::max();
        this -> _upperbound = std::numeric_limits<float>::max();
        this -> _lowerscope = -std::numeric_limits<float>::max();
        this -> _upperscope = std::numeric_limits<float>::max();
        this -> _coverage = 0.0f;
        this -> _rashomon_bound = std::numeric_limits<float>::max();
        this -> _optimal_feature = -1;
        return;
    }

    this -> _support = (float)(capture_set.count()) / (float)(state.dataset.height());
    float const regularization = Configuration::regularization;
    bool terminal = (this -> _capture_set.count() <= 1) || (this -> _feature_set.empty());

    float potential, min_loss, max_loss;
    unsigned int target_index;
    // Careful, the following method modifies capture_set
    state.dataset.summary(this -> _capture_set, this -> _information, potential, min_loss, max_loss, target_index, id, state);

    this -> _base_objective = max_loss + regularization;
    // Calculate initial bounds
    // For misclassification loss: min_loss is 0-1, so min_loss + 2*regularization makes sense
    // For log_loss / squared_error: min_loss == max_loss (entropy or SSE), use base_objective - potential
    float const lowerbound = (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR)
        ? std::max(0.0f, this -> _base_objective - potential)
        : std::min(this -> _base_objective, min_loss + 2 * regularization);
    float const upperbound = this -> _base_objective;
    
    // Debug output for root node initialization
    if (Configuration::verbose && capture_set.count() == capture_set.size()) {
        std::cout << "DEBUG: Root node initialization" << std::endl;
        std::cout << "  Loss function: " << (Configuration::loss_function == LOG_LOSS ? "LOG_LOSS" : (Configuration::loss_function == SQUARED_ERROR ? "SQUARED_ERROR" : "MISCLASSIFICATION")) << std::endl;
        std::cout << "  min_loss: " << min_loss << std::endl;
        std::cout << "  max_loss: " << max_loss << std::endl;
        std::cout << "  potential: " << potential << std::endl;
        std::cout << "  regularization: " << regularization << std::endl;
        std::cout << "  base_objective: " << this -> _base_objective << std::endl;
        std::cout << "  lowerbound: " << lowerbound << std::endl;
        std::cout << "  upperbound: " << upperbound << std::endl;
    }

    // false && rashomon_flag
    if (rashomon_flag) {
        this -> _lowerbound = lowerbound;
        this -> _upperbound = upperbound;
    } else {
        if (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR) {
            // For log-loss (entropy) or squared_error (SSE): min_loss == max_loss
            // We only prune terminal nodes - let the algorithm evaluate actual splits
            if (Configuration::verbose && capture_set.count() == capture_set.size()) {
                std::cout << "DEBUG: Root node pruning logic (LOG_LOSS/SQUARED_ERROR)" << std::endl;
                std::cout << "  terminal: " << terminal << std::endl;
                std::cout << "  potential: " << potential << std::endl;
                std::cout << "  2 * regularization: " << (2 * regularization) << std::endl;
                std::cout << "  lowerbound (calculated): " << lowerbound << std::endl;
                std::cout << "  upperbound (calculated): " << upperbound << std::endl;
            }
            if (terminal) {
                // Terminal node - can only be a leaf
                this -> _lowerbound = this -> _base_objective;
                this -> _upperbound = this -> _base_objective;
                this -> _feature_set.clear();
            } else {
                // Node can be either an internal node or leaf
                // Allow the algorithm to explore splits - actual information gain will be
                // evaluated in prune_features() by comparing split bounds to base_objective
                // Regularization still controls complexity: if information gain < 2*regularization,
                // the split bounds will show that splitting is not beneficial
                this -> _lowerbound = lowerbound;
                this -> _upperbound = upperbound;
                if (Configuration::verbose && capture_set.count() == capture_set.size()) {
                    std::cout << "  Setting bounds to: [" << this -> _lowerbound << ", " << this -> _upperbound << "]" << std::endl;
                }
            }
        } else {
            // Original misclassification loss pruning logic
            if ( (1.0 - min_loss < regularization ) // Insufficient maximum accuracy
                || ( potential < 2 * regularization && (1.0 - max_loss) < regularization) ) // Leaf Support + Incremental Accuracy
            { // Insufficient support and leaf accuracy
                // Node is provably not part of any optimal tree
                this -> _lowerbound = this -> _base_objective;
                this -> _upperbound = this -> _base_objective;
                this -> _feature_set.clear();
            } else if (
                max_loss - min_loss < regularization // Accuracy
                || potential < 2 * regularization // Leaf Support
                || terminal
            ) {
                // Node is provably not an internal node of any optimal tree
                this -> _lowerbound = this -> _base_objective;
                this -> _upperbound = this -> _base_objective;
                this -> _feature_set.clear();
                
            } else {
                // Node can be either an internal node or leaf
                this -> _lowerbound = lowerbound;
                this -> _upperbound = upperbound;
            }
        }
    }

    if (Configuration::depth_budget != 0 && capture_set.get_depth_budget() == 1) {
        // we are using depth constraints, and depth budget is exhausted
        this -> _lowerbound = this -> _base_objective;
        this -> _upperbound = this -> _base_objective;
        this -> _feature_set.clear();
    }

    if (this -> _lowerbound > this -> _upperbound) {
        std::stringstream reason;
        reason << "Invalid Lowerbound (" << this -> _lowerbound << ") or Upperbound (" << this -> _upperbound << ")." << std::endl;
        throw IntegrityViolation("Task::Task", reason.str());
    }
}

float Task::support(void) const { return this -> _support; }

float Task::information(void) const { return this -> _information; }

float Task::base_objective(void) const { return this -> _base_objective; }

float Task::uncertainty(void) const { return std::max((float)(0.0), upperbound() - lowerbound()); }

float Task::lowerbound(void) const { return this -> _lowerbound; }
float Task::upperbound(void) const { return this -> _upperbound; }
float Task::lowerscope(void) const { return this -> _lowerscope; }
float Task::upperscope(void) const { return this -> _upperscope; }
float Task::rashomon_bound(void) const { return this -> _rashomon_bound; }
void Task::set_rashomon_flag(void) { _rashomon_flag = true; return;}
void Task::set_rashomon_bound(float bound) { //std::cout << "set_bound: " << bound << std::endl; 
_rashomon_bound = bound; }


Bitmask const & Task::capture_set(void) const { return this -> _capture_set; }
Bitmask const & Task::feature_set(void) const { return this -> _feature_set; }
unsigned int Task::worker_id(void) const { return this -> _worker_id; }
Tile & Task::identifier(void) { return this -> _identifier; }
std::vector<int> & Task::order(void) { return this -> _order; }

void Task::scope(float new_scope) {
    if (new_scope == 0) { return; }
    new_scope = std::max((float)(0.0), new_scope);
    this -> _upperscope = this -> _upperscope == std::numeric_limits<float>::max() ? new_scope : std::max(this -> _upperscope, new_scope);
    this -> _lowerscope = this -> _lowerscope == -std::numeric_limits<float>::max() ? new_scope : std::min(this -> _lowerscope, new_scope);
}

void Task::prune_feature(unsigned int index) { this -> _feature_set.set(index, false); }

void Task::create_children(unsigned int id, State & state) {    
    // this -> _lowerbound = this -> _base_objective;
    // this -> _upperbound = this -> _base_objective;
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask & buffer = state.locals[id].columns[0];
    bool conditions[2] = {false, true};
    Bitmask const & features = this -> _feature_set;
    for (int j_begin = 0, j_end = 0; features.scan_range(true, j_begin, j_end); j_begin = j_end) {
        for (int j = j_begin; j < j_end; ++j) {
            bool skip = false;
            for (unsigned int k = 0; k < 2; ++k) {
                buffer = this -> _capture_set;
                state.dataset.subset(j, conditions[k], buffer);
                unsigned int buffer_count = buffer.count();
                if (std::min(buffer_count, buffer.size() - buffer_count) <= Configuration::minimum_captured_points) {
                    skip = true;
                    continue;
                }
                Task child(buffer, this -> _feature_set, id, state);
                state.locals[id].neighbourhood[2 * j + k] = child;
            }
            if (skip) { prune_feature(j); }


            // Task & left = State::locals[id].neighbourhood[2 * j];
            // Task & right = State::locals[id].neighbourhood[2 * j + 1];

        }
    }
}

void Task::prune_features(unsigned int id, State & state) {
    if (Configuration::continuous_feature_exchange) { continuous_feature_exchange(id, state); }
    if (Configuration::feature_exchange) { feature_exchange(id, state); }

    // this -> _lowerbound = this -> _base_objective;
    // this -> _upperbound = this -> _base_objective;
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask & buffer = state.locals[id].columns[0];
    bool conditions[2] = {false, true};
    Bitmask const & features = this -> _feature_set;
    int optimal_feature = -1;
    float new_lower = this -> _base_objective;
    float new_upper = this -> _base_objective;
    for (int j_begin = 0, j_end = 0; features.scan_range(true, j_begin, j_end); j_begin = j_end) {
        for (int j = j_begin; j < j_end; ++j) {
            float lower = 0.0, upper = 0.0;
            
            Task & left = state.locals[id].neighbourhood[2 * j];
            Task & right = state.locals[id].neighbourhood[2 * j + 1];

            if (Configuration::rule_list) {
                float lower_negative = left.base_objective() + right.lowerbound();
                float lower_positive = left.lowerbound() + right.base_objective();
                lower = std::min(lower_negative, lower_positive);
                float upper_negative = left.base_objective() + right.upperbound();
                float upper_positive = left.upperbound() + right.base_objective();
                upper = std::min(upper_negative, upper_positive);
            } else {
                lower = left.lowerbound() + right.lowerbound();
                upper = left.upperbound() + right.upperbound();
            }

            // std::cout << "Split: " << j << ", Bound: [" << left.lowerbound() << " + " << right.lowerbound()  << ", " << left.upperbound() << " + " << right.upperbound() << "]" << std::endl;
            // std::cout << "Split: " << j << ", Bound: [" << lower  << ", " << upper << "]" << std::endl;

            // if (this -> _rashomon_flag) { 
            //     if (lower > this -> _rashomon_bound) { continue; } // Hierarchical objective lower bounds for Rashomon set
            // } else if (lower > this -> _upperscope) { continue; } // Hierarchical objective lower bounds
            if (lower > this -> _upperscope) { continue; } // Hierarchical objective lower bounds
            if (upper < new_upper) { optimal_feature = j; }
            new_lower = std::min(new_lower, lower);
            new_upper = std::min(new_upper, upper);
        }
    }
    if (new_lower > this -> _upperscope) { return; }
    
    // Debug output for root node
    if (Configuration::verbose && this -> _capture_set.count() == this -> _capture_set.size()) {
        std::cout << "DEBUG: Root node prune_features()" << std::endl;
        std::cout << "  lowerbound before: " << this -> _lowerbound << std::endl;
        std::cout << "  upperbound before: " << this -> _upperbound << std::endl;
        std::cout << "  new_lower: " << new_lower << std::endl;
        std::cout << "  new_upper: " << new_upper << std::endl;
        std::cout << "  base_objective: " << this -> _base_objective << std::endl;
    }
    
    // For log_loss, the root node's bounds should reflect the actual best objective achievable
    // If splitting reduces the objective (new_upper < base_objective), that's correct
    // The base_objective is just the objective of a single leaf, but splitting might be better
    // So we don't force the bounds to stay at base_objective - we allow them to reflect the actual optimal solution
    
    this -> _lowerbound = new_lower;
    this -> _upperbound = new_upper;
    this -> _optimal_feature = optimal_feature;
    
    // Debug output for root node
    if (Configuration::verbose && this -> _capture_set.count() == this -> _capture_set.size()) {
        std::cout << "  lowerbound after: " << this -> _lowerbound << std::endl;
        std::cout << "  upperbound after: " << this -> _upperbound << std::endl;
    }
}

void Task::continuous_feature_exchange(unsigned int id, State & state) {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask const & features = this -> _feature_set;
    for (auto it = state.dataset.encoder.boundaries.begin(); it != state.dataset.encoder.boundaries.end(); ++it) {
        int start = it -> first, finish = it -> second;
        for (int i = features.scan(start, true), j = features.scan(i + 1, true); j < finish; i = j, j = features.scan(j + 1, true)) {
            float alpha = state.locals[id].neighbourhood[2 * i].lowerbound();
            float beta = state.locals[id].neighbourhood[2 * j].upperbound();
            if (alpha >= beta) { prune_feature(i); }
            if (j >= finish - 1) { break; }
        }

        for (int i = features.rscan(finish - 1, true), j = features.rscan(i - 1, true); j >= start; i = j, j = features.rscan(j - 1, true)) {
            float alpha = state.locals[id].neighbourhood[2 * i + 1].lowerbound();
            float beta = state.locals[id].neighbourhood[2 * j + 1].upperbound();
            if (alpha >= beta) { prune_feature(i); }
            if (j <= start) { break; }
        }
    }
}

void Task::feature_exchange(unsigned int id, State & state) {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask const & features = this -> _feature_set;
    int max = features.size();
    Bitmask & buffer = state.locals[id].columns[0];
    for (int i = features.scan(0, true); i < max; i = features.scan(i + 1, true)) {
        for (int j = features.scan(0, true); j < max; j = features.scan(j + 1, true)) {
            if (i == j) { continue; }
            for (unsigned short k = 0; k < 4; ++k) {
                buffer = this -> _capture_set;
                bool i_sign = (bool)(k & 1);
                bool j_sign = (bool)((k >> 1) & 1);
                state.dataset.subset(i, i_sign, buffer); // population after applying i filter
                int i_count = buffer.count(); 
                state.dataset.subset(j, j_sign, buffer); // population remaining if !j filter is applied
                if (i_count != buffer.count()) { continue; } // implies that i is not a subset of j
                // implies that i IS a subset of j, therefore !j is a subset of !i
                // (since i + !i covers the same set as j + !j)
                float not_i_risk = state.locals[id].neighbourhood[2 * i + (int)(!i_sign)].upperbound();
                float not_j_risk = state.locals[id].neighbourhood[2 * j + (int)(!j_sign)].lowerbound();
                // not_i_risk <= not_j_risk and i IS a subset of j implies that risk_i <= risk_j
                if (not_i_risk <= not_j_risk && features.get(i)) { prune_feature(j); break; }
            }
        }
    }
}

void Task::send_explorers(float new_scope, unsigned int id, State & state) {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    if (!_rashomon_flag && this -> uncertainty() == 0) { return; }
    this -> scope(new_scope);

    float exploration_boundary;
    if (this->_rashomon_flag){
        exploration_boundary = rashomon_bound();
    }
    else {
        exploration_boundary = upperbound();
    }

    if (Configuration::look_ahead) { exploration_boundary = std::min(exploration_boundary, this -> _upperscope); }

    Bitmask const & features = this -> _feature_set;
    for (int j_begin = 0, j_end = 0; features.scan_range(true, j_begin, j_end); j_begin = j_end) {
        for (unsigned int j = j_begin; j < j_end; ++j) {
            Task & left = state.locals[id].neighbourhood[2 * j];
            Task & right = state.locals[id].neighbourhood[2 * j + 1];
            float lower, upper;
            if (Configuration::rule_list) {
                lower = std::min(left.lowerbound() + right.base_objective(), left.base_objective() + right.lowerbound());
                upper = std::min(left.upperbound() + right.base_objective(), left.base_objective() + right.upperbound());
            } else {
                lower = left.lowerbound() + right.lowerbound();
                upper = left.upperbound() + right.upperbound();
            }

            // additional requirement for skipping covered tasks. covered tasks must be unscoped:
            // that is, their upperbound must be strictly less than their scope 

            if (lower > exploration_boundary) { continue; } // Skip children that are out of scope 


            if (!_rashomon_flag && upper <= this -> _coverage) { continue; } // Skip children that have been explored

            if (Configuration::rule_list) {
                send_explorer(left, exploration_boundary - right.base_objective(), -(j + 1), id, state);
                send_explorer(right, exploration_boundary - left.base_objective(), (j + 1), id, state);
            } else {
                send_explorer(left, exploration_boundary - right.lowerbound(), -(j + 1), id, state);
                send_explorer(right, exploration_boundary - left.lowerbound(), (j + 1), id, state);
            }
        }
    }
    this -> _coverage = this -> _upperscope;
}

void Task::send_explorer(Task const & child, float scope, int feature, unsigned int id, State & state) {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    bool send = true;
    auto key = state.graph.children.find(std::make_pair(this -> identifier(), feature));

    if (key != state.graph.children.end()) {
        auto child = state.graph.vertices.find(key -> second);
        if (child != state.graph.vertices.end()) {
            if (scope < child -> second.upperscope()) {
                auto parents = state.graph.edges.find(child -> second.identifier()); // insert backward look-up entry
                if (parents != state.graph.edges.end()) {
                    std::pair<adjacency_iterator, bool> insertion = parents -> second.insert(
                        std::make_pair(this -> identifier(), std::make_pair(Bitmask(state.dataset.width(), false), scope)));
                    insertion.first -> second.first.set(std::abs(feature) - 1, true);
                    insertion.first -> second.second = std::min(insertion.first -> second.second, scope);
                    child -> second.scope(scope);
                    send = false;
                }
            }
        }
    }
    if (send) {
        // Priority calculation: for log_loss / squared_error, use support; for misclassification use support - lowerbound
        float priority = (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR)
            ? this->_support
            : (this->_support - this->_lowerbound);
        state.locals[id].outbound_message.exploration(
            this->_identifier,  // sender tile
            child._capture_set, // recipient capture_set
            this->_feature_set, // recipient feature_set
            feature,            // feature
            scope,              // scope
            priority); // priority
        state.queue.push(state.locals[id].outbound_message);
    }
}


bool Task::update(float lower, float upper, int optimal_feature) {
    bool change = lower != this -> _lowerbound || upper != this -> _upperbound;
    this -> _lowerbound = std::max(this -> _lowerbound, lower);
    // if (this->_rashomon_flag) { this -> _upperbound = this->_rashomon_bound;}
    // else { this -> _upperbound = std::min(this -> _upperbound, upper); }
    this -> _upperbound = std::min(this -> _upperbound, upper); 
    this -> _lowerbound = std::min(this -> _upperbound, this -> _lowerbound);

    this -> _optimal_feature = optimal_feature;

    if (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR) {
        if (this -> _upperbound - this -> _lowerbound <= std::numeric_limits<float>::epsilon()) {
            this -> _lowerbound = this -> _upperbound;
        }
    } else {
        // Original misclassification loss cancellation logic
        if ((Configuration::cancellation && 1.0 - this -> _lowerbound < 0.0)
            || this -> _upperbound - this -> _lowerbound <= std::numeric_limits<float>::epsilon()) {
            this -> _lowerbound = this -> _upperbound;
        }
    }
    return change;
}

std::string Task::inspect(void) const {
    std::stringstream status;
    status << "Capture: " << this -> _capture_set.to_string() << std::endl;
    // status << "  State[SEDRCIT] = " << (int)(sampled()) << (int)(explored()) << (int)(delegated()) << (int)(resolved()) << (int)(cancelled()) << (int)(informed()) << (int)(terminal()) << std::endl;
    status << "  Base: " << this -> _base_objective << ", Bound: [" << this -> _lowerbound << ", " << this -> _upperbound << "]" << std::endl;
    status << "  Coverage: " << this -> _coverage << ", Scope: [" << this -> _lowerscope << ", " << this  -> _upperscope << "]" << std::endl;
    status << "  Feature: " << this -> _feature_set.to_string() << std::endl;
    return status.str();
}