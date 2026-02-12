#include "optimizer.hpp"

#include "optimizer/diagnosis/false_convergence.hpp"
#include "optimizer/diagnosis/non_convergence.hpp"
#include "optimizer/diagnosis/trace.hpp"
#include "optimizer/diagnosis/tree.hpp"
#include "optimizer/dispatch/dispatch.hpp"
#include "optimizer/extraction/models.hpp"
#include "optimizer/extraction/rash_models.hpp"
#include "configuration.hpp"
#include <cstdint>  // For uintptr_t
#include <limits>

Optimizer::Optimizer(void) {
    return;
}

Optimizer::~Optimizer(void) {
    // Destructor - cleanup is handled by member destructors
    // reset_except_dataset() is called explicitly before this destructor runs if needed
    return;
}



void Optimizer::load(std::istream & data_source) { 
    // CRITICAL: Validate stream state before operations
    if (!data_source.good() && !data_source.eof()) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "ERROR: data_source stream is not in good state in Optimizer::load()" << std::endl;
        #else
        std::cerr << "ERROR: data_source stream is not in good state in Optimizer::load()" << std::endl;
        #endif
        throw std::runtime_error("Input stream is not in valid state");
    }
    
    // CRITICAL: Get stream position before loading (for validation)
    std::istream::pos_type pos_before = data_source.tellg();
    
    // CRITICAL: Verify worker_limit is valid before initializing state
    if (Configuration::worker_limit == 0) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "ERROR: worker_limit is 0, setting to 1" << std::endl;
        #else
        std::cerr << "ERROR: worker_limit is 0, setting to 1" << std::endl;
        #endif
        Configuration::worker_limit = 1;
    }
    assert(Configuration::worker_limit > 0 && "worker_limit must be > 0 after correction");
    
    // CRITICAL: Verify state object alignment before use
    void* sptr = &state;
    size_t state_alignment = alignof(State);
    if (reinterpret_cast<uintptr_t>(sptr) % state_alignment != 0) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "ERROR: State object is misaligned in Optimizer::load()!" << std::endl;
        #else
        std::cerr << "ERROR: State object is misaligned in Optimizer::load()!" << std::endl;
        #endif
        throw std::runtime_error("State object alignment error");
    }
    
    // CRITICAL: Initialize state with validated parameters
    state.initialize(data_source, Configuration::worker_limit);
    
    // CRITICAL: Verify stream state after loading
    std::istream::pos_type pos_after = data_source.tellg();
    if (!data_source.good() && !data_source.eof() && pos_after != std::istream::pos_type(-1)) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "WARNING: Stream state changed after state.initialize()" << std::endl;
        #else
        std::cerr << "WARNING: Stream state changed after state.initialize()" << std::endl;
        #endif
    }
    
    // CRITICAL: Verify state.locals was properly initialized
    if (state.locals.size() == 0) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "ERROR: state.locals is empty after initialize()" << std::endl;
        #else
        std::cerr << "ERROR: state.locals is empty after initialize()" << std::endl;
        #endif
        throw std::runtime_error("state.locals must be initialized after initialize()");
    }
    if (state.locals.size() != Configuration::worker_limit) {
        #ifdef USING_RCPP
        Rcpp::Rcout << "ERROR: state.locals size mismatch! locals.size()=" 
                   << state.locals.size() << " worker_limit=" << Configuration::worker_limit << std::endl;
        #else
        std::cerr << "ERROR: state.locals size mismatch! locals.size()=" 
                  << state.locals.size() << " worker_limit=" << Configuration::worker_limit << std::endl;
        #endif
        throw std::runtime_error("state.locals size must match worker_limit");
    }
    assert(state.locals.size() == Configuration::worker_limit && "locals size must match worker_limit");
}

void Optimizer::reset(void) { state.reset(); }

void Optimizer::reset_except_dataset(void) { 
    active = true;
    state.reset_except_dataset();
}

void Optimizer::set_rashomon_flag(void) { this -> rashomon_flag = true; }
void Optimizer::set_rashomon_bound(float bound) { this -> rashomon_bound = bound; }


void Optimizer::initialize(void) {
    // CRITICAL: Verify state.locals is properly initialized before accessing
    assert(state.locals.size() > 0 && "state.locals must be initialized before calling initialize()");
    assert(0 < state.locals.size() && "Worker ID 0 must be within locals bounds");
    
    // Initialize Profile Output
    if (Configuration::profile != "") {
        std::ofstream profile_output(Configuration::profile);
        profile_output << "iterations,time,lowerbound,upperbound,graph_size,queue_size,explore,exploit";
        profile_output << std::endl;
        profile_output.flush();
    }

    // Initialize Timing State
    this -> start_time = std::chrono::high_resolution_clock::now();

    int const n = state.dataset.height();
    int const m = state.dataset.width();
    
    // CRITICAL: Verify dataset dimensions are valid
    assert(n > 0 && "Dataset height must be > 0");
    assert(m >= 0 && "Dataset width must be >= 0");
    
    // CRITICAL: Verify worker 0 exists before accessing state.locals[0]
    if (state.locals.size() == 0) {
        throw std::runtime_error("State.locals is empty - State must be initialized before calling load()");
    }
    
    // Enqueue for exploration
    LocalState& local0 = state.get_local(0);
    local0.outbound_message.exploration(Tile(), Bitmask(n, true, NULL, Configuration::depth_budget), Bitmask(m, true), 0, std::numeric_limits<float>::max());
    state.queue.push(local0.outbound_message);

    return;
}


void Optimizer::objective_boundary(float * lowerbound, float * upperbound) const {
    * lowerbound = this -> global_lowerbound;
    * upperbound = this -> global_upperbound;

}

float Optimizer::uncertainty(void) const {
    float const epsilon = std::numeric_limits<float>::epsilon();
    float value = this -> global_upperbound - this -> global_lowerbound;
    return value < epsilon ? 0 : value;
}

float Optimizer::elapsed(void) const {
    auto now = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(now - this -> start_time);
    return duration.count() / 1000.0f;
}

bool Optimizer::timeout(void) const {
    return (Configuration::time_limit > 0 && elapsed() > Configuration::time_limit);
}

bool Optimizer::complete(void) const {
    return uncertainty() == 0;
}

unsigned int Optimizer::size(void) const {
    return state.graph.size();
}

bool Optimizer::iterate(unsigned int id) {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    bool update = false;

    if (state.queue.pop(state.locals[id].inbound_message)) {
        update = dispatch(state.locals[id].inbound_message, id);
        switch (state.locals[id].inbound_message.code) {
            case Message::exploration_message: { this -> explore += 1; break; }
            case Message::exploitation_message: { this -> exploit += 1; break; }
        }
    }
    // Worker 0 is responsible for managing ticks and snapshots
    if (id == 0) {
        this -> ticks += 1;

        // snapshots that would need to occur every iteration
        // if (Configuration::trace != "") { this -> diagnostic_trace(this -> ticks, state.locals[id].message); }
        if (Configuration::tree != "") { this -> diagnostic_tree(this -> ticks); }

        // snapshots that can skip unimportant iterations
        if (update || complete() || ((this -> ticks) % (this -> tick_duration)) == 0) { // Periodic check for completion for timeout
            // Update the continuation flag for all threads
            this -> active = !complete() && !timeout() && (Configuration::worker_limit > 1 || state.queue.size() > 0);
            this -> print();
            this -> profile();
        }
        
        std::vector<int> memory_checkpoint = Configuration::memory_checkpoints;
        if (rashomon_flag && exported_idx < memory_checkpoint.size() && getCurrentRSS() > memory_checkpoint[exported_idx] * 1000000) {
            export_models(std::to_string(memory_checkpoint[exported_idx]));
            exported_idx++;
            std::cout << "Memory usage after extraction: " << getCurrentRSS() / 1000000 << std::endl;
        }
    }
    return this -> active;
}

void Optimizer::print(void) const {
    if (Configuration::verbose) { // print progress to standard output
        float lowerbound, upperbound;
        objective_boundary(& lowerbound, & upperbound);
        std::cout <<
            "Time: " << elapsed() <<
            ", Objective: [" << lowerbound << ", " << upperbound << "]" <<
            ", Boundary: " << this -> global_boundary <<
            ", Graph Size: " << state.graph.size() <<
            ", Queue Size: " << state.queue.size() << std::endl;
    }
}

void Optimizer::profile(void) {
    if (Configuration::profile != "") {
        std::ofstream profile_output(Configuration::profile, std::ios_base::app);
        float lowerbound, upperbound;
        objective_boundary(& lowerbound, & upperbound);
        profile_output << this -> ticks << "," << elapsed() << "," <<
            lowerbound << "," << upperbound << "," << state.graph.size() << "," << 
            state.queue.size() << "," << this -> explore << "," << this -> exploit;
        profile_output << std::endl;
        profile_output.flush();
        this -> explore = 0;
        this -> exploit = 0;
    }
}

void Optimizer::export_models(std::string suffix) {
    if (Configuration::rashomon_trie != "") {
        std::unordered_set< Model > models;
        this->models(models);
        bool calculate_size = false;
        char const *type = "node";
        Trie* tree = new Trie(calculate_size, type);
        tree->insert_root();
        for (auto iterator = models.begin(); iterator != models.end(); ++iterator) {
            tree->insert_model(&(*iterator));
        }

        std::string serialization;
        tree->serialize(serialization, 2);
        // std::cout << serialization << std::endl;
        // 
        std::stringstream fmt;
        fmt << Configuration::rashomon_trie << "-" << suffix;
        std::string file_name = fmt.str();

        if(Configuration::verbose) { std::cout << "Storing Models in: " << file_name << std::endl; }
        std::ofstream out(file_name);
        out << serialization;
        out.close();
        
        // CRITICAL FIX: Delete Trie object to prevent memory leak
        delete tree;
        tree = nullptr;
        
        state.graph.models.clear();
    }
}

float Optimizer::cart(Bitmask const & capture_set, Bitmask const & feature_set, unsigned int id) {
    if (!capture_set.valid()) { return std::numeric_limits<float>::max(); }
    Bitmask left(state.dataset.height());
    Bitmask right(state.dataset.height());
    float potential, min_loss, max_loss, base_info;
    unsigned int target_index;
    state.dataset.summary(capture_set, base_info, potential, min_loss, max_loss, target_index, id, state);
    float base_risk = max_loss + Configuration::regularization;

    if (!feature_set.valid()) { return base_risk; }
    if (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR) {
        if (feature_set.empty()) {
            return base_risk;
        }
    } else {
        // Original misclassification loss pruning logic
        if (max_loss - min_loss < Configuration::regularization
            || 1.0 - min_loss < Configuration::regularization
            || (potential < 2 * Configuration::regularization && (1.0 - max_loss) < Configuration::regularization)
            || feature_set.empty()) {
            return base_risk;
        }
    }

    int information_maximizer = -1;
    float information_gain = 0;
    for (int j_begin = 0, j_end = 0; feature_set.scan_range(true, j_begin, j_end); j_begin = j_end) {
        for (int j = j_begin; j < j_end; ++j) {
            float left_info, right_info;
            left = capture_set;
            right = capture_set;
            state.dataset.subset(j, false, left);
            state.dataset.subset(j, true, right);

            if (!left.valid() || !right.valid()) { continue; }
            if (left.empty() || right.empty()) { continue; }

            state.dataset.summary(left, left_info, potential, min_loss, max_loss, target_index, id, state);
            state.dataset.summary(right, right_info, potential, min_loss, max_loss, target_index, id, state);

            float gain = left_info + right_info - base_info;
            if (gain > information_gain) {
                information_maximizer = j;
                information_gain = gain;
            }
        }
    }

    if (information_maximizer == -1) { return base_risk; }

    left = capture_set;
    right = capture_set;
    state.dataset.subset(information_maximizer, false, left);
    state.dataset.subset(information_maximizer, true, right);
    float risk = cart(left, feature_set, id) + cart(right, feature_set, id);
    return std::min(risk, base_risk);
}
