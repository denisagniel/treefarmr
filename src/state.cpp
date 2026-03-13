#include "state.hpp"
#include <cstdint>  // For uintptr_t

State::State() : dataset(), graph(), queue(), status(0), locals() {
    // Initialize empty state
}

State::~State() {
    // Destructor - cleanup is handled by member destructors
    // Explicit cleanup can be done via reset() if needed
}

void State::initialize(std::istream & data_source, unsigned int workers) {
    // CRITICAL: Validate worker count
    if (workers == 0) {
        throw std::runtime_error("Worker count must be > 0 in State::initialize()");
    }
    assert(workers > 0 && "Worker count must be > 0");
    
    // CRITICAL: Validate stream state before loading dataset
    if (!data_source.good() && !data_source.eof()) {
        throw std::runtime_error("Input stream is not in valid state in State::initialize()");
    }

    dataset.load(data_source);
    
    // CRITICAL: Verify dataset was loaded successfully
    if (dataset.height() == 0) {
        throw std::runtime_error("Dataset height must be > 0 after load()");
    }
    if (dataset.depth() == 0 && Configuration::loss_function != SQUARED_ERROR) {
        throw std::runtime_error("Dataset depth must be > 0 after load()");
    }
    assert(dataset.height() > 0 && "Dataset height must be > 0");
    assert(dataset.width() >= 0 && "Dataset width must be >= 0");
    
    // Clear graph containers
    graph.clear();
    // Reset queue in place instead of assignment (mutex is non-copyable)
    queue.~Queue();
    new (&queue) Queue();

    // CRITICAL: Resize locals vector to match worker count
    locals.resize(workers);
    if (locals.size() != workers) {
        throw std::runtime_error("Failed to resize locals vector to worker count");
    }
    assert(locals.size() == workers && "locals size must match worker count");

    for (unsigned int i = 0; i < workers; ++i) {
        // CRITICAL: Verify dataset dimensions before initializing local state
        unsigned int h = dataset.height();
        unsigned int w = dataset.width();
        unsigned int d = dataset.depth();
        
        if (h == 0 || (d == 0 && Configuration::loss_function != SQUARED_ERROR)) {
            throw std::runtime_error("Invalid dataset dimensions: h=" + std::to_string(h) +
                                   " w=" + std::to_string(w) + " d=" + std::to_string(d));
        }
        assert(h > 0 && w >= 0 && "Dataset dimensions must be valid");

        locals[i].initialize(h, w, d);
    }
}


void State::reset(void) {
    // Clear in reverse dependency order
    locals.clear();  // First: clear thread-local state
    
    queue.~Queue();  // Second: destroy queue
    new (&queue) Queue();
    
    // Third: clear graph containers
    graph.clear();
    
    dataset.clear(); // Last: clear dataset
}

void State::reset_except_dataset(void) {
    // Clear graph containers
    graph.clear();
    
    // Reset queue in place instead of assignment (mutex is non-copyable)
    queue.~Queue();
    new (&queue) Queue();
    
    // CRITICAL: Reinitialize locals with current dataset dimensions
    // This ensures locals is always properly sized and initialized
    // Use the same worker count as before (locals.size() if non-zero, else 1)
    unsigned int workers = (locals.size() > 0) ? locals.size() : 1;
    
    // Verify dataset is valid before initializing locals
    if (dataset.height() > 0 && (dataset.depth() > 0 || Configuration::loss_function == SQUARED_ERROR)) {
        locals.resize(workers);
        for (unsigned int i = 0; i < workers; ++i) {
            locals[i].initialize(dataset.height(), dataset.width(), dataset.depth());
        }
    } else {
        // Dataset not loaded - clear locals to prevent invalid access
        locals.clear();
    }
}

LocalState& State::get_local(unsigned int id) {
    if (id >= locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(locals.size()));
    }
    return locals[id];
}