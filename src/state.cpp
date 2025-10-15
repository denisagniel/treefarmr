#include "state.hpp"

Dataset State::dataset = Dataset();
Graph State::graph = Graph();
// Initialize queue using placement new since it contains non-copyable mutex
Queue State::queue;
std::vector< LocalState > State::locals = std::vector< LocalState >();
int State::status = 0;

void State::initialize(std::istream & data_source, unsigned int workers) {
    State::dataset.load(data_source);
    State::graph = Graph();
    // Reset queue in place instead of assignment (mutex is non-copyable)
    State::queue.~Queue();
    new (&State::queue) Queue();
    State::locals.resize(workers);
    for (unsigned int i = 0; i < workers; ++i) {
        State::locals[i].initialize(dataset.height(), dataset.width(), dataset.depth());
    }
}


void State::reset(void) {
    // Clear in reverse dependency order
    State::locals.clear();  // First: clear thread-local state
    
    State::queue.~Queue();  // Second: destroy queue
    new (&State::queue) Queue();
    
    State::graph = Graph(); // Third: reset graph (clears TBB containers)
    
    State::dataset.clear(); // Last: clear dataset
}

void State::reset_except_dataset(void) {
    State::graph = Graph();
    // Reset queue in place instead of assignment (mutex is non-copyable)
    State::queue.~Queue();
    new (&State::queue) Queue();
    // Ensure locals is properly sized before accessing it
    if (locals.size() > 0) {
        for (unsigned int i = 0; i < locals.size(); ++i) {
            State::locals[i].initialize(dataset.height(), dataset.width(), dataset.depth());
        }
    }
}