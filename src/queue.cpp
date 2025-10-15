#include "queue.hpp"

Queue::Queue(void) {
    return;
}

Queue::~Queue(void) {
    // Clean up any remaining messages in membership map
    for (auto it = membership.begin(); it != membership.end(); ) {
        message_type* msg = it->first;
        it = membership.unsafe_erase(it);
        delete msg;
    }
    
    // Clean up remaining queue messages
    std::lock_guard<std::mutex> lock(queue_mutex);
    while (!queue.empty()) {
        delete queue.top();
        queue.pop();
    }
}

bool Queue::push(Message const & message) {
    message_type * internal_message = new message_type();
    * internal_message = message;

    // Attempt to copy content into membership set (TBB concurrent_unordered_map is thread-safe)
    auto result = this -> membership.insert(std::make_pair(internal_message, true));
    if (result.second) {
        // Thread-safe insertion into queue
        {
            std::lock_guard<std::mutex> lock(queue_mutex);
            this -> queue.push(internal_message);
        }
        return true;
    } else {
        delete internal_message;
        return false;
    }
}

bool Queue::empty(void) const { 
    std::lock_guard<std::mutex> lock(queue_mutex);
    return this -> queue.empty(); 
}

unsigned int Queue::size(void) const { 
    std::lock_guard<std::mutex> lock(queue_mutex);
    return this -> queue.size(); 
}


bool Queue::pop(Message & message) {
    message_type * internal_message = nullptr;
    
    // Thread-safe pop from queue
    {
        std::lock_guard<std::mutex> lock(queue_mutex);
        if (!this -> queue.empty()) {
            internal_message = this -> queue.top();
            this -> queue.pop();
        }
    }
    
    if (internal_message != nullptr) {
        // Properly erase from membership map to avoid dangling pointers
        auto it = this -> membership.find(internal_message);
        if (it != this -> membership.end()) {
            this -> membership.unsafe_erase(it);
        }
        message = * internal_message;
        delete internal_message;
        return true;
    } else {
        return false;
    }
}
