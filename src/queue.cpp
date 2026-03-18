#include "queue.hpp"
#include <set>
#include <vector>

Queue::Queue(void) {
    return;
}

Queue::~Queue(void) {
    // Messages can be in both membership map AND queue
    // We need to delete each message only once
    // Strategy: Collect all unique message pointers, then delete directly

    // CRITICAL: Use std::set for deduplication. If a message appears in both
    // membership and queue (which can happen), set::insert ensures we only
    // delete each pointer once.
    std::set<message_type*> messages_to_delete;

    // Collect messages from membership map
    {
        std::lock_guard<std::mutex> lock(queue_mutex);
        for (auto it = membership.begin(); it != membership.end(); ++it) {
            message_type* msg = it->first;
            messages_to_delete.insert(msg);
        }

        // Collect messages from queue
        // Create a copy of the queue to iterate over (can't iterate over priority_queue directly)
        std::vector<message_type*> queue_messages;
        while (!queue.empty()) {
            message_type* msg = queue.top();
            queue_messages.push_back(msg);
            queue.pop();
        }
        // Add queue messages to delete set
        for (auto msg : queue_messages) {
            messages_to_delete.insert(msg);
        }
    }

    // Clear membership map (no lock needed, destructor is single-threaded)
    membership.clear();

    // In destructor: delete directly, don't return to pool
    // Pool is about to be destroyed anyway, avoid unnecessary lock contention
    for (auto msg : messages_to_delete) {
        delete msg;
    }
}

bool Queue::push(Message const & message) {
    // Acquire message from pool (reuse or allocate)
    message_type * internal_message = message_pool.acquire();
    * internal_message = message;

    // Thread-safe insertion into membership map and queue
    {
        std::lock_guard<std::mutex> lock(queue_mutex);
        auto result = this -> membership.insert(std::make_pair(internal_message, true));
        if (result.second) {
            this -> queue.push(internal_message);
            return true;
        } else {
            // Collision: release message back to pool
            message_pool.release(internal_message);
            return false;
        }
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

    // Thread-safe pop from queue and erase from membership map
    {
        std::lock_guard<std::mutex> lock(queue_mutex);
        if (!this -> queue.empty()) {
            internal_message = this -> queue.top();
            this -> queue.pop();

            // Erase from membership map to avoid dangling pointers
            auto it = this -> membership.find(internal_message);
            if (it != this -> membership.end()) {
                this -> membership.erase(it);
            }
        }
    }

    if (internal_message != nullptr) {
        message = * internal_message;
        // Release message back to pool for reuse
        message_pool.release(internal_message);
        return true;
    } else {
        return false;
    }
}
