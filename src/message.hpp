#ifndef MESSAGE_H
#define MESSAGE_H

#include <iostream>
#include <vector>
#include <mutex>

#include "bitmask.hpp"
#include "tile.hpp"

// Forward declaration
class Message;

// Thread-safe message pool for reducing allocation overhead
// Pre-allocates messages and reuses them instead of new/delete on every push/pop
class MessagePool {
public:
    MessagePool();
    ~MessagePool();

    // Delete copy constructor and assignment operator (mutexes are not copyable)
    MessagePool(const MessagePool&) = delete;
    MessagePool& operator=(const MessagePool&) = delete;

    // Acquire a message from the pool (allocates new if pool empty)
    Message* acquire();

    // Return a message to the pool for reuse
    void release(Message* msg);

    // Get pool statistics for debugging
    size_t pool_size() const;
    size_t total_allocated() const;

private:
    static constexpr size_t MAX_POOL_SIZE = 1000;  // Prevent unbounded growth
    std::vector<Message*> pool;
    mutable std::mutex pool_mutex;
    size_t total_allocations;
};

// Container for messages in the priority queue
// Messages priority dictates which vertex in the dependency graph will be worked on next
class Message {
public:
    static const char exploration_message = 0b00000000; // 
    static const char exploitation_message = 0b00000001; // 

    Message(void);
    ~Message(void);

    void initialize(unsigned int height, unsigned int width, unsigned int depth);

    // @param sender: A tile used to identify the key to a parent vertex
    // @param recipient_capture: A bitmask indicating the captured points of a child vertex
    // @param recipient_feature: A bitmask indicating the initial features of a child vertex
    // @param feature: an integer indicating the feature used by the parent to produce the child
    // @param scope: a float used to specify the risk tolerance of the parent to the child
    // @param primary, secondar, tertiary: hierarchical priority values used to order messages
    void exploration(
        Tile const & sender, 
        Bitmask const & recipient_capture,
        Bitmask const & recipient_feature,
        int feature,
        float scope,
        float primary = 0, float secondary = 0, float tertiary = 0);

    // @param sender: A tile used to identify the key to a child vertex
    // @param recipient: A tile used to identify the key to a parent vertex
    // @param feature: an integer indicating the feature used by the parent to produce the child
    // @param primary, secondar, tertiary: hierarchical priority values used to order messages
    void exploitation(
        Tile const & sender, 
        Tile const & recipient,
        Bitmask const & features,
        float primary = 0, float secondary = 0, float tertiary = 0);

    // Assignment operator used to transfer ownership of message data
    Message & operator=(Message const & other);

    // Comparison operators used to order messages in the priority queue
    bool operator==(Message const & other) const;
    bool operator<(Message const & other) const;
    bool operator>(Message const & other) const;
    bool operator<=(Message const & other) const;
    bool operator>=(Message const & other) const;

    size_t hash(void) const;

    Tile sender_tile;
    Tile recipient_tile;
    Bitmask recipient_capture;
    Bitmask recipient_feature;

    int feature;
    Bitmask features;
    Bitmask signs;
    float scope;

    char code;
    
private:

    float _primary;
    float _secondary;
    float _tertiary;
};

#endif