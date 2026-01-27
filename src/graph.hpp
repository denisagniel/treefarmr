#ifndef GRAPH_H
#define GRAPH_H

#include <iostream>
#include <utility>
#include <vector>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <mutex>

class Graph;

#include "bitmask.hpp"
#include "task.hpp"
#include "tile.hpp"
#include "additive_metrics.hpp"

// #include "sorted_map.hpp"

class ModelSet;
typedef Tile key_type;
typedef Task value_type;
typedef std::vector<int> translation_type;

// Hash implementations for std::unordered_map
// These delegate to the already implemented hash functions and equality operators

// class GraphTranslationHashComparator {
// public:
//     static size_t hash(std::pair<key_type, key_type> const & key) {
//         size_t seed = key.first.hash();
//         seed ^= key.second.hash() + 0x9e3779b9 + (seed << 6) + (seed >> 2);
//         return seed;
//     }
//     static bool equal(std::pair<key_type, key_type> const & left, std::pair<key_type, key_type> const & right) {
//         return left == right;
//     }
// };

class GraphChildHashComparator {
public:
    static size_t hash(std::pair<key_type, int> const & key) {
        size_t seed = key.second;
        seed ^= key.first.hash() + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        return seed;
    }
    static bool equal(std::pair<key_type, int> const & left, std::pair<key_type, int> const & right) {
        return left == right;
    }
};

// Custom hash function for std::pair<Tile, int>
struct PairHash {
    std::size_t operator()(const std::pair<key_type, int>& p) const {
        std::size_t h1 = std::hash<key_type>{}(p.first);
        std::size_t h2 = std::hash<int>{}(p.second);
        return h1 ^ (h2 << 1);
    }
};

// Use std::unordered_map with mutex protection for thread-safety
// Note: Since worker_limit=1 for regression and we're going single-threaded,
// mutexes are defensive but won't contend in practice
typedef std::unordered_map< // Table for storing forward edges
    std::pair<key_type, int>, key_type, PairHash, std::equal_to<std::pair<key_type, int>>> child_table;

typedef std::unordered_map< // Table for storing tile-orderings
    std::pair<key_type, int>, translation_type, PairHash, std::equal_to<std::pair<key_type, int>>> translation_table;

typedef std::unordered_map< // Table for storing vertices
    key_type, value_type, std::hash<key_type>, std::equal_to<key_type>> vertex_table;

typedef std::unordered_map< // Set of parents for a single vertex
    key_type, std::pair<Bitmask, float>, std::hash<Tile>, std::equal_to<Tile>> adjacency_set; 

typedef std::unordered_map< // Table of all adjacency sets
    key_type, adjacency_set, std::hash<key_type>, std::equal_to<key_type>> adjacency_table;

// Use std::vector for bound lists
typedef std::vector<std::tuple<unsigned int, float, float>> bound_list; // List of split-bounds for a single vertex

// A collection of model sets, representing the result or solution of a
// subproblem. The first entry is a *sorted* list of objective values whereas
// the second entry represents a storage 
typedef std::pair<std::set<Objective, ObjectiveLess>, std::unordered_map<Objective, std::shared_ptr<ModelSet>, ObjectiveHash>> results_t; 
typedef std::tuple< float, results_t > scoped_result_t; // A collection of model sets with an associated scope

typedef std::unordered_map< // Table of all bound lists
    key_type, bound_list, std::hash<key_type>, std::equal_to<key_type>> bound_table;

typedef std::unordered_map< // Table of all saved models
    key_type, scoped_result_t, std::hash<key_type>, std::equal_to<key_type>> models_table;

// Iterator types for std::unordered_map
typedef vertex_table::const_iterator const_vertex_accessor;
typedef vertex_table::iterator vertex_accessor;

typedef translation_table::const_iterator const_translation_accessor;
typedef translation_table::iterator translation_accessor;

typedef child_table::const_iterator const_child_accessor;
typedef child_table::iterator child_accessor;

typedef adjacency_table::const_iterator const_adjacency_accessor;
typedef adjacency_table::iterator adjacency_accessor;

typedef adjacency_set::const_iterator const_adjacency_iterator;
typedef adjacency_set::iterator adjacency_iterator;

typedef bound_table::const_iterator const_bound_accessor;
typedef bound_table::iterator bound_accessor;

typedef bound_list::const_iterator const_bound_iterator;
typedef bound_list::iterator bound_iterator;

typedef models_table::const_iterator const_models_accessor;
typedef models_table::iterator models_accessor;

// Container for storing the dependency graph
// The vertices of his graph act as a memoization table of subproblems
// Entries in the table are not necessarily complete, some are still running, paused, or cancelled.
class Graph {
public:
    translation_table translations;
    child_table children;
    vertex_table vertices;
    adjacency_table edges;
    bound_table bounds;
    models_table models;

    // Mutex for thread-safety (defensive, since we're single-threaded)
    mutable std::mutex graph_mutex;

    Graph(void);
    ~Graph(void);

    // bool exists(key_type const & key) const;
    
    // bool insert(key_type const & key, value_type const & value);
    // bool insert(std::pair< key_type, value_type > const & pair);
    // bool connect(key_type const & parent, key_type const & child, float scope);

    // bool find(const_vertex_accessor & accessor, key_type const & key) const;
    // bool find(vertex_accessor & accessor, key_type const & key) const;

    // bool find(const_adjacency_accessor & accessor, key_type const & key, bool forward = true) const;
    // bool find(adjacency_accessor & accessor, key_type const & key, bool forward = true) const;

    // bool find_or_create(const_vertex_accessor & accessor, key_type const & key,
    // Bitmask & buffer_1, Bitmask & buffer_2, Bitmask & buffer_3,
    // Task const & task, unsigned int index, bool condition);

    // bool find_or_create(vertex_accessor & accessor, key_type const & key,
    // Bitmask & buffer_1, Bitmask & buffer_2, Bitmask & buffer_3,
    // Task const & task, unsigned int index, bool condition);

    bool erase(key_type const & key, bool disconnect = true);
    bool disconnect(key_type const & arent, key_type const & child);
    void clear(void);

    unsigned int size(void) const;
};

#endif
