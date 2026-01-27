#ifndef MEMORY_TRACKER_H
#define MEMORY_TRACKER_H

#include <cstddef>
#include <string>
#include <atomic>

// Memory tracking utilities for debugging memory issues
// Only enabled in debug builds or when MEMORY_TRACKING_ENABLED is defined

#ifdef MEMORY_TRACKING_ENABLED

namespace MemoryTracker {
    // Track allocation
    void track_allocation(size_t size, const char* file, int line);
    
    // Track deallocation
    void track_deallocation(size_t size, const char* file, int line);
    
    // Log large allocation (>1MB)
    void log_large_allocation(size_t size, const char* file, int line);
    
    // Get current memory usage
    size_t get_current_usage();
    
    // Get peak memory usage
    size_t get_peak_usage();
    
    // Reset statistics
    void reset();
    
    // Print statistics
    void print_statistics();
}

// Macros for tracking
#define TRACK_ALLOC(size) MemoryTracker::track_allocation(size, __FILE__, __LINE__)
#define TRACK_DEALLOC(size) MemoryTracker::track_deallocation(size, __FILE__, __LINE__)
#define LOG_LARGE_ALLOC(size) MemoryTracker::log_large_allocation(size, __FILE__, __LINE__)

#else

// No-op macros when tracking is disabled
#define TRACK_ALLOC(size) ((void)0)
#define TRACK_DEALLOC(size) ((void)0)
#define LOG_LARGE_ALLOC(size) ((void)0)

#endif // MEMORY_TRACKING_ENABLED

#endif // MEMORY_TRACKER_H



