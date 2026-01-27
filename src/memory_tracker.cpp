#ifdef MEMORY_TRACKING_ENABLED

#include "memory_tracker.hpp"
#include <iostream>
#include <fstream>
#include <mutex>
#include <atomic>
#include <cstdio>

namespace MemoryTracker {
    static std::atomic<size_t> current_usage(0);
    static std::atomic<size_t> peak_usage(0);
    static std::atomic<size_t> allocation_count(0);
    static std::atomic<size_t> deallocation_count(0);
    static std::mutex log_mutex;
    static const size_t LARGE_ALLOCATION_THRESHOLD = 1024 * 1024; // 1MB
    
    void track_allocation(size_t size, const char* file, int line) {
        current_usage.fetch_add(size);
        allocation_count.fetch_add(1);
        
        size_t current = current_usage.load();
        size_t peak = peak_usage.load();
        
        // Update peak usage
        while (current > peak && !peak_usage.compare_exchange_weak(peak, current)) {
            peak = peak_usage.load();
        }
        
        // Log large allocations
        if (size > LARGE_ALLOCATION_THRESHOLD) {
            log_large_allocation(size, file, line);
        }
    }
    
    void track_deallocation(size_t size, const char* file, int line) {
        current_usage.fetch_sub(size);
        deallocation_count.fetch_add(1);
    }
    
    void log_large_allocation(size_t size, const char* file, int line) {
        std::lock_guard<std::mutex> lock(log_mutex);
        std::ofstream log_file("/tmp/treefarmr_memory.log", std::ios::app);
        if (log_file.is_open()) {
            log_file << "LARGE ALLOCATION: " << size << " bytes at " 
                     << file << ":" << line << std::endl;
            log_file.close();
        }
    }
    
    size_t get_current_usage() {
        return current_usage.load();
    }
    
    size_t get_peak_usage() {
        return peak_usage.load();
    }
    
    void reset() {
        current_usage.store(0);
        peak_usage.store(0);
        allocation_count.store(0);
        deallocation_count.store(0);
    }
    
    void print_statistics() {
        std::lock_guard<std::mutex> lock(log_mutex);
        std::ofstream log_file("/tmp/treefarmr_memory.log", std::ios::app);
        if (log_file.is_open()) {
            log_file << "=== Memory Statistics ===" << std::endl;
            log_file << "Current usage: " << current_usage.load() << " bytes" << std::endl;
            log_file << "Peak usage: " << peak_usage.load() << " bytes" << std::endl;
            log_file << "Allocations: " << allocation_count.load() << std::endl;
            log_file << "Deallocations: " << deallocation_count.load() << std::endl;
            log_file << "========================" << std::endl;
            log_file.close();
        }
    }
}

#endif // MEMORY_TRACKING_ENABLED



