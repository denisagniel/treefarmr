#ifndef GOSDT_H
#define GOSDT_H

#include "graph.hpp"
// SIMDPP architecture definition - conditionally set based on platform
// Note: SIMDPP is currently commented out in the codebase, but this definition
// may be used by other headers, so we set it conditionally to avoid ARM64 issues
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
  // x86/x86_64 architecture
  #ifndef SIMDPP_ARCH_X86_SSE4_1
    #define SIMDPP_ARCH_X86_SSE4_1
  #endif
#elif defined(__aarch64__) || defined(_M_ARM64) || defined(__arm64__)
  // ARM64 architecture (Apple Silicon, etc.)
  // SIMDPP doesn't have ARM64 support in older versions, so we don't define it
  // If SIMDPP is needed, use a version that supports ARM64 or remove SIMDPP usage
  #undef SIMDPP_ARCH_X86_SSE4_1
#else
  // Unknown architecture - don't define SIMDPP architecture
  #undef SIMDPP_ARCH_X86_SSE4_1
#endif

#include <iostream>

#include <thread>
// #include <pthread.h>
// #include <sched.h>
// #include <unistd.h>
#include <chrono>

#include <vector>
#include <string>

// #include <alloca.h>

#include <json/json.hpp>

#include "encoder.hpp"
#include "dataset.hpp"
#include "integrity_violation.hpp"
#include "model.hpp"
#include "optimizer.hpp"

class State; // Forward declaration for State reference parameter

using json = nlohmann::json;

// The main interface of the library
// Note that the algorithm behaviour is modified using the static configuration object using the Configuration class
class GOSDT {
    public:
        GOSDT(void);
        ~GOSDT(void);

        static float time;
        static unsigned int size;
        static unsigned int iterations;
        static unsigned int status;

        // @param config_source: string stream containing a JSON object of configuration parameters
        // @note: See the Configuration class for details about each parameter
        static void configure(std::istream & config_source);

        // @require: The CSV must contain a header.
        // @require: Scientific notation is currently not supported by the parser, use long form decimal notation
        // @require: All rows must have the same number of entries
        // @require: all entries are comma-separated
        // @require: Wrapping quotations are not stripped
        // @param data_source: string containing a CSV of training_data
        void fit(std::istream & data_source);

        // @require: The CSV must contain a header.
        // @require: Scientific notation is currently not supported by the parser, use long form decimal notation
        // @require: All rows must have the same number of entries
        // @require: all entries are comma-separated
        // @require: Wrapping quotations are not stripped
        // @param data_source: string containing a CSV of training_data
        // @modifies result: Contains a JSON array of all optimal models extracted
        void fit(std::istream & data_source, std::string & result);

        // @require: The CSV must contain a header.
        // @require: Scientific notation is currently not supported by the parser, use long form decimal notation
        // @require: All rows must have the same number of entries
        // @require: all entries are comma-separated
        // @require: Wrapping quotations are not stripped
        // @param data_source: string containing a CSV of training_data
        // @modifies results: Set of models extracted from the optimization
        void fit(std::istream & data_source, results_t & results);
        
        // @require: The CSV must contain a header.
        // @require: Scientific notation is currently not supported by the parser, use long form decimal notation
        // @require: All rows must have the same number of entries
        // @require: all entries are comma-separated
        // @require: Wrapping quotations are not stripped
        // @param data_source: string containing a CSV of training_data
        // @modifies results: Set of models extracted from the optimization
        // @modifies models: Set of Model objects (trees) extracted from the optimization
        void fit(std::istream & data_source, results_t & results, std::unordered_set< Model > & models);

        // for Rashomon set construction
        // @param rashomon_bound: 
        void fit_rashomon(Optimizer & optimizer, float rashomon_bound, results_t &results);
        void process_rashomon_result(results_t &results, State & state);

        // for finding the optimal tree
        void fit_gosdt(Optimizer & optimizer, std::unordered_set< Model > & models);

    private:
        // @param id: The worker ID of the current thread
        // @param optimizer: optimizer object which will assign work to the thread
        // @modifies return_reference: reference for returning values to the main thread
        static void work(int const id, Optimizer & optimizer, int & return_reference);
};

#endif
