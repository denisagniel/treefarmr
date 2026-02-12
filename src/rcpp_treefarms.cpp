// CRITICAL FIX: Prevent Rcpp global stream initialization during static init
// Undefine RCPP_USE_GLOBAL_ROSTREAM BEFORE including Rcpp.h
#ifdef RCPP_USE_GLOBAL_ROSTREAM
#undef RCPP_USE_GLOBAL_ROSTREAM
#endif

#include <Rcpp.h>
#include <sstream>
#include <string>
#include <memory>
#include <fstream>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <atomic>
#include <ctime>
#include <cstdio>
#include <csignal>
#include <cstdlib>
#include <cassert>

// Define USING_RCPP so optimizer code uses Rcpp::Rcout instead of std::cout
#define USING_RCPP
#include "gosdt.hpp"
#include "state.hpp"
#include "configuration.hpp"
#include <json/json.hpp>

using namespace Rcpp;
using json = nlohmann::json;

// REMOVED: __attribute__((constructor)) functions can cause hangs during static init
// These were debugging checkpoints but can trigger Rcpp stream initialization
// If needed, move logging to R_init_treefarmr() or .onLoad()
// static void __attribute__((constructor)) before_rcpp_treefarms_static_init() {
//     fprintf(stderr, "[TREEFARMR_CHECKPOINT] BEFORE rcpp_treefarms.cpp static initialization\n");
//     fflush(stderr);
//     FILE* f = fopen("/tmp/treefarmr_load.log", "a");
//     if (f) {
//         fprintf(f, "[CHECKPOINT] BEFORE rcpp_treefarms.cpp static initialization\n");
//         fflush(f);
//         fclose(f);
//     }
// }

// Atomic counter for crash-resistant logging
static std::atomic<int> log_counter(0);

// REMOVED: __attribute__((constructor)) functions can cause hangs during static init
// static void __attribute__((constructor)) after_rcpp_treefarms_static_init() {
//     fprintf(stderr, "[TREEFARMR_CHECKPOINT] AFTER rcpp_treefarms.cpp static initialization\n");
//     fflush(stderr);
//     FILE* f = fopen("/tmp/treefarmr_load.log", "a");
//     if (f) {
//         fprintf(f, "[CHECKPOINT] AFTER rcpp_treefarms.cpp static initialization\n");
//         fflush(f);
//         fclose(f);
//     }
// }

// Signal handler to catch crashes and log them
static void crash_handler(int sig) {
    FILE* f = fopen("/tmp/crash_log.txt", "a");
    if (f) {
        fprintf(f, "CRASH: Signal %d received\n", sig);
        fflush(f);
        fclose(f);
    }
    // Re-raise signal to get core dump
    signal(sig, SIG_DFL);
    raise(sig);
}

// Helper function to log with atomic counter when verbose (survives crashes)
static void atomic_log(const std::string& message) {
    if (!Configuration::verbose) return;
    try {
        int counter = log_counter.fetch_add(1);
        FILE* f = fopen("/tmp/crash_log.txt", "a");
        if (f) {
            fprintf(f, "%d: %s\n", counter, message.c_str());
            fflush(f);
            fclose(f);
        }
        fprintf(stderr, "ATOMIC_LOG[%d]: %s\n", counter, message.c_str());
        fflush(stderr);
    } catch (...) {
        fprintf(stderr, "ATOMIC_LOG_FAILED\n");
        fflush(stderr);
    }
}

// Initialize signal handlers on first call
static bool signal_handlers_initialized = false;
static void init_signal_handlers() {
    if (!signal_handlers_initialized) {
        signal(SIGSEGV, crash_handler);
        signal(SIGBUS, crash_handler);
        signal(SIGABRT, crash_handler);
        signal_handlers_initialized = true;
        atomic_log("Signal handlers initialized");
    }
}


// [[Rcpp::export]]
void treefarms_configure_cpp(std::string configuration) {
    try {
        std::istringstream config_stream(configuration);
        GOSDT::configure(config_stream);
    } catch (std::exception& e) {
        Rcpp::stop("C++ exception in configure: " + std::string(e.what()));
    } catch (...) {
        Rcpp::stop("Unknown C++ exception in configure");
    }
}

// [[Rcpp::export]]
std::string treefarms_fit_cpp(std::string data_csv) {
    std::istringstream data_stream(data_csv);
    GOSDT model;
    std::string result;
    model.fit(data_stream, result);
    return result;
}

// [[Rcpp::export]]
double treefarms_time_cpp() {
    return GOSDT::time;
}

// [[Rcpp::export]]
int treefarms_iterations_cpp() {
    return GOSDT::iterations;
}

// [[Rcpp::export]]
int treefarms_size_cpp() {
    return GOSDT::size;
}

// [[Rcpp::export]]
int treefarms_status_cpp() {
    return GOSDT::status;
}

// [[Rcpp::export]]
Rcpp::CharacterVector treefarms_fit_with_config_cpp(std::string data_csv, std::string configuration) {
    json config_json;
    try {
        config_json = json::parse(configuration);
        if (config_json.contains("verbose")) {
            Configuration::verbose = config_json["verbose"].get<bool>();
        }
        if (config_json.contains("loss_function") && config_json["loss_function"] == "log_loss") {
            Configuration::worker_limit = 1;
        }
    } catch (...) {
        // Fall back to stream-based configure later
    }
    init_signal_handlers();
    if (Configuration::verbose) atomic_log("Entered treefarms_fit_with_config_cpp");
    std::string result = "{}";
    try {
        if (Configuration::verbose) atomic_log("Inside try block");
        
        std::istringstream config_stream(configuration);
        GOSDT::configure(config_stream);
        atomic_log("Configuration complete");
        
        // Force single-threaded for log-loss
        // This is a safety check in case the JSON parsing above didn't work
        if (Configuration::loss_function == LOG_LOSS) {
            if (Configuration::worker_limit != 1) {
                atomic_log("WARNING: worker_limit was not 1 for log-loss, correcting to 1");
                Configuration::worker_limit = 1;
            }
            atomic_log("Verified worker_limit=1 for log-loss");
        }
        
        // CRITICAL: Verify worker_limit is valid
        assert(Configuration::worker_limit > 0 && "worker_limit must be > 0 after configuration");
        
        // Fit model and get result
        atomic_log("About to create GOSDT model");
        // CRITICAL: Keep model in scope until after we return
        // The crash might be in the model destructor
        std::string fit_result;
        {
            atomic_log("Creating data stream");
            std::istringstream data_stream(data_csv);
            atomic_log("Creating GOSDT model object");
            GOSDT model;
            atomic_log("About to call model.fit()");
            model.fit(data_stream, fit_result);
            atomic_log("model.fit() returned, result length=" + std::to_string(fit_result.length()));
            // Model destructor will run here, but we've already serialized
        }
        atomic_log("Model destructor completed");
        
        // Verify result string is valid before return
        atomic_log("Verifying result string");
        if (fit_result.empty()) {
            fit_result = "{}";
            atomic_log("Result was empty, set to {}");
        }
        
        // For log-loss, return string directly (same as non-log-loss)
        // Testing showed that direct string passing works fine and avoids file I/O complexity
        if (Configuration::loss_function == LOG_LOSS) {
            atomic_log("Log-loss detected, returning result directly as string (length=" + std::to_string(fit_result.length()) + ")");
            atomic_log("About to create Rcpp::CharacterVector for log-loss");
            Rcpp::CharacterVector result_vec = Rcpp::CharacterVector::create(fit_result);
            atomic_log("Rcpp::CharacterVector created for log-loss, about to return");
            return result_vec;
        }
        
        // For non-log-loss, return directly
        atomic_log("Non-log-loss path, returning result directly (length=" + std::to_string(fit_result.length()) + ")");
        atomic_log("About to create Rcpp::CharacterVector");
        Rcpp::CharacterVector result_vec = Rcpp::CharacterVector::create(fit_result);
        atomic_log("Rcpp::CharacterVector created, about to return");
        return result_vec;
        
    } catch (std::exception& e) {
        atomic_log("EXCEPTION: " + std::string(e.what()));
        Rcpp::Rcout << "ERROR: " << e.what() << std::endl;
        return Rcpp::CharacterVector::create("{}");
    } catch (...) {
        atomic_log("EXCEPTION: Unknown exception");
        Rcpp::Rcout << "ERROR: Unknown exception" << std::endl;
        return Rcpp::CharacterVector::create("{}");
    }
}

// [[Rcpp::export]]
Rcpp::List treefarms_fit_and_stats_cpp(std::string data_csv, std::string configuration) {
    try {
        std::istringstream config_stream(configuration);
        GOSDT::configure(config_stream);
        
        std::istringstream data_stream(data_csv);
        GOSDT model;
        std::string result;
        model.fit(data_stream, result);
        
        return Rcpp::List::create(
            Rcpp::Named("result") = result,
            Rcpp::Named("time") = GOSDT::time,
            Rcpp::Named("iterations") = GOSDT::iterations,
            Rcpp::Named("size") = GOSDT::size,
            Rcpp::Named("status") = GOSDT::status
        );
    } catch (std::exception& e) {
        Rcpp::stop("C++ exception: " + std::string(e.what()));
    } catch (...) {
        Rcpp::stop("Unknown C++ exception occurred");
    }
}

// [[Rcpp::export]]
void cleanup_static_state() {
    // No longer needed - State is now instance-based, not static
    // Each Optimizer instance manages its own State, which is cleaned up automatically
    // when the Optimizer is destroyed
    return;
}

