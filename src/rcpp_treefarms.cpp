#include <Rcpp.h>
#include <RcppParallel.h>
#include <sstream>
#include <string>
#include <memory>
#include "gosdt.hpp"

using namespace Rcpp;


// [[Rcpp::export]]
void treefarms_configure_cpp(std::string configuration) {
    try {
        Rcpp::Rcout << "DEBUG: Starting configure" << std::endl;
        
        std::istringstream config_stream(configuration);
        Rcpp::Rcout << "DEBUG: Calling GOSDT::configure" << std::endl;
        GOSDT::configure(config_stream);
        Rcpp::Rcout << "DEBUG: Configure completed" << std::endl;
    } catch (std::exception& e) {
        Rcpp::Rcout << "DEBUG: C++ exception in configure: " << e.what() << std::endl;
        Rcpp::stop("C++ exception in configure: " + std::string(e.what()));
    } catch (...) {
        Rcpp::Rcout << "DEBUG: Unknown C++ exception in configure" << std::endl;
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
std::string treefarms_fit_with_config_cpp(std::string data_csv, std::string configuration) {
    // Configure first
    std::istringstream config_stream(configuration);
    GOSDT::configure(config_stream);
    
    // Then fit
    std::istringstream data_stream(data_csv);
    GOSDT model;
    std::string result;
    model.fit(data_stream, result);
    return result;
}

// [[Rcpp::export]]
Rcpp::List treefarms_fit_and_stats_cpp(std::string data_csv, std::string configuration) {
    try {
        Rcpp::Rcout << "DEBUG: Starting fit_and_stats" << std::endl;
        
        // Reset all static state before each call
        Rcpp::Rcout << "DEBUG: Resetting static state" << std::endl;
        // Note: GOSDT::reset() doesn't exist, static state is managed internally
        
        // Configure first
        Rcpp::Rcout << "DEBUG: Parsing config stream" << std::endl;
        std::istringstream config_stream(configuration);
        
        Rcpp::Rcout << "DEBUG: Calling GOSDT::configure" << std::endl;
        GOSDT::configure(config_stream);
        
        Rcpp::Rcout << "DEBUG: Parsing data stream" << std::endl;
        std::istringstream data_stream(data_csv);
        
        Rcpp::Rcout << "DEBUG: Creating GOSDT model" << std::endl;
        GOSDT model;
        
        Rcpp::Rcout << "DEBUG: Calling model.fit" << std::endl;
        std::string result;
        model.fit(data_stream, result);
        
        Rcpp::Rcout << "DEBUG: Fit completed, returning results" << std::endl;
        
        // Return both result and statistics
        return Rcpp::List::create(
            Rcpp::Named("result") = result,
            Rcpp::Named("time") = GOSDT::time,
            Rcpp::Named("iterations") = GOSDT::iterations,
            Rcpp::Named("size") = GOSDT::size,
            Rcpp::Named("status") = GOSDT::status
        );
    } catch (std::exception& e) {
        Rcpp::Rcout << "DEBUG: C++ exception caught: " << e.what() << std::endl;
        Rcpp::stop("C++ exception: " + std::string(e.what()));
    } catch (...) {
        Rcpp::Rcout << "DEBUG: Unknown C++ exception caught" << std::endl;
        Rcpp::stop("Unknown C++ exception occurred");
    }
}

// [[Rcpp::export]]
void cleanup_static_state() {
    try {
        // Clear all State objects in reverse construction order
        State::locals.clear();
        
        // Manually destroy queue and recreate it (placement new/delete pattern)
        State::queue.~Queue();
        new (&State::queue) Queue();
        
        // Reset graph (clears TBB containers while TBB is still alive)
        State::graph = Graph();
        
        // Clear dataset
        State::dataset.clear();
        
    } catch (...) {
        // Ignore cleanup errors during shutdown
    }
}

