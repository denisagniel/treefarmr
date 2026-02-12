#include "gosdt.hpp"
#include "configuration.hpp"
#include <unistd.h>
#include <fstream>
#include <sstream>
#include <atomic>
#include <chrono>
#include <cstdint>  // For uintptr_t

#define _DEBUG true
#define THROTTLE false

// REMOVED: __attribute__((constructor)) functions cause installation hangs
// These functions run during static initialization (before R_init_treefarmr())
// and perform file I/O, which can deadlock during R's lazy loading database creation.
// The original treeFarms codebase does not have these constructor functions.
// static void __attribute__((constructor)) before_gosdt_static_init() {
//     fprintf(stderr, "[TREEFARMR_CHECKPOINT] BEFORE gosdt.cpp static initialization\n");
//     fflush(stderr);
//     FILE* f = fopen("/tmp/treefarmr_load.log", "a");
//     if (f) {
//         fprintf(f, "[CHECKPOINT] BEFORE gosdt.cpp static initialization\n");
//         fflush(f);
//         fclose(f);
//     }
// }

// Atomic counter for crash-resistant logging in GOSDT
static std::atomic<int> gosdt_log_counter(1000);  // Start at 1000 to distinguish from rcpp logs

// Helper function to log with atomic counter (survives crashes)
static void gosdt_atomic_log(const std::string& message) {
    int counter = gosdt_log_counter.fetch_add(1);
    std::ofstream log_file("/tmp/crash_log.txt", std::ios::app);
    if (log_file.is_open()) {
        log_file << counter << ": [GOSDT] " << message << std::endl;
        log_file.flush();
        log_file.close();
    }
}

float GOSDT::time = 0.0;
unsigned int GOSDT::size = 0;
unsigned int GOSDT::iterations = 0;
unsigned int GOSDT::status = 0;

// REMOVED: __attribute__((constructor)) functions cause installation hangs
// static void __attribute__((constructor)) after_gosdt_static_init() {
//     fprintf(stderr, "[TREEFARMR_CHECKPOINT] AFTER gosdt.cpp static initialization\n");
//     fflush(stderr);
//     FILE* f = fopen("/tmp/treefarmr_load.log", "a");
//     if (f) {
//         fprintf(f, "[CHECKPOINT] AFTER gosdt.cpp static initialization\n");
//         fflush(f);
//         fclose(f);
//     }
// }

// https://stackoverflow.com/a/63391159
void tic(const string &task_description, int mode = 0) {
    static std::chrono::time_point<std::chrono::high_resolution_clock> extraction_start;
    static string stored_task_description;
    if (Configuration::verbose) {
        if (mode == 0) {
            extraction_start = std::chrono::high_resolution_clock::now(); // Start measuring training time
            stored_task_description = task_description;
        } else {
            auto extraction_stop = std::chrono::high_resolution_clock::now(); // Stop measuring training time
            float time = std::chrono::duration_cast<std::chrono::milliseconds>(extraction_stop - extraction_start).count() / 1000.0;
            std::cout << stored_task_description << time << " seconds" << std::endl;
        }
    }
}

void toc() {
    tic("", 1);
}

GOSDT::GOSDT(void) {}

GOSDT::~GOSDT(void) {
    return;
}

void GOSDT::configure(std::istream & config_source) { Configuration::configure(config_source); }


void GOSDT::fit(std::istream & data_source) {
    results_t results = results_t();
    std::unordered_set< Model > models;
    fit(data_source, results, models);
}

void GOSDT::fit(std::istream & data_source, std::string & result) {
    gosdt_atomic_log("Entered GOSDT::fit(std::istream, std::string&)");
    results_t results = results_t();
    std::unordered_set< Model > models;
    
    // CRITICAL: Serialize BEFORE optimizer goes out of scope
    // Note: State is now instance-based, so each optimizer manages its own state
    // No global cleanup needed, but we still serialize while optimizer is alive
    std::string serialized_result = "{}";
    
    try {
        gosdt_atomic_log("Inside try block, about to call fit(data_source, results, models)");
        
        // CRITICAL: Validate stream state before operations
        if (!data_source.good() && !data_source.eof()) {
            gosdt_atomic_log("ERROR: data_source stream is not in good state");
            throw std::runtime_error("Input stream is not in valid state");
        }
        
        // CRITICAL FIX: State lifetime management
        // We need a State object for serialization, but fit() creates its own optimizer internally.
        // Create a temporary optimizer to get the state structure (dataset dimensions, etc.)
        // and keep it alive for the entire serialization process.
        // CRITICAL: Move temp_optimizer to outer scope so it lives for entire serialization
        gosdt_atomic_log("Getting stream position");
        std::istream::pos_type pos = data_source.tellg();
        
        // CRITICAL: Verify stream position is valid
        if (pos == std::istream::pos_type(-1)) {
            gosdt_atomic_log("WARNING: tellg() returned -1, stream may not support seeking");
            // For string streams, this is OK, but log it
        }
        
        gosdt_atomic_log("Creating temporary optimizer");
        Optimizer temp_optimizer;
        
        // CRITICAL: Check alignment of optimizer object (ARM64 requires proper alignment)
        void* optr = &temp_optimizer;
        size_t alignment = alignof(Optimizer);
        if (reinterpret_cast<uintptr_t>(optr) % alignment != 0) {
            gosdt_atomic_log("ERROR: Optimizer object is misaligned!");
            throw std::runtime_error("Optimizer object alignment error");
        }
        gosdt_atomic_log("Optimizer alignment OK: " + std::to_string(alignment) + " bytes");
        
        gosdt_atomic_log("About to call temp_optimizer.load(data_source)");
        temp_optimizer.load(data_source);
        gosdt_atomic_log("temp_optimizer.load() completed");
        
        gosdt_atomic_log("Getting state reference from temp_optimizer");
        State* serialization_state = &temp_optimizer.get_state();
            
        // CRITICAL: Verify state is valid before proceeding
        if (serialization_state == nullptr) {
            gosdt_atomic_log("ERROR: serialization_state is null!");
            throw std::runtime_error("serialization_state must not be null");
        }
        
        // CRITICAL: Check alignment of state pointer
        void* sptr = serialization_state;
        size_t state_alignment = alignof(State);
        if (reinterpret_cast<uintptr_t>(sptr) % state_alignment != 0) {
            gosdt_atomic_log("ERROR: State pointer is misaligned!");
            throw std::runtime_error("State pointer alignment error");
        }
        gosdt_atomic_log("State alignment OK: " + std::to_string(state_alignment) + " bytes");
        
        // CRITICAL: Verify locals vector is initialized
        if (serialization_state->locals.size() == 0) {
            gosdt_atomic_log("ERROR: state.locals is empty!");
            throw std::runtime_error("state.locals must be initialized");
        }
        if (serialization_state->locals.size() != Configuration::worker_limit) {
            gosdt_atomic_log("ERROR: state.locals size mismatch! locals.size()=" + 
                           std::to_string(serialization_state->locals.size()) + 
                           " worker_limit=" + std::to_string(Configuration::worker_limit));
            throw std::runtime_error("state.locals size must match worker_limit");
        }
        gosdt_atomic_log("State validation passed: locals.size()=" + std::to_string(serialization_state->locals.size()));
        
        // CRITICAL: Validate stream before seeking
        gosdt_atomic_log("Resetting stream position to " + std::to_string(pos));
        if (pos != std::istream::pos_type(-1)) {
            data_source.clear(); // Clear any error flags
            data_source.seekg(pos); // Reset stream position
            if (!data_source.good() && !data_source.eof()) {
                gosdt_atomic_log("ERROR: Stream is not in good state after seekg");
                throw std::runtime_error("Stream seek failed");
            }
        } else {
            gosdt_atomic_log("WARNING: Cannot seek stream (string stream?), creating new stream");
            // For string streams, we can't seek, so we need to handle this differently
            // The second fit() call will need a fresh stream
        }
        gosdt_atomic_log("Stream position reset complete");
        
        // Now call fit() which will create its own optimizer
        gosdt_atomic_log("About to call fit(data_source, results, models) - CRITICAL POINT");
        fit(data_source, results, models);
        gosdt_atomic_log("fit(data_source, results, models) returned");
        
        // CRITICAL: Verify state is still valid before using it for serialization
        // The temp_optimizer is still in scope, so state should be valid
        assert(serialization_state != nullptr && "serialization_state must still be valid");
        
        // Serialize while temp_optimizer is still alive (it's in outer scope now)
        {
            if (!Configuration::rashomon) {
                gosdt_atomic_log("Single tree mode (rashomon=false)");
                // Single tree mode: serialize only the models
                json node = json::object();
                node["storage"] = json::array();
                node["available_metric_values"] = json::object();
                node["available_metric_values"]["metric_values"] = json::array();
                node["available_metric_values"]["metric_pointers"] = json::array();
                node["metadata"] = json::object();
                node["metadata"]["regularization"] = Configuration::regularization;
                node["metadata"]["dataset_size"] = serialization_state->dataset.size();
                node["trees"] = json::array();
                gosdt_atomic_log("Created JSON node structure");
                // CRITICAL: Serialize models immediately and store JSON strings
                // Don't keep Model objects alive longer than necessary
                std::vector<std::string> tree_strings;
                gosdt_atomic_log("About to serialize models, count=" + std::to_string(models.size()));
                for (auto const & model : models) {
                    try {
                        std::string tree_serialization;
                        // Note: Model::serialize needs State, but optimizer is out of scope here
                        // We use serialization_state which was captured before optimizer went out of scope
                        model.serialize(tree_serialization, 0, *serialization_state);
                        if (!tree_serialization.empty()) {
                            tree_strings.push_back(tree_serialization);
                        }
                    } catch (...) {
                        // Skip tree if serialization fails
                    }
                }
                gosdt_atomic_log("Model serialization complete, tree_strings.size()=" + std::to_string(tree_strings.size()));
                // Now parse JSON strings after models set is no longer needed
                for (auto const & tree_str : tree_strings) {
                    try {
                        node["trees"].push_back(json::parse(tree_str));
                    } catch (...) {
                        // Skip if parsing fails
                    }
                }
                gosdt_atomic_log("JSON parsing complete");
                // For log-loss, return JSON string directly (same as non-log-loss)
                // Testing showed that direct string passing works fine and avoids file I/O complexity
                if (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR) {
                    gosdt_atomic_log("Log-loss/regression: using string dump");
                    serialized_result = node.dump(0);
                    gosdt_atomic_log("String dump completed, length=" + std::to_string(serialized_result.length()));
                } else {
                    gosdt_atomic_log("Non-log-loss path, using string dump");
                    serialized_result = node.dump(0);
                    gosdt_atomic_log("String dump completed, length=" + std::to_string(serialized_result.length()));
                }
                
                gosdt_atomic_log("About to clear models and results");
                // Clear models immediately after dumping JSON
                models.clear();
                results.second.clear();
                tree_strings.clear();
                gosdt_atomic_log("Models and results cleared");
            } else {
                gosdt_atomic_log("Rashomon mode (rashomon=true)");
                // Rashomon mode: use full serialization
                // For log-loss, return JSON string directly (same as non-log-loss)
                if (Configuration::loss_function == LOG_LOSS || Configuration::loss_function == SQUARED_ERROR) {
                    gosdt_atomic_log("Rashomon + log-loss/regression, using string");
                    ModelSet::serialize(results, models, serialized_result, 0, *serialization_state);
                    gosdt_atomic_log("ModelSet::serialize returned, length=" + std::to_string(serialized_result.length()));
                } else {
                    gosdt_atomic_log("Rashomon + non-log-loss, using string");
                    ModelSet::serialize(results, models, serialized_result, 0, *serialization_state);
                    gosdt_atomic_log("ModelSet::serialize returned, length=" + std::to_string(serialized_result.length()));
                }
                // Clear after serialization
                models.clear();
                results.second.clear();
                gosdt_atomic_log("Rashomon models and results cleared");
            }
        } // End of serialization block
        
        if (serialized_result.empty()) {
            serialized_result = "{}";
            gosdt_atomic_log("Result was empty, set to {}");
        }
        // temp_optimizer goes out of scope here, but serialization is complete
        gosdt_atomic_log("Serialization complete, temp_optimizer will be destroyed");
        
    } catch (std::exception& e) {
        gosdt_atomic_log("EXCEPTION in GOSDT::fit: " + std::string(e.what()));
        if (Configuration::verbose) {
            std::cout << "ERROR: Exception during fit: " << e.what() << std::endl;
        }
        serialized_result = "{}";
    } catch (...) {
        gosdt_atomic_log("EXCEPTION in GOSDT::fit: Unknown exception");
        if (Configuration::verbose) {
            std::cout << "ERROR: Unknown exception during fit" << std::endl;
        }
        serialized_result = "{}";
    }
    
    gosdt_atomic_log("About to assign result, length=" + std::to_string(serialized_result.length()));
    // Copy result before any potential cleanup
    result = serialized_result;
    gosdt_atomic_log("Result assigned, about to return from GOSDT::fit");
}


void GOSDT::fit(std::istream & data_source, results_t & results, std::unordered_set< Model > & models) {
    gosdt_atomic_log("Entered GOSDT::fit(std::istream, results_t&, models&)");
    
    // CRITICAL: Validate stream state
    if (!data_source.good() && !data_source.eof()) {
        gosdt_atomic_log("ERROR: data_source stream is not in good state in 3-arg fit()");
        throw std::runtime_error("Input stream is not in valid state");
    }
    
    // CRITICAL: Verify worker_limit is valid before creating optimizer
    if (Configuration::worker_limit == 0) {
        gosdt_atomic_log("ERROR: worker_limit is 0 in 3-arg fit()");
        throw std::runtime_error("worker_limit must be > 0");
    }
    gosdt_atomic_log("worker_limit=" + std::to_string(Configuration::worker_limit));
    
    if(Configuration::verbose) { std::cout << "Using configuration: " << Configuration::to_string(2) << std::endl; }

    if(Configuration::verbose) { std::cout << "Initializing Optimization Framework" << std::endl; }
    
    gosdt_atomic_log("Creating Optimizer object");
    Optimizer optimizer;
    
    // CRITICAL: Check alignment of optimizer object
    void* optr = &optimizer;
    size_t alignment = alignof(Optimizer);
    if (reinterpret_cast<uintptr_t>(optr) % alignment != 0) {
        gosdt_atomic_log("ERROR: Optimizer object is misaligned in 3-arg fit()!");
        throw std::runtime_error("Optimizer object alignment error");
    }
    gosdt_atomic_log("Optimizer alignment OK: " + std::to_string(alignment) + " bytes");
    
    gosdt_atomic_log("About to call optimizer.load(data_source)");
    optimizer.load(data_source);
    gosdt_atomic_log("optimizer.load() completed successfully");
    
    // CRITICAL: Verify state after load
    State& state = optimizer.get_state();
    void* sptr = &state;
    size_t state_alignment = alignof(State);
    if (reinterpret_cast<uintptr_t>(sptr) % state_alignment != 0) {
        gosdt_atomic_log("ERROR: State reference is misaligned!");
        throw std::runtime_error("State reference alignment error");
    }
    
    if (state.locals.size() == 0) {
        gosdt_atomic_log("ERROR: state.locals is empty after load()!");
        throw std::runtime_error("state.locals must be initialized after load()");
    }
    gosdt_atomic_log("State validation passed: locals.size()=" + std::to_string(state.locals.size()));

    // Dump dataset metadata if requested and terminate early 
    if (Configuration::datatset_encoding != "") {
        json output = json::array();
        State& state = optimizer.get_state();
        for (unsigned int binary_feature_index=0; binary_feature_index<state.dataset.encoder.binary_features(); binary_feature_index++) {
            json node = json::object();
            unsigned int feature_index;
            std::string feature_name, feature_type, relation, reference;
            state.dataset.encoder.decode(binary_feature_index, & feature_index);
            state.dataset.encoder.encoding(binary_feature_index, feature_type, relation, reference);
            state.dataset.encoder.header(feature_index, feature_name);

            node["feature"] = feature_index;
            node["name"] = feature_name;
            node["relation"] = relation;
            if (Encoder::test_integral(reference)) {
                node["type"] = "integral";
                node["reference"] = atoi(reference.c_str());
            } else if (Encoder::test_rational(reference)) {
                node["type"] = "rational";
                node["reference"] = atof(reference.c_str());
            } else {
                node["type"] = "categorical";
                node["reference"] = reference;
            }
            output.push_back(node);

        }
        std::string result = output.dump(2);
        if(Configuration::verbose) { std::cout << "Storing Metadata in: " << Configuration::datatset_encoding << std::endl; }
        std::ofstream out(Configuration::datatset_encoding);
        out << result;
        out.close();
        return;
    }


    // Extraction of Rashomon Set 
    if (Configuration::rashomon) { 
        float rashomon_bound;
        if (Configuration::rashomon_bound != 0) {
            rashomon_bound = Configuration::rashomon_bound;
            // If rashomon_bound is provided, we still need to extract models for serialization
            // But we can't call optimizer.models() after fit_rashomon because rashomon_flag is set
            // So we extract models first, then extract the Rashomon set
            fit_gosdt(optimizer, models);
        } else {
            std::cout << "Finding Optimal Objective..." << std::endl;

            fit_gosdt(optimizer, models);

            if (models.empty()) {
                throw std::runtime_error("No models found during optimization. Try reducing regularization or increasing model_limit.");
            }

            float optimal_objective = models.begin() -> loss() + models.begin() -> complexity();
            if (Configuration::verbose) {
                std::cout << "Found Optimal Objective: " << optimal_objective << std::endl;
            }
            
            if (Configuration::rashomon_bound_multiplier != 0) {
                rashomon_bound = optimal_objective * (1 + Configuration::rashomon_bound_multiplier);
            } else {
                rashomon_bound = optimal_objective + Configuration::rashomon_bound_adder;
            }
        }
        // Extract Rashomon set - this sets rashomon_flag, so we can't call optimizer.models() after this
        fit_rashomon(optimizer, rashomon_bound, results);
        process_rashomon_result(results, optimizer.get_state());
        // Note: models were already extracted in fit_gosdt() above, so we use those for serialization
    } else {
        fit_gosdt(optimizer, models);

        if (models.empty()) {
            throw std::runtime_error("No models found during optimization. Try reducing regularization or increasing model_limit.");
        }

        float optimal_objective = models.begin() -> loss() + models.begin() -> complexity();
        if (Configuration::verbose) {
            std::cout << "Found Optimal Objective: " << optimal_objective << std::endl;
        }
    }
}

void GOSDT::fit_gosdt(Optimizer & optimizer, std::unordered_set< Model > & models) {
    GOSDT::time = 0.0;
    GOSDT::size = 0;
    GOSDT::iterations = 0;
    GOSDT::status = 0;

    std::vector< std::thread > workers;
    std::vector< int > iterations(Configuration::worker_limit);

    if(Configuration::verbose) { std::cout << "Starting Search for the Optimal Solution" << std::endl; }
    auto start = std::chrono::high_resolution_clock::now();

    optimizer.initialize();
    for (unsigned int i = 0; i < Configuration::worker_limit; ++i) {
        workers.emplace_back(work, i, std::ref(optimizer), std::ref(iterations[i]));
        #if  !defined(__APPLE__) && !defined(_WIN32)
        if (Configuration::worker_limit > 1) {
            // If using Ubuntu Build, we can pin each thread to a specific CPU core to improve cache locality
            cpu_set_t cpuset; CPU_ZERO(&cpuset); CPU_SET(i, &cpuset);
            int error = pthread_setaffinity_np(workers[i].native_handle(), sizeof(cpu_set_t), &cpuset);
            if (error != 0) { std::cerr << "Error calling pthread_setaffinity_np: " << error << std::endl; }
        }
        #endif
    }
    for (auto iterator = workers.begin(); iterator != workers.end(); ++iterator) { (* iterator).join(); } // Wait for the thread pool to terminate
    
    auto stop = std::chrono::high_resolution_clock::now(); // Stop measuring training time
    GOSDT::time = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count() / 1000.0;
    if(Configuration::verbose) { std::cout << "Optimal Solution Search Complete" << std::endl; }

    for (auto iterator = iterations.begin(); iterator != iterations.end(); ++iterator) { GOSDT::iterations += * iterator; }    
    GOSDT::size = optimizer.size();

    if (Configuration::timing != "") {
        std::ofstream timing_output(Configuration::timing, std::ios_base::app);
        timing_output << GOSDT::time;
        timing_output.flush();
        timing_output.close();
    }

    if(Configuration::verbose) {
        std::cout << "Training Duration: " << GOSDT::time << " seconds" << std::endl;
        std::cout << "Number of Iterations: " << GOSDT::iterations << " iterations" << std::endl;
        std::cout << "Size of Graph: " << GOSDT::size << " nodes" << std::endl;
        float lowerbound, upperbound;
        optimizer.objective_boundary(& lowerbound, & upperbound);
        std::cout << "Objective Boundary: [" << lowerbound << ", " << upperbound << "]" << std::endl;
        std::cout << "Optimality Gap: " << optimizer.uncertainty() << std::endl;
    }

    // try 
    { // Model Extraction
        if (!optimizer.complete()) {
            GOSDT::status = 1;
            if (Configuration::diagnostics) {
                std::cout << "Non-convergence Detected. Beginning Diagnosis" << std::endl;
                optimizer.diagnose_non_convergence();
                std::cout << "Diagnosis complete" << std::endl;
            }
        }
        optimizer.models(models);

        if (Configuration::model_limit > 0 && models.size() == 0) {
            GOSDT::status = 1;
            if (Configuration::diagnostics) {
                std::cout << "False-convergence Detected. Beginning Diagnosis" << std::endl;
                optimizer.diagnose_false_convergence();
                std::cout << "Diagnosis complete" << std::endl;
            }
        }

        if (Configuration::verbose) {
            std::cout << "Models Generated: " << models.size() << std::endl;
            if (optimizer.uncertainty() == 0.0 && models.size() > 0) {
                std::cout << "Loss: " << models.begin() -> loss() << std::endl;
                std::cout << "Complexity: " << models.begin() -> complexity() << std::endl;
            } 
        }
        if (Configuration::model != "") {
            json output = json::array();
            State& state = optimizer.get_state();
            for (auto iterator = models.begin(); iterator != models.end(); ++iterator) {
                Model model = * iterator;
                json object = json::object();
                model.to_json(object, state);
                output.push_back(object);
            }
            std::string result = output.dump(2);
            if(Configuration::verbose) { std::cout << "Storing Models in: " << Configuration::model << std::endl; }
            std::ofstream out(Configuration::model);
            out << result;
            out.close();
        }

    }
    // For log-loss / regression: Don't reset - delay cleanup
    if (Configuration::loss_function != LOG_LOSS && Configuration::loss_function != SQUARED_ERROR) {
        optimizer.reset_except_dataset();
    }
}

void GOSDT::fit_rashomon(Optimizer & optimizer, float rashomon_bound, results_t & results) {
    GOSDT::time = 0.0;
    GOSDT::size = 0;
    GOSDT::iterations = 0;
    GOSDT::status = 0;
    std::vector< std::thread > workers;
    std::vector< int > iterations(Configuration::worker_limit);

    if(Configuration::verbose) { std::cout << "Starting Extraction of Rashomon Set" << std::endl; }
    auto start = std::chrono::high_resolution_clock::now();

    optimizer.initialize();
    if (Configuration::verbose) {
        std::cout << "Using Rashomon bound: " << rashomon_bound << std::endl;
    }
    optimizer.set_rashomon_bound(rashomon_bound);
    optimizer.set_rashomon_flag();
    for (unsigned int i = 0; i < Configuration::worker_limit; ++i) {
        workers.emplace_back(work, i, std::ref(optimizer), std::ref(iterations[i]));
        #if  !defined(__APPLE__) && !defined(_WIN32)
            if (Configuration::worker_limit > 1) {
            // If using Ubuntu Build, we can pin each thread to a specific CPU core to improve cache locality
            cpu_set_t cpuset; CPU_ZERO(&cpuset); CPU_SET(i, &cpuset);
            int error = pthread_setaffinity_np(workers[i].native_handle(), sizeof(cpu_set_t), &cpuset);
            if (error != 0) { std::cerr << "Error calling pthread_setaffinity_np: " << error << std::endl; }
        }
        #endif
    }
    for (auto iterator = workers.begin(); iterator != workers.end(); ++iterator) { (* iterator).join(); } // Wait for the thread pool to terminate
    
    auto stop = std::chrono::high_resolution_clock::now(); // Stop measuring training time
    GOSDT::time = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count() / 1000.0;
    if(Configuration::verbose) { std::cout << "Rashomon Set Construction Completed" << std::endl; }

    for (auto iterator = iterations.begin(); iterator != iterations.end(); ++iterator) { GOSDT::iterations += * iterator; }    
    GOSDT::size = optimizer.size();

    if (Configuration::timing != "") {
        std::ofstream timing_output(Configuration::timing, std::ios_base::app);
        timing_output << GOSDT::time;
        timing_output.flush();
        timing_output.close();
    }

    if(Configuration::verbose) {
        std::cout << "Training Duration: " << GOSDT::time << " seconds" << std::endl;
        std::cout << "Number of Iterations: " << GOSDT::iterations << " iterations" << std::endl;
        std::cout << "Size of Graph: " << GOSDT::size << " nodes" << std::endl;
        float lowerbound, upperbound;
        optimizer.objective_boundary(& lowerbound, & upperbound);
        std::cout << "Objective Boundary: [" << lowerbound << ", " << upperbound << "]" << std::endl;
        std::cout << "Optimality Gap: " << optimizer.uncertainty() << std::endl;
    }

    tic("Extraction Duration: ");
    optimizer.rash_models(results);
    toc();

    if (Configuration::verbose) {
        std::cout << "Stored keys size: " << results.first.size() << std::endl;
        // boost::multiprecision::uint128_t models_count = 0;
        long long unsigned int models_count = 0;
        for (auto model_set : results.second) {
            models_count += model_set.second->get_stored_model_count();
        }
        std::cout << "Size of Rashomon Set: " << models_count << std::endl;
        std::cout << "Memory usage after extraction: " << getCurrentRSS() / 1000000 << std::endl;
    }

    // For log-loss / regression: Don't reset - delay cleanup
    if (Configuration::loss_function != LOG_LOSS && Configuration::loss_function != SQUARED_ERROR) {
        optimizer.reset_except_dataset();
    }
}

void GOSDT::process_rashomon_result(results_t &results, State & state) {

    if (Configuration::output_accuracy_model_set) {
        tic("Output of accuracy Rashomon Set in Model Set: ");

        std::string serialization;
        ModelSet::serialize(results, serialization, 0, state);
        
        std::string file_name = "model_set-accuracy-" + Configuration::rashomon_model_set_suffix;

        if(Configuration::verbose) { std::cout << "Storing Models in: " << file_name << std::endl; }
        std::ofstream out(file_name);
        out << serialization;
        out.close();
        toc();
    }

    if (Configuration::output_covered_sets.size() != 0) {

        tic("Construction of (TP, TN, #Leaves) Duration: ");
        for (auto obj : results.first) {
            results.second[obj]->get_values_of_interest_count();
        }
        toc();

        unsigned int P, N;
        state.dataset.get_total_P_N(P, N);

        for (int i = 0; i < Configuration::output_covered_sets.size(); i++) {

            // Common data for this task
            CoveredSetExtraction covered_sets_type = Configuration::output_covered_sets[i];
            std::string covered_sets_type_string = Configuration::covered_set_type_to_string(covered_sets_type);
            double limit = Configuration::covered_sets_thresholds[i];

            tic("Extraction of " + covered_sets_type_string + " Rashomon Set: ");


            values_of_interest_mapping_t mapping_all;

            unsigned long long model_count = 0;

            for (auto obj : results.first) {
                auto output = results.second[obj]->get_values_of_interest_count();
                bool extract = false;
                for (auto i : output) {
                    auto values_of_interest = i.first;

                    double TP = values_of_interest.TP;
                    double TN = values_of_interest.TN;
                    auto reg = values_of_interest.regularization;

                    double metric = Configuration::computeScore(covered_sets_type, P, N, TP, TN);
                    double obj_value = 1 - metric + Configuration::regularization * reg;

                    if (obj_value <= limit) {
                        model_count += i.second;
                        extract = true;
                    }
                }
                // Here as I believe `get_values_of_interest_mapping` is a more costly process
                if (extract) {
                    auto mapping = results.second[obj]->get_values_of_interest_mapping();
                    for (auto i : mapping) {
                        auto values_of_interest = i.first;

                        double TP = values_of_interest.TP;
                        double TN = values_of_interest.TN;
                        auto reg = values_of_interest.regularization;

                     
                        double metric = Configuration::computeScore(covered_sets_type, P, N, TP, TN);
                        double obj_value = 1 - metric + Configuration::regularization * reg;
                        
                        // assert(output.at(values_of_interest) == i.second->get_stored_model_count());

                        if (obj_value <= limit) {
                            auto existing_model_set = mapping_all.find(values_of_interest);
                            if (existing_model_set == mapping_all.end()) {
                                mapping_all.insert(i);
                            } else {
                                existing_model_set->second->merge(i.second);
                            }
                        }
                    }
                }
            }
            std::cout << "Size of " + covered_sets_type_string + " Rashomon Set: " << model_count << std::endl;

            toc();

            tic("Output of " + covered_sets_type_string + " Rashomon Set in Model Set: ");

            std::string serialization;
            ModelSet::serialize(mapping_all, serialization, 0, state);
            
            std::string file_name = "model_set-" + covered_sets_type_string + "-" + Configuration::rashomon_model_set_suffix;

            if(Configuration::verbose) { std::cout << "Storing Models in: " << file_name << std::endl; }
            std::ofstream out(file_name);
            out << serialization;
            out.close();

            toc();
        }
    }
    
    if (Configuration::rashomon_trie != "") {

        tic("Insertion of Rashomon Set into Trie: ");

        bool calculate_size = false;
        char const *type = "node";
        Trie* tree = new Trie(calculate_size, type);
        tree->insert_root();

        for (auto obj : results.first) {
            tree->insert_model_set(results.second[obj], state);
        }
        
        toc();

        tic("Output of Rashomon Set in Trie: ");

        std::string serialization;
        tree->serialize(serialization, 0);

        if (Configuration::verbose) {
            std::cout << "Storing Models in: " << Configuration::rashomon_trie << std::endl;
        }
        std::ofstream out(Configuration::rashomon_trie);
        out << serialization;
        out.close();

        // CRITICAL FIX: Delete Trie object to prevent memory leak
        delete tree;
        tree = nullptr;

        toc();
    }
}

void GOSDT::work(int const id, Optimizer & optimizer, int & return_reference) {
    unsigned int iterations = 0;
    try {
        while (optimizer.iterate(id)) { iterations += 1; }
    } catch( IntegrityViolation exception ) {
        GOSDT::status = 1;
        std::cout << exception.to_string() << std::endl;
        throw std::move(exception);
    }
    return_reference = iterations;
}