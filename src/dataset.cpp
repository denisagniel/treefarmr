#include "dataset.hpp"
#include "state.hpp"
#include "kmeans.hpp"  // For k-means lower bounds
#include <map>         // For grouping equivalent points
#include <cmath>       // For std::isfinite

Dataset::Dataset(void) {}
Dataset::~Dataset(void) {}

Dataset::Dataset(std::istream & data_source) { load(data_source); }

// Loads the binary-encoded data set into precomputed form:
// Step 1: Build bitmasks for each column and row of the dataset, allowing fast parallel operations
// Step 2: Build the cost matrix. Either from values in an input file or a specified mode.
// Step 3: Compute columnar aggregations of the cost matrix to speed up some calculations from K^2 to K
// Step 4: Data set shape is stored
//   The overall shape of the data set is stored for indexing later
void Dataset::load(std::istream & data_source) {
    // Step 1: Construct all rows, features, and targets in binary form
    construct_bitmasks(data_source);

    // Step 2: Initialize the cost matrix
    construct_cost_matrix();

    // Step 3: Build the majority and minority costs based on the cost matrix
    aggregate_cost_matrix();

    // Step 4: Build the majority bitmask indicating whether a point is in the majority group
    construct_majority();
    
    if (Configuration::verbose) {
        std::cout << "Dataset Dimensions: " << height() << " x " << width() << " x " << depth() << std::endl;
    }
    return;
}

void Dataset::clear(void) {
    this -> features.clear();
    this -> targets.clear();
    this -> target_values.clear();
    this -> rows.clear();
    this -> feature_rows.clear();
    this -> target_rows.clear();
    this -> costs.clear();
    this -> match_costs.clear();
    this -> mismatch_costs.clear();
    this -> max_costs.clear();
    this -> min_costs.clear();
    this -> diff_costs.clear();
    this -> majority = Bitmask();
}

void Dataset::construct_bitmasks(std::istream & data_source) {
    this -> encoder = Encoder(data_source);
    std::vector< Bitmask > rows = this -> encoder.read_binary_rows();
    unsigned int number_of_samples = this -> encoder.samples();
    unsigned int number_of_binary_features = this -> encoder.binary_features();
    unsigned int number_of_binary_targets = this -> encoder.binary_targets();
    this -> _size = number_of_samples;

    this -> rows = this -> encoder.read_binary_rows();

    this -> features.resize(number_of_binary_features, number_of_samples);
    this -> feature_rows.resize(number_of_samples, number_of_binary_features);
    this -> targets.resize(number_of_binary_targets, number_of_samples);
    this -> target_rows.resize(number_of_samples, number_of_binary_targets);

    if (Configuration::loss_function == SQUARED_ERROR) {
        this -> target_values = this -> encoder.regression_targets();
    }

    for (unsigned int i = 0; i < number_of_samples; ++i) {
        for (unsigned int j = 0; j < number_of_binary_features; ++j) {
            this -> features[j].set(i, bool(rows[i][j]));
            this -> feature_rows[i].set(j, bool(rows[i][j]));
        }
        for (unsigned int j = 0; j < number_of_binary_targets; ++j) {
            this -> targets[j].set(i, bool(rows[i][number_of_binary_features + j]));
            this -> target_rows[i].set(j, bool(rows[i][number_of_binary_features + j]));
        }
    }
    this -> shape = std::tuple< int, int, int >(this -> rows.size(), this -> features.size(), this -> targets.size());
};

void Dataset::construct_cost_matrix(void) {
    if (Configuration::loss_function == SQUARED_ERROR) {
        return;
    }
    this -> costs.resize(depth(), std::vector< float >(depth(), 0.0));
    if (Configuration::costs != "") { // Customized cost matrix
        std::ifstream input_stream(Configuration::costs);
        parse_cost_matrix(input_stream);
    } else if (Configuration::balance) { // Class-balancing cost matrix
        for (unsigned int i = 0; i < depth(); ++i) {
            for (unsigned int j = 0; j < depth(); ++j) {
                if (i == j) { this -> costs[i][j] = 0.0; continue; }
                unsigned int target_count = this -> targets[j].count();
                if (target_count == 0) {
                    throw std::runtime_error("Target class " + std::to_string(j) + " has no samples");
                }
                this -> costs[i][j] = 1.0 / (float)(depth() * target_count);
            }
        }
    } else { // Default cost matrix
        for (unsigned int i = 0; i < depth(); ++i) {
            for (unsigned int j = 0; j < depth(); ++j) {
                if (i == j) { this -> costs[i][j] = 0.0; continue; }
                this -> costs[i][j] = 1.0 / (float)(height());
            }
        }
    }
}

void Dataset::parse_cost_matrix(std::istream & input_stream) {
    // Parse given cost matrix
    io::LineReader input("", input_stream);
    unsigned int line_index = 0;
    std::unordered_map< std::string, unsigned int > reference_to_decoded;
    std::vector< std::vector< float > > table;
    while (char * line = input.next_line()) {
        std::stringstream stream(line);
        std::string token;
        std::vector< std::string > row;
        std::vector< float > parsed_row;
        while (stream.good()) {
            getline(stream, token, ',');
            row.emplace_back(token);
        }
        if (row.empty()) { continue; }
        if (line_index == 0) {
            for (unsigned int j = 0; j < row.size(); ++j) { reference_to_decoded[row[j]] = j; }
        } else {
            for (unsigned int j = 0; j < row.size(); ++j) { parsed_row.emplace_back(atof(row[j].c_str())); }
            table.emplace_back(parsed_row);
        }
        ++line_index;
    }

    std::vector< std::string > encoded_to_reference;
    for (unsigned int j = 0; j < depth(); ++j) {
        std::string type, relation, reference;
        encoder.encoding(width() + j, type, relation, reference);
        encoded_to_reference.emplace_back(reference);
    }

    if (table.size() == 1) {
        for (unsigned int i = 0; i < depth(); ++i) {
            for (unsigned int j = 0; j < depth(); ++j) {
                if (i == j) { this -> costs[i][j] = 0.0; continue; }
                if (reference_to_decoded.find(encoded_to_reference[j]) == reference_to_decoded.end()) {
                    std::cout << "No cost specified for class = " << encoded_to_reference[j] << std::endl;
                    exit(1);
                }
                unsigned int _i = 0;
                unsigned int _j = reference_to_decoded[encoded_to_reference[j]];
                this -> costs[i][j] = table[_i][_j];
            }
        }
    } else {
        for (unsigned int i = 0; i < depth(); ++i) {
            for (unsigned int j = 0; j < depth(); ++j) {
                if (reference_to_decoded.find(encoded_to_reference[i]) == reference_to_decoded.end() || reference_to_decoded.find(encoded_to_reference[j]) == reference_to_decoded.end()) {
                    std::cout << "No cost specified for prediction = " << encoded_to_reference[i] << ", class = " << encoded_to_reference[j] << std::endl;
                    exit(1);
                }
                unsigned int _i = reference_to_decoded[encoded_to_reference[i]];
                unsigned int _j = reference_to_decoded[encoded_to_reference[j]];
                this -> costs[i][j] = table[_i][_j];
            }
        }
    }
};

void Dataset::aggregate_cost_matrix(void) {
    if (Configuration::loss_function == SQUARED_ERROR) {
        return;
    }
    this -> match_costs.resize(depth(), 0.0);
    this -> mismatch_costs.resize(depth(), std::numeric_limits<float>::max());
    this -> max_costs.resize(depth(), -std::numeric_limits<float>::max());
    this -> min_costs.resize(depth(), std::numeric_limits<float>::max());
    this -> diff_costs.resize(depth(), std::numeric_limits<float>::max());
    for (unsigned int j = 0; j < depth(); ++j) {
        for (unsigned int i = 0; i < depth(); ++i) {
            this -> max_costs[j] = std::max(this -> max_costs[j], this -> costs[i][j]);
            this -> min_costs[j] = std::min(this -> min_costs[j], this -> costs[i][j]);
            if (i == j) { this -> match_costs[j] = this -> costs[i][j]; continue; }
            this -> mismatch_costs[j] = std::min(this -> mismatch_costs[j], this -> costs[i][j]);
        }
    }
    for (unsigned int j = 0; j < depth(); ++j) {
        this -> diff_costs[j] = this -> max_costs[j] - this -> min_costs[j] ;
    }
}

void Dataset::construct_majority(void) {
    if (Configuration::loss_function == SQUARED_ERROR) {
        return;
    }
    std::vector< Bitmask > keys(height(), width());
    for (unsigned int i = 0; i < height(); ++i) {
        for (unsigned int j = 0; j < width(); ++j) {
            keys[i].set(j, bool(this -> rows[i][j]));
        }
    }

    // Step 1: Construct a map from the binary features to their distributions
    std::unordered_map< Bitmask, std::vector< float > > distributions;
    for (unsigned int i = 0; i < height(); ++i) {
        Bitmask const & key = keys.at(i);
        // Initialize the map and resize the value (of type vector) to the number of unique labels
        // This way the vector can hold the label distribution for this feature combination
        if (distributions[key].size() < depth()) { distributions[key].resize(depth(), 0.0); }
        for (unsigned int j = 0; j < depth(); ++j) {
            distributions[key][j] += (float)rows[i][width() + j];
        }
    }

    // Step 2: Construct a map from the binary features to cost minimizers
    std::unordered_map< Bitmask, unsigned int  > minimizers;
    for (auto it = distributions.begin(); it != distributions.end(); ++it) {
        Bitmask const & key = it -> first;
        std::vector< float > const & distribution = it -> second;
        float minimum = std::numeric_limits<float>::max();
        unsigned int minimizer = 0;
        for (unsigned int i = 0; i < depth(); ++i) {
            float cost = 0.0;
            for (unsigned int j = 0; j < depth(); ++j) {
                cost += this -> costs[i][j] * distribution.at(j); // Cost of predicting i when the class is j
            }
            if (cost < minimum) {
                minimum = cost;
                minimizer = i;
            }
        }
        minimizers.emplace(key, minimizer);
    }

    // Step 3: Set the bits associated with each minimizer
    this -> majority.initialize(height());
    for (unsigned int i = 0; i < height(); ++i) {
        Bitmask const & key = keys.at(i);
        unsigned int minimizer = minimizers[key];
        unsigned int label = this -> rows[i].scan(width(), true) - width();
        this -> majority.set(i, minimizer == label); // Set this bit true if the label matches this minimizer
    }
}

float Dataset::distance(Bitmask const & set, unsigned int i, unsigned int j, unsigned int id, State & state) const {
    // CRITICAL: Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask & buffer = state.locals[id].columns[0];
    float positive_distance = 0.0, negative_distance = 0.0;
    for (unsigned int k = 0; k < depth(); ++k) {
        buffer = this -> features[i];
        this -> features[j].bit_xor(buffer, false);
        set.bit_and(buffer);
        // this -> majority.bit_and(buffer, false);
        this -> targets[k].bit_and(buffer);
        positive_distance += this -> diff_costs[k] * buffer.count();

        buffer = this -> features[i];
        this -> features[j].bit_xor(buffer, true);
        set.bit_and(buffer);
        // this -> majority.bit_and(buffer, false);
        this -> targets[k].bit_and(buffer);
        negative_distance += this -> diff_costs[k] * buffer.count();
    }
    return std::min(positive_distance, negative_distance);
}

// @param feature_index: selects the feature on which to split
// @param positive: determines whether to provide the subset that tests positive on the feature or tests negative on the feature
// @param set: pointer to bit blocks which indicate the original set before splitting
// @modifies set: set will be modified to indicate the positive or negative subset after splitting
// @notes the set in question is an array of the type bitblock. this allows us to specify the set using a stack-allocated array
void Dataset::subset(unsigned int feature_index, bool positive, Bitmask & set) const {
    // Performs bit-wise and between feature and set with possible bit-flip if performing negative test
    this -> features[feature_index].bit_and(set, !positive);
    if (Configuration::depth_budget != 0){ set.set_depth_budget(set.get_depth_budget()-1);} //subproblems have one less depth_budget than their parent
}

void Dataset::subset(unsigned int feature_index, Bitmask & negative, Bitmask & positive) const {
    // Performs bit-wise and between feature and set with possible bit-flip if performing negative test
    this -> features[feature_index].bit_and(negative, true);
    this -> features[feature_index].bit_and(positive, false);
    if (Configuration::depth_budget != 0){
        negative.set_depth_budget(negative.get_depth_budget()-1);
        positive.set_depth_budget(positive.get_depth_budget()-1);
    } //subproblems have one less depth_budget than their parent
}

void Dataset::summary(Bitmask const & capture_set, float & info, float & potential, float & min_loss, float & max_loss, unsigned int & target_index, unsigned int id, State & state) const {
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    if (Configuration::loss_function == SQUARED_ERROR) {
        unsigned int n = capture_set.count();
        info = 0.0f;
        potential = 0.0f;
        target_index = 0;
        if (n == 0) {
            min_loss = 0.0f;
            max_loss = 0.0f;
            return;
        }
        float sum_y = 0.0f;
        for (unsigned int i = 0; i < height(); ++i) {
            if (capture_set.get(i)) {
                sum_y += this -> target_values[i];
            }
        }
        if (n == 0) {
            throw std::runtime_error("Cannot compute mean of empty sample");
        }
        float mean_y = sum_y / (float)n;
        if (!std::isfinite(mean_y)) {
            throw std::runtime_error("Non-finite mean: sum=" + std::to_string(sum_y) + ", n=" + std::to_string(n));
        }
        float sse = 0.0f;
        for (unsigned int i = 0; i < height(); ++i) {
            if (capture_set.get(i)) {
                float d = this -> target_values[i] - mean_y;
                sse += d * d;
            }
        }
        min_loss = sse;
        max_loss = sse;

        // Compute lower bounds for regression (n > 1 required for meaningful bounds)
        if (n > 1) {
            // ALWAYS compute equivalent points bound (OSRT approach when k_cluster=false)
            // Group samples by feature vector - samples with identical features
            // must receive the same prediction, so within-group variance is unavoidable
            std::map<Bitmask, std::vector<float>> equiv_groups;

            for (unsigned int i = 0; i < height(); ++i) {
                if (capture_set.get(i)) {
                    // feature_rows[i] is a Bitmask containing all binary feature values for sample i
                    equiv_groups[this -> feature_rows[i]].push_back(this -> target_values[i]);
                }
            }

            // Compute equivalent points lower bound (sum of within-group variances)
            // This is the MINIMUM achievable loss because samples with same features
            // must get the same prediction, so variance within each group is unavoidable
            std::vector<double> weights, values;
            float equiv_points_loss = 0.0f;  // Sum of within-group SSE

            // C++11 compatible iteration (no structured bindings)
            for (auto it = equiv_groups.begin(); it != equiv_groups.end(); ++it) {
                const std::vector<float> & targets = it->second;
                double w = (double)targets.size();
                double sum = 0.0;
                double sum_sq = 0.0;

                // Compute sum for mean
                for (size_t j = 0; j < targets.size(); ++j) {
                    sum += targets[j];
                }
                double mean = sum / w;

                // Two-pass algorithm for numerically stable variance computation
                // Avoids catastrophic cancellation from sum_sq - w*mean^2
                double within_group_sse = 0.0;
                for (size_t j = 0; j < targets.size(); ++j) {
                    double diff = targets[j] - mean;
                    within_group_sse += diff * diff;
                }
                equiv_points_loss += within_group_sse;

                // Store for optional k-means
                weights.push_back(w);
                values.push_back(mean);
            }

            // Use equiv_points_loss as base lower bound
            float min_achievable_loss = equiv_points_loss;

            // If k-means enabled, try to get an even tighter bound
            if (Configuration::k_cluster) {
                // Call k-Means solver with regularization=0 to get minimum achievable SSE
                // k-Means can further reduce loss by optimally clustering the equivalent points
                ldouble kmeans_sse = compute_kmeans_lower_bound(
                    values,
                    weights,
                    0.0  // regularization = 0 to get pure SSE lower bound
                );

                // k-Means bound includes both:
                // - Between-cluster SSE (from k-means on aggregated points)
                // - Within-cluster SSE (from equiv_points_loss)
                min_achievable_loss = (float)(kmeans_sse + equiv_points_loss);
            }

            // Safeguard: bound should never exceed current SSE
            if (min_achievable_loss > sse || min_achievable_loss < 0.0f) {
                // Bound is invalid - fall back to equiv points only
                min_achievable_loss = equiv_points_loss;
            }

            // Ensure bound is non-negative and doesn't exceed current loss
            min_achievable_loss = std::max(0.0f, std::min(min_achievable_loss, sse));

            // potential = maximum possible reduction in loss
            potential = sse - min_achievable_loss;

            // Update min_loss to reflect lower bound
            min_loss = min_achievable_loss;
        } else {
            // n <= 1: no room for improvement (single sample or empty)
            potential = 0.0f;
        }

        return;
    }
    Bitmask & buffer = state.locals[id].columns[0];
    // Use heap allocation instead of alloca for thread safety
    std::vector<unsigned int> distribution(depth(), 0); // The frequencies of each class
    for (int j = depth(); --j >= 0;) {
        buffer = capture_set; // Set representing the captured points
        this -> targets.at(j).bit_and(buffer); // Captured points with label j
        distribution[j] = buffer.count(); // Calculate frequency
    }

    float min_cost = std::numeric_limits<float>::max();
    unsigned int cost_minimizer = 0;

    if (Configuration::loss_function == LOG_LOSS) {
        // For log-loss, we need to calculate cross-entropy loss
        float total_points = (float)capture_set.count();
        // CRITICAL: Validate total_points early to prevent division by zero
        if (total_points > 0.0f) {
            const float eps = 1e-12f; // Small epsilon to prevent log(0) and ensure numerical stability
            float log_loss = 0.0f;
            for (int j = depth(); --j >= 0;) {
                float raw_prob = distribution[j] / total_points;
                // Clamp probability to [eps, 1-eps] to prevent log(0) and ensure numerical stability
                // This prevents NaN/Inf from log(0) or log(1) in edge cases
                float prob = (raw_prob < eps) ? eps : ((raw_prob > 1.0f - eps) ? 1.0f - eps : raw_prob);
                // Cross-entropy loss: -sum(p_i * log(p_i))
                // Using clamped prob ensures log(prob) is always finite
                log_loss -= prob * log(prob);
            }
            min_cost = log_loss * total_points; // Scale by number of points
            cost_minimizer = 0; // For log-loss, we don't have a single prediction
            
            // Debug output for root node
            if (Configuration::verbose && capture_set.count() == capture_set.size()) {
                std::cout << "DEBUG: Log-loss calculation for root node" << std::endl;
                std::cout << "  total_points: " << total_points << std::endl;
                std::cout << "  entropy (log_loss): " << log_loss << std::endl;
                std::cout << "  min_cost (entropy * total_points): " << min_cost << std::endl;
                for (int j = depth(); --j >= 0;) {
                    float prob = distribution[j] / total_points;
                    std::cout << "  class " << j << " count: " << distribution[j] << ", prob: " << prob << std::endl;
                }
            }
        }
    } else {
        // Original misclassification loss calculation
        for (int i = depth(); --i >= 0;) { // Prediction index
            float cost = 0.0; // accumulator for the cost of making this prediction
            for (int j = depth(); --j >= 0;) { // Class index
                cost += this -> costs.at(i).at(j) * distribution[j]; // cost of prediction-class combination
            }
            if (cost < min_cost) { // track the prediction that minimizes cost
                min_cost = cost;
                cost_minimizer = i;
            }
        }
    }

    float max_cost_reduction = 0.0;
    float equivalent_point_loss = 0.0;
    float support = (float)(capture_set.count()) / (float)(height());
    float information = 0.0;

    if (Configuration::loss_function == LOG_LOSS) {
        // For log-loss, calculate potential reduction using entropy
        float total_points = (float)capture_set.count();
        // CRITICAL: Validate total_points early to prevent division by zero
        if (total_points > 0.0f) {
            const float eps = 1e-12f; // Small epsilon for numerical stability
            float entropy = 0.0f;
            for (int j = depth(); --j >= 0;) {
                float raw_prob = distribution[j] / total_points;
                // Clamp probability to [eps, 1-eps] to prevent log(0) and ensure numerical stability
                float prob = (raw_prob < eps) ? eps : ((raw_prob > 1.0f - eps) ? 1.0f - eps : raw_prob);
                // Entropy: -sum(p_i * log(p_i))
                entropy -= prob * log(prob);
            }
            max_cost_reduction = entropy * total_points;
            equivalent_point_loss = entropy * total_points;
        }
    } else {
        // Original misclassification loss calculation
        for (int j = depth(); --j >= 0;) { // Class index
            // maximum cost difference across predictions
            max_cost_reduction += this -> diff_costs[j] * distribution[j];

            buffer = capture_set; // Set representing the captured points
            this -> majority.bit_and(buffer, false); // Captured majority points
            this -> targets.at(j).bit_and(buffer); // Captured majority points with label j
            equivalent_point_loss += this -> match_costs[j] * buffer.count(); // Calculate frequency

            buffer = capture_set; // Set representing the captured points
            this -> majority.bit_and(buffer, true); // Captured minority points
            this -> targets.at(j).bit_and(buffer); // Captured minority points with label j
            equivalent_point_loss += this -> mismatch_costs[j] * buffer.count(); // Calculate frequency
        }
    }

    // Validate support before computing log(support)
    if (support <= 0.0f) {
        throw std::runtime_error("Cannot compute information with support <= 0");
    }

    for (int j = depth(); --j >= 0;) { // Class index
        float prob = distribution[j];
        if (prob > 0) { information += support * prob * (log(prob) - log(support)); }
    }

    potential = max_cost_reduction;
    min_loss = equivalent_point_loss;
    max_loss = min_cost;
    info = information;
    target_index = cost_minimizer;
}

void Dataset::get_TP_TN(Bitmask const & capture_set, unsigned int id, unsigned int target_index, unsigned int & TP, unsigned int & TN, State & state) {
    if (Configuration::loss_function == SQUARED_ERROR) {
        TP = 0;
        TN = 0;
        return;
    }
    // Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        throw std::runtime_error("Worker ID out of bounds: " + std::to_string(id) + " >= " + std::to_string(state.locals.size()));
    }
    Bitmask & buffer = state.locals[id].columns[0];

    buffer = capture_set; // Set representing the captured points
    this -> targets.at(target_index).bit_and(buffer); // Captured points with label j
    unsigned int true_count = buffer.count(); // Calculate frequency
    
    if (target_index == 1) {
        TP = true_count;
        TN = 0;
    } else {
        TP = 0;
        TN = true_count;
    }
}

void Dataset::get_total_P_N(unsigned int & P, unsigned int & N) {
    if (Configuration::loss_function == SQUARED_ERROR || targets.size() < 2) {
        P = 0;
        N = 0;
        return;
    }
    P = targets.at(1).count();
    N = targets.at(0).count();
}

void Dataset::get_class_distribution(Bitmask const & capture_set, std::vector<float> & distribution, unsigned int id, State & state) const {
    if (Configuration::loss_function == SQUARED_ERROR) {
        distribution.clear();
        return;
    }
    // CRITICAL: Bounds check to prevent segfault
    if (id >= state.locals.size()) {
        std::string error_msg = "Worker ID out of bounds: " + std::to_string(id) 
                              + " >= " + std::to_string(state.locals.size())
                              + " (locals.size()=" + std::to_string(state.locals.size()) + ")";
        throw std::runtime_error(error_msg);
    }
    
    // CRITICAL: Assert that distribution vector is properly sized
    unsigned int dataset_depth = depth();
    assert(dataset_depth > 0 && "Dataset depth must be > 0");
    assert(distribution.size() == dataset_depth && "Distribution vector size must match dataset depth");
    
    // CRITICAL: Bounds check to prevent segfault - use get_local() for consistency
    LocalState& local = state.get_local(id);
    Bitmask & buffer = local.columns[0];
    
    float total_points = (float)capture_set.count();
    assert(total_points >= 0 && "Total points count must be >= 0");
    
    if (total_points > 0) {
        for (unsigned int j = 0; j < dataset_depth; ++j) {
            // CRITICAL: Bounds check for targets vector
            assert(j < targets.size() && "Target index must be within bounds");
            buffer = capture_set;
            this -> targets.at(j).bit_and(buffer);
            float count = (float)buffer.count();
            distribution[j] = count / total_points;
            // CRITICAL: Verify probability is valid
            assert(distribution[j] >= 0.0 && distribution[j] <= 1.0 && "Probability must be in [0,1]");
        }
    } else {
        // If no training data, use uniform distribution
        float uniform_prob = 1.0 / dataset_depth;
        for (unsigned int j = 0; j < dataset_depth; ++j) {
            distribution[j] = uniform_prob;
        }
    }
}

// Assume that data is already of the right size
void Dataset::tile(Bitmask const & capture_set, Bitmask const & feature_set, Tile & tile, std::vector< int > & order, unsigned int id, State & state) const {
    tile.content() = capture_set;
    tile.width(0);
    return;
}

float Dataset::get_mismatch_cost() const {
    return mismatch_costs[0];
}


unsigned int Dataset::height(void) const {
    return std::get<0>(this -> shape);
}

unsigned int Dataset::width(void) const {
    return std::get<1>(this -> shape);
}

unsigned int Dataset::depth(void) const {
    return std::get<2>(this -> shape);
}

unsigned int Dataset::size(void) const {
    return this -> _size;
}

bool Dataset::index_comparator(const std::pair< unsigned int, unsigned int > & left, const std::pair< unsigned int, unsigned int > & right) {
    return left.second < right.second;
}