#ifndef KMEANS_HPP
#define KMEANS_HPP

#include <vector>
#include <algorithm>
#include "lib/ckmeans/include/Ckmeans.1d.dp.h"

// Compute optimal k-Means lower bound with dynamic stopping
// Based on Song & Zhong (2020) algorithm, as used in OSRT
//
// Algorithm 1 from OSRT paper (Zhang et al., AAAI 2023):
// Performs optimal 1D weighted k-means clustering with dynamic stopping
// when adding a new cluster costs more than the regularization penalty.
//
// Parameters:
//   values: Sorted target values (one per equivalent point)
//   weights: Weights for each value (size of equivalent point set)
//   regularization: Penalty per cluster (lambda)
//
// Returns:
//   Minimum achievable loss (k-means objective + regularization * num_clusters)
//
// This provides a lower bound for regression trees with squared_error loss.
// The bound exploits the structure of continuous targets via optimal clustering.
inline ldouble compute_kmeans_lower_bound(
    const std::vector<double> & values,
    const std::vector<double> & weights,
    ldouble regularization
) {
    // Handle trivial cases
    int N = values.size();
    if (N == 0) {
        return 0.0;
    }
    if (N == 1) {
        return regularization;  // One cluster, pay regularization once
    }

    // Allocate DP matrices
    // Kmax = 100 is OSRT's default (sufficient for most cases)
    int Kmax = std::min(100, N);
    std::vector< std::vector< ldouble > > S(Kmax, std::vector<ldouble>(N));
    std::vector< std::vector< size_t > > J(Kmax, std::vector<size_t>(N));

    // Call Song & Zhong (2020) dynamic programming algorithm
    // Uses L2 (squared error) dissimilarity
    // Automatically stops when adding clusters no longer reduces loss by >= reg
    ldouble lower_bound = fill_dp_matrix_dynamic_stop(
        values,
        weights,
        S,
        J,
        L2,              // L2 dissimilarity (squared error)
        regularization
    );

    return lower_bound;
}

#endif // KMEANS_HPP
