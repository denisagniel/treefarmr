# Helper functions to create synthetic datasets for testing

#' Create a simple binary classification dataset
#' @param n Number of samples
#' @param p Number of features
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
simple_dataset <- function(n = 100, p = 3, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create a dataset with a clear pattern for entropy/log-loss testing
#' (e.g., XOR-like pattern)
#' @param n Number of samples (must be a multiple of 4)
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
entropy_dataset <- function(n = 100, seed = 42) {
  set.seed(seed)
  if (n %% 4 != 0) stop("n must be a multiple of 4 for entropy_dataset")
  quarter <- n / 4
  X <- data.frame(
    x1 = c(rep(0, quarter), rep(1, quarter), rep(0, quarter), rep(1, quarter)),
    x2 = c(rep(0, quarter), rep(0, quarter), rep(1, quarter), rep(1, quarter))
  )
  y <- c(rep(0, 2 * quarter), rep(1, 2 * quarter)) # x1 XOR x2 pattern
  list(X = X, y = y)
}

#' Create a larger dataset for performance/consistency testing
#' @param n Number of samples
#' @param p Number of features
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
big_dataset <- function(n = 1000, p = 10, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create an unbalanced dataset
#' @param n Number of samples
#' @param p Number of features
#' @param imbalance_ratio Ratio of minority class (e.g., 0.1 for 10% minority)
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
unbalanced_dataset <- function(n = 100, p = 3, imbalance_ratio = 0.1, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  
  n_minority <- round(n * imbalance_ratio)
  n_majority <- n - n_minority
  
  y <- c(rep(0, n_majority), rep(1, n_minority))
  y <- sample(y) # Shuffle to mix classes
  
  list(X = X, y = y)
}

#' Create a dataset with all features identical
#' @param n Number of samples
#' @param p Number of features
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
identical_features_dataset <- function(n = 100, p = 3, seed = 42) {
  set.seed(seed)
  feature_val <- sample(0:1, n, replace = TRUE)
  X <- as.data.frame(lapply(1:p, function(i) feature_val))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create a dataset with no predictive features (random y)
#' @param n Number of samples
#' @param p Number of features
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
random_y_dataset <- function(n = 100, p = 3, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE) # Completely random
  list(X = X, y = y)
}

#' Create a minimal dataset for edge case testing
#' @return A list with X (data.frame) and y (vector)
minimal_dataset <- function() {
  list(
    X = data.frame(f1 = c(0, 1, 0, 1)),
    y = c(0, 1, 1, 0)
  )
}

#' Create an empty dataset for edge case testing
#' @return A list with X (data.frame) and y (vector)
empty_dataset <- function() {
  list(
    X = data.frame(f1 = integer(0)),
    y = integer(0)
  )
}

#' Create a single-row dataset for edge case testing
#' @return A list with X (data.frame) and y (vector)
single_row_dataset <- function() {
  list(
    X = data.frame(f1 = 1),
    y = 1
  )
}

#' Create a dataset where all features are 0
#' @param n Number of samples
#' @param p Number of features
#' @return A list with X (data.frame) and y (vector)
all_zeros_dataset <- function(n = 50, p = 3) {
  X <- as.data.frame(matrix(0, nrow = n, ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create a dataset where all features are 1
#' @param n Number of samples
#' @param p Number of features
#' @return A list with X (data.frame) and y (vector)
all_ones_dataset <- function(n = 50, p = 3) {
  X <- as.data.frame(matrix(1, nrow = n, ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create a dataset where all y values are the same class
#' @param n Number of samples
#' @param p Number of features
#' @param class_value The class value (0 or 1)
#' @return A list with X (data.frame) and y (vector)
single_class_dataset <- function(n = 50, p = 3, class_value = 0) {
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- rep(class_value, n)
  list(X = X, y = y)
}

#' Create a dataset with many features for stress testing
#' @param n Number of samples
#' @param p Number of features (default 50)
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
many_features_dataset <- function(n = 100, p = 50, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  y <- sample(0:1, n, replace = TRUE)
  list(X = X, y = y)
}

#' Create a dataset with a clear pattern (XOR-like)
#' @param n Number of samples (must be a multiple of 4)
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
pattern_dataset <- function(n = 100, seed = 42) {
  set.seed(seed)
  if (n %% 4 != 0) stop("n must be a multiple of 4 for pattern_dataset")
  quarter <- n / 4
  X <- data.frame(
    x1 = c(rep(0, quarter), rep(1, quarter), rep(0, quarter), rep(1, quarter)),
    x2 = c(rep(0, quarter), rep(0, quarter), rep(1, quarter), rep(1, quarter))
  )
  y <- c(rep(0, 2 * quarter), rep(1, 2 * quarter)) # x1 XOR x2 pattern
  list(X = X, y = y)
}

#' Create an imbalanced dataset
#' @param n Number of samples
#' @param p Number of features
#' @param imbalance_ratio Ratio of minority class (default 0.1)
#' @param seed Random seed
#' @return A list with X (data.frame) and y (vector)
imbalanced_dataset <- function(n = 100, p = 3, imbalance_ratio = 0.1, seed = 42) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("feature_", 1:p)
  
  n_minority <- round(n * imbalance_ratio)
  n_majority <- n - n_minority
  
  y <- c(rep(0, n_majority), rep(1, n_minority))
  y <- sample(y) # Shuffle to mix classes
  
  list(X = X, y = y)
}

# Create test datasets with reduced sizes to minimize memory footprint
# Note: Datasets are created at load time but kept smaller to reduce memory usage
simple_dataset <- simple_dataset(100, 3, 42)
pattern_dataset <- pattern_dataset(100, 42)
entropy_dataset <- entropy_dataset(100, 42)
big_dataset <- big_dataset(200, 10, 42)  # Reduced from 1000 to 200
unbalanced_dataset <- unbalanced_dataset(100, 3, 0.1, 42)
identical_features_dataset <- identical_features_dataset(100, 3, 42)
random_y_dataset <- random_y_dataset(100, 3, 42)
minimal_dataset <- minimal_dataset()
empty_dataset <- empty_dataset()
single_row_dataset <- single_row_dataset()
all_zeros_dataset <- all_zeros_dataset(50, 3)
all_ones_dataset <- all_ones_dataset(50, 3)
single_class_dataset <- single_class_dataset(50, 3, 0)
many_features_dataset <- many_features_dataset(100, 20, 42)  # Reduced from 50 to 20 features
imbalanced_dataset <- imbalanced_dataset(100, 3, 0.1, 42)