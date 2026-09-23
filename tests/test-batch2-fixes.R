# Quick integration test for Batch 2 fixes
# Run with: Rscript tests/test-batch2-fixes.R

# Load development version
devtools::load_all()

cat("=== Testing Batch 2 Fixes ===\n\n")

# Test Issue #11: X and y validation
cat("Test 1: X and y validation\n")
result <- tryCatch({
  fit_tree(NULL, c(1, 0, 1), regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = c(0, 1, 0))
  fit_tree(X, NULL, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = c(0, 1, 0))
  y <- c(1, 0)  # Wrong length
  fit_tree(X, y, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")
cat("  PASS: X and y validation works\n")

# Test Issue #12: worker_limit validation
cat("Test 2: worker_limit validation\n")
result <- tryCatch({
  X <- data.frame(x1 = rbinom(50, 1, 0.5))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, worker_limit = 0, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = rbinom(50, 1, 0.5))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, worker_limit = 2.5, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")
cat("  PASS: worker_limit validation works\n")

# Test Issue #13: logical parameter validation
cat("Test 3: logical parameter validation\n")
result <- tryCatch({
  X <- data.frame(x1 = rbinom(50, 1, 0.5))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, verbose = "yes", regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = rbinom(50, 1, 0.5))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, compute_probabilities = 1, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = rbinom(50, 1, 0.5))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, store_training_data = "TRUE", regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")
cat("  PASS: logical parameter validation works\n")

# Test Issue #15: n_bins minimum validation
cat("Test 4: n_bins minimum validation\n")
result <- tryCatch({
  X <- data.frame(x1 = runif(50))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, discretize_bins = 1, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")

result <- tryCatch({
  X <- data.frame(x1 = runif(50))
  y <- rbinom(50, 1, 0.5)
  fit_tree(X, y, discretize_bins = 0, regularization = 0.1)
  "NO_ERROR"
}, error = function(e) "ERROR")
stopifnot(result == "ERROR")
cat("  PASS: n_bins minimum validation works\n")

# Test valid inputs still work
cat("Test 5: Valid inputs still work\n")
set.seed(42)
X <- data.frame(x1 = runif(50), x2 = runif(50))
y <- rbinom(50, 1, 0.5)
model <- fit_tree(X, y, regularization = 0.1, verbose = FALSE,
                  worker_limit = 1L, compute_probabilities = FALSE)
stopifnot(!is.null(model))
stopifnot(model$n_trees == 1)
cat("  PASS: Valid inputs work correctly\n")

cat("\n=== All Batch 2 (optimaltrees) tests passed! ===\n")
