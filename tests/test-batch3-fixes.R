# Quick integration test for Batch 3 fixes
# Run with: Rscript tests/test-batch3-fixes.R

# Load development version
devtools::load_all()

cat("=== Testing Batch 3 Fixes ===\n\n")

# Test Issues #22-26: Refactored predict methods still work
cat("Test 1: Refactored predict methods (continuous features)\n")
set.seed(42)
X <- data.frame(x1 = runif(100), x2 = runif(100))
y <- rbinom(100, 1, 0.5)
model <- fit_tree(X, y, regularization = 0.1, verbose = FALSE)
X_new <- data.frame(x1 = runif(10), x2 = runif(10))
pred_class <- predict(model, X_new, type = 'class')
pred_prob <- predict(model, X_new, type = 'prob')
stopifnot(length(pred_class) == 10)
stopifnot(all(pred_class %in% c(0, 1)))
stopifnot(nrow(pred_prob) == 10)
stopifnot(ncol(pred_prob) == 2)
cat("  PASS: predict.optimaltrees_model works with refactored helpers\n")

cat("Test 2: Refactored predict methods (binary features)\n")
set.seed(42)
X_bin <- data.frame(x1 = rbinom(100, 1, 0.5), x2 = rbinom(100, 1, 0.5))
y_bin <- rbinom(100, 1, 0.5)
model_bin <- fit_tree(X_bin, y_bin, regularization = 0.1, verbose = FALSE)
X_new_bin <- data.frame(x1 = rbinom(10, 1, 0.5), x2 = rbinom(10, 1, 0.5))
pred_bin <- predict(model_bin, X_new_bin, type = 'class')
stopifnot(length(pred_bin) == 10)
stopifnot(all(pred_bin %in% c(0, 1)))
cat("  PASS: Binary features work\n")

cat("Test 3: Refactored predict methods (regression)\n")
set.seed(42)
X_reg <- data.frame(x1 = runif(100), x2 = runif(100))
y_reg <- rnorm(100)
model_reg <- fit_tree(X_reg, y_reg, loss_function = 'squared_error',
                      regularization = 0.1, verbose = FALSE)
X_new_reg <- data.frame(x1 = runif(10), x2 = runif(10))
pred_reg <- predict(model_reg, X_new_reg)
stopifnot(length(pred_reg) == 10)
stopifnot(all(is.finite(pred_reg)))
cat("  PASS: Regression works\n")

cat("Test 4: Extracted helper functions exist and work\n")
# Test that internal helpers are accessible (via namespace)
apply_model_discretization_fn <- get("apply_model_discretization", envir = asNamespace("optimaltrees"))
get_tree_structure_fn <- get("get_tree_structure", envir = asNamespace("optimaltrees"))
predict_from_tree_fn <- get("predict_from_tree", envir = asNamespace("optimaltrees"))
stopifnot(is.function(apply_model_discretization_fn))
stopifnot(is.function(get_tree_structure_fn))
stopifnot(is.function(predict_from_tree_fn))
cat("  PASS: Helper functions exist\n")

cat("\n=== All Batch 3 (optimaltrees) tests passed! ===\n")
