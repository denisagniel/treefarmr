# Inspect model structure

devtools::load_all("optimaltrees")

set.seed(42)
n <- 800
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n), x4 = runif(n))
y <- rbinom(n, 1, 0.5)

model <- fit_tree(
  X, y,
  loss_function = "log_loss",
  regularization = log(n) / n,
  discretize_method = "quantiles",
  discretize_bins = "adaptive",
  verbose = FALSE
)

cat("=== Model Structure ===\n\n")
cat("Top-level names:\n")
print(names(model))
cat("\n")

cat("model$model names:\n")
print(names(model$model))
cat("\n")

cat("Checking specific fields:\n")
cat("  model$model$tree_json: ", class(model$model$tree_json), ", is.null:", is.null(model$model$tree_json), "\n")
cat("  model$model$result_data: ", class(model$model$result_data), ", is.null:", is.null(model$model$result_data), "\n")
cat("  model$n_trees:", model$n_trees, "\n")
cat("  model$discretization: ", !is.null(model$discretization), "\n\n")

# Try to make predictions
cat("=== Testing Predictions ===\n")
X_test <- X[1:10, ]
pred_result <- tryCatch({
  pred <- predict(model, X_test, type = "prob")
  list(success = TRUE, sd = sd(pred[, 2]))
}, error = function(e) {
  list(success = FALSE, error = conditionMessage(e))
})

if (pred_result$success) {
  cat("  ✓ Predictions work!\n")
  cat("  Prediction SD:", round(pred_result$sd, 4), "\n")
} else {
  cat("  ✗ Predictions failed:\n")
  cat("  ", pred_result$error, "\n")
}
