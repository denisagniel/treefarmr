# Debug discretization issue

library(optimaltrees)
source("dmltree/R/apply_discretization.R")

set.seed(42)
n <- 100
X <- data.frame(x1 = runif(n), x2 = runif(n))
y <- rbinom(n, 1, 0.5)

cat("\n=== Debugging Discretization ===\n\n")

# Fit with discretization
cat("1. Fitting model with discretization\n")
model <- fit_tree(
  X, y,
  loss_function = "misclassification",
  regularization = 0.1,
  discretize_method = "quantiles",
  discretize_bins = 3,
  store_training_data = TRUE,
  verbose = FALSE
)

cat("   Model fitted successfully\n")
cat("   Training data stored:", !is.null(model$X_train), "\n")
cat("   Training features:", if (!is.null(model$X_train)) paste(names(model$X_train), collapse=", ") else "NULL", "\n")

# Check metadata
cat("\n2. Checking discretization metadata\n")
metadata <- model$model$discretization_metadata
cat("   Metadata exists:", !is.null(metadata), "\n")
if (!is.null(metadata)) {
  cat("   Binary feature names:", paste(metadata$binary_names, collapse=", "), "\n")
  cat("   Original features:", paste(names(metadata$features), collapse=", "), "\n")
}

# Try prediction on new data
cat("\n3. Attempting prediction on new continuous data\n")
X_new <- data.frame(x1 = runif(10), x2 = runif(10))
cat("   New data features:", paste(names(X_new), collapse=", "), "\n")

# Apply discretization manually
if (!is.null(metadata)) {
  cat("\n4. Manually applying discretization\n")
  X_new_disc <- apply_discretization_metadata(X_new, metadata)
  cat("   Discretized features:", paste(names(X_new_disc), collapse=", "), "\n")
  cat("   Match training?:", all(names(X_new_disc) %in% names(model$X_train)), "\n")

  # Try predict on discretized data
  cat("\n5. Predicting on manually discretized data\n")
  pred <- tryCatch({
    p <- predict(model, X_new_disc, type = "prob")
    cat("   ✓ SUCCESS!\n")
    TRUE
  }, error = function(e) {
    cat("   ✗ FAILED:", conditionMessage(e), "\n")
    FALSE
  })
}

cat("\n=== Debug Complete ===\n")
