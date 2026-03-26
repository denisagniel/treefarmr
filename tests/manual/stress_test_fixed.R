# Fixed stress test with correct function names and fields
library(optimaltrees)

cat("Running thread-safety stress test with 1000 iterations...\n")
cat("This will take several minutes.\n\n")

# Simple test data
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
y <- rbinom(n, 1, 0.5)

# Run 1000 iterations with worker_limit=4
n_iterations <- 1000
results <- list()
errors <- 0

for (i in 1:n_iterations) {
  if (i %% 100 == 0) cat("  Iteration", i, "/", n_iterations, "\n")

  result <- try({
    fit_tree(
      X = X,
      y = y,
      loss_function = "misclassification",
      regularization = 0.01,
      worker_limit = 4,
      time_limit = 5,
      verbose = FALSE
    )
  }, silent = TRUE)

  if (inherits(result, "try-error")) {
    errors <- errors + 1
    cat("  ERROR in iteration", i, ":", attr(result, "condition")$message, "\n")
  } else {
    results[[i]] <- list(
      accuracy = result$accuracy,
      model_objective = result$model$tree_json$model_objective,
      tree_size = result$model$size,
      iterations = result$training_iterations
    )
  }
}

cat("\n")
cat("─────────────────────────────────────────\n")
cat("STRESS TEST RESULTS\n")
cat("─────────────────────────────────────────\n")
cat("Total iterations:", n_iterations, "\n")
cat("Successful:", length(results), "\n")
cat("Errors:", errors, "\n\n")

if (length(results) > 1) {
  # Check stability of results
  accuracy <- sapply(results, function(x) x$accuracy)
  model_objective <- sapply(results, function(x) x$model_objective)
  tree_size <- sapply(results, function(x) x$tree_size)
  iterations <- sapply(results, function(x) x$iterations)

  cat("Accuracy statistics:\n")
  cat("  Mean:", mean(accuracy), "\n")
  cat("  SD:", sd(accuracy), "\n")
  cat("  Range:", min(accuracy), "-", max(accuracy), "\n\n")

  cat("Model objective statistics:\n")
  cat("  Mean:", mean(model_objective), "\n")
  cat("  SD:", sd(model_objective), "\n")
  cat("  Range:", min(model_objective), "-", max(model_objective), "\n\n")

  cat("Tree size statistics:\n")
  cat("  Mean:", mean(tree_size), "\n")
  cat("  SD:", sd(tree_size), "\n")
  cat("  Range:", min(tree_size), "-", max(tree_size), "\n\n")

  cat("Training iterations statistics:\n")
  cat("  Mean:", mean(iterations), "\n")
  cat("  SD:", sd(iterations), "\n")
  cat("  Range:", min(iterations), "-", max(iterations), "\n\n")

  # Check if results are stable (should be identical for deterministic algorithm)
  if (sd(accuracy) < 1e-10 && sd(model_objective) < 1e-10 && sd(tree_size) < 1e-10) {
    cat("✓ SUCCESS: Results are perfectly stable!\n")
    cat("✓ Accuracy SD: ", sd(accuracy), " (target: < 1e-10)\n")
    cat("✓ Model objective SD: ", sd(model_objective), " (target: < 1e-10)\n")
    cat("✓ Tree size SD: ", sd(tree_size), " (target: < 1e-10)\n")
    cat("✓ Thread-safety verified - no race conditions detected.\n")
  } else {
    cat("✗ WARNING: Results vary across iterations!\n")
    cat("✗ Accuracy SD: ", sd(accuracy), " (target: < 1e-10)\n")
    cat("✗ Model objective SD: ", sd(model_objective), " (target: < 1e-10)\n")
    cat("✗ Tree size SD: ", sd(tree_size), " (target: < 1e-10)\n")
    cat("✗ This suggests potential race conditions or non-determinism.\n")
  }
} else {
  cat("✗ FAILED: Too many errors to assess stability.\n")
}

cat("\n")
cat("Test completed at:", format(Sys.time()), "\n")
