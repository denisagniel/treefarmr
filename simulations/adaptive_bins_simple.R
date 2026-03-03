# Simple Simulation: Adaptive vs Fixed Bins for Tree Estimation
# Compare discretization strategies for regression/classification with continuous X

library(treefarmr)
set.seed(2026)

# Simulation parameters
sample_sizes <- c(100, 200, 500, 1000, 2000)
n_reps <- 50

# DGP: Continuous X with nonlinear regression function
generate_data <- function(n, scenario = "smooth") {
  # Continuous covariates
  X <- data.frame(
    x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1),
    x3 = runif(n, 0, 1)
  )

  if (scenario == "smooth") {
    # Hölder-smooth regression (β = 2)
    mu <- 1 + 2*X$x1 + X$x2^2 + 0.5*X$x3 + X$x1*X$x2
  } else if (scenario == "step") {
    # Step function (β < 1, more difficult)
    mu <- 1 + 2*(X$x1 > 0.5) + (X$x2 > 0.5) * (X$x3 > 0.5)
  }

  # Continuous outcome for regression
  Y_cont <- mu + rnorm(n, 0, 0.5)

  # Binary outcome for classification
  p <- plogis(mu - 1.5)
  Y_bin <- rbinom(n, 1, p)

  list(X = X, Y_cont = Y_cont, Y_bin = Y_bin, mu = mu)
}

# Evaluate prediction MSE
evaluate_prediction <- function(model, X_test, y_test) {
  pred <- predict(model, X_test)
  mse <- mean((pred - y_test)^2)
  mse
}

# Run single simulation
run_simulation <- function(n, discretize_bins, loss_function, scenario) {
  # Generate train and test data
  train <- generate_data(n, scenario)
  test <- generate_data(n, scenario)  # Same size for simplicity

  y_train <- if (loss_function == "squared_error") train$Y_cont else train$Y_bin
  y_test <- if (loss_function == "squared_error") test$Y_cont else test$Y_bin

  # Fit model
  result <- tryCatch({
    model <- treefarms(
      X = train$X,
      y = y_train,
      loss_function = loss_function,
      regularization = 0.1,
      discretize_method = "quantiles",
      discretize_bins = discretize_bins,
      verbose = FALSE
    )

    # Evaluate on test set
    pred_train <- predict(model, train$X)
    pred_test <- predict(model, test$X)

    mse_train <- mean((pred_train - y_train)^2)
    mse_test <- mean((pred_test - y_test)^2)

    # True regression MSE (oracle)
    if (loss_function == "squared_error") {
      mse_oracle <- mean((test$mu - y_test)^2)  # Bayes error
    } else {
      p_true <- plogis(test$mu - 1.5)
      mse_oracle <- mean((p_true - y_test)^2)
    }

    list(
      mse_train = mse_train,
      mse_test = mse_test,
      mse_oracle = mse_oracle,
      n_leaves = if (!is.null(model$model$tree_json)) {
        count_tree_leaves(model$model$tree_json)
      } else {
        NA
      },
      success = TRUE
    )
  }, error = function(e) {
    list(
      mse_train = NA,
      mse_test = NA,
      mse_oracle = NA,
      n_leaves = NA,
      success = FALSE,
      error = e$message
    )
  })

  result
}

# Main simulation loop
results_list <- list()

scenarios <- c("smooth", "step")
loss_functions <- c("squared_error", "log_loss")

for (scenario in scenarios) {
  for (loss_fn in loss_functions) {
    if (scenario == "smooth" && loss_fn == "log_loss") next  # Skip redundant

    cat("\n=== Scenario:", scenario, "| Loss:", loss_fn, "===\n")

    for (n in sample_sizes) {
      cat("Sample size:", n, "\n")

      # Compute expected adaptive bins
      adaptive_bins <- max(2, ceiling(log(n) / 3))

      # Compare: fixed bins vs adaptive
      methods <- list(
        list(bins = 2, name = "Fixed-2"),
        list(bins = 4, name = "Fixed-4"),
        list(bins = "adaptive", name = "Adaptive")
      )

      for (method in methods) {
        cat("  Method:", method$name, "...")

        # Run replicates
        reps <- replicate(n_reps,
                         run_simulation(n, method$bins, loss_fn, scenario),
                         simplify = FALSE)

        # Extract results
        mse_trains <- sapply(reps, function(r) r$mse_train)
        mse_tests <- sapply(reps, function(r) r$mse_test)
        mse_oracles <- sapply(reps, function(r) r$mse_oracle)
        n_leaves <- sapply(reps, function(r) r$n_leaves)
        success_rate <- mean(sapply(reps, function(r) r$success))

        # Remove failed runs
        valid <- !is.na(mse_tests)
        mse_trains <- mse_trains[valid]
        mse_tests <- mse_tests[valid]
        mse_oracles <- mse_oracles[valid]
        n_leaves <- n_leaves[valid]

        if (length(mse_tests) > 0) {
          # Performance metrics
          mean_mse_train <- mean(mse_trains)
          mean_mse_test <- mean(mse_tests)
          mean_mse_oracle <- mean(mse_oracles)
          excess_risk <- mean_mse_test - mean_mse_oracle
          mean_leaves <- mean(n_leaves, na.rm = TRUE)

          # Store results
          results_list[[length(results_list) + 1]] <- data.frame(
            scenario = scenario,
            loss_fn = loss_fn,
            n = n,
            method = method$name,
            bins = if (is.character(method$bins)) adaptive_bins else method$bins,
            mse_train = mean_mse_train,
            mse_test = mean_mse_test,
            mse_oracle = mean_mse_oracle,
            excess_risk = excess_risk,
            n_leaves = mean_leaves,
            success_rate = success_rate,
            n_valid = length(mse_tests)
          )

          cat(" MSE:", round(mean_mse_test, 4),
              "| Leaves:", round(mean_leaves, 1),
              "| Success:", round(success_rate, 3), "\n")
        }
      }
    }
  }
}

# Combine results
results <- do.call(rbind, results_list)

# Save results
saveRDS(results, "simulations/adaptive_bins_simple_results.rds")
write.csv(results, "simulations/adaptive_bins_simple_results.csv", row.names = FALSE)

# Print summary
cat("\n=== SUMMARY ===\n")
print(results)

# Create comparison plots
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Test MSE plot (log-log)
  p1 <- ggplot(results, aes(x = n, y = mse_test, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() +
    facet_wrap(~ scenario + loss_fn, scales = "free_y") +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "Tree Estimation: Adaptive vs Fixed Bins",
         subtitle = "Test Set MSE by Sample Size (log-log scale)",
         x = "Sample Size (log scale)",
         y = "Test MSE (log scale)",
         color = "Discretization",
         shape = "Discretization") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("simulations/adaptive_bins_simple_mse.png", p1, width = 10, height = 6)

  # Excess risk plot (over Bayes error)
  p2 <- ggplot(results, aes(x = n, y = excess_risk, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() +
    facet_wrap(~ scenario + loss_fn, scales = "free_y") +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "Tree Estimation: Adaptive vs Fixed Bins",
         subtitle = "Excess Risk (Test MSE - Oracle) by Sample Size",
         x = "Sample Size (log scale)",
         y = "Excess Risk (log scale)",
         color = "Discretization",
         shape = "Discretization") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("simulations/adaptive_bins_simple_excess.png", p2, width = 10, height = 6)

  # Number of leaves plot
  p3 <- ggplot(results, aes(x = n, y = n_leaves, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() +
    facet_wrap(~ scenario + loss_fn) +
    scale_x_log10() +
    labs(title = "Tree Estimation: Adaptive vs Fixed Bins",
         subtitle = "Tree Complexity (Number of Leaves) by Sample Size",
         x = "Sample Size (log scale)",
         y = "Number of Leaves",
         color = "Discretization",
         shape = "Discretization") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("simulations/adaptive_bins_simple_leaves.png", p3, width = 10, height = 6)

  cat("\nPlots saved to simulations/\n")
}

cat("\nSimulation complete!\n")
cat("\nKey findings to look for:\n")
cat("1. Does adaptive improve MSE as n grows?\n")
cat("2. Do adaptive bins allow more tree leaves as n grows?\n")
cat("3. Is the benefit larger for smooth functions (higher β)?\n")
