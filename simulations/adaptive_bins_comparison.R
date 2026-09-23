# Simulation: Adaptive vs Fixed Bins for DML-ATT
# Compare discretization strategies for theoretical rate guarantees

library(treefarmr)
library(dmltree)

set.seed(2026)

# Simulation parameters
sample_sizes <- c(200, 500, 1000, 2000, 5000)
n_reps <- 100
K <- 5  # Cross-fitting folds

# DGP: Continuous X with nonlinear treatment and outcome functions
generate_data <- function(n) {
  # Continuous covariates
  X <- data.frame(
    x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1),
    x3 = runif(n, 0, 1)
  )

  # Propensity score (nonlinear in X)
  e_true <- plogis(-0.5 + 2*X$x1 + 1.5*X$x2 - 1*X$x3 +
                  2*X$x1*X$x2 - 1.5*X$x2*X$x3)

  # Treatment
  A <- rbinom(n, 1, e_true)

  # Potential outcomes (Hölder smooth functions)
  mu0 <- 1 + 2*X$x1 + X$x2^2 + 0.5*X$x3 + X$x1*X$x2
  mu1 <- mu0 + (0.5 + X$x1 + 0.5*X$x2^2)  # Heterogeneous treatment effect

  # Observed outcome
  Y <- A * mu1 + (1 - A) * mu0 + rnorm(n, 0, 0.5)

  # True ATT (among treated)
  tau_true <- mean((mu1 - mu0)[A == 1])

  list(X = X, A = A, Y = Y, tau_true = tau_true,
       e_true = e_true, mu0 = mu0, mu1 = mu1)
}

# Run single simulation
run_simulation <- function(n, discretize_bins, method_name) {
  data <- generate_data(n)

  # Run DML-ATT with specified discretization
  result <- tryCatch({
    dml_result <- dml_att(
      X = data$X,
      A = data$A,
      Y = data$Y,
      K = K,
      discretize_method = "quantiles",
      discretize_bins = discretize_bins,
      loss_function = "log_loss",
      regularization = 0.1,
      verbose = FALSE
    )

    list(
      estimate = dml_result$theta,
      se = dml_result$se,
      ci_lower = dml_result$ci[1],
      ci_upper = dml_result$ci[2],
      tau_true = data$tau_true,
      success = TRUE
    )
  }, error = function(e) {
    list(
      estimate = NA,
      se = NA,
      ci_lower = NA,
      ci_upper = NA,
      tau_true = data$tau_true,
      success = FALSE,
      error = e$message
    )
  })

  result
}

# Main simulation loop
results_list <- list()

for (n in sample_sizes) {
  cat("\n=== Sample size:", n, "===\n")

  # Compute expected adaptive bins
  adaptive_bins <- max(2, ceiling(log(n) / 3))
  cat("Adaptive bins:", adaptive_bins, "\n")

  # Compare: fixed bins vs adaptive
  methods <- list(
    list(bins = 2, name = "Fixed-2"),
    list(bins = 4, name = "Fixed-4"),
    list(bins = "adaptive", name = "Adaptive")
  )

  for (method in methods) {
    cat("  Method:", method$name, "...\n")

    # Run replicates
    reps <- replicate(n_reps,
                     run_simulation(n, method$bins, method$name),
                     simplify = FALSE)

    # Extract results
    estimates <- sapply(reps, function(r) r$estimate)
    ses <- sapply(reps, function(r) r$se)
    ci_lowers <- sapply(reps, function(r) r$ci_lower)
    ci_uppers <- sapply(reps, function(r) r$ci_upper)
    tau_true <- reps[[1]]$tau_true
    success_rate <- mean(sapply(reps, function(r) r$success))

    # Remove failed runs
    valid <- !is.na(estimates)
    estimates <- estimates[valid]
    ses <- ses[valid]
    ci_lowers <- ci_lowers[valid]
    ci_uppers <- ci_uppers[valid]

    if (length(estimates) > 0) {
      # Performance metrics
      bias <- mean(estimates) - tau_true
      mse <- mean((estimates - tau_true)^2)
      rmse <- sqrt(mse)
      coverage <- mean(ci_lowers <= tau_true & ci_uppers >= tau_true)
      avg_ci_width <- mean(ci_uppers - ci_lowers)

      # Store results
      results_list[[length(results_list) + 1]] <- data.frame(
        n = n,
        method = method$name,
        bins = if (is.character(method$bins)) adaptive_bins else method$bins,
        tau_true = tau_true,
        bias = bias,
        mse = mse,
        rmse = rmse,
        coverage = coverage,
        avg_ci_width = avg_ci_width,
        success_rate = success_rate,
        n_valid = length(estimates)
      )

      cat("    Bias:", round(bias, 4),
          "| RMSE:", round(rmse, 4),
          "| Coverage:", round(coverage, 3),
          "| Success:", round(success_rate, 3), "\n")
    }
  }
}

# Combine results
results <- do.call(rbind, results_list)

# Save results
saveRDS(results, "simulations/adaptive_bins_results.rds")
write.csv(results, "simulations/adaptive_bins_results.csv", row.names = FALSE)

# Print summary
cat("\n=== SUMMARY ===\n")
print(results)

# Create comparison plot
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # RMSE plot
  p1 <- ggplot(results, aes(x = n, y = rmse, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "DML-ATT Estimation: Adaptive vs Fixed Bins",
         subtitle = "Root Mean Squared Error by Sample Size",
         x = "Sample Size (log scale)",
         y = "RMSE (log scale)",
         color = "Discretization",
         shape = "Discretization") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("simulations/adaptive_bins_rmse.png", p1, width = 8, height = 6)

  # Coverage plot
  p2 <- ggplot(results, aes(x = n, y = coverage, color = method, shape = method)) +
    geom_point(size = 3) +
    geom_line() +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray50") +
    scale_x_log10() +
    labs(title = "DML-ATT Estimation: Adaptive vs Fixed Bins",
         subtitle = "95% CI Coverage by Sample Size",
         x = "Sample Size (log scale)",
         y = "Coverage Probability",
         color = "Discretization",
         shape = "Discretization") +
    ylim(0, 1) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave("simulations/adaptive_bins_coverage.png", p2, width = 8, height = 6)

  cat("\nPlots saved to simulations/\n")
}

cat("\nSimulation complete!\n")
