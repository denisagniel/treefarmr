# Large Problem Thread-Safety Benchmark
# Test with problem sizes where parallelization should help

library(optimaltrees)

cat("=== Large Problem Thread-Safety Benchmark ===\n\n")
cat("Testing hypothesis: Small problems have high overhead\n")
cat("                   Large problems should benefit from parallelization\n\n")

# Test multiple problem sizes
problem_sizes <- list(
  tiny = list(n = 100, p = 10, name = "Tiny (100 x 10)"),
  small = list(n = 300, p = 15, name = "Small (300 x 15)"),
  medium = list(n = 800, p = 25, name = "Medium (800 x 25)"),
  large = list(n = 1500, p = 35, name = "Large (1500 x 35)")
)

all_results <- list()

for (size_name in names(problem_sizes)) {
  size_config <- problem_sizes[[size_name]]

  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Testing:", size_config$name, "\n")
  cat(rep("=", 60), "\n", sep = "")

  # Generate data
  set.seed(42)
  n <- size_config$n
  p <- size_config$p
  X <- matrix(rbinom(n * p, 1, 0.5), nrow = n, ncol = p)
  X <- as.data.frame(X)
  names(X) <- paste0("x", 1:p)
  y <- rbinom(n, 1, 0.5)

  workers_to_test <- c(1, 2, 4)
  size_results <- data.frame(
    size = character(),
    workers = integer(),
    time_sec = numeric(),
    speedup = numeric(),
    efficiency_pct = numeric()
  )

  for (w in workers_to_test) {
    cat("\n  ", w, "worker(s)...")

    # Run 3 times, take median
    times <- numeric(3)
    for (i in 1:3) {
      start <- Sys.time()
      model <- fit_tree(X, y, worker_limit = w, regularization = 0.1, verbose = FALSE)
      times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    }

    median_time <- median(times)
    cat(" ", round(median_time, 3), "sec")

    size_results <- rbind(size_results, data.frame(
      size = size_config$name,
      workers = w,
      time_sec = median_time,
      speedup = NA,
      efficiency_pct = NA
    ))
  }

  # Calculate speedup
  baseline <- size_results$time_sec[size_results$workers == 1]
  size_results$speedup <- baseline / size_results$time_sec
  size_results$efficiency_pct <- (size_results$speedup / size_results$workers) * 100

  cat("\n\n  Results:\n")
  print(size_results[, c("workers", "time_sec", "speedup", "efficiency_pct")])

  all_results[[size_name]] <- size_results
}

# Summary Analysis
cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY: When Does Parallelization Help?\n")
cat(rep("=", 60), "\n", sep = "")

summary_df <- do.call(rbind, all_results)

cat("\nEfficiency by problem size (4 workers):\n")
for (size_name in names(problem_sizes)) {
  size_config <- problem_sizes[[size_name]]
  eff_4 <- summary_df$efficiency_pct[summary_df$size == size_config$name &
                                      summary_df$workers == 4]
  speedup_4 <- summary_df$speedup[summary_df$size == size_config$name &
                                  summary_df$workers == 4]

  verdict <- if (eff_4 >= 75) {
    "✓ Good scaling"
  } else if (eff_4 >= 50) {
    "• Moderate scaling"
  } else {
    "✗ Poor scaling (overhead dominates)"
  }

  cat(sprintf("  %s: %.1f%% efficient (%.2fx speedup) - %s\n",
              size_config$name, eff_4, speedup_4, verdict))
}

cat("\n=== KEY FINDING ===\n")
best_size <- names(problem_sizes)[which.max(sapply(names(problem_sizes), function(s) {
  summary_df$efficiency_pct[summary_df$size == problem_sizes[[s]]$name &
                            summary_df$workers == 4]
}))]

best_eff <- max(sapply(names(problem_sizes), function(s) {
  summary_df$efficiency_pct[summary_df$size == problem_sizes[[s]]$name &
                            summary_df$workers == 4]
}))

cat("Best scaling with:", problem_sizes[[best_size]]$name, "\n")
cat("Efficiency:", round(best_eff, 1), "%\n\n")

if (best_eff >= 70) {
  cat("✓ PARALLELIZATION IS EFFECTIVE for large problems\n")
  cat("  Recommendation: worker_limit = 4 for n > ", problem_sizes[[best_size]]$n, "\n", sep = "")
  cat("                  worker_limit = 1 for smaller problems\n")
} else if (best_eff >= 50) {
  cat("• PARALLELIZATION HAS MODERATE BENEFIT\n")
  cat("  Recommendation: worker_limit = 2 (conservative)\n")
  cat("                  Only use higher for very large problems\n")
} else {
  cat("✗ PARALLELIZATION OVERHEAD DOMINATES\n")
  cat("  Recommendation: worker_limit = 1 (default single-threaded)\n")
  cat("  Thread-safety implementation needs optimization\n")
}

cat("\n=== Explanation ===\n")
cat("For small problems (<500 obs), overhead of thread management exceeds benefit.\n")
cat("Parallel tree search only helps when:\n")
cat("  1. Search space is large (many candidate splits)\n")
cat("  2. Each iteration takes significant time\n")
cat("  3. Work can be evenly distributed across threads\n")

cat("\n=== Done ===\n")
