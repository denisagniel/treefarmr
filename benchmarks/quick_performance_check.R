# Quick Thread-Safety Performance Check
# Fast preliminary assessment of parallel performance

library(optimaltrees)

cat("=== Quick Thread-Safety Performance Check ===\n\n")

# Setup test data
set.seed(42)
n <- 300
p <- 15
X <- matrix(rbinom(n * p, 1, 0.5), nrow = n, ncol = p)
X <- as.data.frame(X)
names(X) <- paste0("x", 1:p)
y <- rbinom(n, 1, 0.5)

cat("Dataset: n =", n, ", p =", p, "\n\n")

# Quick timing comparison
workers_to_test <- c(1, 2, 4)

results <- data.frame(
  workers = integer(),
  time_sec = numeric(),
  speedup = numeric()
)

for (w in workers_to_test) {
  cat("Testing", w, "worker(s)...")

  times <- numeric(5)
  for (i in 1:5) {
    start <- Sys.time()
    model <- fit_tree(X, y, worker_limit = w, regularization = 0.1, verbose = FALSE)
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }

  median_time <- median(times)
  cat(" median time:", round(median_time, 3), "sec\n")

  results <- rbind(results, data.frame(
    workers = w,
    time_sec = median_time,
    speedup = NA
  ))
}

# Calculate speedup
baseline <- results$time_sec[results$workers == 1]
results$speedup <- baseline / results$time_sec
results$efficiency_pct <- (results$speedup / results$workers) * 100

cat("\n=== Results Summary ===\n")
print(results)

cat("\n=== Analysis ===\n")
cat("Baseline (1 worker):", round(baseline, 3), "seconds\n")

for (i in 2:nrow(results)) {
  w <- results$workers[i]
  speedup <- results$speedup[i]
  eff <- results$efficiency_pct[i]

  cat(sprintf("%d workers: %.2fx speedup (%.0f%% efficient)\n", w, speedup, eff))

  if (eff >= 75) {
    cat("  → Good scaling\n")
  } else if (eff >= 50) {
    cat("  → Moderate scaling\n")
  } else {
    cat("  → Poor scaling (overhead dominates)\n")
  }
}

cat("\n=== Recommendation ===\n")
best_eff <- results$efficiency_pct[results$workers == 4]
if (best_eff >= 75) {
  cat("✓ Thread-safety implementation is efficient\n")
  cat("  Recommended default: worker_limit = 4\n")
} else if (best_eff >= 60) {
  cat("• Thread-safety has moderate overhead\n")
  cat("  Recommended default: worker_limit = 2\n")
} else {
  cat("⚠ Thread-safety overhead is significant\n")
  cat("  Recommended default: worker_limit = 1\n")
  cat("  Consider profiling to identify synchronization hotspots\n")
}

cat("\n=== Done ===\n")
