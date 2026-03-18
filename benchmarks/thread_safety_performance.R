# Thread-Safety Performance Benchmark
# Measures overhead and scaling of parallel tree fitting

library(optimaltrees)
library(bench)
library(ggplot2)

# Setup -------------------------------------------------------------------

set.seed(42)

# Create test datasets of varying sizes
create_test_data <- function(n, p = 10) {
  X <- matrix(rbinom(n * p, 1, 0.5), nrow = n, ncol = p)
  X <- as.data.frame(X)
  names(X) <- paste0("x", 1:p)
  y <- rbinom(n, 1, 0.5)
  list(X = X, y = y)
}

# Test configurations
configs <- list(
  small = create_test_data(n = 100, p = 10),
  medium = create_test_data(n = 500, p = 20),
  large = create_test_data(n = 1000, p = 30)
)

# Benchmark 1: Single vs Multi-threaded ----------------------------------

cat("=== Benchmark 1: Worker Count Scaling ===\n\n")

# Test with medium-sized dataset
data <- configs$medium
X <- data$X
y <- data$y

# Benchmark across worker counts
worker_benchmark <- bench::mark(
  workers_1 = fit_tree(X, y, worker_limit = 1L, regularization = 0.1, verbose = FALSE),
  workers_2 = fit_tree(X, y, worker_limit = 2L, regularization = 0.1, verbose = FALSE),
  workers_4 = fit_tree(X, y, worker_limit = 4L, regularization = 0.1, verbose = FALSE),
  workers_8 = fit_tree(X, y, worker_limit = 8L, regularization = 0.1, verbose = FALSE),
  min_iterations = 10,
  max_iterations = 50,
  check = FALSE,  # Trees may differ slightly due to search order
  memory = TRUE
)

print(worker_benchmark)

# Calculate speedup
baseline <- median(worker_benchmark$median[worker_benchmark$expression == "workers_1"])
worker_benchmark$speedup <- baseline / worker_benchmark$median
worker_benchmark$efficiency <- worker_benchmark$speedup / c(1, 2, 4, 8)

cat("\n=== Speedup Analysis ===\n")
print(worker_benchmark[, c("expression", "median", "speedup", "efficiency")])

# Benchmark 2: Dataset Size Scaling ------------------------------------

cat("\n=== Benchmark 2: Dataset Size Scaling ===\n\n")

size_benchmark <- bench::press(
  size = c("small", "medium", "large"),
  workers = c(1L, 4L),
  {
    data <- configs[[size]]
    bench::mark(
      fit_tree(data$X, data$y, worker_limit = workers,
               regularization = 0.1, verbose = FALSE),
      min_iterations = 5,
      max_iterations = 20
    )
  }
)

print(size_benchmark)

# Benchmark 3: Overhead of Thread-Safety -------------------------------

cat("\n=== Benchmark 3: Synchronization Overhead ===\n\n")

# Compare identical work with different worker counts
# Any difference beyond perfect scaling is overhead
data <- configs$small
overhead_benchmark <- bench::mark(
  single = fit_tree(data$X, data$y, worker_limit = 1L,
                    regularization = 0.1, verbose = FALSE),
  multi = fit_tree(data$X, data$y, worker_limit = 4L,
                   regularization = 0.1, verbose = FALSE),
  min_iterations = 20
)

cat("Overhead calculation:\n")
cat("Single-threaded median:", format(overhead_benchmark$median[1]), "\n")
cat("Multi-threaded median:", format(overhead_benchmark$median[2]), "\n")
ideal_speedup <- overhead_benchmark$median[1] / 4  # Perfect 4x speedup
actual <- overhead_benchmark$median[2]
overhead_pct <- (actual - ideal_speedup) / ideal_speedup * 100
cat("Overhead vs ideal 4x speedup:", round(overhead_pct, 1), "%\n")

# Benchmark 4: Memory Usage -------------------------------------------

cat("\n=== Benchmark 4: Memory Usage ===\n\n")

data <- configs$large
memory_benchmark <- bench::mark(
  workers_1 = fit_tree(data$X, data$y, worker_limit = 1L,
                       regularization = 0.1, verbose = FALSE),
  workers_4 = fit_tree(data$X, data$y, worker_limit = 4L,
                       regularization = 0.1, verbose = FALSE),
  min_iterations = 5,
  memory = TRUE
)

cat("Memory allocation per worker configuration:\n")
print(memory_benchmark[, c("expression", "mem_alloc", "median")])

# Visualization -------------------------------------------------------

# Plot speedup vs worker count
speedup_data <- data.frame(
  workers = c(1, 2, 4, 8),
  speedup = worker_benchmark$speedup,
  efficiency = worker_benchmark$efficiency * 100
)

p1 <- ggplot(speedup_data, aes(x = workers, y = speedup)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", alpha = 0.7) +
  scale_x_continuous(breaks = c(1, 2, 4, 8)) +
  labs(
    title = "Thread-Safety Performance: Speedup vs Workers",
    subtitle = "Dashed line = ideal linear speedup",
    x = "Number of Workers",
    y = "Speedup (relative to 1 worker)"
  ) +
  theme_minimal()

p2 <- ggplot(speedup_data, aes(x = workers, y = efficiency)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 100, linetype = "dashed",
             color = "gray50", alpha = 0.7) +
  scale_x_continuous(breaks = c(1, 2, 4, 8)) +
  labs(
    title = "Parallel Efficiency",
    subtitle = "100% = perfect scaling",
    x = "Number of Workers",
    y = "Efficiency (%)"
  ) +
  theme_minimal()

# Save plots
ggsave("benchmarks/speedup_plot.png", p1, width = 8, height = 6)
ggsave("benchmarks/efficiency_plot.png", p2, width = 8, height = 6)

cat("\n=== Plots saved to benchmarks/ ===\n")
cat("- speedup_plot.png\n")
cat("- efficiency_plot.png\n")

# Summary Report ------------------------------------------------------

cat("\n=== PERFORMANCE SUMMARY ===\n\n")

cat("1. SCALING:\n")
cat("   2 workers:", sprintf("%.2fx speedup (%.0f%% efficient)\n",
                             speedup_data$speedup[2], speedup_data$efficiency[2]))
cat("   4 workers:", sprintf("%.2fx speedup (%.0f%% efficient)\n",
                             speedup_data$speedup[3], speedup_data$efficiency[3]))
cat("   8 workers:", sprintf("%.2fx speedup (%.0f%% efficient)\n",
                             speedup_data$speedup[4], speedup_data$efficiency[4]))

cat("\n2. OVERHEAD:\n")
cat("   Synchronization overhead:", round(overhead_pct, 1), "%\n")
cat("   (Difference from ideal 4x speedup)\n")

cat("\n3. RECOMMENDATIONS:\n")
if (speedup_data$efficiency[3] >= 75) {
  cat("   ✓ Good scaling up to 4 workers (",
      round(speedup_data$efficiency[3]), "% efficient)\n", sep = "")
  cat("   → Default worker_limit = 4 is reasonable\n")
} else {
  cat("   ⚠ Moderate scaling to 4 workers (",
      round(speedup_data$efficiency[3]), "% efficient)\n", sep = "")
  cat("   → Consider worker_limit = 2 as default\n")
}

if (speedup_data$efficiency[4] < 50) {
  cat("   ⚠ Poor scaling beyond 4 workers\n")
  cat("   → Don't recommend worker_limit > 4\n")
}

cat("\n4. MEMORY:\n")
mem_increase <- as.numeric(memory_benchmark$mem_alloc[2]) /
                as.numeric(memory_benchmark$mem_alloc[1])
cat("   Memory increase (1 → 4 workers):", sprintf("%.1fx\n", mem_increase))
if (mem_increase > 4) {
  cat("   ⚠ Memory overhead higher than linear\n")
} else {
  cat("   ✓ Memory scales reasonably with workers\n")
}

cat("\n=== Benchmark complete ===\n")
