# Minimal test to isolate parallel execution issue

library(optimaltrees)

cat("=== Minimal Parallel Test ===\n\n")

set.seed(42)
n <- 100
p <- 10
X <- matrix(rbinom(n * p, 1, 0.5), nrow = n, ncol = p)
X <- as.data.frame(X)
names(X) <- paste0("x", 1:p)
y <- rbinom(n, 1, 0.5)

cat("Test 1: Single worker (should be fast)\n")
start <- Sys.time()
model1 <- fit_tree(X, y, worker_limit = 1, regularization = 0.1, verbose = FALSE)
time1 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat("  Completed in", round(time1, 4), "seconds\n\n")

cat("Test 2: Two workers (checking for hang)\n")
cat("  Starting...\n")
start <- Sys.time()
model2 <- fit_tree(X, y, worker_limit = 2, regularization = 0.1, verbose = FALSE)
time2 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat("  Completed in", round(time2, 4), "seconds\n\n")

cat("✓ No hang detected\n")
cat("  1 worker:", round(time1, 4), "sec\n")
cat("  2 workers:", round(time2, 4), "sec\n")
cat("  Ratio:", round(time2/time1, 2), "x\n")
