#!/usr/bin/env Rscript
# Profile memory usage of testthat tests

library(testthat)

# Get list of test files
test_dir <- "tests/testthat"
test_files <- list.files(test_dir, pattern = "^test-.*\\.R$", full.names = TRUE)

# Profile each test file
results <- data.frame(
  file = character(),
  peak_memory_mb = numeric(),
  duration_sec = numeric(),
  stringsAsFactors = FALSE
)

for (test_file in test_files) {
  cat("Testing:", basename(test_file), "\n")

  # Get memory before
  gc()
  mem_before <- sum(gc()[, 2])  # Get Vcells used in MB

  # Run test with timing
  start_time <- Sys.time()
  tryCatch({
    test_file(test_file)
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
  end_time <- Sys.time()

  # Get memory after
  gc()
  mem_after <- sum(gc()[, 2])

  # Calculate memory used
  mem_used <- mem_after - mem_before
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  results <- rbind(results, data.frame(
    file = basename(test_file),
    peak_memory_mb = mem_used,
    duration_sec = duration
  ))

  cat("  Memory:", round(mem_used, 2), "MB, Duration:", round(duration, 2), "sec\n\n")

  # Force garbage collection between tests
  gc(full = TRUE)
}

# Sort by memory usage
results <- results[order(-results$peak_memory_mb), ]

# Print summary
cat("\n=== Memory Usage Summary ===\n")
print(results, row.names = FALSE)

cat("\n=== Top 5 Memory Consumers ===\n")
print(head(results, 5), row.names = FALSE)

# Save results
write.csv(results, "test_memory_profile.csv", row.names = FALSE)
cat("\nResults saved to test_memory_profile.csv\n")
