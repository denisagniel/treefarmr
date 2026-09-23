# Time each test file separately with a per-file cap to find a hanging test.
suppressMessages(devtools::load_all(quiet = TRUE))
files <- list.files("tests/testthat", pattern = "^test.*[.]R$", full.names = TRUE)
for (f in files) {
  setTimeLimit(elapsed = 45, transient = TRUE)
  t0 <- Sys.time()
  st <- tryCatch({
    suppressMessages(testthat::test_file(f, reporter = "silent"))
    "done"
  }, error = function(e) paste("ERR/TIMEOUT:", conditionMessage(e)))
  setTimeLimit()
  cat(sprintf("%6.1fs  %-45s %s\n",
              as.numeric(Sys.time() - t0, units = "secs"), basename(f), st))
}
