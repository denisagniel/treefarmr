# Find a hanging/slow test file by running each in a fresh, KILLABLE subprocess
# with a hard wall-clock timeout. Unlike setTimeLimit(), callr kills the OS
# process, so this interrupts hangs inside C/C++ (e.g. GOSDT enumeration).
# Results are flushed to a log after every file, so progress survives a kill.

log_path <- "dev-scripts/hang_hunt_log.txt"
cat("", file = log_path)  # truncate
files <- list.files("tests/testthat", pattern = "^test.*[.]R$", full.names = TRUE)

for (f in files) {
  t0 <- Sys.time()
  res <- tryCatch(
    callr::r(
      function(tf) {
        suppressMessages(devtools::load_all(quiet = TRUE))
        out <- testthat::test_file(tf, reporter = "silent")
        df <- as.data.frame(out)
        c(fail = sum(df$failed), err = sum(df$error), n = nrow(df))
      },
      args = list(tf = normalizePath(f)),
      timeout = 60,          # hard wall-clock seconds; process killed on breach
      show = FALSE
    ),
    error = function(e) paste("TIMEOUT/ERR:", conditionMessage(e))
  )
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  line <- if (is.character(res)) {
    sprintf("%6.1fs  %-45s %s", dt, basename(f), res)
  } else {
    sprintf("%6.1fs  %-45s fail=%d err=%d n=%d",
            dt, basename(f), res[["fail"]], res[["err"]], res[["n"]])
  }
  cat(line, "\n", file = log_path, append = TRUE)
}
cat("HUNT COMPLETE\n", file = log_path, append = TRUE)
