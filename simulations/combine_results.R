#!/usr/bin/env Rscript
# combine_results.R — Aggregate outputs from all SLURM simulation jobs
#
# Usage:
#   Rscript combine_results.R --scratch_dir /n/scratch/users/d/<user>/optimaltrees_sims
#   Rscript combine_results.R --scratch_dir /tmp/test_results  # for local testing
#
# Reads all .rds files from {scratch_dir}/{rate,highdim,overlap,cv}/
# Combines each type and saves to simulations/results/<type>_combined.rds

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--scratch_dir", type = "character",
    default = file.path(Sys.getenv("HOME"), "optimaltrees_sims"),
    dest = "scratch_dir"),
  make_option("--out_dir", type = "character",
    default = "results",
    dest = "out_dir")
)
opts <- parse_args(OptionParser(option_list = option_list))

scratch_dir <- opts$scratch_dir
out_dir     <- opts$out_dir
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

combine_type <- function(type_name) {
  type_dir <- file.path(scratch_dir, type_name)
  if (!dir.exists(type_dir)) {
    cat(sprintf("  [%s] directory not found: %s\n", type_name, type_dir))
    return(invisible(NULL))
  }

  files <- list.files(type_dir, pattern = "\\.rds$", full.names = TRUE)
  cat(sprintf("  [%s] found %d files in %s\n", type_name, length(files), type_dir))
  if (length(files) == 0L) return(invisible(NULL))

  results <- lapply(files, function(f) {
    tryCatch(readRDS(f), error = function(e) {
      cat(sprintf("    WARN: Cannot read %s: %s\n", basename(f), conditionMessage(e)))
      NULL
    })
  })
  results <- Filter(Negate(is.null), results)
  combined <- do.call(rbind, results)

  out_file <- file.path(out_dir, sprintf("%s_combined.rds", type_name))
  saveRDS(combined, out_file)

  cat(sprintf("  [%s] %d rows from %d/%d files -> %s\n",
    type_name, nrow(combined), length(results), length(files), out_file))

  # Print summary
  if ("dgp" %in% names(combined) && "n" %in% names(combined) &&
      "rmse" %in% names(combined)) {
    summ <- tapply(combined$rmse, list(combined$dgp, combined$n), mean, na.rm = TRUE)
    cat(sprintf("  [%s] Mean RMSE by DGP x n:\n", type_name))
    print(round(summ, 4))
  }
  if ("lambda_cv" %in% names(combined) && "lambda_theory" %in% names(combined)) {
    summ <- tapply(combined$ratio_cv_theory, list(combined$dgp, combined$n),
                   mean, na.rm = TRUE)
    cat(sprintf("  [%s] Mean CV/theory ratio by DGP x n:\n", type_name))
    print(round(summ, 2))
  }
  cat("\n")
  invisible(combined)
}

cat("Combining simulation results\n")
cat(sprintf("Scratch dir: %s\n", scratch_dir))
cat(sprintf("Output dir:  %s\n\n", out_dir))

rate_results    <- combine_type("rate")
highdim_results <- combine_type("highdim")
overlap_results <- combine_type("overlap")
cv_results      <- combine_type("cv")

cat("Done.\n")
