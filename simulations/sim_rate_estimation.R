#!/usr/bin/env Rscript
# sim_rate_estimation.R — Cluster job C1: RMSE vs n rate estimation
#
# Purpose: Empirical convergence rate for optimaltrees.
# Fits log-log regression of RMSE ~ n to estimate the empirical rate.
# Theory predicts O(n^{-1/3}) with K = floor(n^{1/3}) bins and lambda = log(n)/n.
#
# Usage (local test, 2 reps):
#   Rscript sim_rate_estimation.R --job_id 1 --n_reps 2 --out_dir /tmp/rate/
#
# Usage (cluster, 10 reps per job):
#   Rscript sim_rate_estimation.R --job_id $SLURM_ARRAY_TASK_ID \
#     --n_reps 10 --out_dir $OUT_DIR
#
# Parameter grid:
#   DGPs: R2 (squared_error), C2 (log_loss)
#   n:    500, 1000, 2000, 4000, 8000, 20000, 50000
#   Total configs: 2 x 7 = 14
#   Array mapping: job_id = (batch - 1) * n_configs + config_id
#     => config_id = ((job_id - 1) %% n_configs) + 1
#     => batch_id  = ((job_id - 1) %/% n_configs) + 1
#
# Module versions (O2 cluster): gcc/14.2.0, R/4.4.2
# See: .claude/skills/setup-cluster-simulations/SKILL.md

suppressPackageStartupMessages({
  result <- tryCatch(
    { library(optparse); TRUE },
    error = function(e) {
      cat("ERROR: Cannot load 'optparse'.", conditionMessage(e), "\n",
          "Install with: install.packages('optparse')\n", file = stderr())
      quit(status = 1L)
    }
  )
  result2 <- tryCatch(
    { library(optimaltrees); TRUE },
    error = function(e) {
      cat("ERROR: Cannot load 'optimaltrees'.", conditionMessage(e), "\n",
          "Install with: R CMD INSTALL <path_to_optimaltrees>\n", file = stderr())
      quit(status = 1L)
    }
  )
})

# ---- DGP definitions (inline for portability) --------------------------------

dgp_r2 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- X$x1^2
  y <- truth + rnorm(n, 0, 0.3)
  list(X = X, y = y, truth = truth)
}

dgp_c2 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- plogis(-1 + 3 * X$x1^2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

null_rmse_r2 <- sqrt(2)
null_rmse_c2 <- local({
  set.seed(1L); x <- rnorm(1e5L)
  truth <- plogis(-1 + 3 * x^2)
  sqrt(mean((truth - mean(truth))^2))
})

# ---- Parameter grid ----------------------------------------------------------

CONFIGS <- expand.grid(
  dgp = c("R2", "C2"),
  n   = c(500L, 1000L, 2000L, 4000L, 8000L, 20000L, 50000L),
  stringsAsFactors = FALSE
)
N_CONFIGS <- nrow(CONFIGS)

# ---- CLI arguments -----------------------------------------------------------

option_list <- list(
  make_option("--job_id",  type = "integer",   default = 1L,         dest = "job_id"),
  make_option("--n_reps",  type = "integer",   default = 10L,        dest = "n_reps"),
  make_option("--out_dir", type = "character", default = "results/rate/", dest = "out_dir")
)
opts <- parse_args(OptionParser(option_list = option_list))

job_id  <- opts$job_id
n_reps  <- opts$n_reps
out_dir <- opts$out_dir

config_id <- ((job_id - 1L) %% N_CONFIGS) + 1L
batch_id  <- ((job_id - 1L) %/% N_CONFIGS) + 1L
cfg       <- CONFIGS[config_id, ]

cat(sprintf("Job %d | config %d/%d | DGP=%s n=%d | batch=%d | reps=%d\n",
  job_id, config_id, N_CONFIGS, cfg$dgp, cfg$n, batch_id, n_reps))

# ---- Simulation helpers ------------------------------------------------------

conv_bins        <- function(n) max(3L, as.integer(floor(n^(1 / 3))))
reg_logn_over_n  <- function(n) log(n) / n
CONV_MAX_DEPTH   <- 3L
TRAIN_FRAC       <- 0.7

run_one_rep <- function(dgp_fn, n, loss_function, seed) {
  d <- dgp_fn(n = n, seed = seed)
  n_train <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx       <- sample(seq_len(n))
  idx_train <- idx[seq_len(n_train)]
  idx_test  <- idx[(n_train + 1L):n]
  X_tr <- d$X[idx_train, , drop = FALSE]
  y_tr <- d$y[idx_train]
  lambda   <- reg_logn_over_n(n_train)
  k_bins   <- conv_bins(n_train)

  t0 <- proc.time()[["elapsed"]]
  model <- optimaltrees(
    X = X_tr, y = y_tr,
    loss_function = loss_function,
    regularization = lambda,
    discretize_bins = k_bins,
    max_depth = CONV_MAX_DEPTH,
    verbose = FALSE
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  pred <- if (loss_function == "squared_error") {
    predict(model, d$X[idx_test, , drop = FALSE])
  } else {
    predict(model, d$X[idx_test, , drop = FALSE], type = "prob")[, 2L]
  }
  rmse <- sqrt(mean((pred - d$truth[idx_test])^2))

  data.frame(
    dgp = cfg$dgp, n = n, seed = seed, batch = batch_id,
    lambda = lambda, k_bins = k_bins, rmse = rmse, elapsed = elapsed
  )
}

# ---- Run replications --------------------------------------------------------

dgp_fn       <- if (cfg$dgp == "R2") dgp_r2 else dgp_c2
loss_fn      <- if (cfg$dgp == "R2") "squared_error" else "log_loss"
seed_base    <- (batch_id - 1L) * n_reps + 1L
seed_vec     <- seq(seed_base, seed_base + n_reps - 1L)

results_list <- vector("list", n_reps)
n_success    <- 0L

for (i in seq_along(seed_vec)) {
  s <- seed_vec[[i]]
  res <- tryCatch(
    run_one_rep(dgp_fn, cfg$n, loss_fn, seed = s),
    error = function(e) {
      message(sprintf("ERROR rep %d (seed %d): %s", i, s, conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) {
    results_list[[i]] <- res
    n_success <- n_success + 1L
    cat(sprintf("  rep %d/%d: RMSE=%.4f lambda=%.5f\n", i, n_reps, res$rmse, res$lambda))
  }
}

results <- do.call(rbind, Filter(Negate(is.null), results_list))
cat(sprintf("Completed %d/%d replications.\n", n_success, n_reps))

if (n_success < n_reps / 2) {
  cat("FATAL: <50% success rate.\n", file = stderr())
  quit(status = 1L)
}

# ---- Save results ------------------------------------------------------------

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir,
  sprintf("rate_%s_n%d_cfg%02d_batch%03d.rds", cfg$dgp, cfg$n, config_id, batch_id))
saveRDS(results, out_file)
cat(sprintf("Saved %d rows to %s\n", nrow(results), out_file))
