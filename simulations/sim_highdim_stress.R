#!/usr/bin/env Rscript
# sim_highdim_stress.R — Cluster job C2: High-dimensional stress test
#
# Purpose: RMSE vs p (number of features) at fixed n.
# Tests whether optimaltrees degrades gracefully when p grows with noise features.
# Constitution §9 stress regime: high-dimensional noise.
#
# Usage (local test, 2 reps):
#   Rscript sim_highdim_stress.R --job_id 1 --n_reps 2 --out_dir /tmp/highdim/
#
# Usage (cluster):
#   Rscript sim_highdim_stress.R --job_id $SLURM_ARRAY_TASK_ID \
#     --n_reps 10 --out_dir $OUT_DIR
#
# Parameter grid:
#   DGPs: R3 (squared_error), C6 (log_loss)
#   n:    1200, 5000
#   p:    2, 5, 10, 20
#   Total configs: 2 x 2 x 4 = 16
#   Array mapping: job_id = (batch - 1) * n_configs + config_id
#
# Module versions (O2 cluster): gcc/14.2.0, R/4.4.2
# See: .claude/skills/setup-cluster-simulations/SKILL.md

suppressPackageStartupMessages({
  tryCatch({ library(optparse) }, error = function(e) {
    cat("ERROR: Cannot load 'optparse'.", conditionMessage(e), "\n",
        "Install with: install.packages('optparse')\n", file = stderr())
    quit(status = 1L)
  })
  tryCatch({ library(optimaltrees) }, error = function(e) {
    cat("ERROR: Cannot load 'optimaltrees'.", conditionMessage(e), "\n",
        "Install with: R CMD INSTALL <path_to_optimaltrees>\n", file = stderr())
    quit(status = 1L)
  })
})

# ---- DGP definitions (inline for portability) --------------------------------

dgp_r3 <- function(n, p = 4L, seed = 1L) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- X$x1 + 0.5 * X$x2
  y <- truth + rnorm(n, 0, 1)
  list(X = X, y = y, truth = truth, null_rmse = sqrt(1.25))
}

dgp_c6 <- function(n, p = 4L, seed = 1L) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  n_signal <- min(3L, p)
  signal <- rowSums(sapply(seq_len(n_signal), function(j) 0.5 * X[[j]]))
  truth <- plogis(signal)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

null_rmse_r3 <- sqrt(1.25)
null_rmse_c6 <- local({
  set.seed(1L)
  X <- matrix(rnorm(1e5L * 3L), ncol = 3L)
  truth <- plogis(0.5 * X[, 1] + 0.5 * X[, 2] + 0.5 * X[, 3])
  sqrt(mean((truth - mean(truth))^2))
})

# ---- Parameter grid ----------------------------------------------------------

CONFIGS <- expand.grid(
  dgp = c("R3", "C6"),
  n   = c(1200L, 5000L),
  p   = c(2L, 5L, 10L, 20L),
  stringsAsFactors = FALSE
)
N_CONFIGS <- nrow(CONFIGS)

# ---- CLI arguments -----------------------------------------------------------

option_list <- list(
  make_option("--job_id",  type = "integer",   default = 1L,           dest = "job_id"),
  make_option("--n_reps",  type = "integer",   default = 10L,          dest = "n_reps"),
  make_option("--out_dir", type = "character", default = "results/highdim/", dest = "out_dir")
)
opts <- parse_args(OptionParser(option_list = option_list))

job_id  <- opts$job_id
n_reps  <- opts$n_reps
out_dir <- opts$out_dir

config_id <- ((job_id - 1L) %% N_CONFIGS) + 1L
batch_id  <- ((job_id - 1L) %/% N_CONFIGS) + 1L
cfg       <- CONFIGS[config_id, ]

cat(sprintf("Job %d | config %d/%d | DGP=%s n=%d p=%d | batch=%d | reps=%d\n",
  job_id, config_id, N_CONFIGS, cfg$dgp, cfg$n, cfg$p, batch_id, n_reps))

# ---- Simulation helpers ------------------------------------------------------

# For high-dim: use CV lambda (more adaptive than formula at large p)
CONV_MAX_DEPTH  <- 3L
TRAIN_FRAC      <- 0.7

run_one_rep <- function(dgp_fn, n, p, loss_function, seed) {
  d <- dgp_fn(n = n, p = p, seed = seed)
  n_train <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx       <- sample(seq_len(n))
  idx_train <- idx[seq_len(n_train)]
  idx_test  <- idx[(n_train + 1L):n]
  X_tr <- d$X[idx_train, , drop = FALSE]
  y_tr <- d$y[idx_train]

  # CV lambda for high-dim (formula lambda can underfit with many noise features)
  lambda <- tryCatch(
    cv_regularization_adaptive(X_tr, y_tr, loss_function = loss_function, verbose = FALSE)$best_lambda,
    error = function(e) log(n_train) / n_train  # fallback to formula
  )

  t0 <- proc.time()[["elapsed"]]
  model <- optimaltrees(
    X = X_tr, y = y_tr,
    loss_function = loss_function,
    regularization = lambda,
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
    dgp = cfg$dgp, n = n, p = p, seed = seed, batch = batch_id,
    lambda = lambda, rmse = rmse, elapsed = elapsed
  )
}

# ---- Run replications --------------------------------------------------------

dgp_fn  <- if (cfg$dgp == "R3") dgp_r3 else dgp_c6
loss_fn <- if (cfg$dgp == "R3") "squared_error" else "log_loss"
seed_base <- (batch_id - 1L) * n_reps + 1L
seed_vec  <- seq(seed_base, seed_base + n_reps - 1L)

results_list <- vector("list", n_reps)
n_success <- 0L

for (i in seq_along(seed_vec)) {
  s <- seed_vec[[i]]
  res <- tryCatch(
    run_one_rep(dgp_fn, cfg$n, cfg$p, loss_fn, seed = s),
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
  sprintf("highdim_%s_n%d_p%02d_cfg%02d_batch%03d.rds",
    cfg$dgp, cfg$n, cfg$p, config_id, batch_id))
saveRDS(results, out_file)
cat(sprintf("Saved %d rows to %s\n", nrow(results), out_file))
