#!/usr/bin/env Rscript
# sim_cv_consistency.R — Cluster job C4: CV lambda consistency check
#
# Purpose: Verify that cv_regularization_adaptive() selects lambda that scales
# consistently with the theory rate log(n)/n across n.
#
# For each (DGP, n), records:
#   - Selected lambda from cv_regularization_adaptive()
#   - Theory lambda = log(n)/n
#   - Ratio: cv_lambda / theory_lambda
#   - OOS RMSE under cv_lambda vs theory_lambda
#
# If the ratio is ~constant across n: CV is consistent (selects lambda at
# the right rate). If ratio grows with n: CV is too conservative at large n.
#
# Usage (local test, 2 reps):
#   Rscript sim_cv_consistency.R --job_id 1 --n_reps 2 --out_dir /tmp/cv/
#
# Usage (cluster):
#   Rscript sim_cv_consistency.R --job_id $SLURM_ARRAY_TASK_ID \
#     --n_reps 10 --out_dir $OUT_DIR
#
# Parameter grid:
#   DGPs: R2 (squared_error), C2 (log_loss)
#   n:    200, 500, 1000, 2000, 5000, 10000
#   Total configs: 2 x 6 = 12
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

# ---- Parameter grid ----------------------------------------------------------

CONFIGS <- expand.grid(
  dgp = c("R2", "C2"),
  n   = c(200L, 500L, 1000L, 2000L, 5000L, 10000L),
  stringsAsFactors = FALSE
)
N_CONFIGS <- nrow(CONFIGS)

# ---- CLI arguments -----------------------------------------------------------

option_list <- list(
  make_option("--job_id",  type = "integer",   default = 1L,         dest = "job_id"),
  make_option("--n_reps",  type = "integer",   default = 10L,        dest = "n_reps"),
  make_option("--out_dir", type = "character", default = "results/cv/", dest = "out_dir")
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

conv_bins       <- function(n) max(3L, as.integer(floor(n^(1 / 3))))
CONV_MAX_DEPTH  <- 3L
TRAIN_FRAC      <- 0.7

rmse_with_lambda <- function(model_fn, X_te, truth_te, loss_function) {
  pred <- if (loss_function == "squared_error") {
    model_fn(X_te)
  } else {
    model_fn(X_te, type = "prob")[, 2L]
  }
  sqrt(mean((pred - truth_te)^2))
}

run_one_rep <- function(dgp_fn, n, loss_function, seed) {
  d <- dgp_fn(n = n, seed = seed)
  n_train <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx       <- sample(seq_len(n))
  idx_train <- idx[seq_len(n_train)]
  idx_test  <- idx[(n_train + 1L):n]
  X_tr <- d$X[idx_train, , drop = FALSE]
  y_tr <- d$y[idx_train]
  X_te <- d$X[idx_test, , drop = FALSE]
  truth_te <- d$truth[idx_test]
  k_bins <- conv_bins(n_train)

  # CV lambda
  lambda_cv <- tryCatch(
    cv_regularization_adaptive(X_tr, y_tr, loss_function = loss_function, verbose = FALSE)$best_lambda,
    error = function(e) NA_real_
  )
  lambda_theory <- log(n_train) / n_train

  # Fit with CV lambda
  rmse_cv <- NA_real_
  if (!is.na(lambda_cv)) {
    m_cv <- tryCatch(
      optimaltrees(X_tr, y_tr, loss_function = loss_function,
                   regularization = lambda_cv,
                   discretize_bins = k_bins,
                   max_depth = CONV_MAX_DEPTH, verbose = FALSE),
      error = function(e) NULL
    )
    if (!is.null(m_cv)) {
      rmse_cv <- rmse_with_lambda(
        function(X, ...) predict(m_cv, X, ...),
        X_te, truth_te, loss_function
      )
    }
  }

  # Fit with theory lambda
  m_th <- tryCatch(
    optimaltrees(X_tr, y_tr, loss_function = loss_function,
                 regularization = lambda_theory,
                 discretize_bins = k_bins,
                 max_depth = CONV_MAX_DEPTH, verbose = FALSE),
    error = function(e) NULL
  )
  rmse_theory <- if (!is.null(m_th)) {
    rmse_with_lambda(
      function(X, ...) predict(m_th, X, ...),
      X_te, truth_te, loss_function
    )
  } else NA_real_

  data.frame(
    dgp = cfg$dgp, n = n, seed = seed, batch = batch_id,
    lambda_cv = lambda_cv,
    lambda_theory = lambda_theory,
    ratio_cv_theory = if (!is.na(lambda_cv)) lambda_cv / lambda_theory else NA_real_,
    rmse_cv = rmse_cv,
    rmse_theory = rmse_theory
  )
}

# ---- Run replications --------------------------------------------------------

dgp_fn  <- if (cfg$dgp == "R2") dgp_r2 else dgp_c2
loss_fn <- if (cfg$dgp == "R2") "squared_error" else "log_loss"
seed_base <- (batch_id - 1L) * n_reps + 1L
seed_vec  <- seq(seed_base, seed_base + n_reps - 1L)

results_list <- vector("list", n_reps)
n_success <- 0L

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
    cat(sprintf("  rep %d/%d: cv_lambda=%.5f theory=%.5f ratio=%.2f RMSE_cv=%.4f\n",
      i, n_reps, res$lambda_cv, res$lambda_theory, res$ratio_cv_theory, res$rmse_cv))
  }
}

results <- do.call(rbind, Filter(Negate(is.null), results_list))
cat(sprintf("Completed %d/%d replications.\n", n_success, n_reps))
if (n_success > 0L) {
  cat(sprintf("  Mean cv_lambda=%.5f, mean theory_lambda=%.5f, mean ratio=%.2f\n",
    mean(results$lambda_cv, na.rm = TRUE),
    mean(results$lambda_theory, na.rm = TRUE),
    mean(results$ratio_cv_theory, na.rm = TRUE)))
}

if (n_success < n_reps / 2) {
  cat("FATAL: <50% success rate.\n", file = stderr())
  quit(status = 1L)
}

# ---- Save results ------------------------------------------------------------

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir,
  sprintf("cv_%s_n%d_cfg%02d_batch%03d.rds", cfg$dgp, cfg$n, config_id, batch_id))
saveRDS(results, out_file)
cat(sprintf("Saved %d rows to %s\n", nrow(results), out_file))
