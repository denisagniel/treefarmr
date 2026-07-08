#!/usr/bin/env Rscript
# sim_propensity_overlap.R — Cluster job C3: Propensity overlap stress test
#
# Purpose: RMSE vs n for DGP with near-zero/near-one propensities in tails.
# Constitution §9 stress regime: behavior near the boundary where optimal tree
# estimators may struggle. Critical for DML/doubletree applications.
#
# DGP C8: truth = plogis(x1 + 2*x2*(x2 > 1)), x1,x2,...~N(0,1).
# Creates near-zero propensities in tails of x2 (~16% of observations).
# Documents how RMSE and coverage change as n grows for this hard case.
#
# Usage (local test, 2 reps):
#   Rscript sim_propensity_overlap.R --job_id 1 --n_reps 2 --out_dir /tmp/overlap/
#
# Usage (cluster):
#   Rscript sim_propensity_overlap.R --job_id $SLURM_ARRAY_TASK_ID \
#     --n_reps 10 --out_dir $OUT_DIR
#
# Parameter grid:
#   DGP: C8
#   n:   500, 1000, 2000, 5000
#   p:   4 (2 signal, 2 noise)
#   Total configs: 4
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

# ---- DGP definition (inline for portability) ---------------------------------

# C8: Overlap stress propensity.
# truth = plogis(x1 + 2*x2*(x2 > 1)): near-zero/near-one in upper tail of x2.
dgp_c8 <- function(n, p = 4L, seed = 1L) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- plogis(X$x1 + 2 * X$x2 * (X$x2 > 1))
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

null_rmse_c8 <- local({
  set.seed(1L); x1 <- rnorm(1e5L); x2 <- rnorm(1e5L)
  truth <- plogis(x1 + 2 * x2 * (x2 > 1))
  sqrt(mean((truth - mean(truth))^2))
})

# ---- Parameter grid ----------------------------------------------------------

CONFIGS <- data.frame(
  n = c(500L, 1000L, 2000L, 5000L),
  p = 4L,
  stringsAsFactors = FALSE
)
N_CONFIGS <- nrow(CONFIGS)

# ---- CLI arguments -----------------------------------------------------------

option_list <- list(
  make_option("--job_id",  type = "integer",   default = 1L,             dest = "job_id"),
  make_option("--n_reps",  type = "integer",   default = 10L,            dest = "n_reps"),
  make_option("--out_dir", type = "character", default = "results/overlap/", dest = "out_dir")
)
opts <- parse_args(OptionParser(option_list = option_list))

job_id  <- opts$job_id
n_reps  <- opts$n_reps
out_dir <- opts$out_dir

config_id <- ((job_id - 1L) %% N_CONFIGS) + 1L
batch_id  <- ((job_id - 1L) %/% N_CONFIGS) + 1L
cfg       <- CONFIGS[config_id, ]

cat(sprintf("Job %d | config %d/%d | DGP=C8 n=%d p=%d | batch=%d | reps=%d\n",
  job_id, config_id, N_CONFIGS, cfg$n, cfg$p, batch_id, n_reps))

# ---- Simulation helpers ------------------------------------------------------

conv_bins       <- function(n) max(3L, as.integer(floor(n^(1 / 3))))
reg_logn_over_n <- function(n) log(n) / n
CONV_MAX_DEPTH  <- 3L
TRAIN_FRAC      <- 0.7

run_one_rep <- function(n, p, seed) {
  d <- dgp_c8(n = n, p = p, seed = seed)
  n_train <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx       <- sample(seq_len(n))
  idx_train <- idx[seq_len(n_train)]
  idx_test  <- idx[(n_train + 1L):n]
  X_tr <- d$X[idx_train, , drop = FALSE]
  y_tr <- d$y[idx_train]
  lambda <- reg_logn_over_n(n_train)
  k_bins <- conv_bins(n_train)

  t0 <- proc.time()[["elapsed"]]
  model <- optimaltrees(
    X = X_tr, y = y_tr,
    loss_function = "log_loss",
    regularization = lambda,
    discretize_bins = k_bins,
    max_depth = CONV_MAX_DEPTH,
    verbose = FALSE
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  pred <- predict(model, d$X[idx_test, , drop = FALSE], type = "prob")[, 2L]
  truth_te <- d$truth[idx_test]
  rmse <- sqrt(mean((pred - truth_te)^2))

  # Also compute quantile-stratified RMSE: lower quantile (overlap stress region)
  # and upper quantile (interior of propensity support)
  q10 <- quantile(truth_te, 0.10)
  q90 <- quantile(truth_te, 0.90)
  tail_idx <- truth_te < q10 | truth_te > q90
  rmse_tail    <- if (sum(tail_idx) > 5L) sqrt(mean((pred[tail_idx] - truth_te[tail_idx])^2)) else NA_real_
  rmse_interior <- sqrt(mean((pred[!tail_idx] - truth_te[!tail_idx])^2))

  data.frame(
    dgp = "C8", n = n, p = p, seed = seed, batch = batch_id,
    lambda = lambda, k_bins = k_bins,
    rmse = rmse, rmse_tail = rmse_tail, rmse_interior = rmse_interior,
    elapsed = elapsed
  )
}

# ---- Run replications --------------------------------------------------------

seed_base <- (batch_id - 1L) * n_reps + 1L
seed_vec  <- seq(seed_base, seed_base + n_reps - 1L)

results_list <- vector("list", n_reps)
n_success <- 0L

for (i in seq_along(seed_vec)) {
  s <- seed_vec[[i]]
  res <- tryCatch(
    run_one_rep(cfg$n, cfg$p, seed = s),
    error = function(e) {
      message(sprintf("ERROR rep %d (seed %d): %s", i, s, conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) {
    results_list[[i]] <- res
    n_success <- n_success + 1L
    cat(sprintf("  rep %d/%d: RMSE=%.4f (tail=%s, int=%.4f)\n",
      i, n_reps, res$rmse,
      if (is.na(res$rmse_tail)) "NA" else sprintf("%.4f", res$rmse_tail),
      res$rmse_interior))
  }
}

results <- do.call(rbind, Filter(Negate(is.null), results_list))
cat(sprintf("Completed %d/%d replications. Null RMSE=%.4f\n",
  n_success, n_reps, null_rmse_c8))

if (n_success < n_reps / 2) {
  cat("FATAL: <50% success rate.\n", file = stderr())
  quit(status = 1L)
}

# ---- Save results ------------------------------------------------------------

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir,
  sprintf("overlap_C8_n%d_cfg%02d_batch%03d.rds", cfg$n, config_id, batch_id))
saveRDS(results, out_file)
cat(sprintf("Saved %d rows to %s\n", nrow(results), out_file))
