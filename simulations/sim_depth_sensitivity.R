#!/usr/bin/env Rscript
# sim_depth_sensitivity.R — Cluster job C5: max_depth sweep
#
# Purpose: Verify that (a) depth-1 trees fail on interaction DGPs, (b) depth-2
# trees recover, and (c) increasing depth beyond 2 gives diminishing/no returns
# on DGPs that are well-captured by shallow trees.  Tests the depth_budget fix
# (commit 3c5d266): max_depth=d now correctly produces trees of actual depth d.
#
# DGPs:
#   R2  — quadratic (1 split suffices; x1^2 needs 1 stump on |x1|)
#   R4  — pure interaction (depth-1 cannot beat null; depth-2 is near-optimal)
#   C2  — quadratic propensity (log_loss; same structure as R2)
#   C4  — pure interaction propensity (log_loss; same structure as R4)
#
# Design:
#   max_depth in {1, 2, 3, 4}
#   n in {300, 1000, 3000}
#   lambda = log(n)/n (theory default)
#   k_bins = floor(n^{1/3})
#   Train/test split: 70/30
#
# Parameter grid: 4 DGPs x 4 depths x 3 n = 48 configs
#
# Usage (local test, 2 reps):
#   Rscript sim_depth_sensitivity.R --job_id 1 --n_reps 2 --out_dir /tmp/depth/
#
# Usage (cluster, 10 reps per job):
#   Rscript sim_depth_sensitivity.R --job_id $SLURM_ARRAY_TASK_ID \
#     --n_reps 10 --out_dir $OUT_DIR
#
# Module versions (O2 cluster): gcc/14.2.0, R/4.4.2
# See: .claude/skills/setup-cluster-simulations/SKILL.md

suppressPackageStartupMessages({
  tryCatch(
    { library(optparse) },
    error = function(e) {
      cat("ERROR: Cannot load 'optparse'.", conditionMessage(e), "\n",
          "Install with: install.packages('optparse')\n", file = stderr())
      quit(status = 1L)
    }
  )
  tryCatch(
    { library(optimaltrees) },
    error = function(e) {
      cat("ERROR: Cannot load 'optimaltrees'.", conditionMessage(e), "\n",
          "Install with: R CMD INSTALL <path_to_optimaltrees>\n", file = stderr())
      quit(status = 1L)
    }
  )
})

# ---- DGP definitions ---------------------------------------------------------

dgp_r2 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- X$x1^2
  y <- truth + rnorm(n, 0, 0.3)
  list(X = X, y = y, truth = truth, null_rmse = sqrt(2))
}

# Pure interaction: truth = 2*(x1>0)*(x2>0). Depth-1 predicts marginal = 0.5
# for all obs → RMSE = 0.5. Depth-2 can perfectly separate four quadrants.
dgp_r4 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  truth <- 2 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0)
  y <- truth + rnorm(n, 0, 0.5)
  list(X = X, y = y, truth = truth, null_rmse = 0.5)
}

dgp_c2 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- plogis(-1 + 3 * X$x1^2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# Pure interaction propensity: truth = plogis(3*(x1>0)*(x2>0) - 0.75).
# Depth-1 confounds x2>0 and x2<=0 within each x1 half-space.
dgp_c4 <- function(n, seed = 1L) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  truth <- plogis(3 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0) - 0.75)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

null_rmse_c2 <- local({
  set.seed(1L); x <- rnorm(1e5)
  truth <- plogis(-1 + 3 * x^2)
  sqrt(mean((truth - mean(truth))^2))
})

null_rmse_c4 <- local({
  set.seed(1L); x1 <- rnorm(1e5); x2 <- rnorm(1e5)
  truth <- plogis(3 * as.numeric(x1 > 0) * as.numeric(x2 > 0) - 0.75)
  sqrt(mean((truth - mean(truth))^2))
})

# ---- Parameter grid ----------------------------------------------------------

CONFIGS <- expand.grid(
  dgp       = c("R2", "R4", "C2", "C4"),
  max_depth = c(1L, 2L, 3L, 4L),
  n         = c(300L, 1000L, 3000L),
  stringsAsFactors = FALSE
)
N_CONFIGS <- nrow(CONFIGS)

# ---- CLI arguments -----------------------------------------------------------

option_list <- list(
  make_option("--job_id",  type = "integer",   default = 1L,           dest = "job_id"),
  make_option("--n_reps",  type = "integer",   default = 10L,          dest = "n_reps"),
  make_option("--out_dir", type = "character", default = "results/depth/", dest = "out_dir")
)
opts <- parse_args(OptionParser(option_list = option_list))

job_id  <- opts$job_id
n_reps  <- opts$n_reps
out_dir <- opts$out_dir

config_id <- ((job_id - 1L) %% N_CONFIGS) + 1L
batch_id  <- ((job_id - 1L) %/% N_CONFIGS) + 1L
cfg       <- CONFIGS[config_id, ]

cat(sprintf("Job %d | config %d/%d | DGP=%s depth=%d n=%d | batch=%d | reps=%d\n",
  job_id, config_id, N_CONFIGS, cfg$dgp, cfg$max_depth, cfg$n, batch_id, n_reps))

# ---- Simulation helpers ------------------------------------------------------

conv_bins       <- function(n) max(3L, as.integer(floor(n^(1 / 3))))
reg_logn_over_n <- function(n) log(n) / n
TRAIN_FRAC      <- 0.7

dgp_lookup <- list(R2 = dgp_r2, R4 = dgp_r4, C2 = dgp_c2, C4 = dgp_c4)
loss_lookup <- list(R2 = "squared_error", R4 = "squared_error",
                    C2 = "log_loss", C4 = "log_loss")

run_one_rep <- function(dgp_fn, n, loss_function, max_depth, seed) {
  d <- dgp_fn(n = n, seed = seed)
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
    loss_function   = loss_function,
    regularization  = lambda,
    discretize_bins = k_bins,
    max_depth       = max_depth,
    verbose         = FALSE
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  X_te <- d$X[idx_test, , drop = FALSE]
  pred <- if (loss_function == "squared_error") {
    predict(model, X_te)
  } else {
    predict(model, X_te, type = "prob")[, 2L]
  }
  truth_te <- d$truth[idx_test]
  rmse     <- sqrt(mean((pred - truth_te)^2))

  # actual depth of the fitted tree — recurse through false/true branches
  tree_depth <- function(node, d = 0L) {
    if (is.null(node$feature)) return(d)  # leaf: no feature key
    max(tree_depth(node$false, d + 1L), tree_depth(node$true, d + 1L))
  }
  actual_depth <- tryCatch(
    tree_depth(model@trees[[1]]),
    error = function(e) NA_integer_
  )

  data.frame(
    dgp       = cfg$dgp, n = n, max_depth = max_depth,
    seed      = seed,    batch = batch_id,
    lambda    = lambda,  k_bins = k_bins,
    actual_depth = actual_depth,
    rmse      = rmse,    elapsed = elapsed
  )
}

# ---- Run replications --------------------------------------------------------

dgp_fn    <- dgp_lookup[[cfg$dgp]]
loss_fn   <- loss_lookup[[cfg$dgp]]
seed_base <- (batch_id - 1L) * n_reps + 1L
seed_vec  <- seq(seed_base, seed_base + n_reps - 1L)

results_list <- vector("list", n_reps)
n_success    <- 0L

for (i in seq_along(seed_vec)) {
  s <- seed_vec[[i]]
  res <- tryCatch(
    run_one_rep(dgp_fn, cfg$n, loss_fn, cfg$max_depth, seed = s),
    error = function(e) {
      message(sprintf("ERROR rep %d (seed %d): %s", i, s, conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) {
    results_list[[i]] <- res
    n_success <- n_success + 1L
    cat(sprintf("  rep %d/%d: RMSE=%.4f depth_actual=%s elapsed=%.1fs\n",
      i, n_reps, res$rmse,
      ifelse(is.na(res$actual_depth), "NA", as.character(res$actual_depth)),
      res$elapsed))
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
  sprintf("depth_%s_d%d_n%d_cfg%02d_batch%03d.rds",
    cfg$dgp, cfg$max_depth, cfg$n, config_id, batch_id))
saveRDS(results, out_file)
cat(sprintf("Saved %d rows to %s\n", nrow(results), out_file))
