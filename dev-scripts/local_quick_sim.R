#!/usr/bin/env Rscript
# local_quick_sim.R — Run depth sensitivity + rate convergence locally.
#
# Study 1: Depth sensitivity (is max_depth constraint working? does interaction
#   DGP recover at depth 2?)
# Study 2: Rate convergence (does RMSE fall polynomially in n?)
#
# Uses parallel::mclapply for speed (~10 cores available).
# Total wall time: < 5 minutes with 10 cores.

suppressPackageStartupMessages({
  library(optimaltrees)
  library(parallel)
})

N_CORES  <- min(8L, parallel::detectCores() - 1L)
N_REPS   <- 30L  # reps per cell; enough for stable means with 30 obs
TRAIN_FRAC <- 0.7
conv_bins       <- function(n) max(3L, as.integer(floor(n^(1/3))))
reg_logn_over_n <- function(n) log(n) / n
tree_depth <- function(node, d = 0L) {
  if (is.null(node$feature)) return(d)
  max(tree_depth(node$false, d + 1L), tree_depth(node$true, d + 1L))
}

# ── DGPs ──────────────────────────────────────────────────────────────────────

dgp_r2 <- function(n, seed) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  list(X = X, y = X$x1^2 + rnorm(n, 0, 0.3), truth = X$x1^2,
       loss = "squared_error")
}

dgp_r4 <- function(n, seed) {  # pure interaction (requires depth >= 2)
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n),
                  x3 = rnorm(n), x4 = rnorm(n))
  truth <- 2 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0)
  list(X = X, y = truth + rnorm(n, 0, 0.5), truth = truth,
       loss = "squared_error")
}

dgp_c2 <- function(n, seed) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- plogis(-1 + 3 * X$x1^2)
  list(X = X, y = rbinom(n, 1, truth), truth = truth,
       loss = "log_loss")
}

dgp_c4 <- function(n, seed) {  # pure interaction (requires depth >= 2)
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n),
                  x3 = rnorm(n), x4 = rnorm(n))
  truth <- plogis(3 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0) - 0.75)
  list(X = X, y = rbinom(n, 1, truth), truth = truth,
       loss = "log_loss")
}

dgps <- list(R2 = dgp_r2, R4 = dgp_r4, C2 = dgp_c2, C4 = dgp_c4)

run_one <- function(dgp_fn, n, max_depth, seed) {
  d   <- dgp_fn(n, seed)
  n_tr <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx  <- sample(seq_len(n))
  X_tr <- d$X[idx[seq_len(n_tr)], , drop = FALSE]
  y_tr <- d$y[idx[seq_len(n_tr)]]
  X_te <- d$X[idx[(n_tr + 1L):n], , drop = FALSE]
  truth_te <- d$truth[idx[(n_tr + 1L):n]]

  lambda <- reg_logn_over_n(n_tr)
  k      <- conv_bins(n_tr)

  m <- tryCatch(
    optimaltrees(X_tr, y_tr, loss_function = d$loss,
                 regularization = lambda, discretize_bins = k,
                 max_depth = max_depth, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(m)) return(NULL)

  pred <- if (d$loss == "squared_error") {
    predict(m, X_te)
  } else {
    predict(m, X_te, type = "prob")[, 2L]
  }
  rmse <- sqrt(mean((pred - truth_te)^2))
  ad   <- tryCatch(tree_depth(m@trees[[1]]), error = function(e) NA_integer_)

  list(rmse = rmse, actual_depth = ad, lambda = lambda, k_bins = k)
}

# ── Study 1: Depth sensitivity ─────────────────────────────────────────────
cat("=== Study 1: Depth sensitivity ===\n")
cat(sprintf("Using %d cores, %d reps per cell\n\n", N_CORES, N_REPS))

depth_grid <- expand.grid(
  dgp_name  = c("R2", "R4", "C2", "C4"),
  max_depth = c(1L, 2L, 3L),
  n         = c(300L, 1000L),
  stringsAsFactors = FALSE
)

depth_tasks <- lapply(seq_len(nrow(depth_grid)), function(i) {
  row <- depth_grid[i, ]
  lapply(seq_len(N_REPS), function(s)
    list(dgp_name = row$dgp_name, n = row$n, max_depth = row$max_depth, seed = s))
}) |> unlist(recursive = FALSE)

t0 <- proc.time()[["elapsed"]]
depth_raw <- mclapply(depth_tasks, function(task) {
  res <- run_one(dgps[[task$dgp_name]], task$n, task$max_depth, task$seed)
  if (is.null(res)) return(NULL)
  data.frame(dgp = task$dgp_name, n = task$n, max_depth = task$max_depth,
             seed = task$seed, rmse = res$rmse, actual_depth = res$actual_depth,
             stringsAsFactors = FALSE)
}, mc.cores = N_CORES)
cat(sprintf("Study 1 done in %.1fs\n", proc.time()[["elapsed"]] - t0))

depth_results <- do.call(rbind, Filter(Negate(is.null), depth_raw))

# Summary table
cat("\nDepth sensitivity summary (mean RMSE across reps):\n")
for (dgp_name in c("R2", "R4", "C2", "C4")) {
  sub <- depth_results[depth_results$dgp == dgp_name, ]
  cat(sprintf("\nDGP=%s:\n", dgp_name))
  cat(sprintf("  %-12s  %-6s  %-10s  %-12s\n",
              "max_depth", "n", "mean_RMSE", "mean_depth"))
  for (d in c(1L, 2L, 3L)) {
    for (n_val in c(300L, 1000L)) {
      ss <- sub[sub$max_depth == d & sub$n == n_val, ]
      if (nrow(ss) == 0) next
      cat(sprintf("  %-12d  %-6d  %-10.4f  %-12.2f\n",
                  d, n_val, mean(ss$rmse), mean(ss$actual_depth, na.rm = TRUE)))
    }
  }
}

# Depth constraint check
cat("\nDepth constraint check (actual_depth <= max_depth for all reps):\n")
violations <- depth_results[!is.na(depth_results$actual_depth) &
                              depth_results$actual_depth > depth_results$max_depth, ]
if (nrow(violations) == 0) {
  cat("  PASS: No violations found.\n")
} else {
  cat(sprintf("  FAIL: %d violations:\n", nrow(violations)))
  print(violations)
}

# ── Study 2: Rate convergence ─────────────────────────────────────────────
cat("\n\n=== Study 2: Rate convergence (n sweep) ===\n")
cat(sprintf("Using %d cores, %d reps per cell\n\n", N_CORES, N_REPS))

rate_grid <- expand.grid(
  dgp_name = c("R2", "C2"),
  n        = c(200L, 500L, 1000L, 2000L, 5000L),
  stringsAsFactors = FALSE
)
# Use max_depth=3 (the theory default) for rate estimation
RATE_MAX_DEPTH <- 3L

rate_tasks <- lapply(seq_len(nrow(rate_grid)), function(i) {
  row <- rate_grid[i, ]
  lapply(seq_len(N_REPS), function(s)
    list(dgp_name = row$dgp_name, n = row$n, seed = s))
}) |> unlist(recursive = FALSE)

t0 <- proc.time()[["elapsed"]]
rate_raw <- mclapply(rate_tasks, function(task) {
  res <- run_one(dgps[[task$dgp_name]], task$n, RATE_MAX_DEPTH, task$seed)
  if (is.null(res)) return(NULL)
  data.frame(dgp = task$dgp_name, n = task$n, seed = task$seed,
             rmse = res$rmse, lambda = res$lambda, k_bins = res$k_bins,
             stringsAsFactors = FALSE)
}, mc.cores = N_CORES)
cat(sprintf("Study 2 done in %.1fs\n", proc.time()[["elapsed"]] - t0))

rate_results <- do.call(rbind, Filter(Negate(is.null), rate_raw))

cat("\nRate convergence summary:\n")
for (dgp_name in c("R2", "C2")) {
  sub <- rate_results[rate_results$dgp == dgp_name, ]
  cat(sprintf("\nDGP=%s:\n", dgp_name))
  cat(sprintf("  %-6s  %-8s  %-10s  %-10s  %-6s\n",
              "n", "lambda", "k_bins", "mean_RMSE", "reps"))
  for (n_val in sort(unique(sub$n))) {
    ss <- sub[sub$n == n_val, ]
    cat(sprintf("  %-6d  %-8.5f  %-10.1f  %-10.4f  %-6d\n",
                n_val,
                mean(ss$lambda), mean(ss$k_bins),
                mean(ss$rmse), nrow(ss)))
  }
}

# Log-log rate estimation
cat("\nEmpirical convergence rate (log-log regression of mean RMSE ~ log(n)):\n")
for (dgp_name in c("R2", "C2")) {
  sub   <- rate_results[rate_results$dgp == dgp_name, ]
  means <- tapply(sub$rmse, sub$n, mean)
  ns    <- as.numeric(names(means))
  fit   <- lm(log(means) ~ log(ns))
  cat(sprintf("  DGP=%s: rate exponent = %.3f (se=%.3f) [theory: -1/3 = -0.333]\n",
              dgp_name,
              coef(fit)["log(ns)"],
              summary(fit)$coefficients["log(ns)", "Std. Error"]))
}

# ── Save results ──────────────────────────────────────────────────────────
out_dir <- "dev-scripts/local_sim_results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(depth_results, file.path(out_dir, "depth_sensitivity.rds"))
saveRDS(rate_results,  file.path(out_dir, "rate_convergence.rds"))
cat(sprintf("\nResults saved to %s/\n", out_dir))
