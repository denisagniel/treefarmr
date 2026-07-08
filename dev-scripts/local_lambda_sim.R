#!/usr/bin/env Rscript
# local_lambda_sim.R — Lambda tuning sensitivity study (local run)
#
# Study 1: Lambda sweep.
#   For each DGP × n, sweep lambda across a grid from 0.1× to 10× theory and
#   record test RMSE. Answers: how sensitive is RMSE to lambda choice? Is
#   theory (log n / n) near the minimum?
#
# Study 2: CV vs theory comparison.
#   For each DGP × n, compare cv_regularization_adaptive to theory lambda on
#   held-out RMSE. Answers: does CV reliably beat theory? by how much?
#
# Design:
#   DGPs: R2 (squared_error), C2 (log_loss)
#   n: Study1: 300,1000,3000; Study2: 300,1000 (cv_regularization_adaptive is
#      ~8s/call at n=1000; n=3000 would be prohibitive locally)
#   Lambda grid (Study 1): 9 points log-spaced, 0.1x–10x theory
#   Reps: 30 (Study 1), 15 (Study 2)
#   Max_depth: 3; Bins: floor(n^{1/3}); Train fraction: 0.7

suppressPackageStartupMessages({
  library(optimaltrees)
  library(parallel)
})

N_CORES    <- min(8L, parallel::detectCores() - 1L)
MAX_DEPTH  <- 3L
TRAIN_FRAC <- 0.7
out_dir    <- "dev-scripts/local_sim_results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

conv_bins       <- function(n) max(3L, as.integer(floor(n^(1/3))))
reg_logn_over_n <- function(n) log(n) / n

# ── DGPs ──────────────────────────────────────────────────────────────────────

dgp_r2 <- function(n, seed) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  list(X = X, y = X$x1^2 + rnorm(n, 0, 0.3), truth = X$x1^2,
       loss = "squared_error")
}

dgp_c2 <- function(n, seed) {
  set.seed(seed)
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  truth <- plogis(-1 + 3 * X$x1^2)
  list(X = X, y = rbinom(n, 1, truth), truth = truth,
       loss = "log_loss")
}

dgps <- list(R2 = dgp_r2, C2 = dgp_c2)

fit_rmse <- function(dgp_fn, n, lambda, seed) {
  d    <- dgp_fn(n, seed)
  n_tr <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx  <- sample(seq_len(n))
  X_tr <- d$X[idx[seq_len(n_tr)], , drop = FALSE]
  y_tr <- d$y[idx[seq_len(n_tr)]]
  X_te <- d$X[idx[(n_tr + 1L):n], , drop = FALSE]
  tt   <- d$truth[idx[(n_tr + 1L):n]]
  k    <- conv_bins(n_tr)
  m    <- tryCatch(
    optimaltrees(X_tr, y_tr, loss_function = d$loss,
                 regularization = lambda, discretize_bins = k,
                 max_depth = MAX_DEPTH, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(m)) return(NA_real_)
  pred <- if (d$loss == "squared_error") predict(m, X_te)
          else predict(m, X_te, type = "prob")[, 2L]
  sqrt(mean((pred - tt)^2))
}

# ── Study 1: Lambda sweep ──────────────────────────────────────────────────────

cat("=== Study 1: Lambda sensitivity sweep ===\n")
cat(sprintf("Using %d cores, 30 reps per cell\n\n", N_CORES))

SWEEP_NS     <- c(300L, 1000L, 3000L)
LAMBDA_MULTS <- c(0.1, 0.2, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 10.0)
N_REPS_S1    <- 30L

sweep_tasks <- do.call(c, lapply(c("R2", "C2"), function(dg)
  lapply(SWEEP_NS, function(n)
    lapply(LAMBDA_MULTS, function(mult)
      lapply(seq_len(N_REPS_S1), function(s)
        list(dgp = dg, n = n, mult = mult, seed = s))
    ) |> unlist(recursive = FALSE)
  ) |> unlist(recursive = FALSE)
))

t0 <- proc.time()[["elapsed"]]
sweep_raw <- mclapply(sweep_tasks, function(task) {
  lambda_th  <- reg_logn_over_n(floor(TRAIN_FRAC * task$n))
  lambda_use <- task$mult * lambda_th
  rmse <- fit_rmse(dgps[[task$dgp]], task$n, lambda_use, task$seed)
  if (is.na(rmse)) return(NULL)
  data.frame(dgp = task$dgp, n = task$n, mult = task$mult,
             lambda = lambda_use, lambda_theory = lambda_th,
             seed = task$seed, rmse = rmse, stringsAsFactors = FALSE)
}, mc.cores = N_CORES)
cat(sprintf("Study 1 done in %.1fs\n", proc.time()[["elapsed"]] - t0))

sweep_results <- do.call(rbind, Filter(Negate(is.null), sweep_raw))
saveRDS(sweep_results, file.path(out_dir, "lambda_sweep.rds"))

cat("\nLambda sweep: mean RMSE vs multiplier\n")
for (dg in c("R2", "C2")) {
  sub <- sweep_results[sweep_results$dgp == dg, ]
  cat(sprintf("\nDGP=%s:\n", dg))
  header <- sprintf("  %-6s", "mult")
  for (n_val in SWEEP_NS) header <- paste0(header, sprintf("  n=%-6d", n_val))
  cat(header, "\n")
  for (mult in LAMBDA_MULTS) {
    line <- sprintf("  %-6.1f", mult)
    for (n_val in SWEEP_NS) {
      ss <- sub[sub$mult == mult & sub$n == n_val, ]
      line <- paste0(line, sprintf("  %-8.4f", mean(ss$rmse)))
    }
    marker <- if (mult == 1.0) "  <-- theory" else ""
    cat(line, marker, "\n")
  }
  cat("  Best mult per n:")
  for (n_val in SWEEP_NS) {
    ss   <- sub[sub$n == n_val, ]
    agg  <- aggregate(rmse ~ mult, ss, mean)
    best <- agg$mult[which.min(agg$rmse)]
    cat(sprintf("  n=%d→%.1fx", n_val, best))
  }
  cat("\n")
}

# ── Study 2: CV vs theory ─────────────────────────────────────────────────────

cat("\n\n=== Study 2: CV vs theory lambda comparison ===\n")
cat(sprintf("Using %d cores, 15 reps per cell (cv_regularization_adaptive is ~1-8s/call)\n\n", N_CORES))

CV_NS     <- c(300L, 1000L)
N_REPS_S2 <- 15L

cv_tasks <- do.call(c, lapply(c("R2", "C2"), function(dg)
  lapply(CV_NS, function(n)
    lapply(seq_len(N_REPS_S2), function(s) list(dgp = dg, n = n, seed = s))
  ) |> unlist(recursive = FALSE)
))

run_cv_rep <- function(dg, n, seed) {
  d    <- dgps[[dg]](n, seed)
  n_tr <- floor(TRAIN_FRAC * n)
  set.seed(seed * 1000L + 7L)
  idx  <- sample(seq_len(n))
  X_tr <- d$X[idx[seq_len(n_tr)], , drop = FALSE]
  y_tr <- d$y[idx[seq_len(n_tr)]]
  X_te <- d$X[idx[(n_tr + 1L):n], , drop = FALSE]
  tt   <- d$truth[idx[(n_tr + 1L):n]]
  k    <- conv_bins(n_tr)
  lambda_th <- reg_logn_over_n(n_tr)

  fit_pred <- function(lambda) {
    m <- tryCatch(
      optimaltrees(X_tr, y_tr, loss_function = d$loss,
                   regularization = lambda, discretize_bins = k,
                   max_depth = MAX_DEPTH, verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(m)) return(NA_real_)
    pred <- if (d$loss == "squared_error") predict(m, X_te)
            else predict(m, X_te, type = "prob")[, 2L]
    sqrt(mean((pred - tt)^2))
  }

  rmse_th <- fit_pred(lambda_th)

  cv_out <- tryCatch(
    cv_regularization_adaptive(X_tr, y_tr, loss_function = d$loss, verbose = FALSE),
    error = function(e) NULL
  )
  lambda_cv <- if (!is.null(cv_out)) cv_out$best_lambda else NA_real_
  rmse_cv   <- if (!is.na(lambda_cv)) fit_pred(lambda_cv) else NA_real_

  data.frame(dgp = dg, n = n, seed = seed,
             lambda_theory = lambda_th,
             lambda_cv     = lambda_cv,
             ratio         = if (!is.na(lambda_cv)) lambda_cv / lambda_th else NA_real_,
             rmse_theory   = rmse_th,
             rmse_cv       = rmse_cv,
             diff          = rmse_cv - rmse_th,
             stringsAsFactors = FALSE)
}

t0 <- proc.time()[["elapsed"]]
cv_raw <- mclapply(cv_tasks, function(task) {
  tryCatch(run_cv_rep(task$dgp, task$n, task$seed), error = function(e) NULL)
}, mc.cores = N_CORES)
cat(sprintf("Study 2 done in %.1fs\n", proc.time()[["elapsed"]] - t0))

cv_results <- do.call(rbind, Filter(Negate(is.null), cv_raw))
saveRDS(cv_results, file.path(out_dir, "cv_vs_theory.rds"))

cat("\nCV vs theory (negative diff = CV better than theory):\n")
cat(sprintf("  %-4s  %-5s  %-10s  %-10s  %-8s  %-8s  %-8s  %-10s\n",
            "DGP", "n", "lam_theory", "lam_cv(med)", "ratio(med)",
            "RMSE_th", "RMSE_cv", "diff(cv-th)"))
for (dg in c("R2", "C2")) {
  for (n_val in CV_NS) {
    ss <- cv_results[cv_results$dgp == dg & cv_results$n == n_val, ]
    ss <- ss[!is.na(ss$rmse_cv), ]
    if (nrow(ss) == 0) next
    cat(sprintf("  %-4s  %-5d  %-10.5f  %-10.5f  %-8.2f  %-8.4f  %-8.4f  %+.4f  [%d reps]\n",
      dg, n_val,
      mean(ss$lambda_theory),
      median(ss$lambda_cv),
      median(ss$ratio),
      mean(ss$rmse_theory),
      mean(ss$rmse_cv),
      mean(ss$diff),
      nrow(ss)))
  }
}

cat(sprintf("\nResults saved to %s/\n", out_dir))
