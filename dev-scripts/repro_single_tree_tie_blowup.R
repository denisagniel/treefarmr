# ---------------------------------------------------------------------------
# Reproduction + timing harness for the single-tree tie-enumeration blowup.
#
# Reuses (read-only) the DGP from
#   doubletree/simulations/remainder_terms_value_recovery/code/dgp.R
# which is the setting that produced the original crash: p = 5 continuous
# covariates binarised to 2 thresholds/coordinate (10 binary features),
# leaf budget L, depth budget = partition_depth of the true partition.
#
# Usage:  Rscript dev-scripts/repro_single_tree_tie_blowup.R [mode]
#   mode = "repro"  (default) L=30, n=2000 crash case
#   mode = "grid"   wall-clock over n in {2000,20000} x L in {5,10,20,30}
#   mode = "ties"   small tie-prone case; dumps every returned tree for the
#                   old-vs-new partition-identity comparison
# ---------------------------------------------------------------------------

library(optimaltrees)

DGP_DIR <- "/Users/dagniel/RAND/rprojects/global-scholars/doubletree/simulations/remainder_terms_value_recovery/code"
stopifnot("DGP source directory not found" = dir.exists(DGP_DIR))
source(file.path(DGP_DIR, "dgp.R"))

MODE <- {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1L) a[[1L]] else "repro"
}
FIT_TIME_LIMIT <- 300

# Count leaves in the nested-list tree JSON returned by fit_tree().
count_leaves <- function(node) {
  if (is.null(node)) return(0L)
  if (!is.null(node$prediction) && is.null(node$feature)) return(1L)
  if (is.null(node$false) && is.null(node$true)) return(1L)
  count_leaves(node$false) + count_leaves(node$true)
}

# Evaluate a serialized tree on a binary design matrix. Internal nodes carry a
# 0-based `feature` index plus `false`/`true` subtrees; leaves carry `prediction`.
eval_tree_json <- function(node, Xb) {
  out <- numeric(nrow(Xb))
  descend <- function(nd, rows) {
    if (length(rows) == 0L) return(invisible(NULL))
    if (is.null(nd$feature)) {
      out[rows] <<- as.numeric(nd$prediction)
      return(invisible(NULL))
    }
    v <- Xb[rows, as.integer(nd$feature) + 1L]
    descend(nd$`false`, rows[v == 0L])
    descend(nd$`true`, rows[v == 1L])
  }
  descend(node, seq_len(nrow(Xb)))
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# fit_tree() returns an S7 OptimalTreesModel exposing `trees` and `n_trees`.
model_trees <- function(m) {
  tr <- m@trees
  if (is.null(tr)) return(list())
  # A single tree may be returned unwrapped (a node list, not a list of nodes).
  if (!is.null(tr$feature) || !is.null(tr$prediction)) list(tr) else tr
}

fit_one <- function(n, L, seed = 20260908) {
  set.seed(seed)
  truth <- make_truth(L)
  dat <- simulate_data(n, truth)
  ctrl <- dat$A == 0L
  lambda_n <- log(n) / n

  elapsed <- system.time({
    fit <- optimaltrees::bisect_lambda_to_budget(
      as.data.frame(dat$Xbin[ctrl, , drop = FALSE]), dat$Y[ctrl],
      leaf_budget = L, lambda_n = lambda_n, m_n = 1L,
      loss_function = "squared_error",
      max_depth = truth$depth_mu, depth_restricted = TRUE,
      fit_time_limit = FIT_TIME_LIMIT, verbose = FALSE
    )
  })[["elapsed"]]

  list(n = n, L = L, depth = truth$depth_mu, n_control = sum(ctrl),
       lambda = fit$lambda, n_leaves = fit$n_leaves, sec = elapsed)
}

if (MODE == "repro") {
  cat("=== REPRO: p=5 -> 10 binary features, L=30, n=2000 ===\n")
  truth <- make_truth(30L)
  cat(sprintf("depth_mu = %d, depth_e = %d\n", truth$depth_mu, truth$depth_e))

  set.seed(20260908)
  dat <- simulate_data(2000L, truth)
  ctrl <- dat$A == 0L
  cat(sprintf("n = 2000, n_control = %d, binary features = %d\n",
              sum(ctrl), ncol(dat$Xbin)))

  # Direct single fit at a lambda small enough to admit a large tree, which is
  # where the cross-product enumeration used to exhaust model_limit.
  lam <- log(2000) / 2000 / 20
  cat(sprintf("\n-- single fit_tree(): regularization = %.3e, max_depth = %d\n",
              lam, truth$depth_mu))
  el <- system.time({
    m <- optimaltrees::fit_tree(
      as.data.frame(dat$Xbin[ctrl, , drop = FALSE]), dat$Y[ctrl],
      loss_function = "squared_error", regularization = lam,
      max_depth = truth$depth_mu, time_limit = FIT_TIME_LIMIT,
      worker_limit = 1L, verbose = FALSE
    )
  })[["elapsed"]]
  trees1 <- model_trees(m)
  nt <- length(trees1)
  tj <- trees1[[1L]]
  cat(sprintf("   n_trees = %s | leaves = %d | %.2f s\n",
              nt, count_leaves(tj), el))

  cat("\n-- budget-constrained fit (bisect_lambda_to_budget, L = 30) --\n")
  r <- fit_one(2000L, 30L)
  cat(sprintf("   leaves = %d (budget %d) | lambda = %.3e | %.1f s\n",
              r$n_leaves, r$L, r$lambda, r$sec))

} else if (MODE == "grid") {
  cat("=== TIMING GRID (fixed code) ===\n")
  cells <- expand.grid(n = c(2000L, 20000L), L = c(5L, 10L, 20L, 30L),
                       KEEP.OUT.ATTRS = FALSE)
  out <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    cl <- cells[i, ]
    cat(sprintf("[%d/%d] n=%6d L=%2d ... ", i, nrow(cells), cl$n, cl$L))
    r <- tryCatch(fit_one(cl$n, cl$L),
                  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
    if (!is.null(r)) {
      cat(sprintf("depth=%d leaves=%2d lambda=%.3e %.1fs\n",
                  r$depth, r$n_leaves, r$lambda, r$sec))
    }
    out[[i]] <- r
  }
  tbl <- do.call(rbind, lapply(Filter(Negate(is.null), out), as.data.frame))
  print(tbl)
  saveRDS(tbl, "/tmp/ot_timing_grid.rds")

} else if (MODE == "ties") {
  # Deliberately tie-saturated: d binary features driving 2^d distinct cell
  # means, with regularization low enough that the optimum is the full product
  # partition. That partition is realizable by T(d) = d * T(d-1)^2 distinct
  # trees (T(3)=12, T(4)=576, T(5)=1658880), all partition-identical.
  d <- as.integer(Sys.getenv("OT_TIE_D", "3"))
  tag <- Sys.getenv("OT_TIE_TAG", "new")
  cat(sprintf("=== TIE-PRONE CASE: d = %d binary features, T(d) = %s ===\n",
              d, format(Reduce(function(t, k) k * t^2, seq_len(d))[1], scientific = FALSE)))

  set.seed(11)
  n <- 64L * 2^d
  Xb <- matrix(rbinom(n * d, 1L, 0.5), nrow = n,
               dimnames = list(NULL, paste0("b", seq_len(d))))
  cellid <- 1L + as.integer(Xb %*% (2^(seq_len(d) - 1L)))
  cell_means <- seq(-2, 3.3, length.out = 2^d)
  y <- cell_means[cellid] + rnorm(n, sd = 0.02)

  el <- system.time({
    m <- optimaltrees::fit_tree(as.data.frame(Xb), y,
                                loss_function = "squared_error",
                                regularization = 1e-6, max_depth = d + 2L,
                                worker_limit = 1L, verbose = FALSE)
  })[["elapsed"]]
  trees <- model_trees(m)
  cat(sprintf("n_trees returned = %d | %.2f s\n", length(trees), el))

  if (length(trees) == 0L) {
    cat("NO TREES RETURNED (model limit exceeded -- this is the original bug)\n")
    saveRDS(list(d = d, n_trees = 0L, sec = el),
            sprintf("/tmp/ot_ties_d%d_%s.rds", d, tag))
  } else {
    design <- as.matrix(expand.grid(rep(list(0:1), d)))
    colnames(design) <- paste0("b", seq_len(d))
    preds <- lapply(trees, function(t) round(eval_tree_json(t, design), 6))
    nl <- vapply(trees, count_leaves, integer(1))
    objs <- vapply(trees, function(t) as.numeric(t$model_objective %||% NA_real_),
                   numeric(1))

    cat("leaf counts (unique):", paste(sort(unique(nl)), collapse = ","), "\n")
    cat("model_objective (unique, 8dp):",
        paste(sort(unique(round(objs, 8))), collapse = ","), "\n")
    cat("DISTINCT fitted functions over all 2^d cells:",
        length(unique(preds)), "\n")
    cat("fitted function (first tree):\n"); print(preds[[1L]])

    saveRDS(list(d = d, n_trees = length(trees), leaf_counts = nl,
                 objectives = objs, distinct_preds = unique(preds),
                 pred1 = preds[[1L]], sec = el),
            sprintf("/tmp/ot_ties_d%d_%s.rds", d, tag))
  }
  cat(sprintf("saved: /tmp/ot_ties_d%d_%s.rds\n", d, tag))
} else {
  stop("unknown mode: ", MODE)
}
