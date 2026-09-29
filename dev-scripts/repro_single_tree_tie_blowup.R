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
#   mode = "ties"   small tie-prone case; enumerates the Rashomon set (with
#                   trivial-extension dedup OFF) for the old-vs-new
#                   partition-identity comparison
#
# Environment variables (all optional; every default matches the original
# 2026-09-08 behaviour except OT_MODEL_LIMIT and OT_TIE_TAG's effect on
# output paths -- see the 2026-09-08 review this rewrite closes,
# quality_reports/reviews/2026-09-08_repro_single_tree_tie_blowup.md):
#   OT_DGP_DIR     Path to the dgp.R directory. Default: derived from this
#                  script's own location, assuming the standard sibling-repo
#                  layout (<parent>/doubletree/simulations/
#                  remainder_terms_value_recovery/code) -- see repo-root
#                  resolution below.
#   OT_MODEL_LIMIT Integer. 0 = unlimited (default). model_limit is the
#                  exact resource single-tree extraction's tie-enumeration
#                  bug exhausted (src/optimizer/extraction/models.hpp:233);
#                  pinned explicitly here rather than inherited from
#                  fit_tree()'s own heuristic, which assumes every feature
#                  is continuous (R/fit_tree.R:168) and silently gives a
#                  different effective limit at every dimension this
#                  script's binary-feature settings actually use (Critical
#                  #1 of the review above).
#   OT_TIE_D       Integer, ties mode only. Number of binary features
#                  (default 3; T(3)=12, T(4)=576, T(5)=1658880 tied trees).
#   OT_TIE_TAG     Free-text build tag (default "new"), used both to name
#                  saved artifacts (grid and ties modes) and recorded
#                  inside them alongside package-version/git-SHA
#                  provenance (review Issues #6-#7) -- set this to "old"
#                  before loading a pre-fix build, or the artifact records
#                  no reliable way to tell afterward which build produced
#                  it.
#   OT_GRID_HOURS  Numeric, grid mode only. Per-cell wall-clock deadline in
#                  hours (default 1). Without this, bisection's own
#                  internal fit count (up to 2*tol_iter+1 = 81 fits per
#                  cell at FIT_TIME_LIMIT = 300s each) makes the 8-cell
#                  grid's worst case ~54 hours (review Issue #9).
#   OT_OUT_DIR     Output directory for saved .rds artifacts. Default:
#                  <repo root>/dev-scripts/out (already covered by this
#                  repo's blanket "*.rds" .gitignore entry). Replaces the
#                  original hardcoded /tmp destination (review Issue #15's
#                  underlying concern: evidence meant to survive a
#                  rebuild-and-rerun cycle should not live in a volatile
#                  directory), though full package's own §5 write_rds()/
#                  fs::dir_create() convention is skipped here deliberately
#                  -- neither readr nor fs is a package dependency, and this
#                  is a dev-scripts/ harness, not package R/ code.
# ---------------------------------------------------------------------------

library(optimaltrees)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Script location / repo-root resolution (review Issue #5) -----------
# Hardcoding an absolute, single-user path (the original DGP_DIR) means the
# script can only ever run on its author's machine, defeating its purpose
# as evidence someone else can independently reproduce. Derive it instead
# from where THIS script actually lives, with a full override available.
this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(normalizePath(sub("^--file=", "", file_arg), mustWork = FALSE))
  }
  NA_character_
}
SCRIPT_PATH <- this_file()
# Falls back to getwd() when sourced interactively (no --file= arg) rather
# than erroring outright -- this script's own stopifnot() on DGP_DIR below
# still catches a wrong guess with a clear message either way.
REPO_ROOT <- if (!is.na(SCRIPT_PATH)) dirname(dirname(SCRIPT_PATH)) else getwd()

DGP_DIR <- Sys.getenv(
  "OT_DGP_DIR",
  file.path(dirname(REPO_ROOT), "doubletree", "simulations",
            "remainder_terms_value_recovery", "code")
)
stopifnot("DGP source directory not found (set OT_DGP_DIR)" = dir.exists(DGP_DIR))
source(file.path(DGP_DIR, "dgp.R"))

OUT_DIR <- Sys.getenv("OT_OUT_DIR", file.path(REPO_ROOT, "dev-scripts", "out"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

MODE <- {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1L) a[[1L]] else "repro"
}
FIT_TIME_LIMIT <- 300

# model_limit is the resource the tie-enumeration bug exhausted
# (models.hpp:233), so pin it explicitly rather than inheriting fit_tree()'s
# heuristic (review Issue #1). 0 = unlimited.
MODEL_LIMIT <- as.integer(Sys.getenv("OT_MODEL_LIMIT", "0"))
TAG <- Sys.getenv("OT_TIE_TAG", "new")
cat(sprintf("model_limit = %d (0 = unlimited) | build tag = %s\n", MODEL_LIMIT, TAG))

# ---- Build provenance, attached to every saved artifact (review Issue #7) -
# The old-vs-new distinction otherwise rests entirely on the free-text
# OT_TIE_TAG env var with no way to detect a mislabelled run after the fact.
provenance <- function() {
  git_sha <- tryCatch(
    system2("git", c("-C", REPO_ROOT, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  if (length(git_sha) != 1L || !nzchar(git_sha)) git_sha <- NA_character_
  list(
    optimaltrees_version = as.character(utils::packageVersion("optimaltrees")),
    lib_path = tryCatch(dirname(system.file(package = "optimaltrees")),
                         error = function(e) NA_character_),
    git_sha = git_sha,
    model_limit = MODEL_LIMIT,
    tag = TAG,
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

# Count leaves in the nested-list tree JSON returned by fit_tree()/fit_rashomon().
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

# fit_tree()/fit_rashomon() return an S7 OptimalTreesModel exposing `trees`
# and `n_trees`.
model_trees <- function(m) {
  tr <- m@trees
  if (is.null(tr)) return(list())
  # A single tree may be returned unwrapped (a node list, not a list of nodes).
  if (!is.null(tr$feature) || !is.null(tr$prediction)) list(tr) else tr
}

# Canonical grid-row schema: every row (ok, truncated, deadline, or error)
# uses exactly these columns, so do.call(rbind, ...) never silently drops or
# misaligns a failed cell (review Issue #8 -- filtering failures out of the
# table is exactly the anti-pattern this schema exists to prevent).
GRID_ROW_SCHEMA <- c("n", "L", "status", "sec", "depth", "n_control", "lambda",
                     "n_leaves", "certified", "feasible", "depth_sufficient",
                     "gap", "n_fits", "any_truncated", "error_message")
na_row <- function(n, L, status, sec = NA_real_, error_message = NA_character_) {
  row <- as.list(rep(NA, length(GRID_ROW_SCHEMA)))
  names(row) <- GRID_ROW_SCHEMA
  row$n <- n; row$L <- L; row$status <- status; row$sec <- sec
  row$error_message <- error_message
  row
}

fit_one <- function(n, L, seed = 20260908, model_limit = MODEL_LIMIT,
                     deadline = NULL) {
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
      fit_time_limit = FIT_TIME_LIMIT, deadline = deadline,
      model_limit = model_limit, verbose = FALSE
    )
  })[["elapsed"]]

  # any_truncated is a distinct failure mode from certified == FALSE and
  # must NOT be collapsed into the timing number
  # (bisect_lambda_to_budget.R:366-372, review Critical #3): a truncated
  # fit's incumbent is not proven optimal, so its n_leaves/lambda are
  # reported here but tagged `status = "truncated"`, never presented as an
  # indistinguishable-from-success timing row.
  row <- list(n = n, L = L,
              status = if (isTRUE(fit$any_truncated)) "truncated" else "ok",
              sec = elapsed, depth = truth$depth_mu, n_control = sum(ctrl),
              lambda = fit$lambda, n_leaves = fit$n_leaves,
              certified = fit$certified, feasible = fit$feasible,
              depth_sufficient = fit$depth_sufficient, gap = fit$gap,
              n_fits = fit$n_fits, any_truncated = fit$any_truncated,
              error_message = NA_character_)
  row[GRID_ROW_SCHEMA]
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
  cat(sprintf("\n-- single fit_tree(): regularization = %.3e, max_depth = %d, model_limit = %d\n",
              lam, truth$depth_mu, MODEL_LIMIT))
  el <- system.time({
    m <- optimaltrees::fit_tree(
      as.data.frame(dat$Xbin[ctrl, , drop = FALSE]), dat$Y[ctrl],
      loss_function = "squared_error", regularization = lam,
      max_depth = truth$depth_mu, time_limit = FIT_TIME_LIMIT,
      model_limit = MODEL_LIMIT, worker_limit = 1L, verbose = FALSE
    )
  })[["elapsed"]]
  # solver_time vs. the requested time_limit distinguishes a genuine
  # truncation from a fast, complete solve (review Issue #3's note: no
  # any_truncated field is available on a bare fit_tree() call, so this is
  # the same comparison bisect_lambda_to_budget() makes internally, done
  # by hand here). `:::` is deliberate: this diagnostic is not, and should
  # not become, exported package API.
  solver_time <- optimaltrees:::treefarms_time_cpp()
  truncated <- FIT_TIME_LIMIT > 0 && solver_time >= (FIT_TIME_LIMIT - 0.5)
  trees1 <- model_trees(m)
  nt <- length(trees1)
  if (nt == 0L) {
    cat(sprintf("   n_trees = 0 -- BUG REPRODUCED (model_limit exhausted) | %.2f s\n", el))
  } else {
    cat(sprintf("   n_trees = %d | leaves = %d | %.2f s%s\n",
                nt, count_leaves(trees1[[1L]]), el,
                if (truncated) " -- TRUNCATED, incumbent not proven optimal" else ""))
  }

  cat(sprintf("\n-- budget-constrained fit (bisect_lambda_to_budget, L = 30, model_limit = %d) --\n",
              MODEL_LIMIT))
  r <- fit_one(2000L, 30L, model_limit = MODEL_LIMIT)
  cat(sprintf("   leaves = %s (budget %d) | lambda = %s | %.1f s | status = %s%s\n",
              r$n_leaves, r$L,
              if (!is.na(r$lambda)) sprintf("%.3e", r$lambda) else NA,
              r$sec, r$status,
              if (identical(r$status, "truncated")) {
                " -- INCUMBENT NOT PROVEN OPTIMAL, timing is a LOWER BOUND, not a solve time"
              } else ""))

} else if (MODE == "grid") {
  cat("=== TIMING GRID (fixed code) ===\n")
  grid_hours <- as.numeric(Sys.getenv("OT_GRID_HOURS", "1"))
  cat(sprintf("model_limit = %d | per-cell deadline = %.2f h | build tag = %s\n",
              MODEL_LIMIT, grid_hours, TAG))

  # Tag by build (review Issue #6) and refuse to silently overwrite a prior
  # run's numbers -- the original single fixed filename meant running the
  # grid on the old binary and then the new one destroyed the "before"
  # comparison with no warning.
  out_path <- file.path(OUT_DIR, sprintf("ot_timing_grid_%s.rds", TAG))
  if (file.exists(out_path)) {
    cli::cli_abort(c(
      "grid mode: output file already exists: {out_path}.",
      "i" = "Set a different OT_TIE_TAG, or remove the existing file explicitly, before re-running.",
      "i" = "Overwriting it here would silently destroy whichever build's numbers are already saved."
    ))
  }

  cells <- expand.grid(n = c(2000L, 20000L), L = c(5L, 10L, 20L, 30L),
                       KEEP.OUT.ATTRS = FALSE)
  out <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    cl <- cells[i, ]
    cat(sprintf("[%d/%d] n=%6d L=%2d ... ", i, nrow(cells), cl$n, cl$L))
    # Fresh per-cell deadline (review Issue #9): without this, bisection's
    # own internal fit count (up to 2*tol_iter+1 = 81 fits/cell at 300s
    # each) makes the 8-cell grid's worst case ~54h. A per-cell (not
    # whole-grid) deadline keeps every cell's budget identical regardless
    # of how long earlier cells took.
    cell_deadline <- Sys.time() + grid_hours * 3600
    r <- tryCatch(
      fit_one(cl$n, cl$L, model_limit = MODEL_LIMIT, deadline = cell_deadline),
      optimaltrees_deadline_exceeded = function(e) {
        cat("DEADLINE EXCEEDED:", conditionMessage(e), "\n")
        na_row(cl$n, cl$L, "deadline", error_message = conditionMessage(e))
      },
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        na_row(cl$n, cl$L, "error", error_message = conditionMessage(e))
      }
    )
    if (r$status %in% c("ok", "truncated")) {
      cat(sprintf("depth=%s leaves=%s lambda=%s %.1fs status=%s\n",
                  r$depth, r$n_leaves,
                  if (!is.na(r$lambda)) sprintf("%.3e", r$lambda) else NA,
                  r$sec, r$status))
    }
    out[[i]] <- r
  }
  # Every cell is retained, in its canonical schema, regardless of status
  # (review Issue #8) -- a reader of the saved artifact must be able to see
  # that a cell failed, not just infer it from a missing row.
  tbl <- do.call(rbind, lapply(out, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  print(tbl)
  n_bad <- sum(!tbl$status %in% c("ok"))
  if (n_bad > 0L) {
    cli::cli_warn("{n_bad} of {nrow(tbl)} grid cell(s) did not complete cleanly (status != \"ok\") -- see the printed table's `status`/`error_message` columns.")
  }
  saveRDS(list(table = tbl, provenance = provenance()), out_path)
  cat(sprintf("saved: %s\n", out_path))

} else if (MODE == "ties") {
  # Deliberately tie-saturated: d binary features driving 2^d distinct cell
  # means, with regularization low enough that the optimum is the full product
  # partition. That partition is realizable by T(d) = d * T(d-1)^2 distinct
  # trees (T(3)=12, T(4)=576, T(5)=1658880), all partition-identical.
  #
  # To observe tie MULTIPLICITY this must not use fit_tree(): it forces
  # single_tree = TRUE (fit_tree.R:221) and prunes trivial extensions by
  # default (fit_tree.R:90), so length(trees) <= 1 on ANY build regardless
  # of the C++ extractor's actual tie behaviour -- the "DISTINCT fitted
  # functions" check this mode reports was previously tautological (review
  # Critical #2). fit_rashomon(rashomon_ignore_trivial_extensions = FALSE)
  # is the only path that can return more than one tree.
  d <- as.integer(Sys.getenv("OT_TIE_D", "3"))
  cat(sprintf("=== TIE-PRONE CASE: d = %d binary features, T(d) = %s | model_limit = %d | tag = %s ===\n",
              d, format(Reduce(function(t, k) k * t^2, seq_len(d))[1], scientific = FALSE),
              MODEL_LIMIT, TAG))

  set.seed(11)
  n <- 64L * 2^d
  Xb <- matrix(rbinom(n * d, 1L, 0.5), nrow = n,
               dimnames = list(NULL, paste0("b", seq_len(d))))
  cellid <- 1L + as.integer(Xb %*% (2^(seq_len(d) - 1L)))
  cell_means <- seq(-2, 3.3, length.out = 2^d)
  y <- cell_means[cellid] + rnorm(n, sd = 0.02)

  el <- system.time({
    m <- optimaltrees::fit_rashomon(
      as.data.frame(Xb), y, loss_function = "squared_error",
      regularization = 1e-6, max_depth = d + 2L,
      rashomon_ignore_trivial_extensions = FALSE,
      model_limit = MODEL_LIMIT, worker_limit = 1L, verbose = FALSE
    )
  })[["elapsed"]]
  trees <- model_trees(m)
  cat(sprintf("n_trees returned = %d | %.2f s\n", length(trees), el))

  out_path <- file.path(OUT_DIR, sprintf("ot_ties_d%d_%s.rds", d, TAG))
  if (length(trees) == 0L) {
    cat("NO TREES RETURNED (model limit exceeded -- this is the original bug)\n")
    saveRDS(list(d = d, n_trees = 0L, sec = el, provenance = provenance()), out_path)
  } else {
    design <- as.matrix(expand.grid(rep(list(0:1), d)))
    colnames(design) <- paste0("b", seq_len(d))
    preds <- lapply(trees, function(tr) round(eval_tree_json(tr, design), 6))
    nl <- vapply(trees, count_leaves, integer(1))
    objs <- vapply(trees, function(tr) as.numeric(tr$model_objective %||% NA_real_),
                   numeric(1))

    cat("leaf counts (unique):", paste(sort(unique(nl)), collapse = ","), "\n")
    cat("model_objective (unique, 8dp):",
        paste(sort(unique(round(objs, 8))), collapse = ","), "\n")
    cat("DISTINCT fitted functions over all 2^d cells:",
        length(unique(preds)), "\n")
    cat("fitted function (first tree):\n"); print(preds[[1L]])

    saveRDS(list(d = d, n_trees = length(trees), leaf_counts = nl,
                 objectives = objs, distinct_preds = unique(preds),
                 pred1 = preds[[1L]], sec = el, provenance = provenance()),
            out_path)
  }
  cat(sprintf("saved: %s\n", out_path))
} else {
  stop("unknown mode: ", MODE)
}
