# ============================================================
# One replication of the two-stage fit + all per-replication metrics
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 2 method, Section 5 metrics, Section 9 reproducibility)
# Purpose: Stage A (bisect_lambda_to_budget at an EXPLICIT quantile-grid
#          resolution m_n) + Stage B (refine_tree), then extract every field
#          Q1 and Q2 need, as exactly one row -- including on every error path.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R; a loaded `optimaltrees`
# Outputs: none (function definitions only; source this file)
# ============================================================

# ---- 0 Small utilities ---------------------------------------------------

# `%||%` is in base R >= 4.4 and in rlang; define a local fallback so this file
# does not depend on which is available.
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# ---- 1 Deterministic per-replication seeds -------------------------------
#
# Spec Section 9: seeds derived per (configuration, replication) by a fixed
# hash, so re-ordering or re-running a subset of configurations cannot change
# any individual replication's data, and no two configurations can collide.
# The replication index ALONE is not enough -- it would give every
# configuration the same data, making excess-risk differences across m_n
# partly a shared-data artifact.
#
# FNV-1a (32-bit) over a canonical key string. Implemented rather than taken
# from a package so the stream is reproducible from this file alone. The
# 16-bit split in `mul32()` is load-bearing: `h * 16777619` with h up to 2^32
# reaches 7.2e16, past the 2^53 exact-integer limit of a double, so the naive
# product would silently lose low bits (and low bits are the whole point of a
# hash). Split into two < 2^41 products, it stays exact.

fnv1a_mul32 <- function(h, m = 16777619) {
  hi <- h %/% 65536
  lo <- h %% 65536
  ((((hi * m) %% 65536) * 65536) + lo * m) %% 4294967296
}

fnv1a_32 <- function(s) {
  bytes <- utf8ToInt(s)
  if (anyNA(bytes) || any(bytes > 255L)) {
    cli::cli_abort(
      "fnv1a_32: key {.val {s}} is not plain ASCII. The byte-wise XOR below \\
       assumes single-byte characters; a multi-byte key would hash a code \\
       point, not its UTF-8 bytes, silently diverging from FNV-1a."
    )
  }
  h <- 2166136261            # FNV-1a 32-bit offset basis
  for (b in bytes) {
    # XOR with a byte only touches the low 8 bits, so it is done on those bits
    # alone. `bitwXor()` coerces to a 32-bit SIGNED integer and returns NA for
    # anything above 2^31 - 1, which `h` routinely exceeds -- XORing the low
    # byte in isolation keeps both operands under 256 and avoids that entirely.
    lo8 <- h %% 256
    h <- h - lo8 + bitwXor(as.integer(lo8), as.integer(b))
    h <- fnv1a_mul32(h)
  }
  h
}

#' Deterministic seed for one (configuration, replication) cell
#'
#' @param n,m_n,delta1,p Configuration coordinates.
#' @param rep Replication index within the configuration (1-based).
#' @param study Namespace string, so another study reusing this helper cannot
#'   collide with this one.
#' @return A single integer in [1, 2147483646], usable directly in `set.seed()`.
sim_seed <- function(n, m_n, delta1, p, rep,
                      study = "continuum-tree-grid-tradeoff") {
  key <- sprintf("%s|n=%d|m=%d|delta1=%s|p=%d|rep=%d", study,
                  as.integer(n), as.integer(m_n),
                  format(delta1, nsmall = 6L, digits = 15L),
                  as.integer(p), as.integer(rep))
  1L + as.integer(fnv1a_32(key) %% 2147483646)
}

# ---- 2 Prepared ground truth and evaluation sample -----------------------

#' Assemble the verified ground truth for one Delta1
#'
#' Thin wrapper over `ground_truth_jump_cosmetic()` that ALSO carries the
#' reference risk on the fixed Monte Carlo sample, since that -- not the
#' closed-form risk -- is what spec Section 3 says each replication's excess
#' risk subtracts (both terms estimated on the same sample).
#'
#' Requires `ground_truth_gate.R` to have PASSED for this Delta1: that gate
#' is what establishes the constructed topology actually is f*. The check is
#' explicit here rather than assumed, because running replications against an
#' unverified f* is exactly the failure this study already hit once.
study_ground_truth <- function(params = dgp_params_jump_cosmetic(),
                                mc_eval,
                                verification_path = NULL) {
  gt <- ground_truth_jump_cosmetic(params)
  assert_mc_eval_matches(mc_eval, params)

  if (!is.null(verification_path)) {
    if (!file.exists(verification_path)) {
      cli::cli_abort(c(
        "study_ground_truth: no verification record at {.file {verification_path}}.",
        "i" = "Run ground_truth_gate.R first -- spec Section 3 makes the \\
               topology verification mandatory per Delta1, and it is the only \\
               thing establishing that this topology is f*."
      ))
    }
    v <- readRDS(verification_path)
    if (!isTRUE(v$passed)) {
      cli::cli_abort(
        "study_ground_truth: the verification record at \\
         {.file {verification_path}} reports FAILED checks. Refusing to build \\
         ground truth from an unverified topology."
      )
    }
    if (!isTRUE(all.equal(v$params$delta1, params$delta1))) {
      cli::cli_abort(c(
        "study_ground_truth: the verification record was produced at \\
         delta1 = {v$params$delta1}, but ground truth is being built at \\
         delta1 = {params$delta1}.",
        "i" = "Spec Section 3 requires topology re-verification per Delta1; \\
               f*'s topology itself is permitted to change between rungs."
      ))
    }
  }

  gt$fstar_risk_mc <- mean(
    (mc_eval$gamma0 - depth2_tree_predict(gt$topology, gt$leaf_means,
                                           mc_eval$x1, mc_eval$x2))^2
  )
  gt
}

#' Attach the coordinate data.frame `predict()` needs to the evaluation sample
#'
#' Built once, outside the replication loop: a 1e7-row data.frame per
#' replication would dominate the runtime this pilot is measuring. R's
#' copy-on-write means the data.frame shares `x1`/`x2`'s storage, so this costs
#' no extra memory.
prepare_mc_eval <- function(mc_eval) {
  mc_eval$X <- data.frame(x1 = mc_eval$x1, x2 = mc_eval$x2)
  mc_eval
}

# ---- 3 Reading the fitted tree ------------------------------------------

#' Flatten a coordinate-space tree to one row per split, addressed by path
#'
#' Walks the node schema documented in `optimaltrees::stage_b` (`kind`,
#' `coord`, `cut`, `left`, `right`) directly rather than reaching into the
#' package's internal walkers with `:::`. Paths match the `path` column of
#' `refine_tree()`'s refinement log ("" for the root, then "left"/"right"
#' joined by "/").
coord_tree_split_rows <- function(tree) {
  rows <- list()
  walk <- function(node, path) {
    if (identical(node$kind, "leaf")) return(invisible())
    rows[[length(rows) + 1L]] <<- data.frame(
      path = paste(path, collapse = "/"), coord = node$coord, cut = node$cut,
      depth = length(path), stringsAsFactors = FALSE
    )
    walk(node$left,  c(path, "left"))
    walk(node$right, c(path, "right"))
  }
  walk(tree, character(0))
  if (length(rows) == 0L) {
    return(data.frame(path = character(0), coord = character(0),
                       cut = numeric(0), depth = integer(0),
                       stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Pull one field for one node path out of a data.frame keyed by `path`
#'
#' Returns the typed NA when that path is absent (a tree with fewer than 4
#' leaves genuinely has no such node -- that is a result, not an error), but
#' aborts if the column itself is missing, since a renamed/absent column means
#' the package's return contract changed and every downstream number built on
#' it would be wrong (interactive-entry F1).
node_field <- function(df, path, column, na_value = NA_real_) {
  if (!column %in% names(df)) {
    cli::cli_abort(c(
      "node_field: column {.field {column}} is absent from the supplied table.",
      "i" = "Available: {.field {names(df)}}.",
      "i" = "This means the package's return contract changed; the metric \\
             built on this column cannot be silently filled with NA."
    ))
  }
  hit <- which(df$path == path)
  if (length(hit) == 0L) return(na_value)
  if (length(hit) > 1L) {
    cli::cli_abort(
      "node_field: path {.val {path}} appears {length(hit)} times; node paths \\
       must be unique."
    )
  }
  df[[column]][[hit]]
}

#' Assert a list carries every named field a metric is about to read
require_fields <- function(x, fields, what) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) {
    cli::cli_abort(c(
      "{what}: expected field(s) {.field {missing}} absent from the return value.",
      "i" = "Present: {.field {names(x)}}.",
      "i" = "Refusing to substitute NA -- a missing field means the package \\
             API changed, not that the quantity is unavailable."
    ))
  }
  invisible(TRUE)
}

# ---- 4 One replication ---------------------------------------------------

# The row schema, declared once so EVERY exit path returns the same columns in
# the same order (the whole point: a failed replication is a row with a status,
# never a dropped row and never a differently-shaped one).
one_sim_na_row <- function() {
  list(
    status = NA_character_, error_class = NA_character_,
    error_message = NA_character_, n_warnings = 0L,
    first_warning = NA_character_,
    # Stage-A grid resolution actually realised
    n_bins_metadata = NA_integer_, n_thresholds_x1 = NA_integer_,
    n_thresholds_x2 = NA_integer_, bins_exact = NA,
    model_limit_arg = NA_real_,
    # bisect_lambda_to_budget() certificate fields
    lambda = NA_real_, n_leaves = NA_integer_, certified = NA, feasible = NA,
    depth_sufficient = NA, depth_restricted = NA, gap = NA_real_,
    any_truncated = NA, used_search = NA, n_fits = NA_integer_,
    solver_status_max = NA_integer_,
    # fitted topology
    n_splits = NA_integer_, n_leaves_refined = NA_integer_,
    root_coord = NA_character_, left_coord = NA_character_,
    right_coord = NA_character_,
    topology_match = NA, match_root = NA, match_left = NA, match_right = NA,
    # fitted thresholds (NA where the topology does not match at that node)
    t1_hat = NA_real_, t2L_hat = NA_real_, t2R_hat = NA_real_,
    t1_err = NA_real_, t2L_err = NA_real_, t2R_err = NA_real_,
    # Stage-A grid cuts, for reference
    grid_cut_root = NA_real_, grid_cut_left = NA_real_, grid_cut_right = NA_real_,
    # Stage-B diagnostics, per node
    reason_root = NA_character_, reason_left = NA_character_,
    reason_right = NA_character_,
    moved_root = NA, moved_left = NA, moved_right = NA,
    n_cand_root = NA_integer_, n_cand_left = NA_integer_,
    n_cand_right = NA_integer_,
    n_node_root = NA_integer_, n_node_left = NA_integer_,
    n_node_right = NA_integer_,
    # risk
    risk_fhat = NA_real_, risk_fstar = NA_real_, excess_risk = NA_real_,
    # timing
    secs_dgp = NA_real_, secs_stage_a = NA_real_, secs_stage_b = NA_real_,
    secs_eval = NA_real_, secs_total = NA_real_
  )
}

#' Run one replication of the two-stage fit and return one row of metrics
#'
#' @param n Sample size.
#' @param m_n Quantile-grid resolution: bins per coordinate for Stage A's
#'   discretization. Supplied EXPLICITLY (spec Section 2), never derived from
#'   `n` by the package's own `"adaptive"`/`"log"` schedule.
#'   \strong{Not to be confused with `bisect_lambda_to_budget()`'s own `m_n`
#'   argument}, which is a minimum-leaf-SIZE floor -- a genuine name collision
#'   between this study's notation and the package's. This function passes the
#'   grid resolution as `discretize_bins` and the leaf floor as `m_n`, both
#'   explicitly, so neither can land in the other's slot.
#' @param delta1 Jump-size parameter. Default 5 (Regime F, spec as revised).
#' @param p Number of covariates. Default 2.
#' @param seed Data seed. Default `NULL` -> `sim_seed(n, m_n, delta1, p, rep)`.
#' @param ground_truth Output of `study_ground_truth()`.
#' @param mc_eval Output of `prepare_mc_eval()`.
#' @param rep Replication index, used only to derive `seed` when `seed` is
#'   `NULL`. Recorded in the returned row either way.
#' @param leaf_budget,max_depth The fixed model class (spec Section 3:
#'   `L = 4`, `D = 2`). `max_depth = 2` with `leaf_budget = 4` is below the
#'   full-compliance depth `leaf_budget - 1 = 3`, so `depth_restricted = TRUE`
#'   is required by `bisect_lambda_to_budget()` and is passed -- the study's
#'   model class IS depth-2 trees, so this is the declared class, not a
#'   speed/rigour tradeoff.
#' @param lambda_n Stage-A regularization tried first. The package default
#'   (0.1). With `max_depth = 2` the search space already tops out at 4 leaves,
#'   so `lambda_n` alone always respects the budget and no bisection happens
#'   (`used_search = FALSE`, `n_fits = 1`) -- the certificate's case (ii).
#' @param min_leaf_n Minimum observations per leaf, enforced in Stage A
#'   (`m_n =`) and Stage B (`min_leaf_n =`). Default 1 (no floor beyond
#'   non-empty), matching the spec's silence on the subject.
#' @param r_n,M_n Stage-B search-interval tuning (`refine_tree()`). Both `NULL`
#'   by default: the spec specifies Stage B as plain off-grid refinement, and
#'   supplying these would additionally intersect a quantile-space radius
#'   `M_n/r_n` into each node's identifying bracket -- an extra restriction the
#'   design does not ask for. Exposed so a later task can turn it on
#'   deliberately rather than by editing this function.
#' @param model_limit `NULL` (default) or a number forwarded to [fit_tree()]
#'   through `bisect_lambda_to_budget()`'s `...`. `NULL` leaves [fit_tree()]'s
#'   own dimensionality heuristic in charge, which at `p * (m_n - 1) > 100`
#'   estimated binary features sets `model_limit = 1e6` (and warns). `0` means
#'   unlimited, which is what that same function's own comment says is correct
#'   for single-tree extraction at low leaf counts. Kept as an explicit
#'   argument, defaulting to the package's behavior, so the two settings can be
#'   compared on identical seeds rather than by editing this function.
#' @return A one-row data.frame. Always one row, with `status` describing the
#'   outcome; never an error propagated to the caller, and never zero rows.
one_sim <- function(n, m_n, delta1 = 5, p = 2, seed = NULL,
                     ground_truth, mc_eval, rep = 1L,
                     leaf_budget = 4L, max_depth = 2L, lambda_n = 0.1,
                     min_leaf_n = 1L, r_n = NULL, M_n = NULL,
                     model_limit = NULL) {
  t_start <- Sys.time()
  if (missing(ground_truth) || missing(mc_eval)) {
    cli::cli_abort(
      "one_sim: {.arg ground_truth} and {.arg mc_eval} are required -- \\
       recomputing either per replication would be both slow and (for \\
       mc_eval) a violation of the spec's fixed-evaluation-sample rule."
    )
  }
  if (is.null(mc_eval$X)) {
    cli::cli_abort(
      "one_sim: {.arg mc_eval} has no {.field X} -- pass it through \\
       {.fn prepare_mc_eval} first."
    )
  }
  require_fields(ground_truth,
                  c("t1_star", "t2_star_L", "t2_star_R", "topology",
                    "leaf_means", "fstar_risk_mc", "params"),
                  "one_sim: ground_truth")
  if (!isTRUE(all.equal(ground_truth$params$delta1, delta1))) {
    cli::cli_abort(
      "one_sim: ground_truth was built at delta1 = \\
       {ground_truth$params$delta1} but this replication asks for \\
       delta1 = {delta1}. f*'s topology is permitted to differ between \\
       Delta1 rungs (spec Section 3), so this cannot be reused."
    )
  }
  # Grid resolution, renamed away from `m_n` immediately -- see the roxygen
  # note on the collision with bisect_lambda_to_budget()'s own `m_n`.
  n_bins <- as.integer(m_n)
  if (n_bins < 2L) {
    cli::cli_abort("one_sim: {.arg m_n} must be >= 2 (it is a bin count).")
  }
  if (is.null(seed)) seed <- sim_seed(n, n_bins, delta1, p, rep)

  row <- one_sim_na_row()
  row$risk_fstar <- ground_truth$fstar_risk_mc

  warn_msgs <- character(0)
  collect_warnings <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  finish <- function() {
    row$n_warnings <- length(warn_msgs)
    row$first_warning <- if (length(warn_msgs)) warn_msgs[[1L]] else NA_character_
    row$secs_total <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    cbind(
      data.frame(n = as.integer(n), m_n = n_bins, delta1 = delta1,
                  p = as.integer(p), rep = as.integer(rep),
                  seed = as.integer(seed), stringsAsFactors = FALSE),
      as.data.frame(row, stringsAsFactors = FALSE)
    )
  }

  # -- data
  t0 <- Sys.time()
  dat <- dgp_jump_cosmetic(n = n, p = p, delta1 = delta1, seed = seed,
                            params = ground_truth$params)
  row$secs_dgp <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # -- Stage A. A thrown condition here is a RESULT to report (with its class
  # and message), not something to discard: the row is still returned.
  #
  # Built with do.call so `model_limit` is either present with a value or
  # ABSENT entirely -- passing `model_limit = NULL` through `...` is NOT the
  # same as omitting it (fit_tree() tests `is.null(dots$model_limit)`, which a
  # present-but-NULL element would satisfy, but a `NULL` element still occupies
  # a name in `...` and the two paths should not be conflated).
  stage_a_args <- list(
    X = dat$X, y = dat$y,
    leaf_budget = leaf_budget,
    lambda_n = lambda_n,
    m_n = min_leaf_n,                 # leaf-SIZE floor (package semantics)
    loss_function = "squared_error",
    max_depth = max_depth,
    depth_restricted = TRUE,
    discretize_method = "quantiles",
    discretize_bins = n_bins          # grid RESOLUTION (this study's m_n)
  )
  if (!is.null(model_limit)) {
    stage_a_args$model_limit <- model_limit
    row$model_limit_arg <- as.numeric(model_limit)
  }
  t0 <- Sys.time()
  fit <- tryCatch(
    collect_warnings(
      do.call(optimaltrees::bisect_lambda_to_budget, stage_a_args)
    ),
    error = function(e) e
  )
  row$secs_stage_a <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(fit, "error")) {
    row$status <- "stage_a_error"
    row$error_class <- paste(class(fit), collapse = ",")
    row$error_message <- conditionMessage(fit)
    return(finish())
  }

  require_fields(fit,
                  c("fit", "lambda", "n_leaves", "feasible", "certified", "gap",
                    "used_search", "depth_sufficient", "depth_restricted",
                    "any_truncated", "n_fits", "fit_log"),
                  "one_sim: bisect_lambda_to_budget")
  row$lambda           <- fit$lambda
  row$n_leaves         <- as.integer(fit$n_leaves)
  row$certified        <- isTRUE(fit$certified)
  row$feasible         <- isTRUE(fit$feasible)
  row$depth_sufficient <- isTRUE(fit$depth_sufficient)
  row$depth_restricted <- isTRUE(fit$depth_restricted)
  row$gap              <- fit$gap
  row$any_truncated    <- isTRUE(fit$any_truncated)
  row$used_search      <- isTRUE(fit$used_search)
  row$n_fits           <- as.integer(fit$n_fits)
  row$solver_status_max <- as.integer(max(fit$fit_log$solver_status))

  # -- confirm Stage A really discretized at EXACTLY m_n bins per coordinate.
  # Asserted, not assumed: the whole study varies this one number, and the
  # package's default is a schedule ("adaptive" = fixed 32), so a silently
  # ignored `discretize_bins` would make every m_n column identical.
  md <- fit$fit@discretization_metadata
  if (is.null(md) || is.null(md$features)) {
    cli::cli_abort(
      "one_sim: the Stage-A fit carries no discretization metadata, so the \\
       realised grid resolution cannot be confirmed."
    )
  }
  n_thr <- vapply(c("x1", "x2"), function(cn) {
    length(md$features[[cn]]$thresholds %||% numeric(0))
  }, integer(1))
  row$n_bins_metadata <- as.integer(md$n_bins)
  row$n_thresholds_x1 <- n_thr[["x1"]]
  row$n_thresholds_x2 <- n_thr[["x2"]]
  row$bins_exact <- identical(as.integer(md$n_bins), n_bins) &&
    all(n_thr == n_bins - 1L)
  if (!row$bins_exact) {
    # Reported loudly as its own status rather than aborting the whole pilot:
    # the row still carries the realised counts so the discrepancy is visible
    # in the results, not swallowed.
    warn_msgs <- c(warn_msgs, sprintf(
      "grid resolution mismatch: requested %d bins, metadata says %s, thresholds per coordinate = (%d, %d)",
      n_bins, format(md$n_bins), n_thr[["x1"]], n_thr[["x2"]]))
    row$status <- "grid_resolution_mismatch"
    return(finish())
  }

  # -- Stage B. Two classed refusals are ORDINARY outcomes to report, not bugs
  # (see refine_tree()'s "Classed refusal conditions" section).
  t0 <- Sys.time()
  refined <- tryCatch(
    collect_warnings(
      optimaltrees::refine_tree(
        model = fit$fit, X = dat$X, y = dat$y,
        min_leaf_n = min_leaf_n, tree_index = 1L,
        max_depth = max_depth, max_leaves = leaf_budget,
        collapse = FALSE, r_n = r_n, M_n = M_n
      )
    ),
    error = function(e) e
  )
  row$secs_stage_b <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(refined, "error")) {
    row$status <- if (inherits(refined, "optimaltrees_stage_b_binary_split")) {
      "stage_b_binary_split"
    } else if (inherits(refined, "optimaltrees_refine_infeasible")) {
      "stage_b_refine_infeasible"
    } else {
      "stage_b_error"
    }
    row$error_class <- paste(class(refined), collapse = ",")
    row$error_message <- conditionMessage(refined)
    return(finish())
  }

  log_df <- refined@refinement_log
  required_log_cols <- c("path", "coord", "grid_cut_lo", "grid_cut_hi",
                          "incumbent_cut", "refined_cut", "bracket_lo",
                          "bracket_hi", "n_node", "n_candidates", "sse_before",
                          "sse_after", "collapsed", "refined", "reason")
  missing_cols <- setdiff(required_log_cols, names(log_df))
  if (length(missing_cols)) {
    cli::cli_abort(c(
      "one_sim: refine_tree()'s refinement log is missing column(s) \\
       {.field {missing_cols}}.",
      "i" = "Present: {.field {names(log_df)}}."
    ))
  }

  # Authoritative final cuts come from the returned TREE, not the log: the log
  # records what each node did when it was processed, the tree records what it
  # ended up as. They must agree -- checked, not assumed.
  splits <- coord_tree_split_rows(refined@tree)
  row$n_splits <- nrow(splits)
  row$n_leaves_refined <- as.integer(optimaltrees::n_leaves(refined))
  if (!setequal(splits$path, log_df$path)) {
    cli::cli_abort(c(
      "one_sim: the refined tree's split paths and the refinement log's paths \\
       disagree.",
      "i" = "tree: {.val {splits$path}}; log: {.val {log_df$path}}."
    ))
  }
  cut_mismatch <- vapply(splits$path, function(pp) {
    abs(node_field(splits, pp, "cut") -
          node_field(log_df, pp, "refined_cut"))
  }, numeric(1))
  if (any(cut_mismatch > 1e-12)) {
    cli::cli_abort(c(
      "one_sim: the refined tree's cut values disagree with the refinement \\
       log's {.field refined_cut} (max |diff| = {max(cut_mismatch)}).",
      "i" = "One of the two is not the final threshold, so threshold-error \\
             metrics cannot be trusted."
    ))
  }

  row$root_coord  <- node_field(splits, "",      "coord", NA_character_)
  row$left_coord  <- node_field(splits, "left",  "coord", NA_character_)
  row$right_coord <- node_field(splits, "right", "coord", NA_character_)

  # Node-wise topology match against f*'s topology (X1 at the root, X2 in each
  # child). "left"/"right" only MEAN the x1<=t1 / x1>t1 sides when the root
  # actually splits x1, so the child matches are conditioned on the root match
  # -- otherwise a tree rooted on x2 would report a spurious child match.
  row$match_root  <- identical(row$root_coord, ground_truth$topology$root$coord)
  row$match_left  <- isTRUE(row$match_root) &&
    identical(row$left_coord, ground_truth$topology$left$coord)
  row$match_right <- isTRUE(row$match_root) &&
    identical(row$right_coord, ground_truth$topology$right$coord)
  row$topology_match <- isTRUE(row$match_root) && isTRUE(row$match_left) &&
    isTRUE(row$match_right) && row$n_splits == 3L

  if (isTRUE(row$match_root)) {
    row$t1_hat <- node_field(splits, "", "cut")
    row$t1_err <- abs(row$t1_hat - ground_truth$t1_star)
  }
  if (isTRUE(row$match_left)) {
    row$t2L_hat <- node_field(splits, "left", "cut")
    row$t2L_err <- abs(row$t2L_hat - ground_truth$t2_star_L)
  }
  if (isTRUE(row$match_right)) {
    row$t2R_hat <- node_field(splits, "right", "cut")
    row$t2R_err <- abs(row$t2R_hat - ground_truth$t2_star_R)
  }

  for (nm in c("root", "left", "right")) {
    pp <- if (identical(nm, "root")) "" else nm
    row[[paste0("grid_cut_", nm)]] <- node_field(log_df, pp, "incumbent_cut")
    row[[paste0("reason_", nm)]]   <- node_field(log_df, pp, "reason", NA_character_)
    row[[paste0("moved_", nm)]]    <- node_field(log_df, pp, "refined", NA)
    row[[paste0("n_cand_", nm)]]   <- node_field(log_df, pp, "n_candidates", NA_integer_)
    row[[paste0("n_node_", nm)]]   <- node_field(log_df, pp, "n_node", NA_integer_)
  }

  # -- excess risk on the single fixed evaluation sample (spec Section 3).
  # f-hat carries its OWN fitted leaf means; f* carries its exact population
  # leaf means (already folded into ground_truth$fstar_risk_mc). Both are
  # integrated against the same 1e7 draws, so the MC-integration offset
  # cancels in the difference.
  t0 <- Sys.time()
  pred <- stats::predict(refined, mc_eval$X)
  if (length(pred) != length(mc_eval$gamma0) || anyNA(pred)) {
    cli::cli_abort(
      "one_sim: predict() on the evaluation sample returned \\
       {length(pred)} value(s) ({sum(is.na(pred))} NA) for \\
       {length(mc_eval$gamma0)} draws."
    )
  }
  row$risk_fhat   <- mean((mc_eval$gamma0 - pred)^2)
  row$excess_risk <- row$risk_fhat - ground_truth$fstar_risk_mc
  row$secs_eval   <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  row$status <- "ok"
  finish()
}
