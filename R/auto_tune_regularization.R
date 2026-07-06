#' Auto-tune Regularization for Non-Empty Rashomon Intersection
#'
#' @description
#' Multi-tier search for regularization strength that yields non-empty
#' Rashomon set intersection across K folds. Implements a robust fallback
#' strategy:
#'
#' \strong{Tier 1:} Try user's lambda with epsilon tuning
#' \strong{Tier 2:} Search lambda bidirectionally (stronger/weaker)
#' \strong{Tier 3:} Fully saturated trees (lambda → 0)
#' \strong{Tier 4:} Give up, return NULL for fold-specific fallback
#'
#' @param X Data.frame or matrix of features
#' @param y Vector of binary class labels
#' @param K Number of folds
#' @param fold_indices Pre-computed fold indices (list of length K)
#' @param loss_function Loss function: "log_loss" or "squared_error"
#' @param regularization_start User's starting lambda value
#' @param epsilon_n_fixed Fixed epsilon_n for lambda search. If NULL, uses the
#'   theory rate \code{select_epsilon_n(n)} = log(n)/n (= o(n^{-1/2}))
#' @param max_attempts Maximum lambda candidates to try in Tier 2. Default: 6
#' @param lambda_min Minimum lambda floor applied in Tier 2 search. Any candidate
#'   lambda below this value is clamped to \code{lambda_min}. Default: \code{NULL},
#'   which computes \code{0.5 * log(n) / n} automatically (half the theoretical
#'   minimum meaningful regularization). This prevents near-zero lambda from producing
#'   stump-only intersections that satisfy the criterion trivially. Set to \code{0}
#'   to disable the floor.
#' @param verbose Print progress information. Default: FALSE
#' @param ... Additional parameters passed to tree fitting
#'
#' @return List with:
#'   \item{converged}{TRUE if intersection found, FALSE otherwise}
#'   \item{tier}{Which tier succeeded: "epsilon_tuned", "lambda_search", "saturated", or "failed"}
#'   \item{regularization}{Lambda value that worked (or original if failed)}
#'   \item{epsilon_n}{Epsilon value used}
#'   \item{result}{CFRashomon object if converged, NULL otherwise}
#'   \item{n_leaves_approx}{Approximate leaf count (if available)}
#'
#' @details
#' For binary features with finite partition space, weaker regularization
#' often stabilizes the partition structure (all folds grow to full depth),
#' while stronger regularization can increase heterogeneity (different pruning
#' choices across folds). This function searches for the sweet spot.
#'
#' @export
auto_tune_regularization_for_intersection <- function(
  X_binary = NULL, X, y, K, fold_indices,
  loss_function,
  regularization_start,
  epsilon_n_fixed = NULL,
  max_attempts = 6,
  lambda_min = NULL,
  verbose = FALSE,
  ...
) {

  n <- nrow(X)

  # Apply theory-motivated floor if not provided:
  # Never try lambda below 0.5 * log(n)/n — that regime produces stumps
  # (every partition is within Rashomon bound of the global mean) and trivially
  # satisfies the intersection criterion without learning any structure.
  if (is.null(lambda_min)) {
    lambda_min <- 0.5 * log(n) / n
  }

  # Set fixed epsilon if not provided
  if (is.null(epsilon_n_fixed)) {
    # Theory rate log(n)/n (= o(n^{-1/2})); NOT sqrt(log(n)/n).
    epsilon_n_fixed <- select_epsilon_n(n)
  }

  if (verbose) {
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("Multi-Tier Regularization Search for Stable Rashomon Partition\n")
    cat(strrep("=", 70), "\n")
    cat(sprintf("Starting lambda: %.6f\n", regularization_start))
    cat(sprintf("Fixed epsilon_n: %.4f\n", epsilon_n_fixed))
    cat("\n")
  }

  # ========================================================================
  # TIER 1: Try user's lambda with epsilon tuning
  # ========================================================================
  if (verbose) {
    cat("TIER 1: User's lambda with epsilon tuning\n")
    cat(strrep("-", 70), "\n")
  }

  result_t1 <- auto_tune_rashomon_intersection(
    X = X, y = y, K = K, fold_indices = fold_indices,
    X_binary = X_binary,
    loss_function = loss_function,
    regularization = regularization_start,
    c_start = 1,
    c_max = 10,
    binary_tolerance = 0.1,
    verbose = verbose,
    ...
  )

  if (result_t1$converged) {
    if (verbose) {
      cat("\n✓ TIER 1 SUCCESS: Found intersection with epsilon tuning\n")
      cat(strrep("=", 70), "\n\n")
    }
    return(list(
      converged = TRUE,
      tier = "epsilon_tuned",
      regularization = regularization_start,
      epsilon_n = result_t1$result@rashomon_bound_multiplier,
      result = result_t1$result,
      n_leaves_approx = NA
    ))
  }

  if (verbose) {
    cat("\n✗ Tier 1 failed: No intersection found with epsilon tuning\n\n")
  }

  # ========================================================================
  # TIER 2: Search lambda bidirectionally
  # ========================================================================
  if (verbose) {
    cat("TIER 2: Bidirectional lambda search\n")
    cat(strrep("-", 70), "\n")
    cat("Searching for regularization strength that yields stable partition...\n\n")
  }

  # Search from weakest to strongest regularization.
  # For DML nuisance estimation we want the most complex intersecting tree
  # (minimal underfitting bias). Take the FIRST intersection found, which
  # corresponds to the weakest lambda (most complex tree). Stumps are a last
  # resort: they always intersect but produce constant predictions and bias.
  lambda_multipliers <- c(
    0.01, 0.05, 0.1, 0.2, 0.5,  # Weaker (more complex trees) - try first
    1,                            # Original
    2, 5, 10, 20                  # Stronger (simpler trees) - last resort
  )

  # Build deduplicated candidate list: apply lambda_min floor, then drop exact
  # duplicates. Without deduplication, multiple low multipliers can all clamp
  # to the same lambda_min value, wasting max_attempts slots on identical calls.
  lambda_candidates <- unique(vapply(
    lambda_multipliers,
    function(m) max(lambda_min, regularization_start * m),
    numeric(1)
  ))
  lambda_candidates <- lambda_candidates[seq_len(min(max_attempts, length(lambda_candidates)))]

  best_result <- NULL
  best_lambda <- NULL
  best_n_leaves <- NA

  for (lambda_try in lambda_candidates) {
    mult <- lambda_try / regularization_start

    if (verbose) {
      cat(sprintf("  Trying lambda = %.6f (%.2fx original)... ", lambda_try, mult))
    }

    # Try this lambda with fixed epsilon
    cf_result <- tryCatch(
      fit_rashomon_folds(
        X_binary = X_binary, y = y, K = K,
        fold_indices = fold_indices,
        fold_seeds = rep(NA_integer_, K),
        loss_function = loss_function,
        regularization = lambda_try,
        rashomon_bound_multiplier = epsilon_n_fixed,
        rashomon_bound_adder = 0,
        rashomon_ignore_trivial_extensions = FALSE,
        single_tree = FALSE,
        n = nrow(X),
        verbose = FALSE,
        parallel = TRUE,
        ...
      ),
      error = function(e) NULL
    )

    if (!is.null(cf_result) && cf_result@n_intersecting > 0) {
      # Found intersection — take it immediately.
      # Searching weakest-first means the first success is the most complex
      # intersecting tree, minimising underfitting bias in nuisance models.
      n_leaves <- tryCatch({
        if (length(cf_result@intersecting_trees) > 0) {
          count_leaves_node(cf_result@intersecting_trees[[1]])
        } else {
          NA
        }
      }, error = function(e) NA)

      if (verbose) {
        if (!is.na(n_leaves)) {
          cat(sprintf("✓ Intersection found! (~%d leaves)\n", n_leaves))
        } else {
          cat("✓ Intersection found!\n")
        }
      }

      best_result <- cf_result
      best_lambda <- lambda_try
      best_n_leaves <- n_leaves
      break  # First success = most complex intersecting tree
    } else {
      if (verbose) {
        cat("✗ Empty\n")
      }
    }
  }

  if (!is.null(best_result)) {
    if (verbose) {
      cat(sprintf("\n✓ TIER 2 SUCCESS: Found stable partition\n"))
      cat(sprintf("  Selected lambda: %.6f (%.2fx original)\n",
                  best_lambda, best_lambda / regularization_start))
      if (!is.na(best_n_leaves)) {
        cat(sprintf("  Tree complexity: ~%d leaves\n", best_n_leaves))
      }
      cat(strrep("=", 70), "\n\n")
    }
    return(list(
      converged = TRUE,
      tier = "lambda_search",
      regularization = best_lambda,
      epsilon_n = epsilon_n_fixed,
      result = best_result,
      n_leaves_approx = best_n_leaves
    ))
  }

  if (verbose) {
    cat("\n✗ Tier 2 failed: No lambda yielded intersection\n\n")
  }

  # ========================================================================
  # TIER 3: Fully saturated trees (lambda → 0)
  # ========================================================================
  if (verbose) {
    cat("TIER 3: Fully saturated trees (minimal regularization)\n")
    cat(strrep("-", 70), "\n")
    cat("Trying lambda → 0 (full-depth trees)...\n\n")
  }

  lambda_saturated <- 0.001 * log(n) / n

  cf_saturated <- tryCatch(
    fit_rashomon_folds(
      X_binary = X_binary, y = y, K = K,
      fold_indices = fold_indices,
      fold_seeds = rep(NA_integer_, K),
      loss_function = loss_function,
      regularization = lambda_saturated,
      rashomon_bound_multiplier = epsilon_n_fixed,
      rashomon_bound_adder = 0,
      rashomon_ignore_trivial_extensions = FALSE,
      single_tree = FALSE,
      n = nrow(X),
      verbose = FALSE,
      parallel = TRUE,
      ...
    ),
    error = function(e) NULL
  )

  if (!is.null(cf_saturated) && cf_saturated@n_intersecting > 0) {
    n_leaves <- tryCatch({
      if (length(cf_saturated@intersecting_trees) > 0) {
        count_leaves_node(cf_saturated@intersecting_trees[[1]])
      } else {
        NA
      }
    }, error = function(e) NA)

    if (verbose) {
      cat("✓ TIER 3 SUCCESS: Saturated trees have intersection\n")
      if (!is.na(n_leaves)) {
        cat(sprintf("  Tree complexity: ~%d leaves (full depth)\n", n_leaves))
      }
      cat("\n")
      cat("⚠ WARNING: Using very weak regularization (essentially no pruning)\n")
      cat("  This may lead to overfitting in nuisance models.\n")
      cat("  Consider this a diagnostic: simpler trees did not stabilize.\n")
      cat(strrep("=", 70), "\n\n")
    }

    return(list(
      converged = TRUE,
      tier = "saturated",
      regularization = lambda_saturated,
      epsilon_n = epsilon_n_fixed,
      result = cf_saturated,
      n_leaves_approx = n_leaves
    ))
  }

  if (verbose) {
    cat("✗ Tier 3 failed: Even saturated trees have empty intersection\n\n")
  }

  # ========================================================================
  # TIER 4: Give up - no intersection possible
  # ========================================================================
  if (verbose) {
    cat("✗ ALL TIERS FAILED\n")
    cat(strrep("=", 70), "\n")
    cat("No regularization level yielded stable partition across folds.\n")
    cat("This indicates substantial cross-fold heterogeneity.\n")
    cat("Will fall back to fold-specific trees.\n")
    cat(strrep("=", 70), "\n\n")
  }

  return(list(
    converged = FALSE,
    tier = "failed",
    regularization = regularization_start,
    epsilon_n = epsilon_n_fixed,
    result = NULL,
    n_leaves_approx = NA
  ))
}
