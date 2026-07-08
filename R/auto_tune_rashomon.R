#' Auto-Tune Rashomon Bound Multiplier for Non-Empty Intersection
#'
#' @description
#' Uses bidirectional exponential search followed by binary refinement to find
#' the smallest epsilon_n that yields non-empty intersection across K folds.
#' Then selects the best tree from the intersection by lowest penalized risk.
#'
#' Algorithm (Bidirectional Search):
#' 1. Initial test: Try c = 1 (c_start)
#' 2a. If c = 1 succeeds: Downward search c = 0.5, 0.25, 0.125, ... (minimize bias)
#' 2b. If c = 1 fails: Upward search c = 2, 4, 8, ... (find intersection)
#' 3. Binary refinement: Narrow to smallest working epsilon_n
#' 4. Tree selection: Pick tree with lowest R_pen from final intersection
#'
#' Why bidirectional? Starting at c=1 may be too large for easy intersection
#' problems. Downward search minimizes bias by finding smallest working epsilon_n.
#'
#' @param X Feature matrix (already discretized)
#' @param y Outcome vector
#' @param K Number of folds
#' @param fold_indices Fold assignment vector
#' @param loss_function Loss function ("log_loss", "misclassification", "squared_error")
#' @param regularization Regularization parameter (fixed)
#' @param c_start Starting constant (default: 1)
#' @param c_min Minimum constant for downward search (default: 0.01)
#' @param c_max Maximum constant for upward search (default: 100). The sole caller
#'   (\code{auto_tune_regularization_for_intersection}) overrides this with a smaller value.
#' @param binary_tolerance Tolerance for binary refinement (default: 0.1)
#' @param verbose Logical, print diagnostic info
#' @param ... Additional arguments passed to fit_rashomon_folds()
#'
#' @return List with:
#'   - epsilon_n: Selected epsilon_n value
#'   - c: Selected multiplier such that epsilon_n = c * log(n)/n (NA if failed)
#'   - n_intersecting: Number of intersecting trees
#'   - converged: TRUE if intersection found, FALSE if all attempts failed
#'   - attempts: Number of attempts tried
#'   - result: Full cross_fitted_rashomon result (or NULL if failed)
#'
#' @keywords internal
auto_tune_rashomon_intersection <- function(X, y, K, fold_indices,
                                           X_binary = NULL,
                                           loss_function, regularization,
                                           c_start = 1,
                                           c_min = 0.01,
                                           c_max = 100,
                                           binary_tolerance = 0.1,
                                           verbose = TRUE,
                                           ...) {
  n <- nrow(X)
  # Base scale = theory rate log(n)/n (select_epsilon_n). The reported multiplier c
  # is defined ON THIS SAME BASE: c = epsilon_n / (log(n)/n). (An earlier version
  # divided the reported c by sqrt(log n/n) -- an undefined variable and an
  # inconsistent scale vs the searched c; both fixed here.) epsilon_n = c*log(n)/n
  # is o(n^{-1/2}) for any fixed c, so a large-but-finite c stays asymptotically
  # valid; the selected c is RETURNED so downstream inference can be evaluated
  # against it empirically.
  eps_base <- select_epsilon_n(n)

  if (verbose) {
    cat("=== AUTO-TUNING RASHOMON INTERSECTION ===\n")
    cat(sprintf("n = %d, log(n)/n = %.4f\n", n, eps_base))
  }

  probe_fn <- function(c_val) {
    epsilon_n <- c_val * eps_base
    tryCatch(
      fit_rashomon_folds(
        X_binary = X_binary, y = y, K = K, fold_indices = fold_indices,
        fold_seeds = rep(NA_integer_, K),
        loss_function = loss_function,
        regularization = regularization,
        rashomon_bound_multiplier = epsilon_n,
        rashomon_bound_adder = 0,
        rashomon_ignore_trivial_extensions = FALSE,
        single_tree = FALSE,
        n = n,
        verbose = FALSE,
        ...
      ),
      error = function(e) NULL
    )
  }

  classify_fn <- function(result) {
    n_int <- get_rashomon_prop(result, "n_intersecting")
    if (!is.null(n_int) && n_int > 0) "lower" else "higher"
  }

  search <- bidirectional_exp_binary_search(
    probe_fn = probe_fn, classify_fn = classify_fn,
    start = c_start, val_min = c_min, val_max = c_max,
    binary_tolerance = binary_tolerance,
    verbose = verbose
  )

  if (!search$converged) {
    # No non-empty intersection even at c_max. We do NOT silently fall back to
    # fold-specific trees (that abandons the single-tree goal); the caller decides.
    # Report the failure honestly with the (large) c reached.
    if (verbose) {
      cat(sprintf("\nUpward search failed: no non-empty intersection up to c = %.1f (epsilon_n = %.4f)\n",
                  c_max, c_max * eps_base))
    }
    return(list(
      epsilon_n = c_max * eps_base,
      c = NA_real_,
      n_intersecting = 0L,
      converged = FALSE,
      attempts = search$attempts,
      result = NULL
    ))
  }

  # Phase 3: Select best tree from intersection
  if (verbose) {
    cat(sprintf("\n--- Phase 3: Tree Selection (%d candidates) ---\n",
                get_rashomon_prop(search$result, "n_intersecting")))
  }
  result_success <- select_best_tree_from_intersection(search$result, verbose)
  final_epsilon  <- get_rashomon_prop(result_success, "rashomon_bound_multiplier")
  final_c        <- final_epsilon / eps_base   # consistent base = log(n)/n

  if (verbose) {
    cat(sprintf("\nAuto-tuning complete: epsilon_n = %.4f (c = %.2f)\n",
                final_epsilon, final_c))
    cat(sprintf("Total attempts: %d\n", search$attempts))
  }

  list(
    epsilon_n = final_epsilon,
    c = final_c,
    n_intersecting = get_rashomon_prop(result_success, "n_intersecting"),
    converged = TRUE,
    attempts = search$attempts,
    result = result_success
  )
}

#' Select Best Tree from Rashomon Intersection
#'
#' @description
#' Returns the intersection result unchanged: \code{find_tree_intersection()} already
#' selects the minimum-penalized-risk tree (\code{model_objective = empirical_loss +
#' lambda * n_leaves}) and reorders it to position 1, and \code{predict.cf_rashomon()}
#' uses the first tree. This wrapper exists only as a named selection step (and for the
#' verbose trace); it no longer re-selects.
#'
#' The former implementation also carried an S3-list reordering branch, but
#' \code{fit_rashomon_folds()} always returns an S7 \code{CFRashomon} object, so that
#' branch was unreachable and has been removed.
#'
#' @param result A \code{CFRashomon} intersection result.
#' @param verbose Logical.
#' @return \code{result}, unchanged.
#' @keywords internal
select_best_tree_from_intersection <- function(result, verbose = TRUE) {
  n_int <- get_rashomon_prop(result, "n_intersecting")
  if (verbose) {
    if (is.null(n_int) || n_int <= 1) {
      cat("  Single tree in intersection, no selection needed\n")
    } else {
      cat("  Using best tree from intersection (already ordered by penalized risk)\n")
    }
  }
  result
}
