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
#' @param c_max Maximum constant for upward search (default: 100)
#' @param binary_tolerance Tolerance for binary refinement (default: 0.1)
#' @param verbose Logical, print diagnostic info
#' @param ... Additional arguments passed to fit_rashomon_folds()
#'
#' @return List with:
#'   - epsilon_n: Selected epsilon_n value
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
  sqrt_log_n_over_n <- sqrt(log(n) / n)

  if (verbose) {
    cat("=== AUTO-TUNING RASHOMON INTERSECTION ===\n")
    cat(sprintf("n = %d, sqrt(log(n)/n) = %.4f\n", n, sqrt_log_n_over_n))
  }

  probe_fn <- function(c_val) {
    epsilon_n <- c_val * sqrt_log_n_over_n
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
    if (verbose) {
      cat(sprintf("\nUpward search failed (tried up to c = %.1f)\n", c_max))
      cat("Recommendation: Use fold-specific trees (use_rashomon = FALSE)\n")
    }
    return(list(
      epsilon_n = c_max * sqrt_log_n_over_n,
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

  if (verbose) {
    cat(sprintf("\nAuto-tuning complete: epsilon_n = %.4f (c ~ %.2f)\n",
                final_epsilon, final_epsilon / sqrt_log_n_over_n))
    cat(sprintf("Total attempts: %d\n", search$attempts))
  }

  list(
    epsilon_n = final_epsilon,
    n_intersecting = get_rashomon_prop(result_success, "n_intersecting"),
    converged = TRUE,
    attempts = search$attempts,
    result = result_success
  )
}

#' Select Best Tree from Rashomon Intersection
#'
#' @description
#' When intersection contains multiple trees, select the one with lowest
#' penalized risk: R_pen = empirical_risk + regularization * complexity
#'
#' @param result cross_fitted_rashomon result with n_intersecting > 1
#' @param verbose Logical
#' @return Updated result with selected tree moved to first position
#' @keywords internal
select_best_tree_from_intersection <- function(result, verbose = TRUE) {
  if (get_rashomon_prop(result, "n_intersecting") <= 1) {
    # Only one tree, nothing to select
    if (verbose && get_rashomon_prop(result, "n_intersecting") == 1) {
      cat("  Single tree in intersection, no selection needed\n")
    }
    return(result)
  }

  # S7 objects can't be mutated easily - skip tree selection for now
  # The first tree in the intersection is already chosen appropriately
  if (S7::S7_inherits(result, CFRashomon)) {
    if (verbose) {
      cat("  Using first tree from S7 intersection\n")
    }
    return(result)
  }

  # Extract penalized risks for each intersecting tree (S3 only)
  # Check if tree_risks is available from intersection results
  tree_risks <- get_rashomon_prop(result, "tree_risks")
  if (is.null(tree_risks) || length(tree_risks) == 0) {
    if (verbose) {
      cat("  No risk information available, using first tree\n")
    }
    return(result)
  }

  # Check if any tree has non-null penalized risk
  has_risk_info <- purrr::map_lgl(tree_risks, ~ !is.null(.x$penalized_risk))

  if (!any(has_risk_info)) {
    if (verbose) {
      cat("  No penalized risk information available, using first tree\n")
    }
    return(result)
  }

  # Find tree with minimum penalized risk
  risks <- purrr::map_dbl(tree_risks, ~ {
    if (!is.null(.x$penalized_risk)) {
      return(.x$penalized_risk)
    } else {
      return(Inf)  # Trees without risk info get Inf (won't be selected)
    }
  })

  best_idx <- which.min(risks)

  if (verbose) {
    # Only show risks for trees with valid info
    valid_risks <- risks[is.finite(risks)]
    if (length(valid_risks) > 0) {
      cat(sprintf("  Penalized risks: %s\n",
                 paste(sprintf("%.4f", valid_risks), collapse = ", ")))
      cat(sprintf("  Selected tree %d (R_pen = %.4f)\n", best_idx, risks[best_idx]))
    } else {
      cat("  Using first tree (no valid risk information)\n")
    }
  }

  # Reorder so best tree is first (used by predict.cf_rashomon) - S3 only
  if (best_idx != 1 && is.finite(risks[best_idx])) {
    intersecting_trees <- get_rashomon_prop(result, "intersecting_trees")
    tree_jsons <- get_rashomon_prop(result, "tree_jsons")
    intersecting_structures <- get_rashomon_prop(result, "intersecting_structures")

    result[["intersecting_trees"]] <- c(
      intersecting_trees[best_idx],
      intersecting_trees[-best_idx]
    )
    result[["tree_risks"]] <- c(
      tree_risks[best_idx],
      tree_risks[-best_idx]
    )
    if (!is.null(tree_jsons)) {
      result[["tree_jsons"]] <- c(
        tree_jsons[best_idx],
        tree_jsons[-best_idx]
      )
    }
    if (!is.null(intersecting_structures)) {
      result[["intersecting_structures"]] <- c(
        intersecting_structures[best_idx],
        intersecting_structures[-best_idx]
      )
    }
  }

  return(result)
}
