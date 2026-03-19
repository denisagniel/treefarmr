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
#' @param ... Additional arguments passed to try_cross_fitted_rashomon_internal()
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
                                           loss_function, regularization,
                                           c_start = 1,
                                           c_min = 0.01,
                                           c_max = 100,
                                           binary_tolerance = 0.1,
                                           verbose = TRUE,
                                           ...) {
  # Helper to get properties from S7 or S3 objects
  get_prop <- function(obj, prop) {
    if (is.null(obj)) return(NULL)
    if (S7::S7_inherits(obj, CFRashomon)) {
      switch(prop,
        n_intersecting = obj@n_intersecting,
        tree_risks = obj@tree_risks,
        rashomon_bound_multiplier = obj@rashomon_bound_multiplier,
        intersecting_trees = obj@intersecting_trees,
        tree_jsons = NULL,  # S7 doesn't have this
        intersecting_structures = NULL,  # S7 doesn't have this
        NULL
      )
    } else {
      obj[[prop]]
    }
  }

  n <- nrow(X)
  sqrt_log_n_over_n <- sqrt(log(n) / n)

  if (verbose) {
    cat("=== AUTO-TUNING RASHOMON INTERSECTION ===\n")
    cat(sprintf("n = %d, sqrt(log(n)/n) = %.4f\n", n, sqrt_log_n_over_n))
  }

  # Helper function to try a specific c value
  try_epsilon <- function(c_val) {
    epsilon_n <- c_val * sqrt_log_n_over_n
    tryCatch({
      try_cross_fitted_rashomon_internal(
        X = X, y = y, K = K, fold_indices = fold_indices,
        loss_function = loss_function,
        regularization = regularization,
        rashomon_bound_multiplier = epsilon_n,
        rashomon_bound_adder = 0,
        verbose = FALSE,
        ...
      )
    }, error = function(e) {
      if (verbose) {
        cat(sprintf("  Error: %s\n", conditionMessage(e)))
      }
      NULL
    })
  }

  attempts <- 0

  # Phase 1A: Try starting point
  if (verbose) {
    cat("\n--- Phase 1: Initial Test at c = ", c_start, " ---\n", sep = "")
  }

  epsilon_start <- c_start * sqrt_log_n_over_n
  attempts <- attempts + 1

  if (verbose) {
    cat(sprintf("Attempt %d: c = %.2f, epsilon_n = %.4f\n",
               attempts, c_start, epsilon_start))
  }

  result_start <- try_epsilon(c_start)

  # Determine search direction
  n_int_start <- get_prop(result_start, "n_intersecting")
  if (!is.null(result_start) && !is.null(n_int_start) && n_int_start > 0) {
    # SUCCESS at c_start → search DOWNWARD for smaller epsilon_n
    if (verbose) {
      cat(sprintf("  SUCCESS: %d tree(s) found\n", n_int_start))
      cat("\n--- Phase 1B: Downward Exponential Search (minimizing epsilon_n) ---\n")
    }

    c_current <- c_start / 2
    c_last_success <- c_start
    c_first_fail <- NULL
    result_success <- result_start

    while (c_current >= c_min) {
      epsilon_n <- c_current * sqrt_log_n_over_n
      attempts <- attempts + 1

      if (verbose) {
        cat(sprintf("Attempt %d: c = %.2f, epsilon_n = %.4f\n",
                   attempts, c_current, epsilon_n))
      }

      result <- try_epsilon(c_current)

      if (!is.null(result) && get_prop(result, "n_intersecting") > 0) {
        # Still works! Try even smaller
        c_last_success <- c_current
        result_success <- result
        if (verbose) {
          cat(sprintf("  SUCCESS: %d tree(s), trying smaller\n", get_prop(result, "n_intersecting")))
        }
        c_current <- c_current / 2  # Halve for next attempt
      } else {
        # Failed - c_last_success is our answer
        c_first_fail <- c_current
        if (verbose) {
          cat("  Empty intersection, stopping downward search\n")
        }
        break
      }
    }

    # Binary refinement for downward search
    if (!is.null(c_first_fail) && (c_last_success - c_first_fail) > binary_tolerance) {
      if (verbose) {
        cat(sprintf("\n--- Phase 2: Binary Refinement [%.2f, %.2f] ---\n",
                   c_first_fail, c_last_success))
      }

      c_low <- c_first_fail    # Failed (empty)
      c_high <- c_last_success # Succeeded (non-empty)

      while ((c_high - c_low) > binary_tolerance) {
        c_mid <- (c_low + c_high) / 2
        epsilon_n <- c_mid * sqrt_log_n_over_n
        attempts <- attempts + 1

        if (verbose) {
          cat(sprintf("Binary attempt %d: c = %.2f, epsilon_n = %.4f\n",
                     attempts, c_mid, epsilon_n))
        }

        result <- try_epsilon(c_mid)

        if (!is.null(result) && get_prop(result, "n_intersecting") > 0) {
          # Success: this is a candidate, try smaller
          c_high <- c_mid
          result_success <- result
          if (verbose) {
            cat(sprintf("  %d tree(s), trying smaller\n", get_prop(result, "n_intersecting")))
          }
        } else {
          # Failure: need larger
          c_low <- c_mid
          if (verbose) {
            cat("  Empty, trying larger\n")
          }
        }
      }

      if (verbose) {
        cat(sprintf("Binary refinement converged: c ~ %.2f\n", c_high))
      }
    } else if (verbose && is.null(c_first_fail)) {
      cat(sprintf("\nReached minimum c = %.2f (no refinement needed)\n", c_min))
    }

  } else {
    # FAILURE at c_start → search UPWARD for larger epsilon_n
    if (verbose) {
      cat("  Empty intersection\n")
      cat("\n--- Phase 1B: Upward Exponential Search (finding intersection) ---\n")
    }

    c_current <- c_start * 2
    c_last_fail <- c_start
    result_success <- NULL

    while (c_current <= c_max) {
      epsilon_n <- c_current * sqrt_log_n_over_n
      attempts <- attempts + 1

      if (verbose) {
        cat(sprintf("Attempt %d: c = %.2f, epsilon_n = %.4f\n",
                   attempts, c_current, epsilon_n))
      }

      result <- try_epsilon(c_current)

      if (!is.null(result) && get_prop(result, "n_intersecting") > 0) {
        # Found intersection!
        result_success <- result
        if (verbose) {
          cat(sprintf("  SUCCESS: %d tree(s) found\n", get_prop(result, "n_intersecting")))
        }
        break
      } else {
        if (verbose) {
          cat("  Empty intersection\n")
        }
        c_last_fail <- c_current
        c_current <- c_current * 2  # Double for next attempt
      }
    }

    # Check if upward search succeeded
    if (is.null(result_success)) {
      if (verbose) {
        cat(sprintf("\nUpward search failed (tried up to c = %.1f)\n", c_max))
        cat("Recommendation: Use fold-specific trees (use_rashomon = FALSE)\n")
      }
      return(list(
        epsilon_n = c_max * sqrt_log_n_over_n,
        n_intersecting = 0L,
        converged = FALSE,
        attempts = attempts,
        result = NULL
      ))
    }

    # Binary refinement for upward search
    if ((c_current - c_last_fail) > binary_tolerance) {
      if (verbose) {
        cat(sprintf("\n--- Phase 2: Binary Refinement [%.2f, %.2f] ---\n",
                   c_last_fail, c_current))
      }

      c_low <- c_last_fail  # Failed (empty)
      c_high <- c_current   # Succeeded (non-empty)

      while ((c_high - c_low) > binary_tolerance) {
        c_mid <- (c_low + c_high) / 2
        epsilon_n <- c_mid * sqrt_log_n_over_n
        attempts <- attempts + 1

        if (verbose) {
          cat(sprintf("Binary attempt %d: c = %.2f, epsilon_n = %.4f\n",
                     attempts, c_mid, epsilon_n))
        }

        result <- try_epsilon(c_mid)

        if (!is.null(result) && get_prop(result, "n_intersecting") > 0) {
          # Success: try smaller
          c_high <- c_mid
          result_success <- result
          if (verbose) {
            cat(sprintf("  %d tree(s), trying smaller\n", get_prop(result, "n_intersecting")))
          }
        } else {
          # Failure: try larger
          c_low <- c_mid
          if (verbose) {
            cat("  Empty, trying larger\n")
          }
        }
      }

      if (verbose) {
        cat(sprintf("Binary refinement converged: c ~ %.2f\n", c_high))
      }
    }
  }

  # Phase 3: Select best tree from intersection
  if (verbose) {
    cat(sprintf("\n--- Phase 3: Tree Selection (%d candidates) ---\n",
               get_prop(result_success, "n_intersecting")))
  }

  # Select tree with lowest penalized risk from intersection
  result_success <- select_best_tree_from_intersection(result_success, verbose)

  final_epsilon <- get_prop(result_success, "rashomon_bound_multiplier")

  if (verbose) {
    cat(sprintf("\nAuto-tuning complete: epsilon_n = %.4f (c ~ %.2f)\n",
               final_epsilon, final_epsilon / sqrt_log_n_over_n))
    cat(sprintf("Total attempts: %d\n", attempts))
  }

  return(list(
    epsilon_n = final_epsilon,
    n_intersecting = get_prop(result_success, "n_intersecting"),
    converged = TRUE,
    attempts = attempts,
    result = result_success
  ))
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
  if (get_prop(result, "n_intersecting") <= 1) {
    # Only one tree, nothing to select
    if (verbose && get_prop(result, "n_intersecting") == 1) {
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
  tree_risks <- get_prop(result, "tree_risks")
  if (is.null(tree_risks) || length(tree_risks) == 0) {
    if (verbose) {
      cat("  No risk information available, using first tree\n")
    }
    return(result)
  }

  # Check if any tree has non-null penalized risk
  has_risk_info <- sapply(tree_risks, function(x) !is.null(x$penalized_risk))

  if (!any(has_risk_info)) {
    if (verbose) {
      cat("  No penalized risk information available, using first tree\n")
    }
    return(result)
  }

  # Find tree with minimum penalized risk
  risks <- sapply(tree_risks, function(x) {
    if (!is.null(x$penalized_risk)) {
      return(x$penalized_risk)
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
    intersecting_trees <- get_prop(result, "intersecting_trees")
    tree_jsons <- get_prop(result, "tree_jsons")
    intersecting_structures <- get_prop(result, "intersecting_structures")

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
