#' Auto-tune TreeFARMS Parameters
#'
#' @description
#' Automatically find optimal parameters (regularization or rashomon_bound_multiplier)
#' to achieve a target number of trees in the Rashomon set.
#'
#' @param X A data.frame or matrix of features
#' @param y A vector of binary class labels
#' @param loss_function Character string: "misclassification" or "log_loss"
#' @param target_trees Integer: target number of trees (default: 1)
#' @param max_trees Integer: maximum acceptable number of trees (default: 5)
#' @param fixed_param Character: which parameter is fixed ("regularization" or "rashomon_bound_multiplier")
#' @param fixed_value Numeric: the fixed parameter value
#' @param search_range Numeric vector: range to search for the variable parameter
#' @param max_iterations Integer: maximum number of search iterations (default: 20)
#' @param verbose Logical: whether to print search progress
#' @param ... Additional parameters passed to TreeFARMS
#'
#' @return A list containing:
#'   \item{model}{The trained TreeFARMS model}
#'   \item{regularization}{Final regularization value}
#'   \item{rashomon_bound_multiplier}{Final rashomon_bound_multiplier value}
#'   \item{n_trees}{Number of trees found}
#'   \item{iterations}{Number of search iterations used}
#'   \item{converged}{Whether the search converged to target}
#'
#' @examples
#' # Generate binary classification data
#' set.seed(42)
#' n <- 100
#' X <- data.frame(X1 = rbinom(n, 1, 0.5), X2 = rbinom(n, 1, 0.5))
#' y <- rbinom(n, 1, plogis(0.5 * X$X1 - 0.3 * X$X2))
#'
#' # Auto-tune to find exactly 1 tree (single optimal model)
#' result <- auto_tune_optimaltrees(
#'   X, y,
#'   loss_function = "log_loss",
#'   target_trees = 1,
#'   max_trees = 1,
#'   fixed_param = "regularization",
#'   fixed_value = 0.1
#' )
#'
#' print(result$n_trees)  # Should be 1
#' print(result$converged)  # Should be TRUE
#'
#' @export
auto_tune_optimaltrees <- function(X, y, loss_function = "misclassification",
                                target_trees = 1, max_trees = 5,
                                fixed_param = "regularization", fixed_value = 0.1,
                                search_range = NULL, max_iterations = 20,
                                verbose = FALSE,
                                discretize_method = "median",
                                discretize_bins = 2,
                                discretize_thresholds = NULL, ...) {

  # Input validation
  if (!fixed_param %in% c("regularization", "rashomon_bound_multiplier")) {
    stop("fixed_param must be 'regularization' or 'rashomon_bound_multiplier'")
  }

  if (target_trees < 1) {
    stop("target_trees must be at least 1")
  }

  if (max_trees < target_trees) {
    stop("max_trees must be at least target_trees")
  }

  # Validate discretization parameters
  if (!discretize_method %in% c("median", "quantiles")) {
    stop("discretize_method must be 'median' or 'quantiles', got: ",
         discretize_method, call. = FALSE)
  }

  if (!is.numeric(discretize_bins) || length(discretize_bins) != 1 || discretize_bins < 2) {
    stop("discretize_bins must be a single numeric value >= 2, got: ",
         discretize_bins, call. = FALSE)
  }

  if (!is.null(discretize_thresholds) && !is.numeric(discretize_thresholds)) {
    stop("discretize_thresholds must be numeric if provided", call. = FALSE)
  }

  # Set default search ranges based on fixed parameter
  if (is.null(search_range)) {
    if (fixed_param == "regularization") {
      # Search rashomon_bound_multiplier
      search_range <- c(0.01, 0.5)
    } else {
      # Search regularization
      if (loss_function == "log_loss") {
        search_range <- c(0.1, 1.0)  # More reasonable range for log_loss
      } else {
        search_range <- c(0.01, 0.5)  # Lower range for misclassification
      }
    }
  }

  if (verbose) {
    message("=== AUTO-TUNING OPTIMALTREES ===")
    message(sprintf("Target trees: %d, Max trees: %d", target_trees, max_trees))
    message(sprintf("Fixed %s: %.3f", fixed_param, fixed_value))
    message(sprintf("Searching %s in range [%.3f, %.3f]",
                ifelse(fixed_param == "regularization", "rashomon_bound_multiplier", "regularization"),
                search_range[1], search_range[2]))
    message(sprintf("Loss function: %s\n", loss_function))
  }

  # Convert to data.frame if matrix
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }

  # Store original X for metadata
  X_original <- X

  # Discretize continuous features
  discretization_result <- discretize_features(
    X = X,
    method = discretize_method,
    n_bins = discretize_bins,
    thresholds = discretize_thresholds
  )

  X <- discretization_result$X_binary
  discretization_metadata <- discretization_result$metadata

  # Build CSV string once for reuse in the loop
  data_df <- as.data.frame(X)
  data_df$class <- y
  header <- paste(names(data_df), collapse = ",")
  body <- apply(data_df, 1L, function(r) paste(as.character(r), collapse = ","))
  csv_string <- paste(c(header, body), collapse = "\n")
  base_config <- list(
    loss_function = loss_function,
    verbose = FALSE,
    worker_limit = 1L,
    rashomon = TRUE
  )

  fit_with_csv <- get(".treefarms_fit_with_csv", envir = asNamespace("optimaltrees"))

  # TIER 1: Exponential + Binary with default range
  if (verbose) {
    message("--- Tier 1: Standard Search ---")
  }

  result_tier1 <- .auto_tune_exponential_binary(
    csv_string = csv_string,
    base_config = base_config,
    fit_with_csv = fit_with_csv,
    X = X,
    y = y,
    X_original = X_original,
    discretization_metadata = discretization_metadata,
    target_trees = target_trees,
    max_trees = max_trees,
    fixed_param = fixed_param,
    fixed_value = fixed_value,
    search_range = search_range,
    max_iterations = max_iterations,
    verbose = verbose
  )

  if (!is.null(result_tier1) && result_tier1$converged) {
    if (verbose) {
      message(sprintf("\n=== SUCCESS: Found %d trees in Tier 1 ===\n", result_tier1$n_trees))
    }
    return(result_tier1)
  }

  # TIER 2: Wider search range
  if (verbose) {
    message("\n--- Tier 2: Wider Search Range ---")
  }

  wider_range <- if (fixed_param == "regularization") {
    c(0.001, 2.0)  # Much wider rashomon_bound_multiplier range
  } else {
    c(0.001, 2.0)  # Much wider regularization range
  }

  result_tier2 <- .auto_tune_exponential_binary(
    csv_string = csv_string,
    base_config = base_config,
    fit_with_csv = fit_with_csv,
    X = X,
    y = y,
    X_original = X_original,
    discretization_metadata = discretization_metadata,
    target_trees = target_trees,
    max_trees = max_trees,
    fixed_param = fixed_param,
    fixed_value = fixed_value,
    search_range = wider_range,
    max_iterations = max_iterations,
    verbose = verbose
  )

  if (!is.null(result_tier2) && result_tier2$converged) {
    if (verbose) {
      message(sprintf("\n=== SUCCESS: Found %d trees in Tier 2 ===\n", result_tier2$n_trees))
    }
    return(result_tier2)
  }

  # TIER 3: Best effort (return closest result with warning)
  best_result <- result_tier2 %||% result_tier1

  if (!is.null(best_result)) {
    warning(sprintf(
      "Auto-tuning did not converge to target %d-%d trees. Returning best result with %d trees.",
      target_trees, max_trees, best_result$n_trees
    ), call. = FALSE)
    best_result$converged <- FALSE
    return(best_result)
  }

  # Only error if no fits succeeded at all
  stop(sprintf("Auto-tuning failed: no successful model fits after %d iterations",
               max_iterations), call. = FALSE)
}

#' Exponential Search + Binary Refinement for Auto-Tuning
#'
#' @description
#' Implements bidirectional exponential search followed by binary refinement.
#' Similar to auto_tune_rashomon_intersection() but adapted for single-model fitting.
#'
#' Algorithm:
#' 1. Start at midpoint of search_range
#' 2. If midpoint succeeds (trees in target range): search downward (minimize parameter)
#' 3. If midpoint fails (trees outside range): search upward or downward as needed
#' 4. Binary refinement after exponential phase brackets target
#'
#' @keywords internal
.auto_tune_exponential_binary <- function(csv_string, base_config, fit_with_csv,
                                         X, y, X_original, discretization_metadata,
                                         target_trees, max_trees,
                                         fixed_param, fixed_value,
                                         search_range, max_iterations,
                                         verbose = FALSE) {

  # Helper function to fit model with specific parameter value
  try_fit <- function(param_value) {
    if (fixed_param == "regularization") {
      regularization <- fixed_value
      rashomon_bound_multiplier <- param_value
    } else {
      regularization <- param_value
      rashomon_bound_multiplier <- fixed_value
    }

    config <- c(base_config, list(
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier
    ))

    tryCatch({
      model <- fit_with_csv(csv_string, config, X, y,
                           single_tree = FALSE, store_training_data = FALSE,
                           compute_probabilities = FALSE,
                           discretization_metadata, X_original)
      list(
        model = model,
        regularization = regularization,
        rashomon_bound_multiplier = rashomon_bound_multiplier,
        n_trees = model@n_trees,  # S7 object: use @ not $
        converged = FALSE
      )
    }, error = function(e) {
      if (verbose) {
        message(sprintf("  Error fitting: %s", e$message))
      }
      NULL
    })
  }

  iterations <- 0
  best_result <- NULL
  best_distance <- Inf  # Distance from target

  # Phase 1: Exponential Search
  # Start at midpoint of search range
  param_start <- mean(search_range)

  if (verbose) {
    message(sprintf("\n--- Phase 1: Exponential Search (starting at %.4f) ---", param_start))
  }

  iterations <- iterations + 1
  if (verbose) {
    message(sprintf("Iteration %d: param = %.4f", iterations, param_start))
  }

  result_start <- try_fit(param_start)

  if (is.null(result_start)) {
    # Starting point failed, just return NULL
    return(NULL)
  }

  n_trees_start <- result_start$n_trees
  if (verbose) {
    message(sprintf("  -> %d trees", n_trees_start))
  }

  # Update best result
  distance <- min(abs(n_trees_start - target_trees), abs(n_trees_start - max_trees))
  if (n_trees_start >= target_trees && n_trees_start <= max_trees) {
    distance <- 0
  }
  if (distance < best_distance) {
    best_result <- result_start
    best_distance <- distance
  }

  # Check if we hit target immediately
  if (n_trees_start >= target_trees && n_trees_start <= max_trees) {
    if (verbose) {
      message("  -> SUCCESS at starting point!")
    }
    result_start$iterations <- iterations
    result_start$converged <- TRUE
    return(result_start)
  }

  # Determine search direction based on starting point
  param_low <- search_range[1]
  param_high <- search_range[2]
  param_current <- param_start

  if (n_trees_start < target_trees) {
    # Too few trees - need more permissive parameter (search lower)
    if (verbose) {
      message("  -> Too few trees, searching downward (more permissive)")
    }
    param_high <- param_start
    param_current <- param_start / 2

    while (param_current >= param_low && iterations < max_iterations) {
      iterations <- iterations + 1
      if (verbose) {
        message(sprintf("Iteration %d: param = %.4f", iterations, param_current))
      }

      result <- try_fit(param_current)
      if (is.null(result)) {
        param_current <- param_current / 2
        next
      }

      n_trees <- result$n_trees
      if (verbose) {
        message(sprintf("  -> %d trees", n_trees))
      }

      # Update best result
      distance <- min(abs(n_trees - target_trees), abs(n_trees - max_trees))
      if (n_trees >= target_trees && n_trees <= max_trees) {
        distance <- 0
      }
      if (distance < best_distance) {
        best_result <- result
        best_distance <- distance
      }

      # Check if we hit target
      if (n_trees >= target_trees && n_trees <= max_trees) {
        if (verbose) {
          message("  -> SUCCESS!")
        }
        result$iterations <- iterations
        result$converged <- TRUE
        return(result)
      }

      # If still too few trees, continue downward
      if (n_trees < target_trees) {
        param_high <- param_current
        param_current <- param_current / 2
      } else {
        # Too many trees now, we've bracketed it
        param_low <- param_current
        break
      }
    }

  } else {
    # Too many trees - need more restrictive parameter (search higher)
    if (verbose) {
      message("  -> Too many trees, searching upward (more restrictive)")
    }
    param_low <- param_start
    param_current <- param_start * 2

    while (param_current <= param_high && iterations < max_iterations) {
      iterations <- iterations + 1
      if (verbose) {
        message(sprintf("Iteration %d: param = %.4f", iterations, param_current))
      }

      result <- try_fit(param_current)
      if (is.null(result)) {
        param_current <- param_current * 2
        next
      }

      n_trees <- result$n_trees
      if (verbose) {
        message(sprintf("  -> %d trees", n_trees))
      }

      # Update best result
      distance <- min(abs(n_trees - target_trees), abs(n_trees - max_trees))
      if (n_trees >= target_trees && n_trees <= max_trees) {
        distance <- 0
      }
      if (distance < best_distance) {
        best_result <- result
        best_distance <- distance
      }

      # Check if we hit target
      if (n_trees >= target_trees && n_trees <= max_trees) {
        if (verbose) {
          message("  -> SUCCESS!")
        }
        result$iterations <- iterations
        result$converged <- TRUE
        return(result)
      }

      # If still too many trees, continue upward
      if (n_trees > max_trees) {
        param_low <- param_current
        param_current <- param_current * 2
      } else {
        # Too few trees now, we've bracketed it
        param_high <- param_current
        break
      }
    }
  }

  # Phase 2: Binary Refinement
  if ((param_high - param_low) > 0.01 && iterations < max_iterations) {
    if (verbose) {
      message(sprintf("\n--- Phase 2: Binary Refinement [%.4f, %.4f] ---", param_low, param_high))
    }

    while ((param_high - param_low) > 0.001 && iterations < max_iterations) {
      iterations <- iterations + 1
      param_mid <- (param_low + param_high) / 2

      if (verbose) {
        message(sprintf("Binary iteration %d: param = %.4f", iterations, param_mid))
      }

      result <- try_fit(param_mid)
      if (is.null(result)) {
        # If fit fails, narrow the range slightly
        param_high <- param_mid
        next
      }

      n_trees <- result$n_trees
      if (verbose) {
        message(sprintf("  -> %d trees", n_trees))
      }

      # Update best result
      distance <- min(abs(n_trees - target_trees), abs(n_trees - max_trees))
      if (n_trees >= target_trees && n_trees <= max_trees) {
        distance <- 0
      }
      if (distance < best_distance) {
        best_result <- result
        best_distance <- distance
      }

      # Check if we hit target
      if (n_trees >= target_trees && n_trees <= max_trees) {
        if (verbose) {
          message("  -> SUCCESS!")
        }
        result$iterations <- iterations
        result$converged <- TRUE
        return(result)
      }

      # Adjust search range
      if (n_trees < target_trees) {
        # Need more trees - go lower (more permissive)
        param_high <- param_mid
      } else {
        # Too many trees - go higher (more restrictive)
        param_low <- param_mid
      }
    }

    if (verbose) {
      message(sprintf("Binary refinement converged at param ~ %.4f", param_mid))
    }
  }

  # Return best result found (may not have converged to target)
  if (!is.null(best_result)) {
    best_result$iterations <- iterations
    best_result$converged <- (best_distance == 0)
    if (verbose && !best_result$converged) {
      message(sprintf("\nNo exact match found. Best result: %d trees", best_result$n_trees))
    }
  }

  return(best_result)
}

