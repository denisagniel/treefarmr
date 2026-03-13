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
    message("Auto-tuning TreeFARMS parameters...\n")
    message(sprintf("Target trees: %d, Max trees: %d\n", target_trees, max_trees))
    message(sprintf("Fixed %s: %.3f\n", fixed_param, fixed_value))
    message(sprintf("Searching %s in range [%.3f, %.3f]\n",
                ifelse(fixed_param == "regularization", "rashomon_bound_multiplier", "regularization"),
                search_range[1], search_range[2]))
    message("Loss function:", loss_function, "\n\n")
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
  
  # Binary search to find optimal parameter
  low <- search_range[1]
  high <- search_range[2]
  iterations <- 0
  best_result <- NULL
  best_trees <- 0
  fit_with_csv <- get(".treefarms_fit_with_csv", envir = asNamespace("optimaltrees"))
  
  while (iterations < max_iterations && (high - low) > 0.001) {
    iterations <- iterations + 1
    
    # Try middle value
    mid <- (low + high) / 2
    
    # Set parameters
    if (fixed_param == "regularization") {
      regularization <- fixed_value
      rashomon_bound_multiplier <- mid
    } else {
      regularization <- mid
      rashomon_bound_multiplier <- fixed_value
    }
    
    if (verbose) {
      message(sprintf("Iteration %d: regularization=%.3f, rashomon_bound_multiplier=%.3f\n", 
                  iterations, regularization, rashomon_bound_multiplier))
    }
    
    config <- c(base_config, list(
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier
    ))
    
    # Train model using internal fit (reuses csv_string)
    tryCatch({
      model <- fit_with_csv(csv_string, config, X, y,
                           single_tree = FALSE, store_training_data = FALSE, compute_probabilities = FALSE,
                           discretization_metadata, X_original)
      n_trees <- model$n_trees
      
      if (verbose) {
        message(sprintf("  -> Found %d trees\n", n_trees))
      }
      
      # Check if this is acceptable
      if (n_trees >= target_trees && n_trees <= max_trees) {
        if (verbose) {
          message("  -> SUCCESS: Found acceptable number of trees!\n")
        }
        return(list(
          model = model,
          regularization = regularization,
          rashomon_bound_multiplier = rashomon_bound_multiplier,
          n_trees = n_trees,
          iterations = iterations,
          converged = TRUE
        ))
      }
      
      # Update best result if this is better
      if (n_trees > 0 && (best_result == NULL || abs(n_trees - target_trees) < abs(best_trees - target_trees))) {
        best_result <- list(
          model = model,
          regularization = regularization,
          rashomon_bound_multiplier = rashomon_bound_multiplier,
          n_trees = n_trees,
          iterations = iterations,
          converged = FALSE
        )
        best_trees <- n_trees
      }
      
      # Adjust search range
      if (n_trees < target_trees) {
        # Need more trees - adjust parameter to be more permissive
        if (fixed_param == "regularization") {
          # Lower rashomon_bound_multiplier for more trees
          high <- mid
        } else {
          # Lower regularization for more trees
          high <- mid
        }
      } else if (n_trees > max_trees) {
        # Too many trees - adjust parameter to be more restrictive
        if (fixed_param == "regularization") {
          # Higher rashomon_bound_multiplier for fewer trees
          low <- mid
        } else {
          # Higher regularization for fewer trees
          low <- mid
        }
      } else {
        # Found acceptable range, but not exact target
        # Accept this result
        if (verbose) {
          message(sprintf("  -> ACCEPTABLE: Found %d trees (target: %d)\n", n_trees, target_trees))
        }
        return(list(
          model = model,
          regularization = regularization,
          rashomon_bound_multiplier = rashomon_bound_multiplier,
          n_trees = n_trees,
          iterations = iterations,
          converged = TRUE
        ))
      }
      
    }, error = function(e) {
      if (verbose) {
        message(sprintf("  -> ERROR: %s\n", e$message))
      }
      # If error, try more restrictive parameters
      if (fixed_param == "regularization") {
        low <- mid
      } else {
        low <- mid
      }
    })
    
    # Check for convergence to prevent infinite loops
    if (abs(high - low) < 0.001) {
      if (verbose) {
        message("Search converged (range too small)\n")
      }
      break
    }
  }
  
  # If we didn't find exact target, return best result
  if (!is.null(best_result)) {
    if (verbose) {
      message(sprintf("\nDid not find exact target (%d trees), but found %d trees\n", 
                  target_trees, best_trees))
    }
    return(best_result)
  }
  
  # If no successful result, return error
  stop(sprintf("Could not find parameters to generate %d-%d trees after %d iterations", 
               target_trees, max_trees, max_iterations))
}

