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
#' @export
auto_tune_treefarms <- function(X, y, loss_function = "misclassification", 
                                target_trees = 1, max_trees = 5,
                                fixed_param = "regularization", fixed_value = 0.1,
                                search_range = NULL, max_iterations = 20,
                                verbose = FALSE, ...) {
  
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
    cat("Auto-tuning TreeFARMS parameters...\n")
    cat(sprintf("Target trees: %d, Max trees: %d\n", target_trees, max_trees))
    cat(sprintf("Fixed %s: %.3f\n", fixed_param, fixed_value))
    cat(sprintf("Searching %s in range [%.3f, %.3f]\n", 
                ifelse(fixed_param == "regularization", "rashomon_bound_multiplier", "regularization"),
                search_range[1], search_range[2]))
    cat("Loss function:", loss_function, "\n\n")
  }
  
  # Binary search to find optimal parameter
  low <- search_range[1]
  high <- search_range[2]
  iterations <- 0
  best_result <- NULL
  best_trees <- 0
  
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
      cat(sprintf("Iteration %d: regularization=%.3f, rashomon_bound_multiplier=%.3f\n", 
                  iterations, regularization, rashomon_bound_multiplier))
    }
    
    # Train model using Rcpp functions
    tryCatch({
      model <- treefarms(X, y, loss_function = loss_function,
                        regularization = regularization,
                        rashomon_bound_multiplier = rashomon_bound_multiplier,
                        verbose = FALSE, ...)
      n_trees <- model$n_trees
      
      if (verbose) {
        cat(sprintf("  -> Found %d trees\n", n_trees))
      }
      
      # Check if this is acceptable
      if (n_trees >= target_trees && n_trees <= max_trees) {
        if (verbose) {
          cat("  -> SUCCESS: Found acceptable number of trees!\n")
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
          cat(sprintf("  -> ACCEPTABLE: Found %d trees (target: %d)\n", n_trees, target_trees))
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
        cat(sprintf("  -> ERROR: %s\n", e$message))
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
        cat("Search converged (range too small)\n")
      }
      break
    }
  }
  
  # If we didn't find exact target, return best result
  if (!is.null(best_result)) {
    if (verbose) {
      cat(sprintf("\nDid not find exact target (%d trees), but found %d trees\n", 
                  target_trees, best_trees))
    }
    return(best_result)
  }
  
  # If no successful result, return error
  stop(sprintf("Could not find parameters to generate %d-%d trees after %d iterations", 
               target_trees, max_trees, max_iterations))
}

