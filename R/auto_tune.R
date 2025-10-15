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

#' Subprocess-based TreeFARMS training (for both loss functions)
#'
#' @description
#' Train TreeFARMS models using subprocess approach for better reliability
#'
#' @param X A data.frame or matrix of features
#' @param y A vector of binary class labels
#' @param loss_function Character string: "misclassification" or "log_loss"
#' @param regularization Numeric: regularization parameter
#' @param rashomon_bound_multiplier Numeric: rashomon bound multiplier
#' @param verbose Logical: whether to print progress
#' @param python_path Path to Python executable
#' @param ... Additional parameters
#'
#' @return A trained TreeFARMS model object
#'
#' @export
treefarms_subprocess <- function(X, y, loss_function = "misclassification",
                                regularization = 0.1, rashomon_bound_multiplier = 0.05,
                                verbose = FALSE, python_path = "~/gosdt-env/bin/python", ...) {
  
  # Input validation (same as treefarms_logloss)
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X must be a data.frame or matrix")
  }
  
  if (!is.numeric(y) && !is.logical(y)) {
    stop("y must be numeric or logical")
  }
  
  if (length(y) != nrow(X)) {
    stop("Length of y must match number of rows in X")
  }
  
  if (!all(y %in% c(0, 1))) {
    stop("y must contain only binary values (0 and 1)")
  }
  
  # Convert to data.frame if matrix
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }
  
  # Check for binary features
  for (col in names(X)) {
    if (!all(X[[col]] %in% c(0, 1))) {
      stop(paste("Feature", col, "must contain only binary values (0 and 1)"))
    }
  }
  
  # Convert y to numeric if logical
  if (is.logical(y)) {
    y <- as.numeric(y)
  }
  
  # Create temporary files
  X_file <- "temp_X.csv"
  y_file <- "temp_y.csv"
  result_file <- "temp_result.json"
  script_file <- "temp_train_subprocess.py"
  
  # Write data to files
  write.csv(X, X_file, row.names = FALSE)
  write.csv(data.frame(y = y), y_file, row.names = FALSE)
  
  # Create Python script
  python_script <- sprintf('
import pandas as pd
import numpy as np
import json
import os
import sys
sys.path.insert(0, "/Users/dagniel/Library/CloudStorage/OneDrive-RANDCorporation/rrr/treeFarms")

from treefarms.model.treefarms import TREEFARMS

# Read data
X = pd.read_csv("%s")
y = pd.read_csv("%s")["y"]
y = pd.Series(y, name="class")

# Create configuration
config = {
    "loss_function": "%s",
    "regularization": %f,
    "rashomon": True,
    "rashomon_bound_multiplier": %f,
    "verbose": %s
}

# Train model
model = TREEFARMS(config)
model.fit(X, y)

# Get results
n_trees = model.model_set.get_tree_count()
print(f"Number of trees: {n_trees}")

if n_trees > 0:
    predictions = model.predict(X)
    probabilities = model.predict_proba(X)
    
    # Calculate accuracy
    accuracy = (predictions == y).mean()
    
    # Get tree information
    trees = []
    for i in range(n_trees):
        tree = model.model_set[i]
        trees.append({
            "json": tree.json(),
            "features": list(tree.features())
        })
else:
    print("No trees in model set")
    predictions = [0] * len(X)
    probabilities = [[0.5, 0.5]] * len(X)
    accuracy = 0.0
    trees = []

# Save results
result = {
    "n_trees": n_trees,
    "accuracy": accuracy,
    "predictions": predictions if isinstance(predictions, list) else predictions.tolist(),
    "probabilities": probabilities if isinstance(probabilities, list) else probabilities.tolist(),
    "trees": trees,
    "config": config
}

with open("%s", "w") as f:
    json.dump(result, f, indent=2)

print("Training completed successfully")
', X_file, y_file, loss_function, regularization, rashomon_bound_multiplier, 
    if(verbose) "True" else "False", result_file)
  
  writeLines(python_script, script_file)
  
  # Run Python script
  cmd <- sprintf('%s "%s"', python_path, script_file)
  
  if (verbose) {
    cat("Running TreeFARMS with subprocess...\n")
    cat("Command:", cmd, "\n")
  }
  
  # Use system() instead of reticulate to avoid segfault
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  
  # Check for errors
  if (length(result) > 0) {
    error_lines <- result[grepl("error|Error|ERROR|Traceback|Exception", result, ignore.case = TRUE)]
    if (length(error_lines) > 0) {
      stop("Error in Python script: ", paste(error_lines, collapse = "\n"))
    }
  }
  
  # Read results
  if (!file.exists(result_file)) {
    stop("Result file not created. Python script may have failed.")
  }
  
  result_data <- jsonlite::fromJSON(result_file)
  
  # Clean up temporary files
  unlink(c(X_file, y_file, result_file, script_file))
  
  # Create result object
  result_obj <- list(
    n_trees = result_data$n_trees,
    accuracy = result_data$accuracy,
    predictions = result_data$predictions,
    probabilities = result_data$probabilities,
    trees = result_data$trees,
    config = result_data$config,
    X_train = X,
    y_train = y,
    loss_function = loss_function,
    regularization = regularization,
    rashomon_bound_multiplier = rashomon_bound_multiplier
  )
  
  class(result_obj) <- "treefarms_subprocess_model"
  
  return(result_obj)
}
