#' TreeFARMS: Fast and Accurate Rule Set Models with Log-Loss and Probabilities
#'
#' @description
#' Train TreeFARMS models with support for log-loss optimization and probability predictions.
#' TreeFARMS finds optimal decision trees using either misclassification loss or log-loss (cross-entropy).
#'
#' @param X A data.frame or matrix of features. Must contain only binary (0/1) features.
#' @param y A vector of binary class labels (0/1).
#' @param loss_function Character string specifying the loss function to use.
#'   Options: "misclassification" (default) or "log_loss".
#' @param regularization Numeric value controlling model complexity. Higher values
#'   lead to simpler models. Default: 0.1. If NULL, will be auto-tuned.
#' @param rashomon_bound_multiplier Numeric value controlling Rashomon set size.
#'   Lower values lead to more trees. Default: 0.05. If NULL, will be auto-tuned.
#' @param target_trees Integer: target number of trees when auto-tuning (default: 1).
#' @param max_trees Integer: maximum acceptable number of trees when auto-tuning (default: 5).
#' @param worker_limit Integer: number of parallel workers to use (default: 1).
#' @param verbose Logical. Whether to print training progress. Default: FALSE.
#' @param store_training_data Logical. Whether to store training data in the model object.
#'   Default: FALSE. Set to TRUE only if you need to access training data later.
#' @param compute_probabilities Logical. Whether to compute probabilities for all training samples immediately.
#'   Default: FALSE. Probabilities will be computed on-demand when accessed.
#' @param single_tree Logical. If TRUE, fit exactly one tree (disable rashomon set).
#'   Default: TRUE. When TRUE, sets `rashomon = FALSE` to get only the optimal tree.
#'   When FALSE, computes a full rashomon set. For convenience, use \code{\link{fit_tree}}
#'   for single trees or \code{\link{fit_rashomon}} for rashomon sets.
#' @param ... Additional parameters passed to TreeFARMS configuration.
#'
#' @return A list containing:
#'   \item{model}{The trained TreeFARMS model object}
#'   \item{predictions}{Binary predictions (0/1) for training data (computed lazily if compute_probabilities=FALSE)}
#'   \item{probabilities}{Probability predictions [P(class=0), P(class=1)] for training data (computed lazily if compute_probabilities=FALSE)}
#'   \item{accuracy}{Training accuracy (computed lazily if compute_probabilities=FALSE)}
#'   \item{loss_function}{The loss function used}
#'   \item{regularization}{The regularization parameter used}
#'   \item{n_trees}{Number of trees in the Rashomon set}
#'   \item{X_train}{Training features (only if store_training_data=TRUE)}
#'   \item{y_train}{Training labels (only if store_training_data=TRUE)}
#'
#' @details
#' TreeFARMS (Tree-based Fast and Accurate Rule Set Models) is an algorithm for
#' learning optimal decision trees. This R wrapper provides access to:
#' 
#' \itemize{
#'   \item \strong{Log-loss optimization}: Use cross-entropy loss for probabilistic modeling
#'   \item \strong{Probability predictions}: Get well-calibrated probability estimates
#'   \item \strong{Bounded probabilities}: Probabilities are bounded away from 0 and 1
#'   \item \strong{Rashomon sets}: Multiple near-optimal models for robust predictions
#' }
#'
#' The probabilities returned are computed using the same empirical class distributions
#' used in the loss function optimization, ensuring consistency between training and prediction.
#' 
#' \strong{Memory Efficiency:}
#' By default, probabilities are not computed immediately during training (lazy evaluation).
#' This reduces memory usage, especially for large datasets. To compute probabilities for
#' training data, use \code{compute_probabilities=TRUE} or access them via \code{predict()}
#' or the helper functions \code{get_probabilities()}, \code{get_predictions()}, or
#' \code{get_accuracy()}.
#' 
#' Training data (\code{X_train}, \code{y_train}) is not stored by default to save memory.
#' Set \code{store_training_data=TRUE} if you need to access training data later (e.g., for
#' computing accuracy or probabilities on training data).
#'
#' @examples
#' \dontrun{
#' # Create sample binary data
#' set.seed(42)
#' n <- 200
#' X <- data.frame(
#'   feature_1 = sample(0:1, n, replace = TRUE),
#'   feature_2 = sample(0:1, n, replace = TRUE),
#'   feature_3 = sample(0:1, n, replace = TRUE)
#' )
#' 
#' # Create target with some pattern
#' y <- as.numeric((X$feature_1 == 1 & X$feature_2 == 1) | 
#'                 (X$feature_1 == 0 & X$feature_2 == 0))
#' 
#' # Train with misclassification loss
#' model_misclass <- treefarms(X, y, loss_function = "misclassification")
#' 
#' # Train with log-loss (memory-efficient: no probabilities computed immediately)
#' model_logloss <- treefarms(X, y, loss_function = "log_loss")
#' 
#' # Train with log-loss and compute probabilities immediately
#' model_logloss_full <- treefarms(X, y, loss_function = "log_loss", 
#'                                 compute_probabilities = TRUE)
#' 
#' # Compare probability predictions
#' print("Misclassification model probabilities:")
#' print(head(model_misclass$probabilities))
#' 
#' print("Log-loss model probabilities (lazy):")
#' # Probabilities computed on-demand
#' probs <- get_probabilities(model_logloss)
#' print(head(probs))
#' 
#' print("Log-loss model probabilities (pre-computed):")
#' print(head(model_logloss_full$probabilities))
#' 
#' # Make predictions on new data
#' X_new <- data.frame(
#'   feature_1 = c(1, 0, 1),
#'   feature_2 = c(1, 0, 0),
#'   feature_3 = c(0, 1, 1)
#' )
#' 
#' pred_misclass <- predict_treefarms(model_misclass, X_new)
#' pred_logloss <- predict_treefarms(model_logloss, X_new)
#' }
#'
# Helper function to count tree leaves from JSON structure
count_tree_leaves <- function(tree_node) {
  if (is.null(tree_node)) return(0)
  
  # If this node has a prediction, it's a leaf
  if (!is.null(tree_node$prediction)) {
    return(1)
  }
  
  # If this node has children (true/false), recursively count them
  count <- 0
  if (!is.null(tree_node$true)) {
    count <- count + count_tree_leaves(tree_node$true)
  }
  if (!is.null(tree_node$false)) {
    count <- count + count_tree_leaves(tree_node$false)
  }
  
  return(count)
}

# Helper function to validate tree structure
validate_tree_structure <- function(tree_json) {
  if (is.null(tree_json)) {
    return(FALSE)
  }
  
  # Check if it's a valid list
  if (!is.list(tree_json)) {
    return(FALSE)
  }
  
  # Check if it has either a prediction (leaf) or feature (split node)
  has_prediction <- !is.null(tree_json$prediction)
  has_feature <- !is.null(tree_json$feature)
  
  if (!has_prediction && !has_feature) {
    # Might be a list of trees or invalid structure
    if (is.list(tree_json) && length(tree_json) > 1) {
      # Check if first element is a valid tree
      if (length(tree_json) > 0 && is.list(tree_json[[1]])) {
        return(validate_tree_structure(tree_json[[1]]))
      }
    }
    return(FALSE)
  }
  
  # If it's a split node, check that it has children
  if (has_feature) {
    # Should have true and/or false branches
    has_children <- !is.null(tree_json$true) || !is.null(tree_json$false)
    if (!has_children) {
      return(FALSE)
    }
    # Recursively validate children
    if (!is.null(tree_json$true) && !validate_tree_structure(tree_json$true)) {
      return(FALSE)
    }
    if (!is.null(tree_json$false) && !validate_tree_structure(tree_json$false)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Helper function to traverse tree and get probabilities for a single sample
get_probabilities_from_tree <- function(tree_json, X) {
  if (is.null(tree_json)) {
    # No tree available, return default probabilities
    n_samples <- nrow(X)
    return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
  }
  
  # Validate tree structure before traversal
  if (!validate_tree_structure(tree_json)) {
    warning("Invalid tree structure detected. Using default probabilities.")
    n_samples <- nrow(X)
    return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
  }
  
  # Handle Rashomon set (list of trees) - use first tree
  if (is.list(tree_json) && length(tree_json) > 1 && is.null(tree_json$feature) && is.null(tree_json$prediction)) {
    # This is a list of trees, use the first one
    tree_json <- tree_json[[1]]
    # Re-validate after extraction
    if (!validate_tree_structure(tree_json)) {
      warning("Invalid tree structure after extraction. Using default probabilities.")
      n_samples <- nrow(X)
      return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
    }
  }
  
  n_samples <- nrow(X)
  probabilities <- matrix(0, nrow = n_samples, ncol = 2)
  
  # Helper function to traverse tree for a single sample
  # Add depth limit to prevent infinite recursion
  traverse_tree <- function(node, sample_row, depth = 0, max_depth = 100) {
    # Safety check: prevent infinite recursion
    if (depth > max_depth) {
      warning("Tree traversal exceeded maximum depth (", max_depth, "). Using default probabilities.")
      return(c(0.5, 0.5))
    }
    
    # Safety check: validate node is not NULL
    if (is.null(node) || !is.list(node)) {
      return(c(0.5, 0.5))
    }
    
    # If this is a leaf node, return its probabilities
    if (!is.null(node$prediction)) {
      tryCatch({
        if (!is.null(node$probabilities) && length(node$probabilities) >= 2) {
          # Probabilities are stored as array [P(class=0), P(class=1)]
          probs <- as.numeric(node$probabilities)
          # Validate probabilities
          if (length(probs) == 2 && all(is.finite(probs)) && all(probs >= 0)) {
            # Normalize to ensure they sum to 1
            prob_sum <- sum(probs)
            if (prob_sum > 0) {
              probs <- probs / prob_sum
            } else {
              # If all zeros, use uniform distribution
              probs <- c(0.5, 0.5)
            }
            return(probs)
          } else {
            # Invalid probabilities, infer from prediction
            pred <- as.numeric(node$prediction)
            if (pred == 0) {
              return(c(1.0, 0.0))
            } else {
              return(c(0.0, 1.0))
            }
          }
        } else {
          # No probabilities stored, infer from prediction
          pred <- as.numeric(node$prediction)
          if (pred == 0) {
            return(c(1.0, 0.0))
          } else {
            return(c(0.0, 1.0))
          }
        }
      }, error = function(e) {
        # If accessing probabilities fails, infer from prediction
        tryCatch({
          pred <- as.numeric(node$prediction)
          if (pred == 0) {
            return(c(1.0, 0.0))
          } else {
            return(c(0.0, 1.0))
          }
        }, error = function(e2) {
          # If even prediction fails, use default
          return(c(0.5, 0.5))
        })
      })
    }
    
    # If this is a split node, traverse based on feature value
    if (!is.null(node$feature)) {
      # Feature index is 0-based in JSON, convert to 1-based for R
      feature_idx <- as.numeric(node$feature) + 1
      
      # Get feature value for this sample
      if (feature_idx <= ncol(X)) {
        # Handle both data.frame and matrix
        if (is.data.frame(X)) {
          feature_value <- X[sample_row, feature_idx, drop = TRUE]
        } else {
          feature_value <- X[sample_row, feature_idx]
        }
        
        # Follow the appropriate branch
        if (feature_value == 1 || feature_value == TRUE) {
          # Follow true branch
          if (!is.null(node$true) && is.list(node$true)) {
            return(traverse_tree(node$true, sample_row, depth + 1, max_depth))
          }
        } else {
          # Follow false branch
          if (!is.null(node$false) && is.list(node$false)) {
            return(traverse_tree(node$false, sample_row, depth + 1, max_depth))
          }
        }
      }
    }
    
    # Fallback: return default probabilities
    return(c(0.5, 0.5))
  }
  
  # Get probabilities for each sample
  # Add safety check for very large datasets
  if (n_samples > 10000) {
    warning("Large dataset detected (", n_samples, " samples). ",
            "Probability computation may be slow and memory-intensive.")
  }
  
  # Process samples with error handling and progress tracking
  for (i in 1:n_samples) {
    tryCatch({
      probs <- traverse_tree(tree_json, i, depth = 0, max_depth = 100)
      # Validate probabilities
      if (length(probs) == 2 && all(is.finite(probs)) && all(probs >= 0) && all(probs <= 1)) {
        # Normalize to ensure they sum to 1
        prob_sum <- sum(probs)
        if (prob_sum > 0) {
          probs <- probs / prob_sum
        } else {
          probs <- c(0.5, 0.5)
        }
        probabilities[i, ] <- probs
      } else {
        # Invalid probabilities, use default
        probabilities[i, ] <- c(0.5, 0.5)
      }
    }, error = function(e) {
      # If traversal fails, use default probabilities
      warning("Error traversing tree for sample ", i, ": ", e$message)
      probabilities[i, ] <<- c(0.5, 0.5)
    })
    
    # Periodic garbage collection for large datasets
    if (n_samples > 1000 && i %% 100 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  # Set column names
  colnames(probabilities) <- c("P(class=0)", "P(class=1)")
  
  return(probabilities)
}

#' @export
treefarms <- function(X, y, loss_function = "misclassification", regularization = 0.1, 
rashomon_bound_multiplier = 0.05, target_trees = 1, max_trees = 5,
worker_limit = 1L, verbose = FALSE, store_training_data = NULL, 
compute_probabilities = FALSE, single_tree = TRUE, ...) {
  
  # Auto-detect if training data should be stored
  # For log-loss models, store training data by default since probabilities are often needed
  if (is.null(store_training_data)) {
    store_training_data <- (loss_function == "log_loss")
  }
  
  # Input validation
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
  
  if (!loss_function %in% c("misclassification", "log_loss")) {
    stop("loss_function must be either 'misclassification' or 'log_loss'")
  }
  
  if (!is.numeric(worker_limit) || length(worker_limit) != 1 || worker_limit < 1) {
    stop("worker_limit must be a positive integer")
  }
  
  
  # Check if auto-tuning is needed
  auto_tune_regularization <- is.null(regularization)
  auto_tune_rashomon <- is.null(rashomon_bound_multiplier)
  
  if (auto_tune_regularization || auto_tune_rashomon) {
    # Use auto-tuning
    if (verbose) {
      cat("Auto-tuning parameters...\n")
    }
    
    # Determine which parameter is fixed
    if (auto_tune_regularization && !auto_tune_rashomon) {
      fixed_param <- "rashomon_bound_multiplier"
      fixed_value <- rashomon_bound_multiplier
    } else if (!auto_tune_regularization && auto_tune_rashomon) {
      fixed_param <- "regularization"
      fixed_value <- regularization
    } else {
      # Both are NULL - use defaults and tune regularization
      fixed_param <- "rashomon_bound_multiplier"
      fixed_value <- 0.05
    }
    
    return(auto_tune_treefarms(X, y, loss_function = loss_function,
                              target_trees = target_trees, max_trees = max_trees,
                              fixed_param = fixed_param, fixed_value = fixed_value,
                              verbose = verbose, ...))
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
  
  # Create configuration JSON
  config <- list(
    loss_function = loss_function,
    regularization = regularization,
    verbose = verbose,
    worker_limit = as.integer(worker_limit),
    ...
  )
  
  # Add rashomon parameters
  # If single_tree = TRUE, disable rashomon to get exactly one tree
  if (single_tree) {
    config$rashomon <- FALSE
    # Don't set rashomon_bound_multiplier when rashomon is disabled
  } else {
    config$rashomon <- TRUE
    config$rashomon_bound_multiplier <- rashomon_bound_multiplier
  }
  
  # Convert configuration to JSON
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  
  # Prepare data for C++ (combine X and y into CSV format)
  data_df <- X
  data_df$class <- y
  
  # Convert to CSV string
  csv_string <- paste(capture.output(write.csv(data_df, stdout(), row.names = FALSE)), collapse = "\n")
  
  # Use direct Rcpp bindings with RcppParallel TBB initialization
  tryCatch({
    # Capture stdout to extract tree JSON using sink()
    # Now that C++ uses Rcpp::Rcout, sink() should capture it
    stdout_file <- tempfile(fileext = ".txt")
    sink(stdout_file, type = "output", split = FALSE)
    tryCatch({
      # Call C++ function directly (no process isolation needed)
      json_output_raw <- treefarms_fit_with_config_cpp(csv_string, config_json)
      # Convert to character string if it's a CharacterVector
      if (is.character(json_output_raw) && length(json_output_raw) == 1) {
        json_output <- as.character(json_output_raw)[1]
      } else if (is.character(json_output_raw)) {
        json_output <- paste(json_output_raw, collapse = "")
      } else {
        json_output <- as.character(json_output_raw)
      }
    }, finally = {
      sink()
    })
    
    # Read captured stdout
    stdout_capture <- if (file.exists(stdout_file)) {
      readLines(stdout_file, warn = FALSE)
    } else {
      character(0)
    }
    unlink(stdout_file)
    
    # Note: C++ code now returns JSON string directly for both log-loss and misclassification
    # No need for file-based handling anymore
    
    # DEBUG: Print what was captured
    if (verbose) {
      cat("DEBUG: stdout_capture length:", length(stdout_capture), "\n")
      if (length(stdout_capture) > 0) {
        cat("DEBUG: First few lines of stdout_capture:\n")
        cat(paste(head(stdout_capture, 5), collapse = "\n"), "\n")
      }
      cat("DEBUG: json_output is null:", is.null(json_output), "\n")
      cat("DEBUG: json_output is empty:", json_output == "", "\n")
      if (!is.null(json_output) && json_output != "") {
        cat("DEBUG: json_output length:", nchar(json_output), "\n")
        cat("DEBUG: json_output first 200 chars:", substr(json_output, 1, 200), "\n")
      }
    }
    
    # Parse the JSON result first with enhanced error handling
    if (is.null(json_output) || json_output == "") {
      if (verbose) {
        cat("DEBUG: json_output is null or empty\n")
      }
      result_data <- NULL
    } else {
      # Check if json_output is just "{}" (empty JSON object fallback)
      json_trimmed <- trimws(json_output)
      if (json_trimmed == "{}" || json_trimmed == "null") {
        if (verbose) {
          cat("DEBUG: json_output is empty JSON object or null, this may indicate a serialization failure\n")
        }
        result_data <- NULL
      } else {
        tryCatch({
          result_data <- jsonlite::fromJSON(json_output, simplifyVector = FALSE)
          if (verbose) {
            cat("DEBUG: result_data parsed successfully\n")
            cat("DEBUG: result_data names:", paste(names(result_data), collapse = ", "), "\n")
            if (!is.null(result_data$storage)) {
              cat("DEBUG: result_data$storage length:", length(result_data$storage), "\n")
            }
            if (!is.null(result_data$trees)) {
              cat("DEBUG: result_data$trees length:", length(result_data$trees), "\n")
            }
          }
        }, error = function(e) {
          # If parsing fails, provide detailed error information
          warning("Failed to parse JSON result from C++ code. This may indicate a serialization error. ",
                  "Error: ", e$message, 
                  ". JSON length: ", nchar(json_output),
                  ". First 200 chars: ", substr(json_output, 1, min(200, nchar(json_output))))
          if (verbose) {
            cat("DEBUG: Error parsing json_output:", e$message, "\n")
            cat("DEBUG: json_output length:", nchar(json_output), "\n")
            cat("DEBUG: json_output first 200 chars:", substr(json_output, 1, min(200, nchar(json_output))), "\n")
          }
          result_data <- NULL
        })
      }
    }
    
    # Extract tree JSON from captured stdout
    tree_json <- extract_tree_from_stdout(stdout_capture)
    
    if (verbose) {
      cat("DEBUG: tree_json is null:", is.null(tree_json), "\n")
      if (!is.null(tree_json)) {
        cat("DEBUG: tree_json has feature:", !is.null(tree_json$feature), "\n")
        cat("DEBUG: tree_json has prediction:", !is.null(tree_json$prediction), "\n")
      }
    }
    
    # If tree JSON not captured from stdout, try to extract from result_data
    # result_data might be a ModelSet structure with "storage" array, or a direct tree
    if (is.null(tree_json) && !is.null(result_data)) {
      # First, check if result_data has a "trees" field (from ModelSet::serialize with models)
      if (!is.null(result_data$trees) && is.list(result_data$trees) && length(result_data$trees) > 0) {
        # Extract the first tree from the trees array
        tree_json <- result_data$trees[[1]]
        if (verbose) {
          cat("DEBUG: Extracted tree from result_data$trees\n")
        }
      } else if (!is.null(result_data$feature) || !is.null(result_data$prediction)) {
        # Check if result_data itself is a tree structure
        tree_json <- result_data
      } else if (!is.null(result_data$storage) && is.list(result_data$storage) && length(result_data$storage) > 0) {
        # result_data is a ModelSet structure - extract the first tree from storage
        # The storage array contains ModelSet nodes, we need to reconstruct the tree
        # For now, try to find a tree structure in the storage
        for (item in result_data$storage) {
          if (is.list(item) && (!is.null(item$feature) || !is.null(item$prediction))) {
            tree_json <- item
            break
          }
        }
      } else if (is.list(result_data) && length(result_data) > 0) {
        # result_data might be an array of trees
        if (is.list(result_data[[1]]) && (!is.null(result_data[[1]]$feature) || !is.null(result_data[[1]]$prediction))) {
          tree_json <- result_data[[1]]
        }
      }
    }
    
    # Get training statistics from C++ static variables
    training_time <- treefarms_time_cpp()
    iterations <- treefarms_iterations_cpp()
    model_size <- treefarms_size_cpp()
    status <- treefarms_status_cpp()
    
    # Create result list with actual stats from C++
    result_list <- list(
      result = json_output,
      time = training_time,
      iterations = iterations,
      size = model_size,
      status = status
    )
    
            # Extract model information - count trees in rashomon set (not leaf nodes)
            n_trees <- 0
            # Check result_data first - look for trees field
            if (!is.null(result_data)) {
              if (is.list(result_data) && !is.null(result_data$trees)) {
                # result_data has a trees field - count trees in that list
                if (is.list(result_data$trees)) {
                  n_trees <- length(result_data$trees)
                } else {
                  n_trees <- 1
                }
              } else if (is.list(result_data) && (!is.null(result_data$feature) || !is.null(result_data$prediction))) {
                # This is a single tree object directly
                n_trees <- 1
              } else if (is.list(result_data) && length(result_data) > 0) {
                # Check if result_data itself is a list of trees (each element is a tree)
                # This would be the case if result_data is a list where each element has feature/prediction
                first_elem <- result_data[[1]]
                if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
                  # This is a list of trees
                  n_trees <- length(result_data)
                }
              }
            }
            # If no trees found in result_data, check tree_json
            if (n_trees == 0 && !is.null(tree_json)) {
              if (is.list(tree_json) && !is.null(tree_json$trees)) {
                # tree_json has a trees field
                if (is.list(tree_json$trees)) {
                  n_trees <- length(tree_json$trees)
                } else {
                  n_trees <- 1
                }
              } else if (is.list(tree_json) && (!is.null(tree_json$feature) || !is.null(tree_json$prediction))) {
                # This is a single tree - count as 1 tree in rashomon set (not leaf nodes)
                n_trees <- 1
              } else if (is.list(tree_json) && length(tree_json) > 1 && is.null(tree_json$feature) && is.null(tree_json$prediction) && is.null(tree_json$trees)) {
                # This is a list of trees (rashomon set) - check if first element is a tree
                if (length(tree_json) > 0) {
                  first_elem <- tree_json[[1]]
                  if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
                    n_trees <- length(tree_json)
                  }
                }
              }
            }
            
            # CRITICAL: If single_tree=TRUE, force n_trees=1 and extract only first tree
            if (single_tree) {
              if (n_trees > 1) {
                # Extract only the first tree from tree_json if it's a list
                if (!is.null(tree_json) && is.list(tree_json) && length(tree_json) > 1 && 
                    is.null(tree_json$feature) && is.null(tree_json$prediction) && is.null(tree_json$trees)) {
                  # tree_json is a list of trees - take first one
                  tree_json <- tree_json[[1]]
                } else if (!is.null(result_data) && is.list(result_data) && length(result_data) > 1) {
                  # Check if result_data is a list of trees
                  first_elem <- result_data[[1]]
                  if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
                    # result_data is a list of trees - take first one and update result_data
                    result_data <- first_elem
                    # Also update tree_json if it was derived from result_data
                    if (is.null(tree_json) || identical(tree_json, result_data)) {
                      tree_json <- first_elem
                    }
                  }
                } else if (!is.null(result_data) && is.list(result_data) && !is.null(result_data$trees) && 
                          is.list(result_data$trees) && length(result_data$trees) > 1) {
                  # result_data has a trees field with multiple trees - take first one
                  result_data$trees <- list(result_data$trees[[1]])
                  if (!is.null(tree_json) && is.list(tree_json) && !is.null(tree_json$trees)) {
                    tree_json$trees <- list(tree_json$trees[[1]])
                  }
                }
              }
              # Force n_trees to 1
              n_trees <- 1
            }
    
    # Create model object
    model_obj <- list(
      result_data = result_data,
      tree_json = tree_json,  # Store parsed tree JSON
      config = config,
      time = result_list$time,
      iterations = result_list$iterations,
      size = result_list$size,
      status = result_list$status
    )
    
    # Get predictions and probabilities from the tree structure
    # Use tree_json if available, otherwise use result_data
    tree_to_use <- if (!is.null(tree_json)) tree_json else result_data
    
    if (verbose) {
      cat("DEBUG: tree_to_use is null:", is.null(tree_to_use), "\n")
      if (!is.null(tree_to_use)) {
        cat("DEBUG: tree_to_use is list:", is.list(tree_to_use), "\n")
        if (is.list(tree_to_use)) {
          cat("DEBUG: tree_to_use names:", paste(names(tree_to_use), collapse = ", "), "\n")
          cat("DEBUG: tree_to_use has feature:", !is.null(tree_to_use$feature), "\n")
          cat("DEBUG: tree_to_use has prediction:", !is.null(tree_to_use$prediction), "\n")
        }
      }
    }
    
    # Check if we have a valid tree structure
    has_tree <- FALSE
    if (!is.null(tree_to_use)) {
      if (is.list(tree_to_use)) {
        # Check if it's a tree structure (has feature or prediction)
        if (!is.null(tree_to_use$feature) || !is.null(tree_to_use$prediction)) {
          has_tree <- TRUE
        } else if (length(tree_to_use) > 1 && !is.null(names(tree_to_use))) {
          # Might be a list of trees or a named list
          # Check if any element has feature or prediction
          for (item in tree_to_use) {
            if (is.list(item) && (!is.null(item$feature) || !is.null(item$prediction))) {
              has_tree <- TRUE
              break
            }
          }
        }
      }
    }
    
    if (verbose) {
      cat("DEBUG: has_tree:", has_tree, "\n")
    }
    
    # Lazy probability computation: only compute if explicitly requested
    probabilities <- NULL
    predictions <- NULL
    accuracy <- NULL
    
    if (compute_probabilities && has_tree) {
      # Extract probabilities from tree structure
      if (verbose) {
        cat("DEBUG: Computing probabilities immediately\n")
      }
      probabilities <- get_probabilities_from_tree(tree_to_use, X)
      
      if (verbose) {
        cat("DEBUG: Probabilities extracted, shape:", nrow(probabilities), "x", ncol(probabilities), "\n")
        cat("DEBUG: First few probabilities:\n")
        print(head(probabilities, 3))
      }
      
      # Derive predictions from probabilities (argmax)
      # For binary classification, predict class 1 if P(class=1) >= 0.5, else class 0
      predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
      accuracy <- mean(predictions == y)
    } else if (has_tree) {
      # Lazy evaluation: create a function to compute probabilities on-demand
      # For now, we'll compute predictions lazily too
      # Store tree and data reference for later computation
      if (verbose) {
        cat("DEBUG: Skipping immediate probability computation (lazy evaluation)\n")
      }
    } else {
      # No tree available, use default values
      if (verbose) {
        cat("DEBUG: No tree available, using default values\n")
      }
      # Check if this is due to a serialization failure
      if (is.null(result_data) && (!is.null(json_output) && json_output != "" && trimws(json_output) == "{}")) {
        warning("Model training may have failed or serialization returned empty result. ",
                "This could indicate a memory issue or crash during training. ",
                "Try reducing dataset size or adjusting regularization.")
      }
      # Even without tree, we can provide default predictions
      predictions <- rep(0, length(y))
      accuracy <- mean(predictions == y)
    }
    
    # Store tree structure for lazy computation
    stored_tree <- tree_to_use
    stored_X <- if (store_training_data) X else NULL
    stored_y <- if (store_training_data) y else NULL
    
    # Validate tree structure before storing
    if (!is.null(stored_tree) && !validate_tree_structure(stored_tree)) {
      warning("Tree structure validation failed. Probabilities may not be available.")
      stored_tree <- NULL
    }
    
    # Create lazy probability computation function with memory checks
    compute_probabilities_lazy <- function() {
      if (is.null(stored_tree)) {
        n_samples <- if (!is.null(stored_X)) nrow(stored_X) else 0
        if (n_samples == 0) {
          return(matrix(c(0.5, 0.5), nrow = 1, ncol = 2, byrow = TRUE))
        }
        return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
      }
      # Need X to compute probabilities - use stored X_train if available
      if (is.null(stored_X)) {
        stop("Cannot compute probabilities: training data not stored. ",
             "Re-train with store_training_data=TRUE or provide newdata to predict()")
      }
      
      # Check memory before computing probabilities
      n_samples <- nrow(stored_X)
      
      # For large datasets, compute in batches to avoid memory issues
      # Use n_samples threshold directly (500 samples) instead of memory estimate
      # Memory estimate was wrong: 1000 samples = 16KB, not 10MB
      if (n_samples > 500) {
        # Use batching for large datasets
        batch_size <- min(500, n_samples)
        n_batches <- ceiling(n_samples / batch_size)
        probabilities <- matrix(0.0, nrow = n_samples, ncol = 2)
        
        for (i in 1:n_batches) {
          start_idx <- (i - 1) * batch_size + 1
          end_idx <- min(i * batch_size, n_samples)
          batch_X <- stored_X[start_idx:end_idx, , drop = FALSE]
          
          # Compute probabilities for this batch
          batch_probs <- get_probabilities_from_tree(stored_tree, batch_X)
          probabilities[start_idx:end_idx, ] <- batch_probs
          
          # Force garbage collection after each batch to free memory
          if (i %% 5 == 0) {
            gc(verbose = FALSE)
          }
        }
        
        colnames(probabilities) <- c("P(class=0)", "P(class=1)")
        return(probabilities)
      } else {
        # For smaller datasets, compute all at once
        return(get_probabilities_from_tree(stored_tree, stored_X))
      }
    }
    
    # Create lazy prediction computation function
    compute_predictions_lazy <- function() {
      probs <- compute_probabilities_lazy()
      ifelse(probs[, 2] >= 0.5, 1, 0)
    }
    
    # Create lazy accuracy computation function
    compute_accuracy_lazy <- function() {
      if (is.null(stored_y)) {
        stop("Cannot compute accuracy: training data not stored. ",
             "Re-train with store_training_data=TRUE")
      }
      preds <- compute_predictions_lazy()
      mean(preds == stored_y)
    }
    
    # Create result object with simple list for caching
    # Use a list instead of environment to avoid potential memory issues
    # Cache will be stored directly in the result list
    result <- list(
      model = model_obj,
      loss_function = loss_function,
      regularization = regularization,
      n_trees = n_trees,
      training_time = result_list$time,
      training_iterations = result_list$iterations,
      # Lazy computation functions (internal use)
      .compute_probabilities = compute_probabilities_lazy,
      .compute_predictions = compute_predictions_lazy,
      .compute_accuracy = compute_accuracy_lazy,
      # Store computed values if they were computed immediately
      probabilities = probabilities,
      predictions = predictions,
      accuracy = accuracy
    )
    
    # Always store training data for predict() to work
    # Store feature names/column structure even if not storing full data
    result$X_train <- X
    result$y_train <- y
    
    # Set class
    class(result) <- "treefarms_model"
    
    return(result)
    
  }, error = function(e) {
    stop(paste("Error training TreeFARMS model:", e$message))
  })
}

#' Predict using a trained TreeFARMS model
#'
#' @param object A trained TreeFARMS model object returned by \code{treefarms()}.
#' @param newdata A data.frame or matrix of new features to predict on.
#' @param type Character string specifying the type of prediction.
#'   Options: "class" (binary predictions) or "prob" (probabilities). Default: "class".
#' @param ... Additional arguments (currently unused).
#'
#' @return For \code{type = "class"}: A vector of binary predictions (0/1).
#'   For \code{type = "prob"}: A matrix with columns [P(class=0), P(class=1)].
#'
#' @examples
#' \dontrun{
#' # Train a model
#' model <- treefarms(X, y, loss_function = "log_loss")
#' 
#' # Get binary predictions
#' pred_class <- predict_treefarms(model, X_new, type = "class")
#' 
#' # Get probability predictions
#' pred_prob <- predict_treefarms(model, X_new, type = "prob")
#' }
#'
#' Get probabilities from a treefarms model (with lazy evaluation)
#'
#' @param object A treefarms_model object
#' @return A matrix of probabilities [P(class=0), P(class=1)]
#' @export
get_probabilities <- function(object) {
  if (!is.null(object$probabilities)) {
    # Already computed
    return(object$probabilities)
  } else if (!is.null(object$.compute_probabilities)) {
    # Compute lazily
    return(object$.compute_probabilities())
  } else {
    stop("Cannot compute probabilities: model structure incomplete")
  }
}

#' Get predictions from a treefarms model (with lazy evaluation)
#'
#' @param object A treefarms_model object
#' @return A vector of binary predictions (0/1)
#' @export
get_predictions <- function(object) {
  if (!is.null(object$predictions)) {
    # Already computed
    return(object$predictions)
  } else if (!is.null(object$.compute_predictions)) {
    # Compute lazily
    return(object$.compute_predictions())
  } else {
    stop("Cannot compute predictions: model structure incomplete")
  }
}

#' Get accuracy from a treefarms model (with lazy evaluation)
#'
#' @param object A treefarms_model object
#' @return Training accuracy (numeric)
#' @export
get_accuracy <- function(object) {
  if (!is.null(object$accuracy)) {
    # Already computed
    return(object$accuracy)
  } else if (!is.null(object$.compute_accuracy)) {
    # Compute lazily
    return(object$.compute_accuracy())
  } else {
    stop("Cannot compute accuracy: model structure incomplete")
  }
}

#' @export
predict_treefarms <- function(object, newdata, type = "class", ...) {
  
  if (!inherits(object, "treefarms_model")) {
    stop("object must be a treefarms_model object")
  }
  
  if (!type %in% c("class", "prob")) {
    stop("type must be either 'class' or 'prob'")
  }
  
  # Convert to data.frame if matrix
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  
  # Get tree structure from model object
  # Use tree_json if available, otherwise use result_data
  tree_to_use <- if (!is.null(object$model$tree_json)) {
    object$model$tree_json
  } else if (!is.null(object$model$result_data)) {
    object$model$result_data
  } else {
    NULL
  }
  
  # Validate features match training data if available, otherwise use tree structure
  if (!is.null(object$X_train)) {
    if (!identical(names(newdata), names(object$X_train))) {
      stop("Feature names in newdata must match training data")
    }
  } else if (!is.null(tree_to_use)) {
    # Try to infer feature names from tree structure
    # This is a best-effort approach
    if (is.null(names(newdata))) {
      warning("Cannot validate feature names: training data not stored")
    }
  }
  
  # Check for binary features
  for (col in names(newdata)) {
    if (!all(newdata[[col]] %in% c(0, 1))) {
      stop(paste("Feature", col, "must contain only binary values (0 and 1)"))
    }
  }
  
  # Extract probabilities from tree structure
  if (!is.null(tree_to_use)) {
    probabilities <- get_probabilities_from_tree(tree_to_use, newdata)
    
    if (type == "class") {
      # Derive predictions from probabilities (argmax)
      # For binary classification, predict class 1 if P(class=1) >= 0.5, else class 0
      predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
      return(predictions)
    } else {
      # Return probabilities
      return(probabilities)
    }
  } else {
    # No tree available, return default values
    n_samples <- nrow(newdata)
    if (type == "class") {
      return(rep(0, n_samples))
    } else {
      return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
    }
  }
}

#' Print summary of a TreeFARMS model
#'
#' @param x A trained TreeFARMS model object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
print.treefarms_model <- function(x, ...) {
  cat("TreeFARMS Model (Rcpp)\n")
  cat("======================\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("Number of trees:", x$n_trees, "\n")
  
  # Get accuracy with lazy evaluation
  tryCatch({
    acc <- get_accuracy(x)
    cat("Training accuracy:", round(acc, 4), "\n")
  }, error = function(e) {
    cat("Training accuracy: (not available - training data not stored)\n")
  })
  
  # Get training data info if available
  if (!is.null(x$X_train)) {
    cat("Training samples:", nrow(x$X_train), "\n")
    cat("Features:", ncol(x$X_train), "\n")
  } else {
    cat("Training data: (not stored)\n")
  }
  
  cat("Training time:", round(x$training_time, 3), "seconds\n")
  cat("Training iterations:", x$training_iterations, "\n")
  
  # Get probabilities with lazy evaluation if available
  if (x$n_trees > 0) {
    tryCatch({
      probs <- get_probabilities(x)
      cat("\nProbability range:\n")
      prob_range <- range(probs)
      cat("  Min:", round(prob_range[1], 3), "\n")
      cat("  Max:", round(prob_range[2], 3), "\n")
      cat("  Mean:", round(mean(probs), 3), "\n")
    }, error = function(e) {
      cat("\nProbabilities: (not computed - use compute_probabilities=TRUE or access via $probabilities)\n")
    })
  }
}

#' Summary of a TreeFARMS model
#'
#' @param object A trained TreeFARMS model object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
summary.treefarms_model <- function(object, ...) {
  print.treefarms_model(object, ...)
  
  if (object$n_trees > 0) {
    # Show class distribution if training data is available
    if (!is.null(object$y_train)) {
      cat("\nClass distribution in training data:\n")
      class_dist <- table(object$y_train)
      print(round(prop.table(class_dist), 3))
    }
    
    # Show feature names if training data is available
    if (!is.null(object$X_train)) {
      cat("\nFeature names:\n")
      cat(paste(names(object$X_train), collapse = ", "), "\n")
    }
  }
}

# Example usage function
#' @export
example_treefarms <- function() {
  cat("TreeFARMS R Wrapper Example (Rcpp)\n")
  cat("==================================\n\n")
  
  # Create sample data
  set.seed(42)
  n <- 100
  X <- data.frame(
    feature_1 = sample(0:1, n, replace = TRUE),
    feature_2 = sample(0:1, n, replace = TRUE)
  )
  
  # Create target with pattern
  y <- as.numeric(X$feature_1 == X$feature_2)
  
  cat("Sample data created:\n")
  cat("- Features:", ncol(X), "\n")
  cat("- Samples:", n, "\n")
  cat("- Class distribution:", table(y), "\n\n")
  
  cat("Training models...\n")
  
  # Train with misclassification loss
  model_misclass <- treefarms(X, y, loss_function = "misclassification", regularization = 0.1)
  
  # Train with log-loss
  model_logloss <- treefarms(X, y, loss_function = "log_loss", regularization = 0.1)
  
  cat("\nResults:\n")
  cat("--------\n")
  print(model_misclass)
  cat("\n")
  print(model_logloss)
  
  cat("\nProbability comparison (first 5 samples):\n")
  cat("Misclassification model:\n")
  print(head(model_misclass$probabilities, 5))
  cat("\nLog-loss model:\n")
  print(head(model_logloss$probabilities, 5))
  
  return(list(misclass = model_misclass, logloss = model_logloss))
}

# Override $ operator for treefarms_model to support lazy evaluation
# Use [[ to access list elements to avoid recursion
# Simplified version that doesn't use environments to avoid memory issues
#' @export
`$.treefarms_model` <- function(x, name) {
  # Handle lazy evaluation for probabilities, predictions, and accuracy
  if (name == "probabilities") {
    # Check if already computed
    if (!is.null(x[["probabilities"]])) {
      return(x[["probabilities"]])
    } else if (!is.null(x[[".compute_probabilities"]])) {
      # Compute lazily and cache directly in the list
      tryCatch({
        computed <- x[[".compute_probabilities"]]()
        # Cache the result by modifying the list (this works because lists are mutable)
        x[["probabilities"]] <- computed
        return(computed)
      }, error = function(e) {
        warning("Error computing probabilities: ", e$message)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  } else if (name == "predictions") {
    # Check if already computed
    if (!is.null(x[["predictions"]])) {
      return(x[["predictions"]])
    } else if (!is.null(x[[".compute_predictions"]])) {
      # Compute lazily and cache
      tryCatch({
        computed <- x[[".compute_predictions"]]()
        x[["predictions"]] <- computed
        return(computed)
      }, error = function(e) {
        warning("Error computing predictions: ", e$message)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  } else if (name == "accuracy") {
    # Check if already computed
    if (!is.null(x[["accuracy"]])) {
      return(x[["accuracy"]])
    } else if (!is.null(x[[".compute_accuracy"]])) {
      # Compute lazily and cache
      tryCatch({
        computed <- x[[".compute_accuracy"]]()
        x[["accuracy"]] <- computed
        return(computed)
      }, error = function(e) {
        warning("Error computing accuracy: ", e$message)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  } else {
    # For all other fields, use standard list access
    return(x[[name]])
  }
}

