#' TreeFARMS: Fast and Accurate Rule Set Models with Log-Loss and Probabilities
#'
#' @description
#' Train TreeFARMS models with support for log-loss optimization and probability predictions.
#' TreeFARMS finds optimal decision trees using either misclassification loss or log-loss (cross-entropy).
#'
#' @param X A data.frame or matrix of features. Can contain continuous or
#'   binary (0/1) features. Continuous features will be automatically discretized.
#' @param y For classification: binary (0/1). For regression: numeric vector (use \code{loss_function = "squared_error"}).
#' @param loss_function Character string specifying the loss function:
#'   \itemize{
#'     \item \code{"misclassification"} (default) - 0/1 loss for classification
#'     \item \code{"log_loss"} - Cross-entropy loss for probabilistic classification
#'     \item \code{"squared_error"} - L2 loss for regression (alias: \code{"regression"})
#'     \item \code{"absolute_error"} - L1 loss for robust regression (NEW)
#'     \item \code{"huber"} - Robust hybrid L1/L2 loss (NEW, requires \code{huber_delta})
#'     \item \code{"quantile"} - Quantile regression (NEW, requires \code{quantile_tau})
#'     \item \code{"custom"} - User-defined loss function (NEW, requires \code{custom_loss})
#'   }
#'   For regression losses, \code{y} may be continuous and prediction returns fitted values.
#' @param regularization Numeric value controlling model complexity. Higher values
#'   lead to simpler models. Default: 0.1. If NULL, will be auto-tuned.
#' @param rashomon_bound_multiplier Numeric value controlling Rashomon set size (multiplicative).
#'   Lower values lead to more trees. Default: 0.05. If NULL, will be auto-tuned.
#'   Rashomon bound = optimum * (1 + rashomon_bound_multiplier). Ignored if \code{rashomon_bound_adder} is non-zero.
#' @param rashomon_bound_adder Numeric value for additive Rashomon bound. Default: 0.
#'   If non-zero, Rashomon bound = optimum + rashomon_bound_adder and \code{rashomon_bound_multiplier} is not used.
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
#' @param discretize_method Method for discretizing continuous features:
#'   "median" (default) or "quantiles".
#' @param discretize_bins Number of bins for quantile discretization (default: 2).
#'   Can be numeric >= 2, or "adaptive" for data-dependent bins that grow with n
#'   (theory requires bins → ∞ for optimal rates). Adaptive uses max(2, ceiling(log(n)/3)).
#'   Example: n_bins=4 creates 4 bins with 3 thresholds (quartiles).
#' @param discretize_thresholds Optional named list of custom thresholds
#'   (e.g., list(age = c(30, 50))).
#' @param cart_lookahead Logical. Enable OSRT one-step lookahead bounds (Theorem 3.2).
#'   Default: TRUE (recommended). When enabled, uses scope-based pruning from the OSRT
#'   paper to eliminate subtrees more aggressively, improving optimization speed without
#'   affecting optimality. Can be set to FALSE to disable for debugging.
#' @param cart_lookahead_depth Integer. Reserved for future use. Currently ignored.
#' @param k_cluster Logical. Enable k-Means lower bounds for regression (OSRT Theorems 3.4-3.5).
#'   Default: TRUE (recommended). When enabled for regression with \code{loss_function = "squared_error"},
#'   uses optimal 1D k-Means clustering to compute tighter lower bounds, improving
#'   optimization speed (typically 1.5-4x faster). Based on Song & Zhong (2020) algorithm.
#'   Has no effect on classification tasks. Can be set to FALSE to disable.
#' @param huber_delta Numeric. Threshold parameter for Huber loss (default: 1.0).
#'   Controls transition between L2 (quadratic) and L1 (linear) behavior.
#'   - For errors |y - pred| <= delta: uses L2 loss
#'   - For errors |y - pred| > delta: uses L1 loss
#'   Only used when \code{loss_function = "huber"}.
#' @param quantile_tau Numeric in (0,1). Quantile level for quantile regression (default: 0.5).
#'   - tau = 0.5: median regression
#'   - tau = 0.9: 90th percentile regression
#'   - tau = 0.1: 10th percentile regression
#'   Only used when \code{loss_function = "quantile"}.
#' @param custom_loss A list specifying a custom loss function (only used when \code{loss_function = "custom"}).
#'   Must contain:
#'   \itemize{
#'     \item \code{aggregate}: Function that takes a numeric vector \code{y} and returns
#'       a named list of summary statistics
#'     \item \code{loss}: Function that takes summary statistics and a prediction,
#'       returns a list with \code{min_loss}, \code{max_loss}, and \code{potential}
#'     \item \code{optimal}: Function that takes summary statistics and returns
#'       the optimal prediction (single numeric value)
#'     \item \code{is_regression}: Logical flag indicating if this is a regression
#'       loss (TRUE) or classification loss (FALSE)
#'   }
#'   See \code{\link{example_custom_squared_error}} for examples.
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
    cli::cli_warn("Invalid tree structure detected. Using default probabilities.")
    n_samples <- nrow(X)
    return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
  }
  
  # Handle Rashomon set (list of trees) - use first tree
  if (is.list(tree_json) && length(tree_json) > 1 && is.null(tree_json$feature) && is.null(tree_json$prediction)) {
    # This is a list of trees, use the first one
    tree_json <- tree_json[[1]]
    # Re-validate after extraction
    if (!validate_tree_structure(tree_json)) {
      cli::cli_warn("Invalid tree structure after extraction. Using default probabilities.")
      n_samples <- nrow(X)
      return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
    }
  }
  
  n_samples <- nrow(X)
  max_depth <- 100L

  # Use environment to hold mutable state (avoids super-assignment)
  state_env <- new.env(parent = emptyenv())
  state_env$probabilities <- matrix(0.5, nrow = n_samples, ncol = 2)

  leaf_probs_from_node <- function(node) {
    if (is.null(node) || !is.list(node)) return(c(0.5, 0.5))
    if (is.null(node$prediction)) return(c(0.5, 0.5))
    tryCatch({
      if (!is.null(node$probabilities) && length(node$probabilities) >= 2) {
        probs <- as.numeric(node$probabilities)
        if (length(probs) == 2 && all(is.finite(probs)) && all(probs >= 0)) {
          prob_sum <- sum(probs)
          if (prob_sum > 0) probs <- probs / prob_sum else probs <- c(0.5, 0.5)
          return(probs)
        }
      }
      pred <- as.numeric(node$prediction)
      if (pred == 0) c(1.0, 0.0) else c(0.0, 1.0)
    }, error = function(e) {
      tryCatch({
        pred <- as.numeric(node$prediction)
        if (pred == 0) c(1.0, 0.0) else c(0.0, 1.0)
      }, error = function(e2) c(0.5, 0.5))
    })
  }

  traverse_batch <- function(node, row_indices, depth) {
    if (length(row_indices) == 0) return()
    if (depth > max_depth) {
      cli::cli_warn("Tree traversal exceeded maximum depth ({max_depth}). Using default probabilities.")
      return()
    }
    if (is.null(node) || !is.list(node)) return()
    if (!is.null(node$prediction)) {
      probs <- leaf_probs_from_node(node)
      state_env$probabilities[row_indices, 1] <- probs[1]
      state_env$probabilities[row_indices, 2] <- probs[2]
      return()
    }
    if (!is.null(node$feature)) {
      feature_idx <- as.integer(as.numeric(node$feature) + 1)
      if (feature_idx < 1 || feature_idx > ncol(X)) return()
      if (is.data.frame(X)) {
        feature_vals <- X[row_indices, feature_idx, drop = TRUE]
      } else {
        feature_vals <- X[row_indices, feature_idx]
      }
      go_true <- (feature_vals == 1 | feature_vals == TRUE)
      true_idx <- row_indices[go_true]
      false_idx <- row_indices[!go_true]
      if (length(true_idx) > 0 && !is.null(node$true) && is.list(node$true)) {
        traverse_batch(node$true, true_idx, depth + 1)
      }
      if (length(false_idx) > 0 && !is.null(node$false) && is.list(node$false)) {
        traverse_batch(node$false, false_idx, depth + 1)
      }
    }
  }

  traverse_batch(tree_json, seq_len(n_samples), 0L)
  colnames(state_env$probabilities) <- c("P(class=0)", "P(class=1)")
  return(state_env$probabilities)
}

#' Get fitted values from a regression tree
#'
#' Traverses the tree for each row in X and returns the leaf mean (fitted value) for regression trees.
#' @param tree_json Tree structure (list or list of trees; for Rashomon set uses first tree)
#' @param X New data (data.frame or matrix) with same binary features as training
#' @return Numeric vector of fitted values, one per row of X
#' @export
get_fitted_from_tree <- function(tree_json, X) {
  if (is.null(tree_json)) {
    return(rep(NA_real_, nrow(X)))
  }
  if (!is.list(tree_json)) {
    return(rep(NA_real_, nrow(X)))
  }
  if (!is.null(tree_json$feature) || !is.null(tree_json$prediction)) {
    tree <- tree_json
  } else if (length(tree_json) >= 1 && is.list(tree_json[[1]])) {
    tree <- tree_json[[1]]
  } else {
    return(rep(NA_real_, nrow(X)))
  }
  n_samples <- nrow(X)
  max_depth <- 100L

  # Use environment to hold mutable state (replaces global assignment)
  state_env <- new.env(parent = emptyenv())
  state_env$fitted <- rep(NA_real_, n_samples)

  traverse_batch_fitted <- function(node, row_indices, depth) {
    if (length(row_indices) == 0) return()
    if (depth > max_depth) return()
    if (is.null(node) || !is.list(node)) return()
    if (!is.null(node$prediction)) {
      val <- as.numeric(node$prediction)
      state_env$fitted[row_indices] <- val  # No <<- needed
      return()
    }
    if (!is.null(node$feature)) {
      feature_idx <- as.integer(as.numeric(node$feature) + 1)
      if (feature_idx < 1 || feature_idx > ncol(X)) return()
      if (is.data.frame(X)) {
        feature_vals <- X[row_indices, feature_idx, drop = TRUE]
      } else {
        feature_vals <- X[row_indices, feature_idx]
      }
      go_true <- (feature_vals == 1 | feature_vals == TRUE)
      true_idx <- row_indices[go_true]
      false_idx <- row_indices[!go_true]
      if (length(true_idx) > 0 && !is.null(node$true) && is.list(node$true)) {
        traverse_batch_fitted(node$true, true_idx, depth + 1)
      }
      if (length(false_idx) > 0 && !is.null(node$false) && is.list(node$false)) {
        traverse_batch_fitted(node$false, false_idx, depth + 1)
      }
    }
  }
  traverse_batch_fitted(tree, seq_len(n_samples), 0L)
  state_env$fitted  # Return from environment
}

#' @export
optimaltrees <- function(X, y, loss_function = "misclassification", regularization = 0.1,
rashomon_bound_multiplier = 0.05, rashomon_bound_adder = 0, target_trees = 1, max_trees = 5,
worker_limit = 1L, model_limit = 10000, verbose = FALSE, store_training_data = NULL,
compute_probabilities = FALSE, single_tree = TRUE,
discretize_method = "median", discretize_bins = 2, discretize_thresholds = NULL,
cart_lookahead = TRUE, cart_lookahead_depth = 0L, k_cluster = TRUE,
huber_delta = 1.0, quantile_tau = 0.5, custom_loss = NULL, ...) {
  
  if (is.null(store_training_data)) {
    store_training_data <- (loss_function == "log_loss" || loss_function == "squared_error")
  }
  
  # Input validation
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("{.arg X} must be a data.frame or matrix.")
  }
  
  if (!is.numeric(y) && !is.logical(y)) {
    cli::cli_abort("{.arg y} must be numeric or logical.")
  }

  if (nrow(X) == 0) {
    cli::cli_abort("{.arg X} must have at least one row.")
  }

  if (length(y) != nrow(X)) {
    cli::cli_abort("Length of {.arg y} must match number of rows in {.arg X}.")
  }
  
  
  # Determine if this is a regression task
  if (loss_function == "custom") {
    # For custom losses, check specification first
    if (is.null(custom_loss)) {
      cli::cli_abort("{.arg custom_loss} must be provided when {.code loss_function = 'custom'}.")
    }
    validate_custom_loss(custom_loss)
    # Determine if regression based on custom loss specification
    is_regression <- custom_loss$is_regression
  } else {
    # For built-in losses
    is_regression <- loss_function %in% c("squared_error", "regression", "absolute_error", "huber", "quantile")
  }

  if (!is_regression && !all(y %in% c(0, 1))) {
    cli::cli_abort("{.arg y} must contain only binary values (0 and 1) for classification.")
  }
  if (is_regression && !is.numeric(y)) {
    cli::cli_abort("{.arg y} must be numeric for regression.")
  }
  valid_losses <- c("misclassification", "log_loss", "squared_error", "regression",
                    "absolute_error", "huber", "quantile", "custom")
  if (!loss_function %in% valid_losses) {
    cli::cli_abort("{.arg loss_function} must be one of: {.val {valid_losses}}.")
  }
  if (loss_function == "regression") {
    loss_function <- "squared_error"
  }

  # Validate loss-specific parameters
  if (!is.numeric(huber_delta) || length(huber_delta) != 1 || huber_delta <= 0) {
    cli::cli_abort("{.arg huber_delta} must be a positive number.")
  }
  if (!is.numeric(quantile_tau) || length(quantile_tau) != 1 ||
      quantile_tau <= 0 || quantile_tau >= 1) {
    cli::cli_abort("{.arg quantile_tau} must be a number in (0, 1).")
  }
  
  if (!is.numeric(worker_limit) || length(worker_limit) != 1 || worker_limit < 1) {
    cli::cli_abort("{.arg worker_limit} must be a positive integer.")
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
    
    return(auto_tune_optimaltrees(X, y, loss_function = loss_function,
                                  target_trees = target_trees, max_trees = max_trees,
                                  fixed_param = fixed_param, fixed_value = fixed_value,
                                  verbose = verbose,
                                  discretize_method = discretize_method,
                                  discretize_bins = discretize_bins,
                                  discretize_thresholds = discretize_thresholds,
                                  ...))
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

  # Check for binary features (vectorized)
  m <- as.matrix(X)
  bad <- !m %in% c(0L, 1L) & !is.na(m)
  if (any(bad)) {
    idx <- which(bad)[1L]
    col_idx <- ((idx - 1L) %/% nrow(m)) + 1L
    col <- names(X)[col_idx]
    cli::cli_abort("Feature {.field {col}} must contain only binary values (0 and 1).")
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
    model_limit = as.integer(model_limit),
    look_ahead = cart_lookahead,  # Map to C++ look_ahead (OSRT one-step lookahead)
    # cart_lookahead_depth is currently unused - reserved for future enhancements
    k_cluster = k_cluster,
    huber_delta = huber_delta,
    quantile_tau = quantile_tau,
    ...
  )
  
  # Add rashomon parameters
  # If single_tree = TRUE, disable rashomon to get exactly one tree
  if (single_tree) {
    config$rashomon <- FALSE
    # Don't set rashomon_bound_multiplier when rashomon is disabled
  } else {
    config$rashomon <- TRUE
    if (rashomon_bound_adder != 0) {
      config$rashomon_bound_adder <- rashomon_bound_adder
      config$rashomon_bound_multiplier <- 0
    } else {
      config$rashomon_bound_multiplier <- rashomon_bound_multiplier
      config$rashomon_bound_adder <- 0
    }
  }
  
  # Convert configuration to JSON
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  
  # Prepare data for C++ (combine X and y into CSV format)
  data_df <- X
  data_df$class <- y
  
  # Build CSV string in one pass (no capture.output(write.csv))
  header <- paste(names(data_df), collapse = ",")
  body <- apply(data_df, 1L, function(r) paste(as.character(r), collapse = ","))
  csv_string <- paste(c(header, body), collapse = "\n")

  # Set custom loss specification if provided
  if (loss_function == "custom") {
    set_custom_loss_spec(custom_loss)
    on.exit(clear_custom_loss_spec(), add = TRUE)
  }

  fit_result <- .treefarms_fit_with_csv(csv_string, config, X, y, single_tree, store_training_data, compute_probabilities,
                                        discretization_metadata, X_original)
  return(fit_result)
}

# ============================================================================
# Internal helper functions for .treefarms_fit_with_csv()
# ============================================================================

#' Parse C++ JSON Result with Error Handling
#'
#' @param json_output Character string containing JSON from C++ backend
#' @param verbose Logical. Whether to print diagnostic messages.
#' @return Parsed JSON as list, or NULL if parsing fails
#' @keywords internal
parse_cpp_json_result <- function(json_output, verbose = FALSE) {
  # Handle NULL or empty output
  if (is.null(json_output) || json_output == "") {
    if (verbose) {
      cli::cli_inform("C++ returned empty or NULL JSON output")
    }
    return(NULL)
  }

  # Check if json_output is just "{}" (empty JSON object fallback)
  json_trimmed <- trimws(json_output)
  if (json_trimmed == "{}" || json_trimmed == "null") {
    if (verbose) {
      cli::cli_inform("C++ returned empty JSON object: {json_trimmed}")
    }
    return(NULL)
  }

  # Try to parse JSON with error handling
  tryCatch({
    result_data <- jsonlite::fromJSON(json_output, simplifyVector = FALSE)
    if (verbose) {
      if (!is.null(result_data$storage)) {
        cli::cli_inform("Parsed JSON with storage field")
      }
      if (!is.null(result_data$trees)) {
        cli::cli_inform("Parsed JSON with trees field ({length(result_data$trees)} trees)")
      }
    }
    return(result_data)
  }, error = function(e) {
    # If parsing fails, provide detailed error information
    cli::cli_warn("Failed to parse JSON result from C++ code. This may indicate a serialization error. ",
            "Error: ", e$message,
            ". JSON length: ", nchar(json_output),
            ". First 200 chars: ", substr(json_output, 1, min(200, nchar(json_output))))
    if (verbose) {
      message("JSON parsing failed")
    }
    return(NULL)
  })
}

#' Extract Tree Structure from Parsed C++ Result
#'
#' Handles variable JSON formats from C++ backend
#'
#' @param result_data Parsed JSON data from C++ (list or NULL)
#' @param verbose Logical. Whether to print diagnostic messages.
#' @return Tree JSON structure, or NULL if not found
#' @keywords internal
extract_tree_from_cpp_result <- function(result_data, verbose = FALSE) {
  if (is.null(result_data)) {
    return(NULL)
  }

  tree_json <- NULL

  # Strategy 1: Check for trees field (from ModelSet::serialize with models)
  if (!is.null(result_data$trees) && is.list(result_data$trees) && length(result_data$trees) > 0) {
    tree_json <- result_data$trees[[1]]
    if (verbose) {
      message("Extracted tree from result_data$trees array (first of ", length(result_data$trees), ")")
    }
    return(tree_json)
  }

  # Strategy 2: Check if result_data itself is a tree structure
  if (!is.null(result_data$feature) || !is.null(result_data$prediction)) {
    tree_json <- result_data
    if (verbose) {
      message("result_data is itself a tree structure")
    }
    return(tree_json)
  }

  # Strategy 3: Check storage field (ModelSet structure)
  if (!is.null(result_data$storage) && is.list(result_data$storage) && length(result_data$storage) > 0) {
    for (item in result_data$storage) {
      if (is.list(item) && (!is.null(item$feature) || !is.null(item$prediction))) {
        tree_json <- item
        if (verbose) {
          message("Extracted tree from result_data$storage")
        }
        break
      }
    }
    return(tree_json)
  }

  # Strategy 4: Check if result_data is an array of trees
  if (is.list(result_data) && length(result_data) > 0) {
    if (is.list(result_data[[1]]) && (!is.null(result_data[[1]]$feature) || !is.null(result_data[[1]]$prediction))) {
      tree_json <- result_data[[1]]
      if (verbose) {
        message("result_data is an array of trees, extracted first")
      }
      return(tree_json)
    }
  }

  if (verbose) {
    message("Could not extract tree from result_data")
  }

  return(tree_json)
}

#' Count Trees in Rashomon Set
#'
#' @param result_data Parsed JSON data from C++
#' @param tree_json Extracted tree structure
#' @return Integer count of trees in Rashomon set
#' @keywords internal
count_trees_in_result <- function(result_data, tree_json) {
  n_trees <- 0

  # Strategy 1: Check result_data for trees field
  if (!is.null(result_data)) {
    if (is.list(result_data) && !is.null(result_data$trees)) {
      if (is.list(result_data$trees)) {
        n_trees <- length(result_data$trees)
      } else {
        n_trees <- 1
      }
      return(n_trees)
    }

    # Strategy 2: Check if result_data is a single tree
    if (is.list(result_data) && (!is.null(result_data$feature) || !is.null(result_data$prediction))) {
      n_trees <- 1
      return(n_trees)
    }

    # Strategy 3: Check if result_data is a list of trees
    if (is.list(result_data) && length(result_data) > 0) {
      first_elem <- result_data[[1]]
      if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
        n_trees <- length(result_data)
        return(n_trees)
      }
    }
  }

  # Strategy 4: Check tree_json if no trees found in result_data
  if (n_trees == 0 && !is.null(tree_json)) {
    if (is.list(tree_json) && !is.null(tree_json$trees)) {
      if (is.list(tree_json$trees)) {
        n_trees <- length(tree_json$trees)
      } else {
        n_trees <- 1
      }
      return(n_trees)
    }

    if (is.list(tree_json) && (!is.null(tree_json$feature) || !is.null(tree_json$prediction))) {
      n_trees <- 1
      return(n_trees)
    }

    # Check if tree_json is a list of trees (Rashomon set)
    if (is.list(tree_json) && length(tree_json) > 1 &&
        is.null(tree_json$feature) && is.null(tree_json$prediction) && is.null(tree_json$trees)) {
      if (length(tree_json) > 0) {
        first_elem <- tree_json[[1]]
        if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
          n_trees <- length(tree_json)
          return(n_trees)
        }
      }
    }
  }

  return(n_trees)
}

#' Enforce Single Tree Constraint
#'
#' If single_tree=TRUE and multiple trees exist, extract only the first tree
#'
#' @param single_tree Logical. Whether to enforce single tree constraint
#' @param n_trees Current count of trees
#' @param tree_json Current tree JSON structure
#' @param result_data Current parsed result data
#' @return List with updated tree_json, result_data, and n_trees
#' @keywords internal
enforce_single_tree <- function(single_tree, n_trees, tree_json, result_data) {
  # If single_tree constraint is disabled or only one tree exists, no action needed
  if (!single_tree || n_trees <= 1) {
    return(list(tree_json = tree_json, result_data = result_data, n_trees = n_trees))
  }

  # Multiple trees exist and single_tree=TRUE: extract only first tree

  # Strategy 1: tree_json is a list of trees
  if (!is.null(tree_json) && is.list(tree_json) && length(tree_json) > 1 &&
      is.null(tree_json$feature) && is.null(tree_json$prediction) && is.null(tree_json$trees)) {
    tree_json <- tree_json[[1]]
  }

  # Strategy 2: result_data is a list of trees
  if (!is.null(result_data) && is.list(result_data) && length(result_data) > 1) {
    first_elem <- result_data[[1]]
    if (is.list(first_elem) && (!is.null(first_elem$feature) || !is.null(first_elem$prediction))) {
      result_data <- first_elem
      # Also update tree_json if it was derived from result_data
      if (is.null(tree_json) || identical(tree_json, result_data)) {
        tree_json <- first_elem
      }
    }
  }

  # Strategy 3: result_data has trees field with multiple trees
  if (!is.null(result_data) && is.list(result_data) && !is.null(result_data$trees) &&
      is.list(result_data$trees) && length(result_data$trees) > 1) {
    result_data$trees <- list(result_data$trees[[1]])
    if (!is.null(tree_json) && is.list(tree_json) && !is.null(tree_json$trees)) {
      tree_json$trees <- list(tree_json$trees[[1]])
    }
  }

  # Force n_trees to 1
  n_trees <- 1

  return(list(tree_json = tree_json, result_data = result_data, n_trees = n_trees))
}

#' Create Model Object from Parsed Results
#'
#' @param result_data Parsed JSON data from C++
#' @param tree_json Extracted tree structure
#' @param config Configuration list
#' @param result_list Training statistics from C++
#' @param X Training features
#' @param y Training labels
#' @param store_training_data Logical. Whether to store training data
#' @return List with model_obj and metadata
#' @keywords internal
create_model_object <- function(result_data, tree_json, config, result_list,
                                X, y, store_training_data) {
  # Create base model object
  model_obj <- list(
    result_data = result_data,
    tree_json = tree_json,
    config = config,
    time = result_list$time,
    iterations = result_list$iterations,
    size = result_list$size,
    status = result_list$status
  )

  # Determine which tree to use for predictions
  tree_to_use <- if (!is.null(tree_json)) tree_json else result_data

  # Check if we have a valid tree structure
  has_tree <- FALSE
  if (!is.null(tree_to_use)) {
    if (is.list(tree_to_use)) {
      # Check if it's a tree structure (has feature or prediction)
      if (!is.null(tree_to_use$feature) || !is.null(tree_to_use$prediction)) {
        has_tree <- TRUE
      } else if (length(tree_to_use) > 1 && !is.null(names(tree_to_use))) {
        # Might be a list of trees or a named list
        for (item in tree_to_use) {
          if (is.list(item) && (!is.null(item$feature) || !is.null(item$prediction))) {
            has_tree <- TRUE
            break
          }
        }
      }
    }
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

  return(list(
    model_obj = model_obj,
    stored_tree = stored_tree,
    stored_X = stored_X,
    stored_y = stored_y,
    has_tree = has_tree
  ))
}

#' Build Regression Result Object
#'
#' @param has_tree Logical. Whether a valid tree exists
#' @param X Training features
#' @param y Training labels
#' @param stored_X Stored training features (or NULL)
#' @param stored_y Stored training labels (or NULL)
#' @param stored_tree Stored tree structure
#' @param loss_function Loss function name
#' @param regularization Regularization parameter
#' @param n_trees Number of trees
#' @param result_list Training statistics
#' @return Result list with lazy evaluation functions
#' @keywords internal
build_regression_result <- function(has_tree, X, y, stored_X, stored_y,
                                   stored_tree, loss_function, regularization,
                                   n_trees, result_list) {
  # Define lazy evaluation functions
  compute_predictions_lazy <- function() {
    if (is.null(stored_X)) return(rep(NA_real_, length(y)))
    if (is.null(stored_tree)) return(rep(NA_real_, nrow(stored_X)))
    get_fitted_from_tree(stored_tree, stored_X)
  }

  compute_probabilities_lazy <- NULL

  compute_accuracy_lazy <- function() {
    if (is.null(stored_y)) return(NA_real_)
    preds <- compute_predictions_lazy()
    if (is.null(preds) || length(preds) == 0) return(NA_real_)
    mean((stored_y - preds)^2)
  }

  # Compute predictions and MSE if tree exists
  if (has_tree && !is.null(X) && nrow(X) > 0) {
    predictions <- get_fitted_from_tree(stored_tree, X)
    accuracy <- mean((y - predictions)^2)
  } else {
    predictions <- rep(NA_real_, length(y))
    accuracy <- NA_real_
  }

  # Build result list
  result <- list(
    loss_function = loss_function,
    regularization = regularization,
    n_trees = n_trees,
    training_time = result_list$time,
    training_iterations = result_list$iterations,
    .compute_probabilities = compute_probabilities_lazy,
    .compute_predictions = compute_predictions_lazy,
    .compute_accuracy = compute_accuracy_lazy,
    probabilities = NULL,
    predictions = predictions,
    accuracy = accuracy
  )

  return(result)
}

#' Build Classification Result Object
#'
#' @param compute_probabilities Logical. Whether to compute probabilities immediately
#' @param has_tree Logical. Whether a valid tree exists
#' @param X Training features
#' @param y Training labels
#' @param stored_X Stored training features (or NULL)
#' @param stored_y Stored training labels (or NULL)
#' @param stored_tree Stored tree structure
#' @param loss_function Loss function name
#' @param regularization Regularization parameter
#' @param n_trees Number of trees
#' @param result_list Training statistics
#' @param result_data Parsed result data (for fallback warnings)
#' @param json_output Original JSON output (for fallback warnings)
#' @param verbose Logical. Whether to print diagnostic messages
#' @return Result list with lazy evaluation functions and cache
#' @keywords internal
build_classification_result <- function(compute_probabilities, has_tree, X, y,
                                       stored_X, stored_y, stored_tree,
                                       loss_function, regularization, n_trees,
                                       result_list, result_data = NULL,
                                       json_output = NULL, verbose = FALSE) {
  # Initialize variables
  probabilities <- NULL
  predictions <- NULL
  accuracy <- NULL

  # Compute immediately if requested and tree exists
  if (compute_probabilities && has_tree) {
    probabilities <- get_probabilities_from_tree(stored_tree, X)
    if (verbose) {
      print(head(probabilities, 3))
    }
    predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
    accuracy <- mean(predictions == y)
  } else if (has_tree) {
    # Compute for predictions/accuracy but not probabilities
    probs <- get_probabilities_from_tree(stored_tree, X)
    predictions <- ifelse(probs[, 2] >= 0.5, 1, 0)
    accuracy <- mean(predictions == y)
  } else {
    # No valid tree - provide fallback
    if (is.null(result_data) && (!is.null(json_output) && json_output != "" && trimws(json_output) == "{}")) {
      warning("Model training may have failed or serialization returned empty result. ",
              "Try reducing dataset size or adjusting regularization.")
    }
    predictions <- rep(0, length(y))
    accuracy <- mean(predictions == y)
  }

  # Create cache environment for lazy evaluation
  cache <- new.env()

  # Define lazy probability computation with caching
  compute_probabilities_lazy <- function() {
    if (exists("probabilities", cache, inherits = FALSE)) return(cache$probabilities)
    if (is.null(stored_tree)) {
      n_samples <- if (!is.null(stored_X)) nrow(stored_X) else 0
      if (n_samples == 0) return(matrix(c(0.5, 0.5), nrow = 1, ncol = 2, byrow = TRUE))
      return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
    }
    if (is.null(stored_X)) {
      stop("Cannot compute probabilities: training data not stored. ",
           "Re-train with store_training_data=TRUE or provide newdata to predict()")
    }
    n_samples <- nrow(stored_X)

    # Use batch processing for large datasets
    if (n_samples > 500) {
      batch_size <- min(500, n_samples)
      n_batches <- ceiling(n_samples / batch_size)
      probs <- matrix(0.0, nrow = n_samples, ncol = 2)
      for (i in 1:n_batches) {
        start_idx <- (i - 1) * batch_size + 1
        end_idx <- min(i * batch_size, n_samples)
        batch_X <- stored_X[start_idx:end_idx, , drop = FALSE]
        probs[start_idx:end_idx, ] <- get_probabilities_from_tree(stored_tree, batch_X)
      }
      colnames(probs) <- c("P(class=0)", "P(class=1)")
    } else {
      probs <- get_probabilities_from_tree(stored_tree, stored_X)
    }

    # Cache results
    cache$probabilities <- probs
    cache$predictions <- ifelse(probs[, 2] >= 0.5, 1, 0)
    cache$accuracy <- if (!is.null(stored_y)) mean(cache$predictions == stored_y) else NA_real_

    return(cache$probabilities)
  }

  compute_predictions_lazy <- function() {
    if (exists("predictions", cache, inherits = FALSE)) return(cache$predictions)
    compute_probabilities_lazy()
    return(cache$predictions)
  }

  compute_accuracy_lazy <- function() {
    if (exists("accuracy", cache, inherits = FALSE)) return(cache$accuracy)
    if (is.null(stored_y)) {
      stop("Cannot compute accuracy: training data not stored. ",
           "Re-train with store_training_data=TRUE")
    }
    compute_predictions_lazy()
    return(cache$accuracy)
  }

  # Build result list
  result <- list(
    loss_function = loss_function,
    regularization = regularization,
    n_trees = n_trees,
    training_time = result_list$time,
    training_iterations = result_list$iterations,
    .compute_probabilities = compute_probabilities_lazy,
    .compute_predictions = compute_predictions_lazy,
    .compute_accuracy = compute_accuracy_lazy,
    probabilities = probabilities,
    predictions = predictions,
    accuracy = accuracy,
    .cache = cache
  )

  return(result)
}

#' Finalize Result Object with Metadata
#'
#' @param result Result list to finalize
#' @param model_obj Model object to add
#' @param X Training features
#' @param y Training labels
#' @param store_training_data Logical. Whether training data should be stored
#' @param discretization_metadata Discretization metadata
#' @param X_original Original feature names
#' @return Finalized result object with class
#' @keywords internal
finalize_result_object <- function(result, model_obj, X, y, store_training_data,
                                   discretization_metadata, X_original) {
  # Extract tree structures from model_obj
  # model_obj can have tree_json (single tree) or trees (list of trees)
  trees <- if (!is.null(model_obj$trees)) {
    model_obj$trees
  } else if (!is.null(model_obj$tree_json)) {
    list(model_obj$tree_json)
  } else {
    list()
  }

  # Determine if regression based on loss function
  is_regression <- result$loss_function %in% c("squared_error", "absolute_error", "huber", "quantile", "custom")

  # IMPORTANT: Use actual tree count from extracted trees, not result$n_trees
  # The result$n_trees may reflect C++ internal state that doesn't match
  # what we actually extract from tree_json/trees fields
  n_trees_actual <- length(trees)

  # Create S7 model object
  s7_model <- new_optimal_trees_model(
    loss_function = result$loss_function,
    regularization = result$regularization,
    n_trees = n_trees_actual,
    trees = trees,
    accuracy = if (is_regression) NA_real_ else (result$accuracy %||% NA_real_),
    predictions = result$predictions,
    probabilities = result$probabilities,
    X_train = if (store_training_data) X else NULL,
    y_train = if (store_training_data) y else NULL,
    discretization_metadata = discretization_metadata,
    is_regression = is_regression
  )

  # Note: Lazy evaluation functions and cache are not needed in S7 model
  # The values are already computed and stored in the properties above
  # For backward compatibility, S3 methods can still access via $ operator
  # which will be intercepted by the custom $.optimaltrees_model function

  return(s7_model)
}

# ============================================================================
# Main fitting function (orchestrator)
# ============================================================================

.treefarms_fit_with_csv <- function(csv_string, config, X, y, single_tree, store_training_data, compute_probabilities,
                                    discretization_metadata = NULL, X_original = NULL) {
  # Extract config parameters
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  verbose <- isTRUE(config$verbose)
  is_regression <- config$loss_function %in% c("squared_error", "regression")
  loss_function <- config$loss_function
  regularization <- config$regularization

  tryCatch({
    # Step 1: Call C++ backend
    json_output_raw <- treefarms_fit_with_config_cpp(csv_string, config_json)
    if (is.character(json_output_raw) && length(json_output_raw) == 1) {
      json_output <- as.character(json_output_raw)[1]
    } else if (is.character(json_output_raw)) {
      json_output <- paste(json_output_raw, collapse = "")
    } else {
      json_output <- as.character(json_output_raw)
    }

    if (verbose && !is.null(json_output) && json_output != "") {
      message("C++ backend returned JSON output")
    }

    # Step 2: Parse JSON result
    result_data <- parse_cpp_json_result(json_output, verbose)

    # Step 3: Extract tree structure
    tree_json <- extract_tree_from_cpp_result(result_data, verbose)

    if (verbose && !is.null(tree_json)) {
      message("Successfully extracted tree structure")
    }

    # Step 4: Get training statistics from C++
    training_time <- treefarms_time_cpp()
    iterations <- treefarms_iterations_cpp()
    model_size <- treefarms_size_cpp()
    status <- treefarms_status_cpp()

    result_list <- list(
      result = json_output,
      time = training_time,
      iterations = iterations,
      size = model_size,
      status = status
    )

    # Step 5: Count trees in Rashomon set
    n_trees <- count_trees_in_result(result_data, tree_json)

    # Step 6: Enforce single tree constraint if needed
    enforcement_result <- enforce_single_tree(single_tree, n_trees, tree_json, result_data)
    tree_json <- enforcement_result$tree_json
    result_data <- enforcement_result$result_data
    n_trees <- enforcement_result$n_trees

    # Step 7: Create model object with validation
    model_components <- create_model_object(result_data, tree_json, config, result_list,
                                           X, y, store_training_data)
    model_obj <- model_components$model_obj
    stored_tree <- model_components$stored_tree
    stored_X <- model_components$stored_X
    stored_y <- model_components$stored_y
    has_tree <- model_components$has_tree

    if (verbose) {
      message("Model object created, has_tree: ", has_tree)
    }

    # Step 8: Build result object (regression vs classification)
    if (is_regression) {
      result <- build_regression_result(has_tree, X, y, stored_X, stored_y,
                                       stored_tree, loss_function, regularization,
                                       n_trees, result_list)
    } else {
      result <- build_classification_result(compute_probabilities, has_tree, X, y,
                                           stored_X, stored_y, stored_tree,
                                           loss_function, regularization, n_trees,
                                           result_list, result_data, json_output, verbose)
    }

    # Step 9: Finalize result with metadata
    result <- finalize_result_object(result, model_obj, X, y, store_training_data,
                                     discretization_metadata, X_original)

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
#' @param object A optimaltrees_model object
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
#' @param object A optimaltrees_model object
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
    cli::cli_abort("Cannot compute predictions: model structure incomplete.")
  }
}

#' Get accuracy from a treefarms model (with lazy evaluation)
#'
#' @param object A optimaltrees_model object
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
predict_optimaltrees <- function(object, newdata, type = "class", ...) {
  
  if (!inherits(object, "optimaltrees_model")) {
    stop("object must be a optimaltrees_model object")
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
      cli::cli_abort("Feature {.field {col}} must contain only binary values (0 and 1).")
    }
  }
  
  # Regression: return fitted values
  if (identical(object$loss_function, "squared_error")) {
    if (!is.null(tree_to_use)) {
      return(get_fitted_from_tree(tree_to_use, newdata))
    }
    return(rep(NA_real_, nrow(newdata)))
  }
  
  # Extract probabilities from tree structure (classification)
  if (!is.null(tree_to_use)) {
    probabilities <- get_probabilities_from_tree(tree_to_use, newdata)
    
    if (type == "class") {
      predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
      return(predictions)
    } else {
      return(probabilities)
    }
  } else {
    # No tree structure available - this should never happen for a properly fitted model
    stop(
      "Cannot make predictions: tree structure not found in model object.\n",
      "This indicates a problem during model fitting or an invalid model object.\n",
      "Check that the model was fitted successfully."
    )
  }
}

#' Print summary of a TreeFARMS model
#'
#' @param x A trained TreeFARMS model object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
print.optimaltrees_model <- function(x, ...) {
  # Handle both S3 and S7 objects
  is_s7 <- S7::S7_inherits(x, OptimalTreesModel)

  # Helper to get properties (works for both S3 and S7)
  get_prop <- function(obj, name) {
    if (is_s7) {
      switch(name,
        loss_function = obj@loss_function,
        regularization = obj@regularization,
        n_trees = obj@n_trees,
        accuracy = obj@accuracy,
        X_train = obj@X_train,
        is_regression = obj@is_regression,
        predictions = obj@predictions,
        probabilities = obj@probabilities,
        n_train = NULL,  # S7 doesn't have n_train
        training_time = NULL,  # S7 doesn't have these
        training_iterations = NULL,
        NULL  # return NULL for unknown properties on S7
      )
    } else {
      obj[[name]]
    }
  }

  cat("TreeFARMS Model\n")
  cat("===============\n")
  cat("Loss function:", get_prop(x, "loss_function"), "\n")
  cat("Regularization:", get_prop(x, "regularization"), "\n")
  cat("Number of trees:", get_prop(x, "n_trees"), "\n")

  # Get accuracy
  is_regression <- get_prop(x, "is_regression")
  if (!is.null(is_regression) && is_regression) {
    cat("(Regression model - accuracy not applicable)\n")
  } else {
    tryCatch({
      acc <- if (is_s7) get_prop(x, "accuracy") else get_accuracy(x)
      if (!is.na(acc)) {
        cat("Training accuracy:", round(acc, 4), "\n")
      }
    }, error = function(e) {
      cat("Training accuracy: (not available)\n")
    })
  }

  # Get training data info
  X_train <- get_prop(x, "X_train")
  n_train_prop <- get_prop(x, "n_train")
  n_samp <- if (!is.null(n_train_prop)) n_train_prop else if (!is.null(X_train)) nrow(X_train) else NA

  if (!is.na(n_samp)) {
    cat("Training samples:", n_samp, "\n")
  }
  if (!is.null(X_train) && nrow(X_train) > 0) {
    cat("Features:", ncol(X_train), "\n")
  }

  # Training time/iterations (S3 only)
  if (!is_s7) {
    training_time <- get_prop(x, "training_time")
    training_iterations <- get_prop(x, "training_iterations")
    if (!is.null(training_time)) {
      cat("Training time:", round(training_time, 3), "seconds\n")
    }
    if (!is.null(training_iterations)) {
      cat("Training iterations:", training_iterations, "\n")
    }
  }

  # Probabilities (classification only)
  if (is.null(is_regression) || !is_regression) {
    if (get_prop(x, "n_trees") > 0) {
      tryCatch({
        probs <- if (is_s7) get_prop(x, "probabilities") else get_probabilities(x)
        if (!is.null(probs)) {
          cat("\nProbability range:\n")
          prob_range <- range(probs[, 2])  # P(class=1)
          cat("  Min:", round(prob_range[1], 3), "\n")
          cat("  Max:", round(prob_range[2], 3), "\n")
          cat("  Mean:", round(mean(probs[, 2]), 3), "\n")
        }
      }, error = function(e) {
        # Silently skip if not available
      })
    }
  }
}

#' Summary of a TreeFARMS model
#'
#' @param object A trained TreeFARMS model object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
summary.optimaltrees_model <- function(object, ...) {
  print.optimaltrees_model(object, ...)

  # Handle both S7 and S3 objects
  is_s7 <- S7::S7_inherits(object, OptimalTreesModel)
  n_trees <- if (is_s7) object@n_trees else object$n_trees
  y_train <- if (is_s7) object@y_train else object$y_train
  X_train <- if (is_s7) object@X_train else object$X_train

  if (n_trees > 0) {
    # Show class distribution if training data is available
    if (!is.null(y_train)) {
      cat("\nClass distribution in training data:\n")
      class_dist <- table(y_train)
      print(round(prop.table(class_dist), 3))
    }

    # Show feature names if training data is available
    if (!is.null(X_train)) {
      cat("\nFeature names:\n")
      cat(paste(names(X_train), collapse = ", "), "\n")
    }
  }
}

# Example usage function
#' @export
example_optimaltrees <- function() {
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

# Override $ operator for optimaltrees_model to support lazy evaluation
# Use [[ to access list elements to avoid recursion
# Simplified version that doesn't use environments to avoid memory issues
#' @export
`$.optimaltrees_model` <- function(x, name) {
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

