#' Refit Tree Structure with New Data
#'
#' @description
#' Refit leaf values for a fixed tree structure using new training data.
#' Used in M-split algorithm: select one structure, refit on multiple splits.
#'
#' @param structure TreeStructure object (from extract_tree_structure)
#' @param X_new Training features (data.frame or matrix)
#' @param y_new Training outcomes (numeric vector)
#' @param loss_function "squared_error", "log_loss", or "misclassification"
#' @param store_training_data Logical. Store X_new and y_new in model? (default FALSE)
#' @param discretization_metadata Optional list containing discretization metadata
#'   from the original model. If provided, X_new will be discretized using the
#'   same thresholds as training. Required when structure uses discretized
#'   feature names but X_new has original continuous features. Default: NULL.
#' @param allow_partial_leaves Logical. If FALSE (default), stop with an error when
#'   the new data has no observations in one or more leaves of the structure. If TRUE,
#'   fill missing leaves with the overall mean of \code{y_new} and emit a warning.
#'   This should only be set to TRUE when partial coverage is expected and acceptable.
#'
#' @return List with class "refit_result" containing:
#'   \item{model}{OptimalTreesModel with refit leaf values}
#'   \item{n_per_leaf}{Named integer vector of observation counts per leaf.
#'     Names are leaf paths (e.g., "0-1", "1-0-1", "root"), values are counts.
#'     Used for weighted tree averaging.}
#'
#' @details
#' Algorithm:
#' 1. Assign each observation to a leaf (traverse structure)
#' 2. Compute leaf predictions:
#'    - squared_error: mean(y) per leaf
#'    - log_loss: empirical probability per leaf
#'    - misclassification: majority class per leaf
#' 3. Reconstruct tree with new leaf values
#'
#' The tree structure (splits) remains fixed. Only leaf values change.
#'
#' @examples
#' \dontrun{
#' # Fit tree on data 1
#' model1 <- fit_tree(X1, y1)
#' structure <- extract_tree_structure(model1)
#'
#' # Refit same structure on data 2
#' refit_result <- refit_tree_structure(structure, X2, y2, "log_loss")
#' model2 <- refit_result$model
#' leaf_counts <- refit_result$n_per_leaf
#'
#' # Make predictions
#' preds <- predict(model2, X_test, type = "prob")
#' }
#'
#' @export
refit_tree_structure <- function(structure, X_new, y_new, loss_function,
                                  store_training_data = FALSE,
                                  discretization_metadata = NULL,
                                  allow_partial_leaves = FALSE) {
  # Validate inputs
  if (!S7::S7_inherits(structure, TreeStructure)) {
    stop("structure must be a TreeStructure object", call. = FALSE)
  }

  if (!is.data.frame(X_new) && !is.matrix(X_new)) {
    stop("X_new must be a data.frame or matrix", call. = FALSE)
  }

  if (!is.numeric(y_new) && !is.integer(y_new)) {
    stop("y_new must be numeric or integer", call. = FALSE)
  }

  if (nrow(X_new) != length(y_new)) {
    stop(sprintf("nrow(X_new) = %d must equal length(y_new) = %d",
                 nrow(X_new), length(y_new)), call. = FALSE)
  }

  valid_losses <- c("squared_error", "log_loss", "misclassification")
  if (!loss_function %in% valid_losses) {
    stop(sprintf("loss_function must be one of: %s",
                 paste(valid_losses, collapse = ", ")), call. = FALSE)
  }

  # Convert to data.frame if matrix
  if (is.matrix(X_new)) {
    X_new <- as.data.frame(X_new)
  }

  # Apply discretization if metadata provided
  if (!is.null(discretization_metadata)) {
    X_new <- apply_discretization(X_new, discretization_metadata)

    # Verify feature names match structure expectations
    expected_names <- structure@feature_names
    actual_names <- colnames(X_new)

    if (!setequal(expected_names, actual_names)) {
      missing <- setdiff(expected_names, actual_names)
      extra <- setdiff(actual_names, expected_names)
      stop(sprintf("Feature name mismatch after discretization.\n  Missing: %s\n  Extra: %s",
                   paste(missing, collapse=", "), paste(extra, collapse=", ")),
           call. = FALSE)
    }

    # Reorder columns to match structure
    X_new <- X_new[, expected_names, drop = FALSE]
  }

  # Step 1: Assign observations to leaves
  leaf_assignments <- assign_observations_to_leaves(structure, X_new)

  # Step 1.5: Capture counts per leaf (for weighted averaging)
  n_per_leaf <- table(leaf_assignments)  # Named integer: leaf_id -> count

  # Step 2: Compute leaf predictions based on loss function
  leaf_preds <- compute_leaf_predictions(y_new, leaf_assignments, loss_function)

  # Step 2.5: Handle empty leaves (leaves in structure but no observations assigned)
  all_leaf_paths <- structure@leaf_paths
  missing_leaves <- setdiff(all_leaf_paths, names(leaf_preds))

  if (length(missing_leaves) > 0) {
    if (!allow_partial_leaves) {
      stop(sprintf(
        "refit_tree_structure: %d leaf(ves) received no training observations: %s\nThe new data does not cover all leaves of this structure. This indicates a covariate distribution mismatch between training and new data.\nTo allow partial coverage (filling missing leaves with the overall mean), set allow_partial_leaves = TRUE.",
        length(missing_leaves),
        paste(missing_leaves, collapse = ", ")
      ), call. = FALSE)
    }

    # Use overall mean as default for empty leaves (only when allow_partial_leaves = TRUE)
    default_pred <- mean(y_new)

    warning(sprintf(
      "refit_tree_structure: %d leaf(ves) received no training observations: %s\nUsing overall mean (%.4f) for these leaves.",
      length(missing_leaves),
      paste(missing_leaves, collapse = ", "),
      default_pred
    ), call. = FALSE)

    for (leaf_path in missing_leaves) {
      leaf_preds[[leaf_path]] <- default_pred
    }
  }

  # Step 3: Reconstruct tree with new leaf values
  tree_list <- reconstruct_tree_with_leaves(structure, leaf_preds, loss_function)

  # Step 4: Compute predictions and probabilities
  if (loss_function == "squared_error") {
    predictions <- get_fitted_from_tree(tree_list, X_new)
    probabilities <- NULL
    is_regression <- TRUE
    accuracy <- NA_real_
  } else {
    # Classification
    probabilities <- get_probabilities_from_tree(tree_list, X_new)
    predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
    accuracy <- mean(predictions == y_new)
    is_regression <- FALSE
  }

  # Step 5: Preserve or create discretization metadata
  if (is.null(discretization_metadata)) {
    discretization_metadata <- list(
      all_binary = TRUE,
      features = list(),
      original_names = colnames(X_new)
    )
  }

  # Step 6: Create OptimalTreesModel object
  model <- new_optimal_trees_model(
    loss_function = loss_function,
    regularization = 1.0,  # Placeholder (not applicable for refit)
    n_trees = 1L,
    trees = list(tree_list),
    accuracy = accuracy,
    predictions = predictions,
    probabilities = probabilities,
    X_train = if (store_training_data) X_new else NULL,
    y_train = if (store_training_data) y_new else NULL,
    discretization_metadata = discretization_metadata,
    is_regression = is_regression
  )

  # Step 7: Return model + leaf counts (for weighted averaging)
  structure(list(
    model = model,
    n_per_leaf = setNames(as.integer(n_per_leaf), names(n_per_leaf))
  ), class = c("refit_result", "list"))
}

# =============================================================================
# Internal Helper Functions
# =============================================================================

#' Assign Observations to Leaves
#'
#' @description
#' Traverse tree structure for each observation and return leaf assignment.
#'
#' @param structure TreeStructure object
#' @param X Data.frame or matrix of features
#' @return Character vector of leaf IDs (one per observation)
#' @keywords internal
assign_observations_to_leaves <- function(structure, X) {
  n <- nrow(X)
  leaf_ids <- character(n)

  for (i in seq_len(n)) {
    leaf_ids[i] <- traverse_to_leaf(structure, X[i, , drop = FALSE], structure@feature_names)
  }

  leaf_ids
}

#' Traverse to Leaf for Single Observation
#'
#' @param structure TreeStructure object
#' @param x_row Single row data.frame (1 x p)
#' @param feature_names Feature names from training
#' @return Character: leaf ID (path from root)
#' @keywords internal
traverse_to_leaf <- function(structure, x_row, feature_names) {
  # Start at root
  if (length(structure@splits) == 0) {
    # Single-leaf tree
    return("root")
  }

  # Build path by following splits
  path <- integer(0)

  while (TRUE) {
    # Find split at current path
    current_split <- NULL
    for (split in structure@splits) {
      if (identical(split$path, path)) {
        current_split <- split
        break
      }
    }

    # If no split found at current path, we've reached a leaf
    if (is.null(current_split)) {
      # Return path as leaf ID
      if (length(path) == 0) {
        return("root")
      } else {
        return(paste(path, collapse = "-"))
      }
    }

    # Get feature value (0-indexed feature to 1-indexed column)
    # Fall back to integer indexing when feature_names is NULL (store_training_data = FALSE)
    feature_col <- if (!is.null(feature_names)) {
      feature_names[current_split$feature + 1]
    } else {
      current_split$feature + 1L
    }
    feature_val <- as.numeric(x_row[[feature_col]])

    # Evaluate split condition
    if (current_split$relation == "==") {
      goes_right <- (feature_val == current_split$reference)
    } else if (current_split$relation == "<=") {
      goes_right <- (feature_val <= current_split$reference)
    } else if (current_split$relation == ">") {
      goes_right <- (feature_val > current_split$reference)
    } else {
      stop(sprintf("Unknown relation: %s", current_split$relation), call. = FALSE)
    }

    # Follow branch
    if (goes_right) {
      path <- c(path, 1L)
      # Check if right child is a leaf (using explicit flag)
      if (current_split$right_is_leaf) {
        return(paste(path, collapse = "-"))
      }
    } else {
      path <- c(path, 0L)
      # Check if left child is a leaf (using explicit flag)
      if (current_split$left_is_leaf) {
        return(paste(path, collapse = "-"))
      }
    }

    # Continue loop to find split at new path
  }
}

#' Compute Leaf Predictions from Assignments
#'
#' @param y_new Outcome vector
#' @param leaf_assignments Character vector of leaf IDs
#' @param loss_function Loss function ("squared_error", "log_loss", "misclassification")
#' @return Named numeric vector of predictions (names = leaf IDs)
#' @keywords internal
compute_leaf_predictions <- function(y_new, leaf_assignments, loss_function) {
  # Group observations by leaf
  leaf_groups <- split(y_new, leaf_assignments)

  if (loss_function == "squared_error") {
    # Regression: mean per leaf
    leaf_preds <- vapply(leaf_groups, mean, numeric(1))
  } else if (loss_function %in% c("log_loss", "misclassification")) {
    # Classification: empirical probability
    leaf_preds <- vapply(leaf_groups, mean, numeric(1))
  } else {
    stop(sprintf("Unsupported loss_function: %s", loss_function), call. = FALSE)
  }

  leaf_preds
}

#' Reconstruct Tree with New Leaf Values
#'
#' @description
#' Rebuild tree list structure with new leaf predictions.
#' Maintains original structure but updates prediction values.
#'
#' @param structure TreeStructure object
#' @param leaf_predictions Named numeric vector (names = leaf IDs, values = predictions)
#' @param loss_function Character. Loss type ("squared_error", "log_loss", "misclassification").
#' @return Nested list (tree_json format)
#' @keywords internal
reconstruct_tree_with_leaves <- function(structure, leaf_predictions, loss_function) {
  # Base case: single-leaf tree
  if (length(structure@splits) == 0) {
    if (!("root" %in% names(leaf_predictions))) {
      stop(
        "Single-leaf tree reconstruction failed: 'root' prediction not found.\n\nThis indicates a bug in leaf prediction computation.",
        call. = FALSE
      )
    }

    pred_val <- leaf_predictions[["root"]]
    if (is.null(pred_val) || is.na(pred_val)) {
      stop(
        "Single-leaf tree reconstruction failed: 'root' prediction is NULL or NA.\n\nThis indicates a bug in leaf prediction computation.",
        call. = FALSE
      )
    }

    node <- list(prediction = pred_val)
    if (loss_function != "squared_error") {
      node$probabilities <- c(1 - pred_val, pred_val)
    }
    return(node)
  }

  # Recursive case: rebuild from structure
  # Strategy: traverse structure, create tree nodes, assign predictions at leaves
  reconstruct_from_structure(structure@splits[[1]], structure@splits,
                              leaf_predictions, integer(0), loss_function)
}

#' Reconstruct Tree from Structure (Simplified)
#'
#' @param current_split Current split information
#' @param all_splits List of all splits (for lookup)
#' @param leaf_preds Named numeric vector
#' @param path Current path from root
#' @param loss_function Character. Loss type for determining whether to include probabilities.
#' @return Nested list (tree node)
#' @keywords internal
reconstruct_from_structure <- function(current_split, all_splits, leaf_preds, path, loss_function) {
  # Left child
  left_path <- c(path, 0L)
  left_path_str <- paste(left_path, collapse = "-")

  if (current_split$left_is_leaf) {
    # Left is a leaf - get prediction
    if (!(left_path_str %in% names(leaf_preds))) {
      stop(sprintf(
        "Leaf path '%s' not found in predictions.\n\nAvailable: %s\n\nThis indicates a bug in leaf prediction computation.",
        left_path_str,
        paste(names(leaf_preds), collapse = ", ")
      ), call. = FALSE)
    }

    pred <- leaf_preds[[left_path_str]]
    if (is.null(pred) || is.na(pred)) {
      stop(sprintf(
        "Leaf '%s' has NULL/NA prediction.\n\nThis indicates a bug in leaf prediction computation.",
        left_path_str
      ), call. = FALSE)
    }

    false_node <- list(prediction = pred)
    if (loss_function != "squared_error") {
      false_node$probabilities <- c(1 - pred, pred)
    }
  } else {
    # Left is a split - recurse
    left_split <- find_split_for_path(all_splits, left_path)
    false_node <- reconstruct_from_structure(left_split, all_splits, leaf_preds, left_path, loss_function)
  }

  # Right child
  right_path <- c(path, 1L)
  right_path_str <- paste(right_path, collapse = "-")

  if (current_split$right_is_leaf) {
    # Right is a leaf - get prediction
    if (!(right_path_str %in% names(leaf_preds))) {
      stop(sprintf(
        "Leaf path '%s' not found in predictions.\n\nAvailable: %s\n\nThis indicates a bug in leaf prediction computation.",
        right_path_str,
        paste(names(leaf_preds), collapse = ", ")
      ), call. = FALSE)
    }

    pred <- leaf_preds[[right_path_str]]
    if (is.null(pred) || is.na(pred)) {
      stop(sprintf(
        "Leaf '%s' has NULL/NA prediction.\n\nThis indicates a bug in leaf prediction computation.",
        right_path_str
      ), call. = FALSE)
    }

    true_node <- list(prediction = pred)
    if (loss_function != "squared_error") {
      true_node$probabilities <- c(1 - pred, pred)
    }
  } else {
    # Right is a split - recurse
    right_split <- find_split_for_path(all_splits, right_path)
    true_node <- reconstruct_from_structure(right_split, all_splits, leaf_preds, right_path, loss_function)
  }

  # Build split node
  list(
    feature = current_split$feature,
    name = current_split$feature_name,
    relation = current_split$relation,
    reference = current_split$reference,
    false = false_node,
    true = true_node
  )
}

#' Find Split for Given Path
#'
#' @param all_splits List of split information
#' @param target_path Integer vector (path from root)
#' @return Split information, or stops with error if not found
#' @keywords internal
find_split_for_path <- function(all_splits, target_path) {
  # Simple: just match the path field directly
  for (split in all_splits) {
    if (identical(split$path, target_path)) {
      return(split)
    }
  }

  # Not found - this is a bug (split should exist)
  stop(sprintf(
    "Split not found for path '%s'.\n\nAvailable split paths: %s\n\nThis indicates a bug in tree structure extraction.",
    paste(target_path, collapse = "-"),
    paste(sapply(all_splits, function(s) paste(s$path, collapse = "-")),
          collapse = ", ")
  ), call. = FALSE)
}
