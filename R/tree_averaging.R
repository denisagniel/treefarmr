# Tree Averaging Utilities
#
# Functions for averaging leaf values across K trees with the same structure but
# different leaf values (e.g. from cross-fitting). Produces a single interpretable
# tree in optimaltrees' nested-list format. General decision-tree machinery; used by
# downstream packages (e.g. doubletree's averaged-tree ATT estimators) but not
# causal-inference-specific. Relocated from doubletree 2026-07-08.

#' Validate Tree Structure for Averaging
#'
#' Check that a tree object is in the correct nested-list format for averaging.
#' Recursively validates internal nodes and leaf nodes.
#'
#' @param tree Nested list representing a tree
#' @param tree_name Character name for error messages (default "tree")
#'
#' @return NULL if valid, stops with error if invalid
#'
#' @keywords internal
validate_tree_for_averaging <- function(tree, tree_name = "tree") {
  if (!is.list(tree) || length(tree) == 0) {
    stop("Tree '", tree_name, "' must be a non-empty list, got: ", class(tree)[1], call. = FALSE)
  }

  # Check if S7 object (wrong type)
  if (inherits(tree, "S7_object")) {
    stop("Tree '", tree_name, "' is an S7 object (", class(tree)[1], "). ",
         "Expected nested list with fields like 'feature', 'true', 'false'. ",
         "Hint: Use model@trees[[1]] instead of extract_tree_structure(model).",
         call. = FALSE)
  }

  # Check for required fields
  is_leaf <- !is.null(tree$prediction)
  is_internal <- !is.null(tree$feature)

  if (!is_leaf && !is_internal) {
    stop("Tree '", tree_name, "' has neither 'prediction' (leaf) nor 'feature' (internal node). ",
         "Available fields: ", paste(names(tree), collapse = ", "), call. = FALSE)
  }

  # Validate leaf node
  if (is_leaf) {
    if (is.null(tree$probabilities) || length(tree$probabilities) != 2) {
      stop("Leaf node in '", tree_name, "' missing 'probabilities' or wrong length. ",
           "Expected c(p0, p1), got length: ", length(tree$probabilities), call. = FALSE)
    }
  }

  # Validate internal node
  if (is_internal) {
    if (is.null(tree$true) || is.null(tree$false)) {
      stop("Internal node in '", tree_name, "' missing 'true' or 'false' children.", call. = FALSE)
    }
    # Recursively validate children
    validate_tree_for_averaging(tree$true, paste0(tree_name, "$true"))
    validate_tree_for_averaging(tree$false, paste0(tree_name, "$false"))
  }

  NULL  # Valid
}

#' Extract Leaf Values from a Tree
#'
#' Recursively traverse a tree (nested list structure) and extract
#' all leaf values with their paths from the root.
#'
#' @param tree_node Nested list representing a tree (from refit_structure_on_data).
#'   Internal nodes have: feature, relation, reference, true, false.
#'   Leaf nodes have: prediction, probabilities.
#' @param path Integer vector tracking current path from root (internal use)
#'
#' @return Named numeric vector where names are leaf paths (e.g., "0-1", "1-0-1")
#'   and values are probabilities P(Y=1|leaf) for binary outcomes
#'
#' @export
extract_leaf_values <- function(tree_node, path = integer(0)) {
  # Validate input
  if (!is.list(tree_node) || length(tree_node) == 0) {
    stop("tree_node must be a non-empty list", call. = FALSE)
  }

  # Check if this is a leaf (has 'prediction' field)
  if (!is.null(tree_node$prediction)) {
    # This is a leaf
    if (is.null(tree_node$probabilities) || length(tree_node$probabilities) != 2) {
      stop("Leaf node missing 'probabilities' or wrong length. Expected c(p0, p1).", call. = FALSE)
    }

    # Extract P(Y=1) from probabilities = c(P(Y=0), P(Y=1))
    # Note: probabilities might be a list from JSON, convert to numeric vector
    probs <- as.numeric(tree_node$probabilities)
    p1 <- probs[2]

    # Create path string
    if (length(path) == 0) {
      path_str <- "root"
    } else {
      path_str <- paste(path, collapse = "-")
    }

    # Return named numeric vector (not list)
    result <- setNames(p1, path_str)
    return(result)
  }

  # This is an internal node (has 'feature')
  if (is.null(tree_node$feature)) {
    stop("Internal node must have 'feature' field", call. = FALSE)
  }

  if (is.null(tree_node$true) || is.null(tree_node$false)) {
    stop("Internal node must have 'true' and 'false' children", call. = FALSE)
  }

  # Recursively extract from left (false) and right (true) children
  # Path convention: 0 = left/false, 1 = right/true
  left_values <- extract_leaf_values(tree_node$false, c(path, 0L))
  right_values <- extract_leaf_values(tree_node$true, c(path, 1L))

  # Combine and return
  c(left_values, right_values)
}

#' Average Leaf Values Across K Trees (Weighted by Sample Size)
#'
#' Given K trees with the same structure but different leaf values,
#' compute weighted average leaf values where weights are the number
#' of observations in each leaf.
#'
#' @param tree_list List of K trees (each from refit_structure_on_data or fold_refits)
#' @param weight_list List of K weight vectors. Each element is a named integer
#'   vector where names are leaf IDs and values are observation counts.
#'
#' @return Named numeric vector of weighted averaged leaf values
#'
#' @keywords internal
average_leaf_values <- function(tree_list, weight_list) {
  # Validate input
  if (!is.list(tree_list) || length(tree_list) == 0) {
    stop("tree_list must be a non-empty list of trees", call. = FALSE)
  }

  if (!is.list(weight_list) || length(weight_list) == 0) {
    stop("weight_list must be a non-empty list of weight vectors", call. = FALSE)
  }

  if (length(tree_list) != length(weight_list)) {
    stop("tree_list and weight_list must have same length", call. = FALSE)
  }

  K <- length(tree_list)

  # Extract leaf values from each tree with better error handling
  leaf_values_list <- vector("list", K)
  for (i in seq_len(K)) {
    leaf_values_list[[i]] <- tryCatch({
      extract_leaf_values(tree_list[[i]])
    }, error = function(e) {
      stop("Failed to extract leaf values from tree ", i, " of ", K,
           ": ", e$message, call. = FALSE)
    })
  }

  # Get union of all leaf paths across trees
  all_paths <- unique(unlist(lapply(leaf_values_list, names)))

  # Compute weighted mean for each leaf
  weighted_means <- numeric(length(all_paths))
  names(weighted_means) <- all_paths

  for (path in all_paths) {
    sum_weighted <- 0
    sum_weights <- 0

    for (k in 1:K) {
      # Does this tree have this leaf?
      if (path %in% names(leaf_values_list[[k]])) {
        value_k <- leaf_values_list[[k]][[path]]

        # Get weight (could be 0 if leaf collapsed)
        weight_k <- if (path %in% names(weight_list[[k]])) {
          weight_list[[k]][[path]]
        } else {
          0
        }

        sum_weighted <- sum_weighted + (value_k * weight_k)
        sum_weights <- sum_weights + weight_k
      }
    }

    if (sum_weights > 0) {
      weighted_means[[path]] <- sum_weighted / sum_weights
    } else {
      # Every tree collapsed this leaf (zero observations in all K folds) → the
      # averaged value is undefined. Fail loudly rather than invent a neutral 0.5,
      # which is nonsensical (esp. for a continuous m0) and silently biases estimates.
      stop("average_leaf_values: leaf '", path, "' has zero total weight across all ",
           K, " trees (collapsed in every fold). This indicates a degenerate averaged ",
           "structure; check the modal-structure selection and fold sizes.",
           call. = FALSE)
    }
  }

  return(weighted_means)
}

#' Rebuild Tree with Averaged Leaf Values
#'
#' Rebuild a tree structure with averaged leaf values.
#' Uses the structure from the first tree and replaces all leaf values
#' with the averaged values.
#'
#' @param tree_template First tree (used for structure)
#' @param averaged_values Named numeric vector from average_leaf_values()
#' @param path Integer vector tracking current path from root (internal use)
#'
#' @return Tree (nested list) with averaged leaf values
#'
#' @keywords internal
rebuild_tree_with_averaged_values <- function(tree_template, averaged_values, path = integer(0)) {
  # Validate inputs
  if (!is.list(tree_template) || length(tree_template) == 0) {
    stop("tree_template must be a non-empty list", call. = FALSE)
  }

  if (!is.numeric(averaged_values) || is.null(names(averaged_values))) {
    stop("averaged_values must be a named numeric vector", call. = FALSE)
  }

  # Check if this is a leaf
  if (!is.null(tree_template$prediction)) {
    # Create path string
    if (length(path) == 0) {
      path_str <- "root"
    } else {
      path_str <- paste(path, collapse = "-")
    }

    # Get averaged value for this leaf
    if (!(path_str %in% names(averaged_values))) {
      stop("Leaf path '", path_str, "' not found in averaged_values.\n",
           "Available: ", paste(names(averaged_values), collapse = ", "),
           call. = FALSE)
    }

    p1_avg <- averaged_values[[path_str]]

    # Validate probability
    if (p1_avg < 0 || p1_avg > 1) {
      stop("Averaged probability out of [0,1]: ", p1_avg, " at path ", path_str,
           call. = FALSE)
    }

    # Return leaf node with averaged value
    return(list(
      prediction = as.integer(p1_avg >= 0.5),
      probabilities = c(1 - p1_avg, p1_avg)
    ))
  }

  # This is an internal node - rebuild with same structure
  if (is.null(tree_template$feature)) {
    stop("Internal node must have 'feature' field", call. = FALSE)
  }

  # Recursively rebuild children
  false_child <- rebuild_tree_with_averaged_values(
    tree_template$false, averaged_values, c(path, 0L)
  )
  true_child <- rebuild_tree_with_averaged_values(
    tree_template$true, averaged_values, c(path, 1L)
  )

  # Return internal node with same split, averaged children
  list(
    feature = tree_template$feature,
    relation = tree_template$relation,
    reference = tree_template$reference,
    false = false_child,
    true = true_child
  )
}

#' Average Trees with Same Structure (Weighted by Sample Size)
#'
#' Takes K trees with the same structure but different leaf values (from cross-fitting),
#' averages the leaf values weighted by sample size, and returns a single tree.
#' This maintains cross-fit validity while producing one interpretable tree.
#'
#' @param tree_list List of K trees (from fold_refits). Each tree should be a nested list
#'   from \code{optimaltrees::refit_structure_on_data()} or similar.
#' @param weight_list List of K weight vectors (sample counts per leaf).
#'   Each element is a named integer vector from \code{refit_result$n_per_leaf}.
#'
#' @return Single tree (nested list) with weighted averaged leaf values
#'
#' @details
#' This function is used by \code{\link{estimate_att_doubletree_averaged}} and
#' \code{\link{estimate_att_msplit_averaged}} to create a single interpretable tree
#' from K cross-fitted trees.
#'
#' The weighted averaging pools all observations across M×K training folds:
#' \code{averaged_leaf_value = sum(n_k × value_k) / sum(n_k)}
#'
#' This is superior to simple averaging because:
#' - Leaves with more observations get more weight (theoretically sound)
#' - Naturally handles empty leaves (weight = 0)
#' - Solves structure mismatch problem (leaves that collapse get zero weight)
#'
#' The averaging maintains cross-fit validity because each tree's leaf values were
#' fitted on training data (excluding the test fold), so averaging them preserves
#' the cross-fit property.
#'
#' @seealso \code{\link{estimate_att_doubletree_averaged}}, \code{\link{estimate_att_msplit_averaged}}
#'
#' @export
average_trees <- function(tree_list, weight_list) {
  # Validate
  if (!is.list(tree_list) || length(tree_list) == 0) {
    stop("tree_list must be a non-empty list", call. = FALSE)
  }

  if (!is.list(weight_list) || length(weight_list) != length(tree_list)) {
    stop("weight_list must be a list with same length as tree_list", call. = FALSE)
  }

  # Validate each tree structure
  for (i in seq_along(tree_list)) {
    tryCatch({
      validate_tree_for_averaging(tree_list[[i]], tree_name = paste0("tree_", i))
    }, error = function(e) {
      stop("Tree ", i, " in tree_list is invalid: ", e$message, call. = FALSE)
    })
  }

  # Compute weighted averaged leaf values
  averaged_values <- average_leaf_values(tree_list, weight_list)

  # Rebuild tree using first tree as template
  single_tree <- rebuild_tree_with_averaged_values(tree_list[[1]], averaged_values)

  return(single_tree)
}

#' Predict from an averaged tree
#'
#' Predict P(Y=1|X) (binary) or the leaf value for new observations using a tree in
#' optimaltrees' nested-list format, e.g. the output of \code{\link{average_trees}}.
#' Traverses splits on binary/discretized features.
#'
#' @param tree Tree (nested list), e.g. from \code{average_trees()}.
#' @param X Data.frame of covariates (in the tree's feature space; discretize first
#'   with \code{\link{apply_discretization}} if the tree was built on binned features).
#'
#' @return Numeric vector of predictions, one per row of \code{X}.
#' @seealso \code{\link{average_trees}}, \code{\link{apply_discretization}}
#' @export
predict_averaged_tree <- function(tree, X) {
  n <- nrow(X)
  predictions <- numeric(n)

  for (i in 1:n) {
    predictions[i] <- predict_averaged_tree_single(tree, X[i, , drop = FALSE])
  }

  return(predictions)
}

#' Predict for Single Observation
#'
#' Traverse tree to get prediction for one observation.
#'
#' @param tree_node Current node (nested list)
#' @param x_row Single-row data.frame
#'
#' @return Numeric P(Y=1)
#'
#' @keywords internal
predict_averaged_tree_single <- function(tree_node, x_row) {
  # Leaf?
  if (!is.null(tree_node$prediction)) {
    return(tree_node$probabilities[2])  # P(Y=1)
  }

  # Internal node - evaluate split
  feature_idx <- tree_node$feature + 1  # 0-indexed to 1-indexed
  feature_val <- as.numeric(x_row[[feature_idx]])

  # For binary features with relation "=="
  if (tree_node$relation == "==") {
    # reference is typically 1 for binary
    if (abs(feature_val - tree_node$reference) < 1e-10) {
      # Goes right (true)
      return(predict_averaged_tree_single(tree_node$true, x_row))
    } else {
      # Goes left (false)
      return(predict_averaged_tree_single(tree_node$false, x_row))
    }
  } else if (tree_node$relation == "<=") {
    if (feature_val <= tree_node$reference) {
      return(predict_averaged_tree_single(tree_node$true, x_row))
    } else {
      return(predict_averaged_tree_single(tree_node$false, x_row))
    }
  } else if (tree_node$relation == ">") {
    if (feature_val > tree_node$reference) {
      return(predict_averaged_tree_single(tree_node$true, x_row))
    } else {
      return(predict_averaged_tree_single(tree_node$false, x_row))
    }
  } else {
    stop("Unknown relation: ", tree_node$relation, call. = FALSE)
  }
}
