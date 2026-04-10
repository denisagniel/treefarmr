#' Tree Structure Operations for M-Split
#'
#' @description
#' Functions for extracting, comparing, and refitting tree structures.
#' Enables M-split algorithm: select modal structure across M independent
#' cross-fits, refit on all splits, average predictions.
#'
#' @details
#' Tree structure: Split decisions only (no leaf values).
#' - Internal nodes: feature, relation (always "=="), reference (always 1 for binary)
#' - Leaf identification: path from root (e.g., "0-1-0" = left-right-left)
#'
#' This separates structure (which splits) from parameters (leaf values),
#' allowing structure selection and refitting.
#'
#' @name tree_structure
NULL

# =============================================================================
# S7 Class: TreeStructure
# =============================================================================

#' Tree Structure (Structure Only, No Leaf Values)
#'
#' @description
#' S7 class representing tree structure without leaf values.
#' Used for M-split algorithm: select one structure, refit multiple times.
#'
#' @details
#' Properties:
#' - splits: List of split information (path, feature, relation, reference, leaf flags)
#' - leaf_paths: Character vector of leaf paths from root
#' - n_leaves: Number of leaves
#' - max_depth: Maximum depth
#' - feature_names: Feature names from training data
#'
#' @export
TreeStructure <- S7::new_class(
  name = "TreeStructure",
  package = "optimaltrees",

  properties = list(
    splits = S7::class_list,           # List of split information
    leaf_paths = S7::class_character,  # Leaf paths (renamed from leaf_ids)
    n_leaves = S7::class_integer,
    max_depth = S7::class_integer,
    feature_names = S7::class_character
  ),

  validator = function(self) {
    # Leaf count validation
    if (self@n_leaves < 1) {
      return("@n_leaves must be at least 1")
    }

    if (length(self@leaf_paths) != self@n_leaves) {
      return(sprintf("length(@leaf_paths) = %d must equal @n_leaves = %d",
                     length(self@leaf_paths), self@n_leaves))
    }

    # Depth validation
    if (self@max_depth < 0) {
      return("@max_depth must be non-negative")
    }

    # Feature names validation
    if (length(self@feature_names) == 0) {
      return("@feature_names must not be empty")
    }

    # Splits validation
    if (length(self@splits) > 0) {
      # Check each split has required fields (including new ones)
      required_fields <- c("path", "feature", "feature_name", "relation",
                           "reference", "left_is_leaf", "right_is_leaf")
      for (i in seq_along(self@splits)) {
        split <- self@splits[[i]]
        missing <- setdiff(required_fields, names(split))
        if (length(missing) > 0) {
          return(sprintf("Split %d missing fields: %s",
                         i, paste(missing, collapse = ", ")))
        }
      }
    }

    NULL
  }
)

# Print method for TreeStructure
S7::method(print, TreeStructure) <- function(x, ...) {
  cat("TreeStructure\n")
  cat("=============\n")
  cat(sprintf("Leaves: %d\n", x@n_leaves))
  cat(sprintf("Depth:  %d\n", x@max_depth))
  cat(sprintf("Splits: %d\n", length(x@splits)))
  cat(sprintf("Features: %s\n", paste(x@feature_names, collapse = ", ")))

  if (length(x@splits) > 0) {
    cat("\nSplit Summary:\n")
    for (i in seq_len(min(5, length(x@splits)))) {
      s <- x@splits[[i]]
      cat(sprintf("  %d. %s %s %.4f\n",
                  i, s$feature_name, s$relation, s$reference))
    }
    if (length(x@splits) > 5) {
      cat(sprintf("  ... and %d more splits\n", length(x@splits) - 5))
    }
  }

  invisible(x)
}

# =============================================================================
# Extract Tree Structure
# =============================================================================

#' Extract Tree Structure from Model
#'
#' @description
#' Extracts the tree structure (split decisions only) from an OptimalTreesModel,
#' excluding leaf prediction values. The structure can be used for comparison
#' across models or for refitting with new data.
#'
#' @param model OptimalTreesModel object (S7 class)
#' @param tree_index Which tree to extract (default 1)
#' @return TreeStructure object (S7 class)
#'
#' @details
#' The structure includes:
#' - All split nodes with feature, relation, reference
#' - Leaf identifiers (paths from root)
#' - Metadata (n_leaves, max_depth, feature_names)
#'
#' Leaf values are NOT included in the structure.
#'
#' @examples
#' \dontrun{
#' # Fit a tree
#' model <- fit_tree(X, y, loss_function = "log_loss")
#'
#' # Extract structure
#' structure <- extract_tree_structure(model)
#' print(structure)
#' }
#'
#' @export
extract_tree_structure <- function(model, tree_index = 1) {
  # Validate input
  if (!S7::S7_inherits(model, OptimalTreesModel)) {
    stop("model must be an OptimalTreesModel (S7 class)", call. = FALSE)
  }

  if (tree_index < 1 || tree_index > model@n_trees) {
    stop(sprintf("tree_index must be between 1 and %d (number of trees)",
                 model@n_trees), call. = FALSE)
  }

  # Get tree from model
  tree <- model@trees[[tree_index]]

  # Extract structure recursively
  structure <- extract_structure_recursive(tree, path = integer(0))

  # Create TreeStructure object
  TreeStructure(
    splits = structure$splits,
    leaf_paths = structure$leaf_paths,
    n_leaves = as.integer(structure$n_leaves),
    max_depth = as.integer(structure$max_depth),
    feature_names = colnames(model@X_train)
  )
}

#' Extract Structure Recursively (Internal Helper)
#'
#' @param node Current tree node (list)
#' @param path Path from root (integer vector of 0s and 1s)
#' @return List with splits, leaf_paths, n_leaves, max_depth
#' @keywords internal
extract_structure_recursive <- function(node, path) {
  # Base case: leaf node
  if (!is.null(node$prediction)) {
    leaf_path <- if (length(path) == 0) "root" else paste(path, collapse = "-")
    return(list(
      splits = list(),
      leaf_paths = leaf_path,
      n_leaves = 1L,
      max_depth = length(path)
    ))
  }

  # Recursive case: split node
  if (is.null(node$feature)) {
    stop("Tree node is neither leaf nor split (missing both prediction and feature)",
         call. = FALSE)
  }

  # Recurse left and right
  left_path <- c(path, 0L)
  right_path <- c(path, 1L)

  left_struct <- extract_structure_recursive(node$false, left_path)
  right_struct <- extract_structure_recursive(node$true, right_path)

  # Create split information WITH EXPLICIT PATH
  split_info <- list(
    path = path,                                    # NEW: split's own path
    feature = as.integer(node$feature),
    feature_name = if (!is.null(node$name)) node$name else
                   sprintf("feature_%d", node$feature),
    relation = if (!is.null(node$relation)) node$relation else "==",
    reference = if (!is.null(node$reference)) as.numeric(node$reference) else 1.0,
    left_is_leaf = !is.null(node$false$prediction),   # NEW: explicit leaf flag
    right_is_leaf = !is.null(node$true$prediction)    # NEW: explicit leaf flag
  )

  # Combine results
  list(
    splits = c(list(split_info), left_struct$splits, right_struct$splits),
    leaf_paths = c(left_struct$leaf_paths, right_struct$leaf_paths),
    n_leaves = left_struct$n_leaves + right_struct$n_leaves,
    max_depth = max(left_struct$max_depth, right_struct$max_depth)
  )
}

# =============================================================================
# Compare Structures
# =============================================================================

#' Compare Two Tree Structures
#'
#' @description
#' Determines if two TreeStructure objects represent identical tree structures.
#' Compares split decisions (feature, relation, reference) but not leaf values.
#'
#' @param structure1 TreeStructure object
#' @param structure2 TreeStructure object
#' @param tol Numerical tolerance for reference values (default 1e-8)
#' @return Logical: TRUE if structures are identical, FALSE otherwise
#'
#' @details
#' Two structures are identical if:
#' - Same number of leaves
#' - Same number of splits
#' - All splits match (feature, relation, reference within tol)
#' - Splits are in the same order (depth-first traversal)
#'
#' @examples
#' \dontrun{
#' model1 <- fit_tree(X, y, seed = 1)
#' model2 <- fit_tree(X, y, seed = 1)
#' s1 <- extract_tree_structure(model1)
#' s2 <- extract_tree_structure(model2)
#' compare_structures(s1, s2)  # Should be TRUE (same seed)
#' }
#'
#' @export
compare_structures <- function(structure1, structure2, tol = 1e-8) {
  # Quick checks
  if (!S7::S7_inherits(structure1, TreeStructure) ||
      !S7::S7_inherits(structure2, TreeStructure)) {
    stop("Both arguments must be TreeStructure objects", call. = FALSE)
  }

  if (structure1@n_leaves != structure2@n_leaves) {
    return(FALSE)
  }

  if (length(structure1@splits) != length(structure2@splits)) {
    return(FALSE)
  }

  # Compare splits one by one
  for (i in seq_along(structure1@splits)) {
    s1 <- structure1@splits[[i]]
    s2 <- structure2@splits[[i]]

    # Compare feature
    if (s1$feature != s2$feature) {
      return(FALSE)
    }

    # Compare relation
    if (s1$relation != s2$relation) {
      return(FALSE)
    }

    # Compare reference (within tolerance)
    if (abs(s1$reference - s2$reference) > tol) {
      return(FALSE)
    }
  }

  # All checks passed
  TRUE
}

#' Compute Hash of Tree Structure
#'
#' @description
#' Computes a deterministic hash of a tree structure for fast comparison.
#' Structures with the same hash are identical (with high probability).
#'
#' @param structure TreeStructure object
#' @param precision Decimal precision for reference values (default 8)
#' @return Character string (xxhash64 digest)
#'
#' @details
#' The hash is computed from a string representation of all splits:
#' "feature_relation_reference|feature_relation_reference|..."
#'
#' This allows O(1) comparison via hash table lookup instead of O(n) comparison.
#'
#' @examples
#' \dontrun{
#' s1 <- extract_tree_structure(model1)
#' s2 <- extract_tree_structure(model2)
#'
#' # Fast comparison via hash
#' structure_hash(s1) == structure_hash(s2)
#'
#' # Build hash table for many structures
#' hashes <- sapply(structures, structure_hash)
#' counts <- table(hashes)  # Count frequency of each structure
#' }
#'
#' @export
structure_hash <- function(structure, precision = 8) {
  if (!S7::S7_inherits(structure, TreeStructure)) {
    stop("structure must be a TreeStructure object", call. = FALSE)
  }

  if (length(structure@splits) == 0) {
    # Single-leaf tree (no splits)
    return(digest::digest("single-leaf", algo = "xxhash64"))
  }

  # Create deterministic string representation
  str_rep <- paste(
    vapply(structure@splits, function(s) {
      sprintf("f%d_%s_%.8f",
              s$feature,
              s$relation,
              round(s$reference, precision))
    }, character(1)),
    collapse = "|"
  )

  digest::digest(str_rep, algo = "xxhash64")
}
