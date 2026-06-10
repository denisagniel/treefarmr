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
    feature_names = S7::class_character,
    partition_hash = S7::class_character  # Partition-based hash
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

    # Partition hash validation
    if (length(self@partition_hash) != 1 || nchar(self@partition_hash) == 0) {
      return("@partition_hash must be a non-empty string")
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

  # Compute partition-based hash
  partition_hash_val <- partition_hash(tree)

  # Create TreeStructure object
  feature_names <- if (!is.null(model@X_train)) colnames(model@X_train) else NULL
  TreeStructure(
    splits = structure$splits,
    leaf_paths = structure$leaf_paths,
    n_leaves = as.integer(structure$n_leaves),
    max_depth = as.integer(structure$max_depth),
    feature_names = feature_names,
    partition_hash = partition_hash_val
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
# Partition-Based Structure Comparison
# =============================================================================

#' Extract Leaf Partitions from Tree
#'
#' @description
#' Extract the partition defined by a tree: the set of all leaf regions.
#' Each region is a conjunction of split conditions from root to leaf.
#'
#' @param tree_node Nested list representing tree (from model@trees[[1]])
#' @param conditions_so_far List of conditions accumulated from root (internal use)
#' @param path Integer vector of path from root (internal use)
#'
#' @return List of leaf regions. Each region is a list with:
#'   \item{conditions}{Data frame with columns: feature, relation, reference}
#'   \item{path_str}{Character path for debugging (e.g., "0-1-0")}
#'
#' @keywords internal
extract_tree_partition <- function(tree_node, conditions_so_far = list(), path = integer(0)) {
  # Check if leaf
  if (!is.null(tree_node$prediction)) {
    # This is a leaf - return its conditions
    conditions_df <- if (length(conditions_so_far) > 0) {
      do.call(rbind, lapply(conditions_so_far, as.data.frame))
    } else {
      data.frame(feature = integer(0), relation = character(0), reference = numeric(0))
    }

    # Sort conditions by (feature, relation, reference) for canonical form
    if (nrow(conditions_df) > 0) {
      conditions_df <- conditions_df[order(conditions_df$feature, conditions_df$relation, conditions_df$reference), ]
    }

    path_str <- if (length(path) == 0) "root" else paste(path, collapse = "-")

    return(list(list(
      conditions = conditions_df,
      path_str = path_str
    )))
  }

  # Internal node - recurse
  feature_idx <- tree_node$feature
  relation <- tree_node$relation
  reference <- tree_node$reference

  # Left child (false): condition is NOT satisfied
  # For "==": goes left if feature != reference
  # For "<=": goes left if feature > reference
  left_cond <- if (relation == "==") {
    list(feature = feature_idx, relation = "!=", reference = reference)
  } else if (relation == "<=") {
    list(feature = feature_idx, relation = ">", reference = reference)
  } else {
    stop("Unknown relation: ", relation, call. = FALSE)
  }

  # Right child (true): condition IS satisfied
  right_cond <- list(feature = feature_idx, relation = relation, reference = reference)

  # Recurse
  left_leaves <- extract_tree_partition(tree_node$false, c(conditions_so_far, list(left_cond)), c(path, 0L))
  right_leaves <- extract_tree_partition(tree_node$true, c(conditions_so_far, list(right_cond)), c(path, 1L))

  c(left_leaves, right_leaves)
}

#' Compute Partition-Based Structure Hash
#'
#' @description
#' Hash a tree based on its partition (leaf regions), not split sequence.
#' Trees with the same partition get the same hash.
#'
#' @param tree_node Nested list representing tree
#' @param precision Number of digits for rounding reference values (default 8)
#'
#' @return Character hash
#'
#' @keywords internal
partition_hash <- function(tree_node, precision = 8) {
  # Extract leaf regions
  leaves <- extract_tree_partition(tree_node)

  # Create canonical string for each leaf
  leaf_strings <- vapply(leaves, function(leaf) {
    if (nrow(leaf$conditions) == 0) {
      return("root")
    }

    # Create condition strings: "f0_==_1.00000000&f2_==_0.00000000"
    cond_strs <- apply(leaf$conditions, 1, function(row) {
      sprintf("f%d_%s_%.8f",
              as.integer(row["feature"]),
              row["relation"],
              round(as.numeric(row["reference"]), precision))
    })

    paste(cond_strs, collapse = "&")
  }, character(1))

  # Sort leaf strings for canonical ordering
  leaf_strings <- sort(leaf_strings)

  # Hash the sorted list
  partition_str <- paste(leaf_strings, collapse = "|")
  digest::digest(partition_str, algo = "xxhash64")
}

# =============================================================================
# Compare Structures
# =============================================================================

#' Compare Two Tree Structures (Partition-Based)
#'
#' @description
#' Determines if two TreeStructure objects represent equivalent tree structures.
#' Uses partition-based comparison: trees are equal if they define the same
#' set of leaf regions, regardless of split order.
#'
#' @param structure1 TreeStructure object
#' @param structure2 TreeStructure object
#' @param tol Unused (kept for API compatibility)
#' @return Logical: TRUE if structures are equivalent, FALSE otherwise
#'
#' @details
#' Two trees are considered equivalent if they partition the feature space
#' into the same regions, even if they achieve this via different split sequences.
#'
#' Example: These two trees are equivalent:
#'   Tree A: Root splits on x1, then left splits on x2
#'   Tree B: Root splits on x2, then left splits on x1
#'   (if they create the same 4 leaf regions)
#'
#' The comparison is based on partition hashes, which are computed when
#' the TreeStructure is created.
#'
#' @examples
#' \dontrun{
#' model1 <- fit_tree(X, y, seed = 1)
#' model2 <- fit_tree(X, y, seed = 1)
#' s1 <- extract_tree_structure(model1)
#' s2 <- extract_tree_structure(model2)
#' compare_structures(s1, s2)  # TRUE if same partition
#' }
#'
#' @export
compare_structures <- function(structure1, structure2, tol = 1e-8) {
  # Validate input
  if (!S7::S7_inherits(structure1, TreeStructure) ||
      !S7::S7_inherits(structure2, TreeStructure)) {
    stop("Both arguments must be TreeStructure objects", call. = FALSE)
  }

  # Compare partition hashes
  structure1@partition_hash == structure2@partition_hash
}

#' Compute Hash of Tree Structure (Partition-Based)
#'
#' @description
#' Returns the partition-based hash for a TreeStructure.
#' Two structures with the same partition (same leaves) get the same hash,
#' regardless of split order.
#'
#' @param structure TreeStructure object
#' @param precision Unused (kept for API compatibility)
#' @return Character string (xxhash64 digest)
#'
#' @details
#' The hash is based on the partition defined by the tree: the set of all
#' leaf regions. Trees are considered equivalent if they partition the
#' feature space into the same regions, even if they achieve this via
#' different split sequences.
#'
#' Example: These two trees get the same hash:
#'   Tree A: Root splits on x1, then left splits on x2
#'   Tree B: Root splits on x2, then left splits on x1
#'   (if they create the same 4 leaf regions)
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

  # Return pre-computed partition hash
  structure@partition_hash
}
