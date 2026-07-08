# Structure Selection Utilities
#
# General decision-tree structure utilities: modal-structure selection across a set
# of fitted structures (e.g. from M sample splits or Rashomon folds) and structure
# diversity metrics. Built on structure_hash()/TreeStructure. Not causal-inference
# specific; used by downstream packages (e.g. doubletree's M-split estimators).
# Relocated from doubletree 2026-07-08.

#' Select Modal Structure from List
#'
#' @description
#' Given a list of TreeStructure objects, identify the modal (most frequent)
#' structure using hash-based comparison.
#'
#' @param structures List of TreeStructure objects (from \code{extract_tree_structure}),
#'   or a list of \code{list(structure = <TreeStructure>, discretization_metadata = ...)}.
#' @param min_leaves Integer. Minimum leaf count for a structure to be eligible to win
#'   the modal vote. Prevents degenerate stumps (1 leaf) from winning when non-stump
#'   structures disagree; falls back to including stumps only if every structure has
#'   fewer than \code{min_leaves} leaves. Default 1L (no exclusion).
#' @return List with:
#'   \item{structure}{TreeStructure object (modal structure)}
#'   \item{frequency}{Numeric: proportion of structures matching modal (in [0, 1])}
#'   \item{counts}{Named integer vector: frequency of each unique structure}
#'   \item{hash}{Character: hash of modal structure}
#'
#' @details
#' Uses \code{structure_hash()} for O(1) comparison via hash table instead of O(M^2)
#' pairwise structure comparisons. Ties (multiple structures with the same frequency)
#' resolve to the first encountered.
#'
#' @examples
#' \dontrun{
#' # Fit M models on different splits
#' structures <- vector("list", M)
#' for (m in 1:M) {
#'   model <- fit_tree(X[[m]], y[[m]])
#'   structures[[m]] <- extract_tree_structure(model)
#' }
#'
#' # Select modal structure
#' modal <- select_structure_modal(structures)
#' cat(sprintf("Modal frequency: %.1f%%\n", modal$frequency * 100))
#' }
#'
#' @export
select_structure_modal <- function(structures, min_leaves = 1L) {
  if (!is.list(structures) || length(structures) == 0) {
    stop("structures must be a non-empty list", call. = FALSE)
  }

  # Detect format: new (list with structure + metadata) or old (TreeStructure directly)
  first_elem <- structures[[1]]
  is_new_format <- is.list(first_elem) &&
                   !S7::S7_inherits(first_elem, TreeStructure) &&
                   "structure" %in% names(first_elem)

  # Extract TreeStructure objects for comparison
  if (is_new_format) {
    # New format: list(structure = TreeStructure, discretization_metadata = ...)
    tree_structures <- lapply(structures, function(s) s$structure)
    metadata_list <- lapply(structures, function(s) s$discretization_metadata)
  } else {
    # Old format: TreeStructure directly (backward compatibility)
    tree_structures <- structures
    metadata_list <- NULL
  }

  # Check all elements are TreeStructure
  is_tree_struct <- vapply(tree_structures, function(s) {
    S7::S7_inherits(s, TreeStructure)
  }, logical(1))

  if (!all(is_tree_struct)) {
    stop("All elements must be TreeStructure objects or lists with 'structure' field", call. = FALSE)
  }

  # Compute hashes and leaf counts
  hashes <- vapply(tree_structures, structure_hash, character(1))
  n_leaves_each <- vapply(tree_structures, function(s) s@n_leaves, integer(1))

  # Count frequencies
  counts <- table(hashes)

  # Find modal hash: prefer structures with >= min_leaves leaves.
  # This prevents degenerate stump structures (1 leaf) from winning the modal
  # vote when non-stump structures disagree with each other. Only falls back
  # to including stumps if all structures satisfy n_leaves < min_leaves.
  if (min_leaves > 1L) {
    valid_idx <- which(n_leaves_each >= min_leaves)
    if (length(valid_idx) > 0) {
      valid_counts <- table(hashes[valid_idx])
      modal_hash <- names(valid_counts)[which.max(valid_counts)]
    } else {
      # All structures are degenerate (e.g. all stumps) - use regular modal
      modal_hash <- names(counts)[which.max(counts)]
    }
  } else {
    modal_hash <- names(counts)[which.max(counts)]
  }
  modal_idx <- which(hashes == modal_hash)[1]  # First occurrence

  # Return in new format (with metadata if available)
  result <- list(
    structure = tree_structures[[modal_idx]],
    frequency = as.numeric(max(counts)) / length(structures),
    counts = counts,
    hash = modal_hash
  )

  # Add discretization_metadata if available
  if (is_new_format) {
    result$discretization_metadata <- metadata_list[[modal_idx]]
  }

  result
}

#' Functional consistency of predictions across replicates
#'
#' @description
#' For observations with identical covariates (\eqn{X_i = X_j}), measure the maximum
#' difference in replicate-averaged predictions: \eqn{\max |\bar\mu(X_i) - \bar\mu(X_j)|}.
#' This is ~0 for a functionally consistent estimator; large values mean averaging
#' across replicates (e.g. M sample splits) has not eliminated randomness for tied
#' covariate patterns. Generic (two prediction matrices + X); used by M-split
#' diagnostics but not causal-specific. Relocated from doubletree 2026-07-08.
#'
#' @param predictions_a Numeric matrix (n x M) of first-quantity predictions across
#'   M replicates (e.g. propensity in a causal application).
#' @param predictions_b Numeric matrix (n x M) of second-quantity predictions (e.g.
#'   control outcome).
#' @param X Covariate matrix/data.frame (rows define the tied-pattern grouping).
#'
#' @return List with \code{max_diff_a}, \code{max_diff_b}, \code{n_unique_patterns},
#'   \code{n_groups_with_ties}, \code{avg_group_size}.
#' @export
compute_functional_consistency <- function(predictions_a, predictions_b, X) {
  n <- nrow(X)

  # Group observations by covariate pattern (row -> string for fast grouping).
  if (is.matrix(X)) {
    X_str <- apply(X, 1, function(row) paste(row, collapse = "_"))
  } else {
    X_str <- apply(as.matrix(X), 1, function(row) paste(row, collapse = "_"))
  }

  groups <- split(seq_len(n), X_str)

  max_diff_a <- 0
  max_diff_b <- 0
  n_groups_with_ties <- 0L

  for (group_idx in groups) {
    if (length(group_idx) > 1) {
      n_groups_with_ties <- as.integer(n_groups_with_ties + 1)

      avg_a <- rowMeans(predictions_a[group_idx, , drop = FALSE])
      avg_b <- rowMeans(predictions_b[group_idx, , drop = FALSE])

      if (length(avg_a) > 1) {
        max_diff_a <- max(max_diff_a, max(as.matrix(dist(avg_a))))
        max_diff_b <- max(max_diff_b, max(as.matrix(dist(avg_b))))
      }
    }
  }

  list(
    max_diff_a = max_diff_a,
    max_diff_b = max_diff_b,
    n_unique_patterns = length(groups),
    n_groups_with_ties = n_groups_with_ties,
    avg_group_size = mean(vapply(groups, length, integer(1)))
  )
}

#' Analyze Structure Diversity
#'
#' @description
#' Compute summary statistics for structure diversity across a set of fitted structures.
#'
#' @param structures List of TreeStructure objects
#' @return List with:
#'   \item{n_unique}{Integer: number of unique structures}
#'   \item{shannon_entropy}{Numeric: Shannon entropy of structure distribution}
#'   \item{simpson_index}{Numeric: Simpson diversity index}
#'   \item{modal_frequency}{Numeric: frequency of modal structure}
#'
#' @details
#' Diversity metrics:
#' - n_unique: Higher = more diverse
#' - shannon_entropy: Higher = more diverse (0 = all same, log(M) = uniform)
#' - simpson_index: Higher = more diverse (0 = all same, 1-1/M = uniform)
#' - modal_frequency: Lower = more diverse
#'
#' @examples
#' \dontrun{
#' diversity <- analyze_structure_diversity(structures)
#' cat(sprintf("Unique structures: %d / %d\n",
#'             diversity$n_unique, length(structures)))
#' }
#'
#' @export
analyze_structure_diversity <- function(structures) {
  hashes <- vapply(structures, structure_hash, character(1))
  counts <- table(hashes)
  M <- length(structures)

  # Number of unique structures
  n_unique <- length(counts)

  # Shannon entropy: H = -sum(p_i * log(p_i))
  probs <- as.numeric(counts) / M
  shannon_entropy <- -sum(probs * log(probs))

  # Simpson index: D = 1 - sum(p_i^2)
  simpson_index <- 1 - sum(probs^2)

  # Modal frequency
  modal_frequency <- max(counts) / M

  list(
    n_unique = n_unique,
    shannon_entropy = shannon_entropy,
    simpson_index = simpson_index,
    modal_frequency = modal_frequency
  )
}
