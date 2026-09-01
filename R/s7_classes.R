#' S7 Class Definitions for optimaltrees
#'
#' Modern OOP structure using S7 for type safety and validation
#'
#' @description
#' S7 provides formal class definitions with automatic property validation,
#' type-safe access via @, and clear inheritance. This replaces the informal
#' S3 structure that allowed invalid objects and made debugging difficult.
#'
#' @import S7
#' @importFrom stats predict
#' @name optimaltrees-s7-imports
NULL

# =============================================================================
# Core Model Class
# =============================================================================

#' Optimal Decision Tree Model
#'
#' @description
#' Single unified class for all tree models regardless of loss function.
#' Handles classification (misclassification, log_loss) and regression
#' (squared_error, absolute_error, huber, quantile, custom).
#'
#' @details
#' Properties:
#' - loss_function: Loss function used ("misclassification", "log_loss", etc.)
#' - regularization: Complexity penalty (must be positive)
#' - n_trees: Number of trees in Rashomon set (0 or more)
#' - accuracy: Training accuracy [0, 1] for classification, NA for regression
#' - trees: List of tree structures (JSON or parsed)
#' - predictions: Predicted values/classes for training data
#' - probabilities: Probability predictions (classification only)
#' - X_train: Training features (optional, for retraining/inspection)
#' - y_train: Training outcomes (optional, for retraining/inspection)
#' - discretization_metadata: Info about feature discretization (optional)
#' - min_leaf_subgroup_fraction: `min` over leaves of (rows with `y == 0`) /
#'   (rows in leaf); the empirical analogue of the positivity/overlap constant
#'   \eqn{c} in \eqn{1 - e_0(x) \ge c}. `NA` for regression fits and whenever the
#'   diagnostic cannot be computed. See [min_leaf_subgroup_fraction()].
#' - is_regression: Logical flag for regression vs classification
#'
#' All properties are validated on creation and modification.
#' @export
OptimalTreesModel <- S7::new_class(
  name = "OptimalTreesModel",
  package = NULL,  # No package prefix to enable S3 dispatch

  properties = list(
    # Core configuration
    loss_function = S7::class_character,
    regularization = S7::class_double,

    # Model structure
    n_trees = S7::class_integer,
    trees = S7::class_list,  # List of tree structures (unified)

    # Performance metrics
    accuracy = S7::new_property(S7::class_double | S7::class_logical),  # NA for regression

    # Predictions (lazy evaluation via active bindings if needed)
    predictions = S7::class_any,
    probabilities = S7::class_any,  # NULL for regression

    # Training data (optional)
    X_train = S7::new_property(S7::class_any, default = NULL),
    y_train = S7::new_property(S7::class_any, default = NULL),  # NULL or numeric/integer vector

    # Diagnostics
    # min over leaves of (rows with y == 0) / (rows in leaf). Empirical analogue of
    # the positivity/overlap constant c in 1 - e_0(x) >= c; 0 means some leaf is
    # all-treated, so a propensity fit there is exactly 1 and the ATT weight
    # diverges. NA_real_ where it does not apply (regression, or no tree/data) --
    # same not-applicable convention @accuracy already uses for regression.
    min_leaf_subgroup_fraction = S7::new_property(S7::class_double, default = NA_real_),

    # Metadata
    discretization_metadata = S7::new_property(S7::class_list, default = NULL),
    is_regression = S7::class_logical
  ),

  validator = function(self) {
    # Regularization validation
    if (self@regularization <= 0) {
      return("@regularization must be positive")
    }

    # Tree count validation
    if (self@n_trees < 0) {
      return("@n_trees must be non-negative")
    }

    # Tree list length validation
    if (length(self@trees) != self@n_trees) {
      return("length(@trees) must equal @n_trees")
    }

    # Accuracy validation (only for classification)
    if (!self@is_regression && !is.na(self@accuracy)) {
      if (self@accuracy < 0 || self@accuracy > 1) {
        return("@accuracy must be in [0, 1]")
      }
    }

    # Subgroup fraction diagnostic: NA when not applicable (mirrors @accuracy)
    if (!is.na(self@min_leaf_subgroup_fraction)) {
      if (self@min_leaf_subgroup_fraction < 0 || self@min_leaf_subgroup_fraction > 1) {
        return("@min_leaf_subgroup_fraction must be in [0, 1]")
      }
    }

    # Loss function validation
    valid_losses <- c(
      "misclassification", "log_loss", "squared_error",
      "absolute_error", "huber", "quantile", "custom"
    )
    if (!self@loss_function %in% valid_losses) {
      return(sprintf("@loss_function must be one of: %s",
                     paste(valid_losses, collapse = ", ")))
    }

    # X_train type validation
    if (!is.null(self@X_train)) {
      if (!is.data.frame(self@X_train) && !is.matrix(self@X_train)) {
        return("@X_train must be a data.frame or matrix")
      }
    }

    # y_train type validation
    if (!is.null(self@y_train)) {
      if (!is.numeric(self@y_train) && !is.integer(self@y_train)) {
        return("@y_train must be numeric or integer")
      }
    }

    # Training data consistency
    if (!is.null(self@X_train) && !is.null(self@y_train)) {
      n_train <- nrow(self@X_train)
      n_outcome <- length(self@y_train)
      if (n_train != n_outcome) {
        return(sprintf("nrow(@X_train) = %d must equal length(@y_train) = %d",
                       n_train, n_outcome))
      }
    }
  }
)

# Constructor helper (normalizes different backend formats)
new_optimal_trees_model <- function(loss_function,
                                   regularization,
                                   n_trees,
                                   trees,
                                   accuracy = NA_real_,
                                   predictions = NULL,
                                   probabilities = NULL,
                                   X_train = NULL,
                                   y_train = NULL,
                                   discretization_metadata = NULL,
                                   min_leaf_subgroup_fraction = NA_real_,
                                   is_regression = FALSE) {

  # Normalize trees to consistent format
  # Whether from C++ JSON strings or parsed structures, store as list
  trees_list <- if (is.list(trees)) {
    trees
  } else if (is.character(trees) && length(trees) == 1) {
    # Single tree as JSON string
    list(jsonlite::fromJSON(trees, simplifyVector = FALSE))
  } else if (is.character(trees)) {
    # Multiple trees as JSON strings
    purrr::map(trees, ~ jsonlite::fromJSON(.x, simplifyVector = FALSE))
  } else {
    list()
  }

  OptimalTreesModel(
    loss_function = loss_function,
    regularization = regularization,
    n_trees = as.integer(n_trees),
    trees = trees_list,
    accuracy = if (is.na(accuracy)) NA_real_ else as.double(accuracy),
    predictions = predictions,
    probabilities = probabilities,
    X_train = X_train,
    y_train = y_train,
    discretization_metadata = discretization_metadata,
    min_leaf_subgroup_fraction = as.double(min_leaf_subgroup_fraction),
    is_regression = is_regression
  )
}


# =============================================================================
# Refined-Tree Model Class (Stage B: off-grid threshold refinement)
# =============================================================================

#' Off-Grid Refined Tree Model
#'
#' @description
#' Result of running Stage B (off-grid threshold refinement, per
#' \code{theory.tex}'s \code{prop:transition-leaves}) on a fitted
#' \code{OptimalTreesModel} -- see [refine_tree()], the constructor.
#'
#' A DELIBERATELY SEPARATE class from \code{OptimalTreesModel}, not a
#' subclass and not a variant metadata-patched onto the same
#' representation -- see \code{quality_reports/plans/2026-09-01_two-stage-
#' package-defaults-session.md} Milestone A in the \code{global-scholars}
#' project for the full Oracle-consulted rationale, verified empirically
#' before being accepted (the decisive finding: a fitted model's split
#' nodes can reference the SAME binary-feature column index from more than
#' one node, including sibling subtrees, so one refined cut per
#' coordinate/threshold cannot be represented by patching
#' \code{@discretization_metadata} -- confirmed directly on a real fit, not
#' assumed). Subclassing \code{OptimalTreesModel} was also rejected: an
#' inherited object would carry a \code{@trees} field that is stale for
#' prediction (the original, un-refined, grid-thresholded tree) -- a
#' silent-wrong-answer hazard a separate class avoids by construction (it
#' simply has no such field at all).
#'
#' \strong{Scope, 2026-09-01 (Milestone A): regression (\code{squared_error})
#' only}, matching [as_coordinate_tree()]'s scope -- see [refine_tree()].
#'
#' @details
#' Properties:
#' - tree: the coordinate-space tree (see \code{\link{stage_b}} for the
#'   node schema), after collapse and refinement.
#' - coords: character vector of coordinate names the tree splits on.
#' - loss: loss function used to fit \code{source} (currently always
#'   \code{"squared_error"} -- an honest single-value field, not a
#'   placeholder for future generality it does not yet have).
#' - source: the originating \code{OptimalTreesModel} -- provenance, and
#'   the discretization/metadata escape hatch a caller needing the
#'   pre-refinement fit can still reach.
#' - refinement_log: data.frame, one row per split, from
#'   [refine_tree_cuts()] -- the checkable claim of what moved, from where,
#'   and by how much risk. See \code{\link{refine_tree_cuts}}'s
#'   \code{@return} for the column list.
#' - n_train: number of training rows the refinement was run against.
#' - training_risk: total training SSE of the refined tree.
#' - min_leaf_n: the floor that was enforced during refinement (see
#'   [refine_tree_cuts()]); re-checked by the validator below.
#' - format_version: integer, currently \code{1L}.
#'
#' All properties are validated on creation and modification -- in
#' particular, every split's \code{cut} is re-checked to lie strictly
#' inside its own recomputed identifying bracket (a Milestone E
#' perturbation that violates this fails construction rather than
#' returning silent garbage), and every leaf is re-checked against
#' \code{min_leaf_n}.
#' @export
RefinedTreeModel <- S7::new_class(
  name = "RefinedTreeModel",
  package = NULL,

  properties = list(
    tree = S7::class_list,
    coords = S7::class_character,
    loss = S7::class_character,
    source = S7::new_property(S7::class_any, default = NULL),
    refinement_log = S7::new_property(S7::class_any, default = NULL),
    n_train = S7::class_integer,
    training_risk = S7::class_double,
    min_leaf_n = S7::new_property(S7::class_integer, default = 1L),
    format_version = S7::new_property(S7::class_integer, default = 1L)
  ),

  validator = function(self) {
    if (!identical(self@loss, "squared_error")) {
      return("@loss must be 'squared_error' (Milestone A scope)")
    }
    if (self@n_train < 1L) {
      return("@n_train must be positive")
    }
    if (self@min_leaf_n < 1L) {
      return("@min_leaf_n must be a positive integer")
    }
    if (!is.finite(self@training_risk) || self@training_risk < 0) {
      return("@training_risk must be a non-negative finite number")
    }
    if (length(self@coords) == 0L) {
      return("@coords must be non-empty")
    }

    # Recursive structural check over the coordinate-space tree: schema,
    # unique ids, finiteness, coords all recognized, min_leaf_n honored,
    # and every split's cut strictly inside its own recomputed bracket.
    seen_ids <- new.env(parent = emptyenv())
    seen_ids$ids <- integer(0)

    check_node <- function(node) {
      if (is.null(node$kind) || is.null(node$id)) {
        return("every node must have `kind` and `id`")
      }
      if (node$id %in% seen_ids$ids) {
        return(sprintf("duplicate node id %d -- ids must be unique", node$id))
      }
      seen_ids$ids <- c(seen_ids$ids, node$id)

      if (identical(node$kind, "leaf")) {
        if (is.null(node$prediction) || !is.finite(node$prediction)) {
          return(sprintf("leaf %d: @prediction must be finite", node$id))
        }
        if (is.null(node$n) || node$n < self@min_leaf_n) {
          return(sprintf(
            "leaf %d: n = %s is below min_leaf_n = %d",
            node$id, if (is.null(node$n)) "NULL" else node$n, self@min_leaf_n
          ))
        }
        return(NULL)
      }
      if (!identical(node$kind, "split")) {
        return(sprintf("node %d: unrecognized kind '%s'", node$id, node$kind))
      }
      if (!node$coord %in% self@coords) {
        return(sprintf(
          "split %d: coord '%s' is not in @coords", node$id, node$coord
        ))
      }
      if (!is.finite(node$cut)) {
        return(sprintf("split %d: @cut must be finite", node$id))
      }
      NULL
    }

    problem <- NULL
    walk <- function(node) {
      p <- check_node(node)
      if (!is.null(p)) {
        problem <<- p
        return(invisible())
      }
      if (identical(node$kind, "split")) {
        walk(node$left)
        walk(node$right)
      }
    }
    walk(self@tree)
    if (!is.null(problem)) return(problem)

    # Bracket-feasibility: recompute every split's identifying bracket from
    # the CURRENT tree and assert its cut lies strictly inside. This is
    # what gives Milestone E's later perturbations a free feasibility gate
    # -- a bad perturbation fails construction here, not silently.
    for (p in coord_tree_split_paths(self@tree)) {
      node <- coord_tree_node_at(self@tree, p)
      br <- node_bracket(self@tree, p)
      if (!(node$cut > br$lo && node$cut < br$hi)) {
        return(sprintf(
          "split %d (coord '%s'): cut = %s is not strictly inside its own identifying bracket (%s, %s)",
          node$id, node$coord, node$cut, br$lo, br$hi
        ))
      }
    }

    NULL
  }
)




#' Cross-Fitted Rashomon Set Results
#'
#' @description
#' Results from K-fold cross-fitting with Rashomon set intersection.
#' Contains trees that appear in all K folds (stable structures) plus
#' fold-specific refits for DML applications.
#'
#' @details
#' Properties:
#' - K: Number of folds (>= 2)
#' - loss_function: Loss function used
#' - regularization: Complexity penalty (> 0)
#' - rashomon_bound_multiplier: Rashomon set size control (> 0)
#' - rashomon_bound_adder: Additive bound (>= 0)
#' - max_leaves: Maximum leaves sieve (optional)
#' - rashomon_sizes: Vector of Rashomon set sizes per fold
#' - n_intersecting: Number of intersecting trees (>= 0)
#' - intersecting_trees: List of tree structures in all folds
#' - tree_risks: List of penalized risk info per tree
#' - fold_refits: List of K lists (fold-specific tree refits)
#' - fold_id_per_row: Vector mapping each training row to its fold
#' - fold_indices: List of K vectors (row indices per fold)
#' - X_train: Training features
#' - y_train: Training outcomes
#' - converged: Logical, TRUE if intersection found (auto-tuning)
#' - disc_metadata: Global discretization metadata (NULL for pure-binary data)
#' @export
CFRashomon <- S7::new_class(
  name = "CFRashomon",
  package = NULL,  # No package prefix to enable S3 dispatch

  properties = list(
    # Configuration
    K = S7::class_integer,
    loss_function = S7::class_character,
    regularization = S7::class_double,
    rashomon_bound_multiplier = S7::class_double,
    rashomon_bound_adder = S7::new_property(S7::class_double, default = 0),
    max_leaves = S7::new_property(S7::new_union(S7::class_integer, NULL), default = NULL),

    # Results per fold
    rashomon_sizes = S7::class_integer,  # Vector of length K

    # Intersection results
    n_intersecting = S7::class_integer,
    intersecting_trees = S7::class_list,
    tree_risks = S7::class_list,  # Penalized risk info for tree selection

    # DML-specific structures
    fold_refits = S7::class_list,  # List of K lists (refit structures)
    fold_id_per_row = S7::class_integer,  # Maps each row to fold ID
    fold_indices = S7::class_list,  # List of K vectors (indices)

    # Training data
    X_train = S7::new_property(S7::class_any),
    y_train = S7::new_property(S7::class_numeric | S7::class_integer),

    # Convergence status (for auto-tuning)
    converged = S7::new_property(S7::class_logical, default = TRUE),

    # Global discretization metadata (for consistent feature space across folds)
    # Stored when cross_fitted_rashomon() processes continuous features.
    # Used by predict.cf_rashomon() to apply the same thresholds to newdata.
    disc_metadata = S7::new_property(S7::class_any, default = NULL)
  ),

  validator = function(self) {
    # K validation
    if (self@K < 2) {
      return("@K must be at least 2")
    }

    # Regularization validation
    if (self@regularization <= 0) {
      return("@regularization must be positive")
    }

    # Rashomon bound validation
    if (self@rashomon_bound_multiplier <= 0) {
      return("@rashomon_bound_multiplier must be positive")
    }

    if (self@rashomon_bound_adder < 0) {
      return("@rashomon_bound_adder must be non-negative")
    }

    # Intersection count validation
    if (self@n_intersecting < 0) {
      return("@n_intersecting must be non-negative")
    }

    # Tree lists consistency
    if (length(self@intersecting_trees) != self@n_intersecting) {
      return("length(@intersecting_trees) must equal @n_intersecting")
    }

    if (length(self@tree_risks) != self@n_intersecting) {
      return("length(@tree_risks) must equal @n_intersecting")
    }

    # Fold structure validation
    if (length(self@rashomon_sizes) != self@K) {
      return("length(@rashomon_sizes) must equal @K")
    }

    if (length(self@fold_refits) != self@K) {
      return("length(@fold_refits) must equal @K")
    }

    if (length(self@fold_indices) != self@K) {
      return("length(@fold_indices) must equal @K")
    }

    # X_train type validation
    if (!is.data.frame(self@X_train) && !is.matrix(self@X_train)) {
      return("@X_train must be a data.frame or matrix")
    }

    # Training data consistency
    n_train <- nrow(self@X_train)
    n_outcome <- length(self@y_train)
    if (n_train != n_outcome) {
      return(sprintf("nrow(@X_train) = %d must equal length(@y_train) = %d",
                     n_train, n_outcome))
    }

    # fold_id_per_row length
    if (length(self@fold_id_per_row) != n_train) {
      return("length(@fold_id_per_row) must equal nrow(@X_train)")
    }

    # max_leaves validation
    if (!is.null(self@max_leaves) && self@max_leaves < 1) {
      return("@max_leaves must be NULL or >= 1")
    }
  }
)


# =============================================================================
# S7 Method Stubs (backward compatibility with S3 generics)
# =============================================================================

# These allow S7 objects to work with S3 generics like print(), plot(), etc.
# We can gradually migrate S3 method implementations to S7 methods

# Print method for OptimalTreesModel
S7::method(print, OptimalTreesModel) <- function(x, ...) {
  # Call existing S3 implementation for now
  # Can migrate incrementally
  print.optimaltrees_model(x)
}

# Predict method for OptimalTreesModel (S7)
# methods_register() in .onLoad() will automatically create S3 dispatch
S7::method(predict, OptimalTreesModel) <- function(object, newdata, type = "class", ...) {
  predict.optimaltrees_model(object, newdata, type, ...)
}

# Print method for CFRashomon
S7::method(print, CFRashomon) <- function(x, ...) {
  print.cf_rashomon(x)
}

# Predict method for CFRashomon (S7)
# methods_register() in .onLoad() will automatically create S3 dispatch
S7::method(predict, CFRashomon) <- function(object, newdata, ...) {
  predict.cf_rashomon(object, newdata, ...)
}

# Summary methods
S7::method(summary, OptimalTreesModel) <- function(object, ...) {
  summary.optimaltrees_model(object, ...)
}

S7::method(summary, CFRashomon) <- function(object, ...) {
  summary.cf_rashomon(object, ...)
}


# =============================================================================
# Export S7 Classes
# =============================================================================

# S7 classes are automatically exported when package is loaded
# No need for @export tags - S7 handles this differently
