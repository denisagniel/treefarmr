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
    lapply(trees, function(t) jsonlite::fromJSON(t, simplifyVector = FALSE))
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
    is_regression = is_regression
  )
}


# =============================================================================
# Cross-Fitted Rashomon Class
# =============================================================================

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
    converged = S7::new_property(S7::class_logical, default = TRUE)
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
# Loaded Model Class (Serialization)
# =============================================================================

#' Loaded Optimal Trees Model
#'
#' @description
#' Wrapper for models loaded from disk via save_optimaltrees/load_optimaltrees.
#' Contains the model plus serialization metadata.
#'
#' @details
#' This class exists to distinguish loaded models from freshly trained ones,
#' which can be useful for deciding whether certain operations (like retraining)
#' are safe.
OptimalTreesModelLoaded <- S7::new_class(
  name = "OptimalTreesModelLoaded",
  package = "optimaltrees",

  properties = list(
    model = OptimalTreesModel,  # The actual model
    loaded_from = S7::new_property(S7::class_character, default = ""),  # File path
    loaded_at = S7::new_property(S7::class_POSIXct, default = Sys.time())  # Timestamp
  ),

  validator = function(self) {
    # Main validation is in the nested model
    # Just validate metadata here
    if (nchar(self@loaded_from) == 0) {
      return("@loaded_from should specify source file path")
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
