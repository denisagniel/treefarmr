#' S3 Predict Methods for TreeFARMS Models
#'
#' @description
#' S3 methods for making predictions with TreeFARMS models.
#' Provides a unified interface for all model types.

# Constants -------------------------------------------------------------------

# Issue #32: Classification threshold for binary predictions
# For binary classification, predict class 1 if P(class=1) >= threshold
.CLASSIFICATION_THRESHOLD <- 0.5

# Helper functions (internal) ------------------------------------------------

#' Apply discretization and validate newdata
#'
#' @description
#' Issue #34: This helper function centralizes discretization and validation logic
#' that was previously duplicated across three predict methods. It handles both
#' continuous and binary features, applies stored discretization metadata when
#' available, and validates the result.
#'
#' @details
#' This function is called by all three predict methods (treefarms_model,
#' optimaltrees_model, cf_rashomon) to ensure consistent preprocessing.
#'
#' Three cases are handled:
#' 1. Model has discretization metadata + all_binary=TRUE: Pass through binary features
#' 2. Model has discretization metadata + all_binary=FALSE: Apply stored discretization
#' 3. No discretization metadata: Assume features are already binary (legacy behavior)
#'
#' After discretization, validates:
#' - No missing values (stop if any NA)
#' - All values are binary 0 or 1 (stop if not)
#'
#' @param newdata Data frame of features
#' @param object Model object with discretization metadata
#' @return Discretized and validated data frame (guaranteed binary)
#' @keywords internal
apply_model_discretization <- function(newdata, object) {
  # Apply discretization if model was trained on continuous features
  if (!is.null(object$discretization)) {
    if (isTRUE(object$discretization$all_binary)) {
      # Data was already binary during training, no transformation needed
      expected_original <- object$X_original_names
      if (!all(expected_original %in% colnames(newdata))) {
        stop("newdata must have the same features as training data (all_binary branch)")
      }
      newdata <- newdata[, expected_original, drop = FALSE]
    } else {
      # Apply stored discretization to continuous features
      expected_original <- names(object$discretization$features)
      if (!all(expected_original %in% colnames(newdata))) {
        stop("newdata must have the same features as training data: ",
             paste(expected_original, collapse = ", "))
      }
      apply_discretization <- get("apply_discretization", envir = asNamespace("optimaltrees"))
      newdata <- apply_discretization(newdata, object$discretization)
    }
  } else {
    # No discretization metadata - assume binary features
    expected_names <- if (!is.null(object$X_train)) {
      colnames(object$X_train)
    } else if (!is.null(object$X_original_names)) {
      object$X_original_names
    } else {
      stop("Cannot determine expected feature names. Model object is missing discretization, X_train, and X_original_names.")
    }
    if (!all(expected_names %in% colnames(newdata))) {
      stop("newdata must have the same features as training data (no discretization branch)")
    }
    newdata <- newdata[, expected_names, drop = FALSE]
  }

  # Check for missing values
  if (any(is.na(newdata))) {
    stop("newdata contains missing values")
  }

  # Issue #35: Check for non-binary values (vectorized)
  # Use integer literals 0L, 1L for type-safe comparison (not just 0.0, 1.0)
  m <- as.matrix(newdata)
  if (any(!m %in% c(0L, 1L) & !is.na(m))) {
    stop("newdata must contain only binary values (0 and 1)")
  }

  newdata
}

#' Extract tree structure from model object
#'
#' @description
#' Issue #34: This helper centralizes tree extraction logic that was duplicated
#' across three predict methods. It handles different storage formats with
#' fallback logic.
#'
#' @details
#' TreeFARMS models store the tree structure in one of two places:
#' - object$model$tree_json (primary, JSON format)
#' - object$model$result_data (fallback, internal format)
#'
#' Returns NULL if neither exists (will cause error in predict_from_tree).
#'
#' @param object Model object
#' @return Tree structure (tree_json or result_data), or NULL if not found
#' @keywords internal
get_tree_structure <- function(object) {
  if (!is.null(object$model$tree_json)) {
    object$model$tree_json
  } else if (!is.null(object$model$result_data)) {
    object$model$result_data
  } else {
    NULL
  }
}

#' Make predictions from tree structure
#'
#' @description
#' Issue #34: This helper centralizes prediction logic that was duplicated across
#' three predict methods. It handles both regression and classification tasks.
#'
#' @details
#' Called by all three predict methods after discretization and tree extraction.
#' Assumes newdata is already validated and binary.
#'
#' Two prediction modes:
#' 1. Regression (loss_function = "squared_error"): Returns fitted values (leaf means)
#' 2. Classification (other loss functions): Returns probabilities or class predictions
#'
#' For classification, type="class" applies threshold (.CLASSIFICATION_THRESHOLD = 0.5)
#' to convert P(class=1) to binary predictions. type="prob" returns the full
#' probability matrix [P(class=0), P(class=1)].
#'
#' @param tree_to_use Tree structure (from get_tree_structure)
#' @param newdata Validated and discretized data frame (guaranteed binary)
#' @param loss_function Loss function used during training
#' @param type Prediction type ("class" or "prob")
#' @return Predictions: numeric vector (class or regression) or matrix (prob)
#' @keywords internal
predict_from_tree <- function(tree_to_use, newdata, loss_function, type = "class") {
  if (is.null(tree_to_use)) {
    stop(
      "Cannot make predictions: tree structure not found in model object.\n",
      "This indicates a problem during model fitting or an invalid model object.\n",
      "Expected: object$model$tree_json or object$model$result_data to exist."
    )
  }

  # Regression: return fitted values (leaf means)
  if (identical(loss_function, "squared_error")) {
    get_fitted_from_tree <- get("get_fitted_from_tree", envir = asNamespace("optimaltrees"))
    return(get_fitted_from_tree(tree_to_use, newdata))
  }

  # Classification: extract probabilities
  get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("optimaltrees"))
  probabilities <- get_probabilities_from_tree(tree_to_use, newdata)

  if (type == "class") {
    # Use defined threshold for class prediction
    predictions <- ifelse(probabilities[, 2] >= .CLASSIFICATION_THRESHOLD, 1, 0)
    return(predictions)
  } else {
    return(probabilities)
  }
}

# S3 Predict Methods ----------------------------------------------------------

#' Predict method for treefarms_model objects
#'
#' @param object A treefarms_model object
#' @param newdata A data.frame or matrix of new features
#' @param type Character: "class" or "prob"
#' @param ... Additional arguments
#'
#' @return Predictions based on type
#' @export
predict.treefarms_model <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)

  # Validate input
  if (!inherits(object, "treefarms_model")) {
    stop("object must be a treefarms_model")
  }
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data.frame or matrix")
  }

  # Convert matrix to data.frame if needed
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }

  # Issue #23: Use extracted discretization helper
  newdata <- apply_model_discretization(newdata, object)

  # Issue #25: Use extracted tree structure helper
  tree_to_use <- get_tree_structure(object)

  # Issue #26: Use extracted prediction helper
  predict_from_tree(tree_to_use, newdata, object$loss_function, type)
}

#' Predict method for optimaltrees_model objects
#'
#' @param object A optimaltrees_model object
#' @param newdata A data.frame or matrix of new features
#' @param type Character: "class" or "prob"
#' @param ... Additional arguments
#'
#' @return Predictions based on type
#' @export
predict.optimaltrees_model <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)

  # Validate input
  if (!inherits(object, "optimaltrees_model")) {
    stop("object must be a optimaltrees_model")
  }
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data.frame or matrix")
  }

  # Convert matrix to data.frame if needed
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }

  # Issue #23: Use extracted discretization helper
  newdata <- apply_model_discretization(newdata, object)

  # Issue #25: Use extracted tree structure helper
  tree_to_use <- get_tree_structure(object)

  # Issue #26: Use extracted prediction helper
  predict_from_tree(tree_to_use, newdata, object$loss_function, type)
}

#' Predict method for cf_rashomon objects
#'
#' @param object A cf_rashomon object
#' @param newdata A data.frame or matrix of new features
#' @param type Character: "class" or "prob"
#' @param ensemble Logical: whether to ensemble across all intersecting trees (ignored if \code{fold_indices} is provided)
#' @param fold_indices Optional integer vector of length \code{nrow(newdata)} giving the fold id for each row.
#'   When provided, predictions use the fold-specific refit \eqn{\tilde{\eta}^{(-k)}} (valid DML). When \code{NULL}, uses fold 1 model (backward compatible).
#' @param ... Additional arguments
#'
#' @return Predictions based on type. If \code{fold_indices} is provided, each row gets \eqn{\tilde{\eta}^{(-k(i))}(X_i)}.
#' @export
predict.cf_rashomon <- function(object, newdata, type = c("class", "prob"),
                               ensemble = TRUE, fold_indices = NULL, ...) {
  type <- match.arg(type)

  if (object$n_intersecting == 0) {
    stop("No intersecting trees available for prediction")
  }

  # Validate input
  if (!inherits(object, "cf_rashomon")) {
    stop("object must be a cf_rashomon")
  }

  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("newdata must be a data.frame or matrix")
  }

  # Convert matrix to data.frame if needed
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }

  # Issue #23: Use extracted discretization helper
  newdata <- apply_model_discretization(newdata, object)

  n_rows <- nrow(newdata)
  
  # Regression: return fitted values (vector)
  if (identical(object$loss_function, "squared_error")) {
    get_fitted_from_tree <- get("get_fitted_from_tree", envir = asNamespace("optimaltrees"))
    if (!is.null(fold_indices)) {
      if (length(fold_indices) != n_rows) {
        stop("fold_indices must have length nrow(newdata)")
      }
      if (is.null(object$fold_refits) || length(object$fold_refits) == 0) {
        stop("object has no fold_refits; refit is required for fold-specific prediction")
      }
      fitted <- rep(NA_real_, n_rows)
      for (k in 1:object$K) {
        idx_k <- which(fold_indices == k)
        if (length(idx_k) == 0) next
        refit_tree <- object$fold_refits[[k]][[1L]]
        fitted[idx_k] <- get_fitted_from_tree(refit_tree, newdata[idx_k, , drop = FALSE])
      }
      na_rows <- which(is.na(fitted))
      if (length(na_rows) > 0) {
        warning("Some fold_indices were not in 1:K; using fold 1 refit for those rows")
        refit_1 <- object$fold_refits[[1L]][[1L]]
        fitted[na_rows] <- get_fitted_from_tree(refit_1, newdata[na_rows, , drop = FALSE])
      }
      return(fitted)
    }
    if (!is.null(object$fold_refits) && length(object$fold_refits) > 0) {
      refit_tree <- object$fold_refits[[1L]][[1L]]
      return(get_fitted_from_tree(refit_tree, newdata))
    }
    return(predict(object$fold_models[[1]], newdata, ...))
  }
  
  # DML: fold-specific predictions using fold_refits (classification)
  if (!is.null(fold_indices)) {
    if (length(fold_indices) != n_rows) {
      stop("fold_indices must have length nrow(newdata)")
    }
    if (is.null(object$fold_refits) || length(object$fold_refits) == 0) {
      stop("object has no fold_refits; refit is required for fold-specific prediction")
    }
    get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("optimaltrees"))
    probs <- matrix(NA_real_, nrow = n_rows, ncol = 2)
    for (k in 1:object$K) {
      idx_k <- which(fold_indices == k)
      if (length(idx_k) == 0) next
      refit_tree <- object$fold_refits[[k]][[1L]]
      probs[idx_k, ] <- get_probabilities_from_tree(refit_tree, newdata[idx_k, , drop = FALSE])
    }
    na_rows <- which(is.na(probs[, 1L]))
    if (length(na_rows) > 0) {
      warning("Some fold_indices were not in 1:K; using fold 1 refit for those rows")
      refit_1 <- object$fold_refits[[1L]][[1L]]
      probs[na_rows, ] <- get_probabilities_from_tree(refit_1, newdata[na_rows, , drop = FALSE])
    }
    if (type == "class") {
      return(ifelse(probs[, 2L] >= .CLASSIFICATION_THRESHOLD, 1, 0))
    }
    return(probs)
  }
  
  # Backward compatible: no fold_indices
  if (ensemble && object$n_intersecting > 1) {
    get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("optimaltrees"))
    predictions_list <- list()
    for (i in seq_len(object$n_intersecting)) {
      # Use i-th intersecting tree (not same model repeatedly)
      tree_json <- object$intersecting_trees[[i]]
      probs <- get_probabilities_from_tree(tree_json, newdata)
      if (type == "class") {
        predictions_list[[i]] <- ifelse(probs[, 2] >= .CLASSIFICATION_THRESHOLD, 1, 0)
      } else {
        predictions_list[[i]] <- probs[, 2]
      }
    }
    # Aggregate predictions
    if (type == "class") {
      pred_matrix <- do.call(cbind, predictions_list)
      final_predictions <- apply(pred_matrix, 1, function(x) {
        as.numeric(names(sort(table(x), decreasing = TRUE))[1])
      })
      return(final_predictions)
    } else {
      pred_matrix <- do.call(cbind, predictions_list)
      return(rowMeans(pred_matrix))
    }
  } else {
    model <- object$fold_models[[1]]
    return(predict(model, newdata, type = type, ...))
  }
}