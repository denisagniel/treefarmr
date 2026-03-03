#' S3 Predict Methods for TreeFARMS Models
#'
#' @description
#' S3 methods for making predictions with TreeFARMS models.
#' Provides a unified interface for all model types.

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

  # Apply discretization if model used continuous features
  if (!is.null(object$discretization)) {
    newdata <- apply_discretization(newdata, object$discretization)
    # Reorder to match training (X_train is already discretized binary names)
    newdata <- newdata[, colnames(object$X_train), drop = FALSE]
  }

  # Validate feature names
  if (!all(colnames(object$X_train) %in% colnames(newdata))) {
    stop("newdata must have the same features as training data")
  }
  
  # Ensure same column order
  newdata <- newdata[, colnames(object$X_train), drop = FALSE]
  
  # Check for missing values
  if (any(is.na(newdata))) {
    stop("newdata contains missing values")
  }
  
  # Check for non-binary values (vectorized)
  m <- as.matrix(newdata)
  if (any(!m %in% c(0L, 1L) & !is.na(m))) {
    stop("newdata must contain only binary values (0 and 1)")
  }
  
  # Get tree structure from model object
  tree_to_use <- if (!is.null(object$model$tree_json)) {
    object$model$tree_json
  } else if (!is.null(object$model$result_data)) {
    object$model$result_data
  } else {
    NULL
  }
  
  # Regression: return fitted values (leaf means)
  if (identical(object$loss_function, "squared_error")) {
    get_fitted_from_tree <- get("get_fitted_from_tree", envir = asNamespace("treefarmr"))
    if (!is.null(tree_to_use)) {
      return(get_fitted_from_tree(tree_to_use, newdata))
    }
    return(rep(NA_real_, nrow(newdata)))
  }
  
  # Extract probabilities from tree structure (classification)
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
    # Fallback: if no tree available, try to use stored predictions/probabilities
    # This handles edge cases where tree structure might not be available
    n_samples <- nrow(newdata)
    if (type == "class") {
      if (!is.null(object$predictions) && length(object$predictions) >= n_samples) {
        return(object$predictions[1:n_samples])
      } else {
        return(rep(0, n_samples))
      }
    } else {
      if (!is.null(object$probabilities) && nrow(object$probabilities) >= n_samples) {
        return(object$probabilities[1:n_samples, , drop = FALSE])
      } else {
        return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
      }
    }
  }
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

  # Apply discretization if model used continuous features
  if (!is.null(object$discretization)) {
    newdata <- apply_discretization(newdata, object$discretization)
  }

  # Validate feature names
  if (!all(colnames(object$X_train) %in% colnames(newdata))) {
    stop("newdata must have the same features as training data")
  }
  
  # Ensure same column order
  newdata <- newdata[, colnames(object$X_train), drop = FALSE]
  
  # Check for missing values
  if (any(is.na(newdata))) {
    stop("newdata contains missing values")
  }
  
  # Check for non-binary values (vectorized)
  m <- as.matrix(newdata)
  if (any(!m %in% c(0L, 1L) & !is.na(m))) {
    stop("newdata must contain only binary values (0 and 1)")
  }
  
  n_rows <- nrow(newdata)
  
  # Regression: return fitted values (vector)
  if (identical(object$loss_function, "squared_error")) {
    get_fitted_from_tree <- get("get_fitted_from_tree", envir = asNamespace("treefarmr"))
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
    get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("treefarmr"))
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
      return(ifelse(probs[, 2L] >= 0.5, 1, 0))
    }
    return(probs)
  }
  
  # Backward compatible: no fold_indices
  if (ensemble && object$n_intersecting > 1) {
    get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("treefarmr"))
    predictions_list <- list()
    for (i in seq_len(object$n_intersecting)) {
      # Use i-th intersecting tree (not same model repeatedly)
      tree_json <- object$intersecting_trees[[i]]
      probs <- get_probabilities_from_tree(tree_json, newdata)
      if (type == "class") {
        predictions_list[[i]] <- ifelse(probs[, 2] >= 0.5, 1, 0)
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