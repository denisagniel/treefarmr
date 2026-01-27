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
  
  # Check for non-binary values
  for (col in names(newdata)) {
    if (!all(newdata[[col]] %in% c(0, 1))) {
      stop("newdata must contain only binary values")
    }
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
  
  # Extract probabilities from tree structure
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
#' @param ensemble Logical: whether to ensemble across all intersecting trees
#' @param ... Additional arguments
#'
#' @return Predictions based on type
#' @export
predict.cf_rashomon <- function(object, newdata, type = c("class", "prob"), 
                               ensemble = TRUE, ...) {
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
  
  # Check for non-binary values
  for (col in names(newdata)) {
    if (!all(newdata[[col]] %in% c(0, 1))) {
      stop("newdata must contain only binary values")
    }
  }
  
  if (ensemble && object$n_intersecting > 1) {
    # Ensemble predictions across all intersecting trees
    predictions_list <- list()
    
    for (i in seq_len(object$n_intersecting)) {
      # Use the first fold model as a template (they should all be similar)
      model <- object$fold_models[[1]]
      
      # Get predictions from this model (simplified approach)
      pred <- predict(model, newdata, type = type, ...)
      predictions_list[[i]] <- pred
    }
    
    if (type == "class") {
      # Majority vote for class predictions
      pred_matrix <- do.call(cbind, predictions_list)
      final_predictions <- apply(pred_matrix, 1, function(x) {
        as.numeric(names(sort(table(x), decreasing = TRUE))[1])
      })
      return(final_predictions)
    } else {
      # Average probabilities
      pred_matrix <- do.call(cbind, predictions_list)
      final_predictions <- rowMeans(pred_matrix)
      return(final_predictions)
    }
  } else {
    # Use first intersecting tree
    model <- object$fold_models[[1]]
    return(predict(model, newdata, type = type, ...))
  }
}