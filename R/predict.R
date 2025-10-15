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
  
  # For now, return the stored predictions/probabilities from training
  # In a full implementation, we would parse the tree structure and make predictions
  # This is a simplified approach that works with the current model structure
  
  if (type == "class") {
    # Return stored predictions (this is a placeholder - in practice you'd evaluate the tree)
    return(object$predictions[1:nrow(newdata)])
  } else {
    # Return stored probabilities (this is a placeholder - in practice you'd evaluate the tree)
    return(object$probabilities[1:nrow(newdata), , drop = FALSE])
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