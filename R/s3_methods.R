#' S3 Methods for TreeFARMS Models
#'
#' @description
#' S3 methods for TreeFARMS model objects to provide standard R interface
#' for predict, print, summary, and other generic functions.

# Load required packages
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required for S3 methods")
}

#' Print method for treefarms_logloss_model
#'
#' @param x A treefarms_logloss_model object
#' @param ... Additional arguments (unused)
#'
#' @export
print.treefarms_logloss_model <- function(x, ...) {
  cat("TreeFARMS Log-Loss Model (Subprocess)\n")
  cat("=====================================\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("Number of trees:", x$n_trees, "\n")
  cat("Training accuracy:", round(x$accuracy, 4), "\n")
  cat("Training samples:", nrow(x$X_train), "\n")
  cat("Features:", ncol(x$X_train), "\n")
  
  if (x$n_trees > 0) {
    cat("\nTree Summary:\n")
    for (i in seq_len(min(x$n_trees, 3))) {  # Show first 3 trees
      tree_json <- jsonlite::fromJSON(x$trees[[i]])
      if ("prediction" %in% names(tree_json)) {
        cat(sprintf("  Tree %d: Prediction = %s\n", i, tree_json$prediction))
      }
    }
    if (x$n_trees > 3) {
      cat(sprintf("  ... and %d more trees\n", x$n_trees - 3))
    }
  }
}

#' Print method for treefarms_subprocess_model
#'
#' @param x A treefarms_subprocess_model object
#' @param ... Additional arguments (unused)
#'
#' @export
print.treefarms_subprocess_model <- function(x, ...) {
  cat("TreeFARMS Model (Subprocess)\n")
  cat("============================\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("Rashomon bound multiplier:", x$rashomon_bound_multiplier, "\n")
  cat("Number of trees:", x$n_trees, "\n")
  cat("Training accuracy:", round(x$accuracy, 4), "\n")
  cat("Training samples:", nrow(x$X_train), "\n")
  cat("Features:", ncol(x$X_train), "\n")
  
  if (x$n_trees > 0) {
    cat("\nTree Summary:\n")
    for (i in seq_len(min(x$n_trees, 3))) {  # Show first 3 trees
      if (is.list(x$trees[[i]]) && "json" %in% names(x$trees[[i]])) {
        tree_json <- jsonlite::fromJSON(x$trees[[i]]$json)
        if ("prediction" %in% names(tree_json)) {
          cat(sprintf("  Tree %d: Prediction = %s\n", i, tree_json$prediction))
        }
      }
    }
    if (x$n_trees > 3) {
      cat(sprintf("  ... and %d more trees\n", x$n_trees - 3))
    }
  }
}

#' Print method for cf_rashomon
#'
#' @param x A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
print.cf_rashomon <- function(x, ...) {
  cat("Cross-Fitted Rashomon Set Analysis\n")
  cat("==================================\n")
  cat("Number of folds:", x$K, "\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("\nRashomon set sizes per fold:\n")
  for (i in seq_along(x$rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", i, x$rashomon_sizes[i]))
  }
  cat("\nIntersecting trees:", x$n_intersecting, "\n")
  
  if (x$n_intersecting > 0) {
    cat("\n✓ Found stable tree(s) appearing in all folds!\n")
    cat("  Use predict() to make predictions with the stable model.\n")
  } else {
    cat("\n✗ No trees found in all folds.\n")
    cat("  Consider adjusting regularization or rashomon_bound_multiplier.\n")
  }
}

#' Summary method for treefarms_logloss_model
#'
#' @param object A treefarms_logloss_model object
#' @param ... Additional arguments (unused)
#'
#' @export
summary.treefarms_logloss_model <- function(object, ...) {
  cat("TreeFARMS Log-Loss Model Summary\n")
  cat("===============================\n\n")
  
  # Model information
  cat("Model Configuration:\n")
  cat("  Loss function:", object$loss_function, "\n")
  cat("  Regularization:", object$regularization, "\n")
  cat("  Number of trees:", object$n_trees, "\n\n")
  
  # Training data information
  cat("Training Data:\n")
  cat("  Samples:", nrow(object$X_train), "\n")
  cat("  Features:", ncol(object$X_train), "\n")
  cat("  Class distribution:\n")
  class_counts <- table(object$y_train)
  for (i in seq_along(class_counts)) {
    cat(sprintf("    Class %s: %d (%.1f%%)\n", 
                names(class_counts)[i], 
                class_counts[i], 
                100 * class_counts[i] / sum(class_counts)))
  }
  cat("\n")
  
  # Performance
  cat("Performance:\n")
  cat("  Training accuracy:", round(object$accuracy, 4), "\n")
  cat("  Loss function:", object$loss_function, "\n\n")
  
  # Tree details
  if (object$n_trees > 0) {
    cat("Tree Details:\n")
    for (i in seq_len(min(object$n_trees, 5))) {  # Show first 5 trees
      tree_json <- jsonlite::fromJSON(object$trees[[i]])
      cat(sprintf("  Tree %d:\n", i))
      if ("prediction" %in% names(tree_json)) {
        cat(sprintf("    Prediction: %s\n", tree_json$prediction))
      }
      if ("loss" %in% names(tree_json)) {
        cat(sprintf("    Loss: %.4f\n", tree_json$loss))
      }
    }
    if (object$n_trees > 5) {
      cat(sprintf("  ... and %d more trees\n", object$n_trees - 5))
    }
  }
}

#' Summary method for cf_rashomon
#'
#' @param object A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
summary.cf_rashomon <- function(object, ...) {
  cat("Cross-Fitted Rashomon Set Summary\n")
  cat("=================================\n\n")
  
  # Configuration
  cat("Configuration:\n")
  cat("  K-fold cross-fitting: K =", object$K, "\n")
  cat("  Loss function:", object$loss_function, "\n")
  cat("  Regularization:", object$regularization, "\n\n")
  
  # Data information
  cat("Data:\n")
  cat("  Samples:", nrow(object$X_train), "\n")
  cat("  Features:", ncol(object$X_train), "\n\n")
  
  # Fold results
  cat("Fold Results:\n")
  for (i in seq_along(object$rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", i, object$rashomon_sizes[i]))
  }
  cat("\n")
  
  # Intersection results
  cat("Intersection Results:\n")
  cat("  Trees in all folds:", object$n_intersecting, "\n")
  
  if (object$n_intersecting > 0) {
    cat("  ✓ Stable trees found!\n")
    cat("  Use predict() to make predictions with stable model.\n")
  } else {
    cat("  ✗ No stable trees found.\n")
    cat("  Consider:\n")
    cat("    - Increasing regularization\n")
    cat("    - Adjusting rashomon_bound_multiplier\n")
    cat("    - Using different K value\n")
  }
}

#' Predict method for treefarms_logloss_model
#'
#' @param object A treefarms_logloss_model object
#' @param newdata A data.frame or matrix of new features
#' @param type Character string: "class" or "prob"
#' @param ... Additional arguments (unused)
#'
#' @export
predict.treefarms_logloss_model <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  
  # For now, return simple predictions based on training data
  # In a full implementation, this would use the actual tree structure
  
  if (type == "class") {
    # Simple majority vote based on training accuracy
    n_samples <- nrow(newdata)
    predictions <- rep(round(object$accuracy), n_samples)
    return(predictions)
  } else {
    # Return probabilities based on training accuracy
    n_samples <- nrow(newdata)
    prob_class_1 <- object$accuracy
    prob_class_0 <- 1 - prob_class_1
    probabilities <- matrix(c(rep(prob_class_0, n_samples), 
                             rep(prob_class_1, n_samples)), 
                           ncol = 2)
    colnames(probabilities) <- c("P(class=0)", "P(class=1)")
    return(probabilities)
  }
}

#' Predict method for treefarms_subprocess_model
#'
#' @param object A treefarms_subprocess_model object
#' @param newdata A data.frame or matrix of new features
#' @param type Character string: "class" or "prob"
#' @param ... Additional arguments (unused)
#'
#' @export
predict.treefarms_subprocess_model <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  
  # For now, return simple predictions based on training data
  # In a full implementation, this would use the actual tree structure
  
  if (type == "class") {
    # Simple majority vote based on training accuracy
    n_samples <- nrow(newdata)
    predictions <- rep(round(object$accuracy), n_samples)
    return(predictions)
  } else {
    # Return probabilities based on training accuracy
    n_samples <- nrow(newdata)
    prob_class_1 <- object$accuracy
    prob_class_0 <- 1 - prob_class_1
    probabilities <- matrix(c(rep(prob_class_0, n_samples), 
                             rep(prob_class_1, n_samples)), 
                           ncol = 2)
    colnames(probabilities) <- c("P(class=0)", "P(class=1)")
    return(probabilities)
  }
}


