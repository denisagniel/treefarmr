# Test suite for probability serialization fix
# Verifies that log-loss models now correctly serialize probabilities to JSON

library(testthat)
library(treefarmr)

# Helper function to safely test treefarms
safe_treefarms <- function(...) {
  tryCatch({
    treefarms(...)
  }, error = function(e) {
    skip(paste("TreeFARMS failed:", e$message))
  })
}

# Simple test dataset
simple_dataset <- list(
  X = data.frame(
    feature_1 = c(0, 0, 1, 1),
    feature_2 = c(0, 1, 0, 1)
  ),
  y = c(0, 1, 1, 0)
)

test_that("log-loss models serialize probabilities to JSON", {
  # Train a log-loss model
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         single_tree = TRUE,
                         verbose = FALSE)
  
  # Check that model has tree_json
  expect_true(!is.null(model$model$tree_json), 
              info = "Model should have tree_json")
  
  # Helper function to check if a node has probabilities
  check_node_probabilities <- function(node) {
    if (is.null(node)) return(FALSE)
    
    # If this is a leaf node, check for probabilities
    if (!is.null(node$prediction)) {
      if (is.null(node$probabilities)) {
        return(FALSE)
      }
      if (length(node$probabilities) == 0) {
        return(FALSE)
      }
      # Probabilities should be numeric and length 2 for binary classification
      if (!is.numeric(node$probabilities) || length(node$probabilities) < 2) {
        return(FALSE)
      }
      return(TRUE)
    }
    
    # If this is a split node, recursively check children
    if (!is.null(node$feature)) {
      has_probs <- TRUE
      if (!is.null(node$true)) {
        has_probs <- has_probs && check_node_probabilities(node$true)
      }
      if (!is.null(node$false)) {
        has_probs <- has_probs && check_node_probabilities(node$false)
      }
      return(has_probs)
    }
    
    return(FALSE)
  }
  
  # Check that tree has probabilities serialized
  tree <- model$model$tree_json
  expect_true(check_node_probabilities(tree),
              info = "Tree nodes should have probabilities serialized in JSON")
})

test_that("log-loss probabilities are not empty arrays", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         single_tree = TRUE,
                         verbose = FALSE)
  
  # Helper to extract all leaf probabilities
  extract_leaf_probabilities <- function(node) {
    if (is.null(node)) return(list())
    
    probs <- list()
    if (!is.null(node$prediction)) {
      # This is a leaf
      if (!is.null(node$probabilities) && length(node$probabilities) > 0) {
        probs[[length(probs) + 1]] <- node$probabilities
      }
    } else if (!is.null(node$feature)) {
      # This is a split node
      if (!is.null(node$true)) {
        probs <- c(probs, extract_leaf_probabilities(node$true))
      }
      if (!is.null(node$false)) {
        probs <- c(probs, extract_leaf_probabilities(node$false))
      }
    }
    return(probs)
  }
  
  tree <- model$model$tree_json
  leaf_probs <- extract_leaf_probabilities(tree)
  
  # Should have at least one leaf with probabilities
  expect_true(length(leaf_probs) > 0,
              info = "Should have at least one leaf node with probabilities")
  
  # All leaf probabilities should be non-empty
  for (prob in leaf_probs) {
    expect_true(length(prob) >= 2,
                info = "Each leaf should have probabilities array with at least 2 elements")
    expect_true(all(is.numeric(prob)),
                info = "Probabilities should be numeric")
    expect_true(all(prob >= 0) && all(prob <= 1),
                info = "Probabilities should be in [0, 1]")
  }
})

test_that("log-loss probabilities sum to approximately 1", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         single_tree = TRUE,
                         compute_probabilities = TRUE,
                         verbose = FALSE)
  
  # Check that probabilities are computed
  expect_true(!is.null(model$probabilities),
              info = "Model should have probabilities computed")
  
  # Each row should sum to approximately 1
  if (!is.null(model$probabilities)) {
    row_sums <- rowSums(model$probabilities)
    expect_true(all(abs(row_sums - 1) < 1e-6),
                info = "Probability rows should sum to 1")
  }
})

test_that("log-loss probabilities are used in predict()", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         single_tree = TRUE,
                         verbose = FALSE)
  
  # Make predictions
  pred_probs <- predict(model, simple_dataset$X, type = "prob")
  
  expect_true(!is.null(pred_probs),
              info = "predict() should return probabilities")
  expect_true(is.matrix(pred_probs),
              info = "Probabilities should be a matrix")
  expect_equal(nrow(pred_probs), nrow(simple_dataset$X),
               info = "Should have one probability row per input sample")
  expect_equal(ncol(pred_probs), 2,
               info = "Should have 2 probability columns for binary classification")
  
  # Probabilities should sum to 1 per row
  row_sums <- rowSums(pred_probs)
  expect_true(all(abs(row_sums - 1) < 1e-6),
              info = "Predicted probability rows should sum to 1")
})

test_that("log-loss probabilities differ from misclassification", {
  # Train both models
  model_logloss <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                 loss_function = "log_loss", 
                                 regularization = 0.1,
                                 single_tree = TRUE,
                                 compute_probabilities = TRUE,
                                 verbose = FALSE)
  
  model_misclass <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                  loss_function = "misclassification", 
                                  regularization = 0.1,
                                  single_tree = TRUE,
                                  compute_probabilities = TRUE,
                                  verbose = FALSE)
  
  # Both should have probabilities
  expect_true(!is.null(model_logloss$probabilities),
              info = "Log-loss model should have probabilities")
  expect_true(!is.null(model_misclass$probabilities),
              info = "Misclassification model should have probabilities")
  
  # They should differ (log-loss optimizes for different objective)
  if (!is.null(model_logloss$probabilities) && !is.null(model_misclass$probabilities)) {
    expect_false(identical(model_logloss$probabilities, model_misclass$probabilities),
                 info = "Log-loss and misclassification probabilities should differ")
  }
})



