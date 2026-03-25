# Test suite for rashomon utility functions
# Tests the helper functions for working with Rashomon sets

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Create test data with clear pattern for Rashomon analysis
set.seed(42)
pattern_data <- list(
  X = data.frame(
    x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
    x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
  ),
  y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
)

# Create a test model with Rashomon set
test_model <- treefarms(pattern_data$X, pattern_data$y, 
                       loss_function = "misclassification", 
                       regularization = 0.01,  # Low regularization to allow multiple trees
                       rashomon_bound_multiplier = 0.1,
                       verbose = FALSE)

test_that("get_rashomon_trees works", {
  trees <- get_rashomon_trees(test_model)
  
  expect_true(is.list(trees))
  expect_true(length(trees) >= 0)
  
  if (length(trees) > 0) {
    # Check tree structure - should be list of tree objects
    expect_true(is.list(trees[[1]]))
    # Each tree should have some structure
    expect_true(length(trees[[1]]) > 0)
  }
})

test_that("tree_to_json works", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) > 0) {
    json_str <- tree_to_json(trees[[1]])
    
    expect_true(is.character(json_str))
    expect_true(nchar(json_str) > 0)
    
    # Should be valid JSON
    expect_no_error(jsonlite::fromJSON(json_str))
  }
})

test_that("compare_trees works", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) >= 2) {
    # Compare two different trees
    result <- compare_trees(trees[[1]], trees[[2]])
    
    expect_true(is.logical(result))
    expect_true(length(result) == 1)
  } else if (length(trees) >= 1) {
    # Compare tree with itself
    result <- compare_trees(trees[[1]], trees[[1]])
    
    expect_true(is.logical(result))
    expect_true(result == TRUE)  # Should be identical
  }
})

test_that("get_tree_rules works", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) > 0) {
    rules <- get_tree_rules(trees[[1]], feature_names = c("x1", "x2"))
    
    expect_true(is.character(rules))
    expect_true(length(rules) > 0)
  }
})

test_that("count_trees works", {
  count <- count_trees(test_model)
  
  expect_true(is.numeric(count))
  expect_true(count >= 0)
  expect_true(length(count) == 1)
  expect_equal(count, test_model@n_trees)
})

test_that("find_tree_intersection works", {
  # Create multiple Rashomon sets
  trees1 <- get_rashomon_trees(test_model)
  trees2 <- get_rashomon_trees(test_model)
  trees3 <- get_rashomon_trees(test_model)
  
  if (length(trees1) > 0) {
    result <- find_tree_intersection(list(trees1, trees2, trees3))
    
    expect_true(is.list(result))
    expect_true("intersecting_trees" %in% names(result))
    expect_true("n_intersecting" %in% names(result))
    expect_true("tree_jsons" %in% names(result))
    
    expect_true(is.numeric(result$n_intersecting))
    expect_true(result$n_intersecting >= 0)
  }
})

test_that("rashomon utilities handle edge cases", {
  # Test with model that has no trees
  no_tree_model <- treefarms(pattern_data$X, pattern_data$y, 
                            loss_function = "misclassification", 
                            regularization = 10,  # High regularization to prevent trees
                            verbose = FALSE)
  
  trees <- get_rashomon_trees(no_tree_model)
  expect_true(is.list(trees))
  expect_true(length(trees) == 0)
  
  count <- count_trees(no_tree_model)
  expect_equal(count, 0)
})

test_that("rashomon utilities handle different model types", {
  # Test with log_loss model
  logloss_model <- treefarms(pattern_data$X, pattern_data$y, 
                            loss_function = "log_loss", 
                            regularization = 0.01,
                            rashomon_bound_multiplier = 0.1,
                            verbose = FALSE)
  
  expect_no_error({
    trees <- get_rashomon_trees(logloss_model)
    if (length(trees) > 0) {
      json_str <- tree_to_json(trees[[1]])
      rules <- get_tree_rules(trees[[1]], feature_names = c("x1", "x2"))
    }
  })
})

test_that("rashomon utilities with auto-tuned model", {
  # Test with auto-tuned model
  auto_model <- treefarms(pattern_data$X, pattern_data$y, 
                         loss_function = "misclassification", 
                         regularization = NULL,
                         verbose = FALSE)
  
  expect_no_error({
    trees <- get_rashomon_trees(auto_model)
    count <- count_trees(auto_model)
    
    if (length(trees) > 0) {
      json_str <- tree_to_json(trees[[1]])
      rules <- get_tree_rules(trees[[1]], feature_names = c("x1", "x2"))
    }
  })
})

test_that("tree_to_json handles different tree structures", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) > 0) {
    # Test with different tree structures
    for (i in seq_along(trees)) {
      expect_no_error({
        json_str <- tree_to_json(trees[[i]])
        expect_true(is.character(json_str))
        expect_true(nchar(json_str) > 0)
      })
    }
  }
})

test_that("get_tree_rules handles different feature names", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) > 0) {
    # Test with different feature name formats
    expect_no_error({
      rules1 <- get_tree_rules(trees[[1]], feature_names = c("x1", "x2"))
      rules2 <- get_tree_rules(trees[[1]], feature_names = c("feature_1", "feature_2"))
      rules3 <- get_tree_rules(trees[[1]])  # No feature names
    })
    
    expect_true(is.character(rules1))
    expect_true(is.character(rules2))
    expect_true(is.character(rules3))
  }
})

test_that("compare_trees handles identical and different trees", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) >= 1) {
    # Compare tree with itself
    result_same <- compare_trees(trees[[1]], trees[[1]])
    expect_true(result_same)
    
    if (length(trees) >= 2) {
      # Compare different trees
      result_diff <- compare_trees(trees[[1]], trees[[2]])
      expect_true(is.logical(result_diff))
    }
  }
})

test_that("find_tree_intersection with empty sets", {
  # Test with empty tree sets
  empty_trees <- list()
  
  result <- find_tree_intersection(list(empty_trees, empty_trees))
  expect_equal(result$n_intersecting, 0)
  expect_true(is.list(result$intersecting_trees))
  expect_true(length(result$intersecting_trees) == 0)
})

test_that("rashomon utilities preserve tree structure", {
  trees <- get_rashomon_trees(test_model)
  
  if (length(trees) > 0) {
    original_tree <- trees[[1]]
    
    # Convert to JSON and back should preserve structure
    json_str <- tree_to_json(original_tree)
    parsed_tree <- jsonlite::fromJSON(json_str)
    
    # Should be able to get rules from both
    rules_original <- get_tree_rules(original_tree, feature_names = c("x1", "x2"))
    rules_parsed <- get_tree_rules(parsed_tree, feature_names = c("x1", "x2"))
    
    expect_true(is.character(rules_original))
    expect_true(is.character(rules_parsed))
  }
})

test_that("count_trees matches model n_trees", {
  # Test that count_trees returns the same as model@n_trees
  count <- count_trees(test_model)
  expect_equal(count, test_model@n_trees)
  
  # Test with different models
  models <- list(
    treefarms(pattern_data$X, pattern_data$y, regularization = 0.01, verbose = FALSE),
    treefarms(pattern_data$X, pattern_data$y, regularization = 0.1, verbose = FALSE),
    treefarms(pattern_data$X, pattern_data$y, regularization = 1.0, verbose = FALSE)
  )
  
  for (model in models) {
    count <- count_trees(model)
    expect_equal(count, model@n_trees)
  }
})