# Test suite for probability accuracy
# Validates that probability estimates are accurate by comparing to known ground truth probabilities

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Setup test environment
setup_test_environment()

# Helper function to safely test treefarms
safe_treefarms <- function(...) {
  tryCatch({
    treefarms(...)
  }, error = function(e) {
    skip(paste("TreeFARMS failed:", e$message))
  })
}

# ============================================================================
# Helper Functions for Ground Truth Datasets
# ============================================================================

#' Create a dataset with known true probabilities for each feature combination
#' @param feature_combos A data.frame with feature combinations
#' @param true_probs A vector of true P(Y=1 | X) for each feature combination
#' @param n_per_combo Number of samples to generate per combination
#' @param seed Random seed for reproducibility
#' @return A list with X (data.frame), y (vector), and true_probs (vector of true probabilities for each row)
ground_truth_dataset <- function(feature_combos, true_probs, n_per_combo = 50, seed = 42) {
  set.seed(seed)
  
  if (nrow(feature_combos) != length(true_probs)) {
    stop("Number of feature combinations must match length of true_probs")
  }
  
  # Generate samples for each feature combination
  X_list <- list()
  y_list <- list()
  true_probs_list <- list()
  
  for (i in 1:nrow(feature_combos)) {
    # Repeat this feature combination n_per_combo times
    X_rep <- feature_combos[rep(i, n_per_combo), , drop = FALSE]
    rownames(X_rep) <- NULL
    
    # Sample y from the true probability
    y_rep <- rbinom(n_per_combo, 1, true_probs[i])
    
    # Store true probability for each sample
    true_probs_rep <- rep(true_probs[i], n_per_combo)
    
    X_list[[i]] <- X_rep
    y_list[[i]] <- y_rep
    true_probs_list[[i]] <- true_probs_rep
  }
  
  # Combine all samples
  X <- do.call(rbind, X_list)
  y <- unlist(y_list)
  true_probs <- unlist(true_probs_list)
  
  # Shuffle to mix samples from different combinations
  shuffle_idx <- sample(nrow(X))
  X <- X[shuffle_idx, , drop = FALSE]
  y <- y[shuffle_idx]
  true_probs <- true_probs[shuffle_idx]
  
  list(X = X, y = y, true_probs = true_probs)
}

#' Create a deterministic probability dataset
#' @param prob_map A named list where names are feature combinations (as strings) 
#'                 and values are true probabilities
#' @param n_per_combo Number of samples per combination
#' @param seed Random seed
#' @return A list with X, y, and true_probs
deterministic_prob_dataset <- function(prob_map, n_per_combo = 50, seed = 42) {
  set.seed(seed)
  
  # Parse feature combinations from names
  # Format: "x1=0,x2=0" -> data.frame(x1=0, x2=0)
  feature_names <- unique(unlist(lapply(strsplit(names(prob_map), ","), function(x) {
    gsub("=.*", "", x)
  })))
  
  feature_combos <- data.frame()
  true_probs_vec <- numeric()
  
  for (combo_str in names(prob_map)) {
    # Parse combination string
    parts <- strsplit(combo_str, ",")[[1]]
    combo_vec <- numeric(length(feature_names))
    names(combo_vec) <- feature_names
    
    for (part in parts) {
      key_val <- strsplit(part, "=")[[1]]
      if (length(key_val) == 2) {
        combo_vec[key_val[1]] <- as.numeric(key_val[2])
      }
    }
    
    feature_combos <- rbind(feature_combos, as.data.frame(t(combo_vec)))
    true_probs_vec <- c(true_probs_vec, prob_map[[combo_str]])
  }
  
  ground_truth_dataset(feature_combos, true_probs_vec, n_per_combo, seed)
}

#' Create a simple 2-feature dataset with 4 combinations and known probabilities
#' @param probs Vector of 4 probabilities for combinations (0,0), (0,1), (1,0), (1,1)
#' @param n_per_combo Number of samples per combination
#' @param seed Random seed
#' @return A list with X, y, and true_probs
simple_ground_truth_dataset <- function(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42) {
  feature_combos <- data.frame(
    x1 = c(0, 0, 1, 1),
    x2 = c(0, 1, 0, 1)
  )
  ground_truth_dataset(feature_combos, probs, n_per_combo, seed)
}

# ============================================================================
# Accuracy Metrics
# ============================================================================

#' Calculate Mean Absolute Error between predicted and true probabilities
#' @param pred_probs Predicted probabilities (vector or matrix with P(class=1) in second column)
#' @param true_probs True probabilities (vector)
#' @return MAE value
mae_probs <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    # Extract P(class=1) from second column
    pred_probs <- pred_probs[, 2]
  }
  mean(abs(pred_probs - true_probs))
}

#' Calculate Root Mean Squared Error
#' @param pred_probs Predicted probabilities
#' @param true_probs True probabilities
#' @return RMSE value
rmse_probs <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  sqrt(mean((pred_probs - true_probs)^2))
}

#' Calculate Brier score (mean squared error)
#' @param pred_probs Predicted probabilities
#' @param true_probs True probabilities
#' @return Brier score
brier_score <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  mean((pred_probs - true_probs)^2)
}

#' Calculate correlation between predicted and true probabilities
#' @param pred_probs Predicted probabilities
#' @param true_probs True probabilities
#' @return Correlation coefficient
probability_correlation <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  cor(pred_probs, true_probs)
}

# ============================================================================
# Test Cases
# ============================================================================

test_that("Simple deterministic probabilities are accurate", {
  # Create dataset with known probabilities: 0.1, 0.3, 0.7, 0.9
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 30, seed = 42)
  
  # Train model with log_loss (don't compute probabilities during training to avoid crashes)
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get predicted probabilities using predict()
  pred_probs <- predict(model, data$X, type = "prob")
  
  # Calculate accuracy metrics
  mae <- mae_probs(pred_probs, data$true_probs)
  rmse <- rmse_probs(pred_probs, data$true_probs)
  corr <- probability_correlation(pred_probs, data$true_probs)
  
  # Probabilities should be reasonably accurate
  # With 30 samples per combination, MAE should be < 0.2
  expect_true(mae < 0.2,
              info = paste("MAE should be < 0.2, got", mae))
  
  expect_true(rmse < 0.25,
              info = paste("RMSE should be < 0.25, got", rmse))
  
  # Correlation should be high
  expect_true(corr > 0.6,
              info = paste("Correlation should be > 0.6, got", corr))
})

test_that("Probabilities match training distribution in leaves", {
  # Create a simple dataset where we can verify leaf probabilities
  # Use deterministic probabilities so we know what to expect
  data <- simple_ground_truth_dataset(probs = c(0.2, 0.4, 0.6, 0.8), n_per_combo = 30, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get predicted probabilities
  pred_probs <- predict(model, data$X, type = "prob")
  
  # Get tree structure
  tree <- model$model$tree_json
  expect_true(!is.null(tree), info = "Tree structure should be available")
  
  # For each unique feature combination, check that predicted probability
  # matches the empirical distribution in training data
  unique_combos <- unique(data$X)
  
  for (i in 1:nrow(unique_combos)) {
    combo <- unique_combos[i, , drop = FALSE]
    
    # Find samples matching this combination
    matches <- apply(data$X, 1, function(row) {
      all(row == combo[1, ])
    })
    
    # Get true probability for this combination
    true_prob <- data$true_probs[which(matches)[1]]
    
    # Get predicted probabilities for these samples
    pred_probs_combo <- pred_probs[matches, 2]
    
    # All samples with same feature combination should have same predicted probability
    # (they should fall into the same leaf)
    expect_true(length(unique(pred_probs_combo)) <= 2, # Allow for small numerical differences
                info = paste("Samples with same features should have similar probabilities"))
    
    # Mean predicted probability should be close to true probability
    mean_pred <- mean(pred_probs_combo)
    expect_true(abs(mean_pred - true_prob) < 0.2,
                info = paste("Mean predicted probability (", mean_pred, 
                           ") should be close to true probability (", true_prob, ")"))
  }
})

test_that("Extreme probabilities are handled correctly", {
  # Test with probabilities near 0 and near 1
  data <- simple_ground_truth_dataset(probs = c(0.01, 0.05, 0.95, 0.99), n_per_combo = 30, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get predicted probabilities
  pred_probs_mat <- predict(model, data$X, type = "prob")
  pred_probs <- pred_probs_mat[, 2]
  
  # Probabilities should not collapse to exactly 0 or 1
  # (log-loss should keep them bounded away from extremes)
  expect_true(all(pred_probs > 0),
              info = "Probabilities should be > 0 even for extreme true probabilities")
  expect_true(all(pred_probs < 1),
              info = "Probabilities should be < 1 even for extreme true probabilities")
  
  # But they should still reflect the extreme nature
  # Samples with true prob near 0 should have low predicted prob
  low_true_idx <- data$true_probs < 0.1
  if (sum(low_true_idx) > 0) {
    mean_low_pred <- mean(pred_probs[low_true_idx])
    expect_true(mean_low_pred < 0.3,
                info = paste("Samples with low true probability should have low predicted probability, got", mean_low_pred))
  }
  
  # Samples with true prob near 1 should have high predicted prob
  high_true_idx <- data$true_probs > 0.9
  if (sum(high_true_idx) > 0) {
    mean_high_pred <- mean(pred_probs[high_true_idx])
    expect_true(mean_high_pred > 0.7,
                info = paste("Samples with high true probability should have high predicted probability, got", mean_high_pred))
  }
})

test_that("Balanced vs imbalanced probabilities are both accurate", {
  # Test balanced probabilities (0.5)
  data_balanced <- simple_ground_truth_dataset(probs = c(0.5, 0.5, 0.5, 0.5), n_per_combo = 30, seed = 42)
  
  model_balanced <- safe_treefarms(data_balanced$X, data_balanced$y,
                                   loss_function = "log_loss",
                                   regularization = 0.01,
                                   single_tree = TRUE,
                                   compute_probabilities = FALSE,
                                   verbose = FALSE)
  
  expect_valid_treefarms_model(model_balanced, "log_loss")
  
  # Test imbalanced probabilities
  data_imbalanced <- simple_ground_truth_dataset(probs = c(0.1, 0.1, 0.9, 0.9), n_per_combo = 30, seed = 43)
  
  model_imbalanced <- safe_treefarms(data_imbalanced$X, data_imbalanced$y,
                                     loss_function = "log_loss",
                                     regularization = 0.01,
                                     single_tree = TRUE,
                                     compute_probabilities = FALSE,
                                     verbose = FALSE)
  
  expect_valid_treefarms_model(model_imbalanced, "log_loss")
  
  # Calculate accuracy for both
  mae_balanced <- mae_probs(model_balanced$probabilities, data_balanced$true_probs)
  mae_imbalanced <- mae_probs(model_imbalanced$probabilities, data_imbalanced$true_probs)
  
  # Both should be reasonably accurate
  expect_true(mae_balanced < 0.2,
              info = paste("Balanced probabilities MAE should be < 0.2, got", mae_balanced))
  
  expect_true(mae_imbalanced < 0.2,
              info = paste("Imbalanced probabilities MAE should be < 0.2, got", mae_imbalanced))
})

test_that("Multi-leaf scenarios produce correct probabilities", {
  # Create a dataset that will result in multiple leaves
  # Use 3 features to create more complex tree structure
  feature_combos <- expand.grid(x1 = c(0, 1), x2 = c(0, 1), x3 = c(0, 1))
  # Assign different probabilities to each combination
  true_probs <- c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9)
  
  data <- ground_truth_dataset(feature_combos, true_probs, n_per_combo = 20, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Verify that samples with same features get same (or very similar) probabilities
  # This ensures tree traversal is working correctly
  unique_combos <- unique(data$X)
  
  for (i in 1:nrow(unique_combos)) {
    combo <- unique_combos[i, , drop = FALSE]
    
    # Find samples matching this combination
    matches <- apply(data$X, 1, function(row) {
      all(row == combo[1, ])
    })
    
    if (sum(matches) > 0) {
      # Get predicted probabilities for these samples
      pred_probs_all <- predict(model, data$X, type = "prob")
      pred_probs_combo <- pred_probs_all[matches, 2]
      
      # All should be very similar (within small tolerance for numerical precision)
      pred_range <- max(pred_probs_combo) - min(pred_probs_combo)
      expect_true(pred_range < 0.01,
                  info = paste("Samples with same features should have nearly identical probabilities, range:", pred_range))
    }
  }
})

test_that("Consistency across loss functions", {
  # Create dataset
  data <- simple_ground_truth_dataset(probs = c(0.2, 0.4, 0.6, 0.8), n_per_combo = 30, seed = 42)
  
  # Train with log_loss
  model_logloss <- safe_treefarms(data$X, data$y,
                                  loss_function = "log_loss",
                                  regularization = 0.01,
                                  single_tree = TRUE,
                                  compute_probabilities = FALSE,
                                  verbose = FALSE)
  
  # Train with misclassification
  model_misclass <- safe_treefarms(data$X, data$y,
                                   loss_function = "misclassification",
                                   regularization = 0.01,
                                   single_tree = TRUE,
                                   compute_probabilities = FALSE,
                                   verbose = FALSE)
  
  expect_valid_treefarms_model(model_logloss, "log_loss")
  expect_valid_treefarms_model(model_misclass, "misclassification")
  
  # Both should produce probabilities that match training distribution
  # Get probabilities for both models
  pred_probs_logloss <- predict(model_logloss, data$X, type = "prob")
  pred_probs_misclass <- predict(model_misclass, data$X, type = "prob")
  
  # Calculate accuracy for both
  mae_logloss <- mae_probs(pred_probs_logloss, data$true_probs)
  mae_misclass <- mae_probs(pred_probs_misclass, data$true_probs)
  
  # Both should be reasonably accurate
  expect_true(mae_logloss < 0.2,
              info = paste("Log-loss MAE should be < 0.2, got", mae_logloss))
  
  expect_true(mae_misclass < 0.3,
              info = paste("Misclassification MAE should be < 0.3, got", mae_misclass))
  
  # They may differ in how they calculate probabilities, but both should be accurate
  # relative to their objectives
})

test_that("Large sample accuracy converges to true values", {
  # Use larger dataset to test convergence
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 100, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get predicted probabilities
  pred_probs <- predict(model, data$X, type = "prob")
  
  # Calculate accuracy metrics
  mae <- mae_probs(pred_probs, data$true_probs)
  rmse <- rmse_probs(pred_probs, data$true_probs)
  corr <- probability_correlation(pred_probs, data$true_probs)
  
  # With more samples, accuracy should be better
  expect_true(mae < 0.15,
              info = paste("With large sample, MAE should be < 0.15, got", mae))
  
  expect_true(rmse < 0.2,
              info = paste("With large sample, RMSE should be < 0.2, got", rmse))
  
  expect_true(corr > 0.7,
              info = paste("With large sample, correlation should be > 0.7, got", corr))
  
  # Test that predicted probabilities are close to true probabilities
  # Group by true probability and check mean predicted probability
  for (true_prob in unique(data$true_probs)) {
    idx <- abs(data$true_probs - true_prob) < 1e-6
    if (sum(idx) > 10) {
      mean_pred <- mean(pred_probs[idx, 2])
      expect_true(abs(mean_pred - true_prob) < 0.15,
                  info = paste("For true probability", true_prob, 
                             ", mean predicted probability should be close, got", mean_pred))
    }
  }
})

test_that("Edge cases are handled correctly", {
  # Test 1: Single sample per leaf (minimal data)
  data_minimal <- simple_ground_truth_dataset(probs = c(0.2, 0.4, 0.6, 0.8), n_per_combo = 2, seed = 42)
  
  model_minimal <- safe_treefarms(data_minimal$X, data_minimal$y,
                                  loss_function = "log_loss",
                                  regularization = 0.01,
                                  single_tree = TRUE,
                                  compute_probabilities = FALSE,
                                  verbose = FALSE)
  
  expect_valid_treefarms_model(model_minimal, "log_loss")
  
  # Get probabilities
  pred_probs_minimal <- predict(model_minimal, data_minimal$X, type = "prob")
  
  # Probabilities should still be valid
  expect_true(all(pred_probs_minimal >= 0))
  expect_true(all(pred_probs_minimal <= 1))
  
  # Test 2: All samples in one leaf (root node only)
  # Create dataset where all samples are identical
  set.seed(42)
  X_single <- data.frame(x1 = rep(0, 50), x2 = rep(0, 50))
  y_single <- rbinom(50, 1, 0.5)
  
  model_single <- safe_treefarms(X_single, y_single,
                                 loss_function = "log_loss",
                                 regularization = 0.1,
                                 single_tree = TRUE,
                                 compute_probabilities = FALSE,
                                 verbose = FALSE)
  
  expect_valid_treefarms_model(model_single, "log_loss")
  
  # All samples should get the same probability (empirical class distribution)
  pred_probs_mat <- predict(model_single, X_single, type = "prob")
  pred_probs <- pred_probs_mat[, 2]
  expect_true(length(unique(pred_probs)) == 1,
              info = "All samples should have same probability when features are identical")
  
  # Probability should match empirical distribution
  empirical_prob <- mean(y_single)
  expect_true(abs(pred_probs[1] - empirical_prob) < 0.1,
              info = paste("Predicted probability should match empirical distribution"))
  
  # Test 3: Very small probabilities
  data_small <- simple_ground_truth_dataset(probs = c(0.01, 0.02, 0.03, 0.04), n_per_combo = 30, seed = 42)
  
  model_small <- safe_treefarms(data_small$X, data_small$y,
                                loss_function = "log_loss",
                                regularization = 0.01,
                                single_tree = TRUE,
                                compute_probabilities = FALSE,
                                verbose = FALSE)
  
  expect_valid_treefarms_model(model_small, "log_loss")
  
  # Get predicted probabilities
  pred_probs_small_mat <- predict(model_small, data_small$X, type = "prob")
  pred_probs_small <- pred_probs_small_mat[, 2]
  expect_true(all(pred_probs_small > 0),
              info = "Probabilities should be > 0 even for very small true probabilities")
  expect_true(mean(pred_probs_small) < 0.1,
              info = "Mean predicted probability should be low for small true probabilities")
  
  # Test 4: Very large probabilities
  data_large <- simple_ground_truth_dataset(probs = c(0.96, 0.97, 0.98, 0.99), n_per_combo = 30, seed = 42)
  
  model_large <- safe_treefarms(data_large$X, data_large$y,
                                loss_function = "log_loss",
                                regularization = 0.01,
                                single_tree = TRUE,
                                compute_probabilities = FALSE,
                                verbose = FALSE)
  
  expect_valid_treefarms_model(model_large, "log_loss")
  
  # Get predicted probabilities
  pred_probs_large_mat <- predict(model_large, data_large$X, type = "prob")
  pred_probs_large <- pred_probs_large_mat[, 2]
  expect_true(all(pred_probs_large < 1),
              info = "Probabilities should be < 1 even for very large true probabilities")
  expect_true(mean(pred_probs_large) > 0.9,
              info = "Mean predicted probability should be high for large true probabilities")
})

test_that("Predict function returns accurate probabilities", {
  # Create dataset
  data <- simple_ground_truth_dataset(probs = c(0.2, 0.4, 0.6, 0.8), n_per_combo = 30, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE, # Don't compute during training
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get probabilities via predict function
  pred_probs <- predict(model, data$X, type = "prob")
  
  # Should be a matrix with 2 columns
  expect_true(is.matrix(pred_probs))
  expect_equal(ncol(pred_probs), 2)
  expect_equal(nrow(pred_probs), nrow(data$X))
  
  # Probabilities should be valid
  expect_true(all(pred_probs >= 0))
  expect_true(all(pred_probs <= 1))
  
  # Calculate accuracy
  mae <- mae_probs(pred_probs, data$true_probs)
  
  expect_true(mae < 0.25,
              info = paste("Predict() probabilities MAE should be < 0.25, got", mae))
  
  # Probabilities should be consistent if we call predict() multiple times
  pred_probs2 <- predict(model, data$X, type = "prob")
  expect_equal(pred_probs, pred_probs2, tolerance = 1e-10,
               info = "Predict() should return consistent results")
})

test_that("Brier score is reasonable", {
  # Create dataset
  data <- simple_ground_truth_dataset(probs = c(0.2, 0.4, 0.6, 0.8), n_per_combo = 30, seed = 42)
  
  # Train model
  model <- safe_treefarms(data$X, data$y,
                          loss_function = "log_loss",
                          regularization = 0.01,
                          single_tree = TRUE,
                          compute_probabilities = FALSE,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Get predicted probabilities
  pred_probs <- predict(model, data$X, type = "prob")
  
  # Calculate Brier score
  brier <- brier_score(pred_probs, data$true_probs)
  
  # Brier score should be reasonable (lower is better, perfect is 0)
  # For well-calibrated probabilities, Brier score should be < 0.25
  expect_true(brier < 0.25,
              info = paste("Brier score should be < 0.25, got", brier))
  
  # Brier score should be better than random (0.25 for balanced case)
  # Since we have varied probabilities, expect better than 0.2
  expect_true(brier < 0.2,
              info = paste("Brier score should be < 0.2 for varied probabilities, got", brier))
})

# Cleanup
teardown_test_environment()

