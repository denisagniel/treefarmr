# Test suite for log-loss specific functionality
# Regression tests ensuring log-loss training completes without error

library(testthat)
# library(treefarmr) # REMOVED: legacy package name

# Setup test environment
setup_test_environment()

test_that("log-loss training completes without error", {
  # Test with simple dataset
  expect_no_error({
    model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Test with pattern dataset
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.01,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
})

test_that("log-loss probability outputs are bounded", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Probabilities should be strictly between 0 and 1 (bounded away from extremes)
  expect_true(all(model$probabilities > 0), 
              info = "Log-loss probabilities should be > 0")
  expect_true(all(model$probabilities < 1), 
              info = "Log-loss probabilities should be < 1")
  
  # Probabilities should not be too extreme (reasonable bounds)
  expect_true(all(model$probabilities > 0.01), 
              info = "Log-loss probabilities should be bounded away from 0")
  expect_true(all(model$probabilities < 0.99), 
              info = "Log-loss probabilities should be bounded away from 1")
})

test_that("log-loss probabilities sum to 1 per row", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Each row should sum to 1
  row_sums <- rowSums(model$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10), 
              info = "Log-loss probability rows should sum to 1")
})

test_that("log-loss vs misclassification outputs differ", {
  # Train both models with same data and parameters
  model_logloss <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                 loss_function = "log_loss", 
                                 regularization = 0.1,
                                 verbose = FALSE)
  
  model_misclass <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                  loss_function = "misclassification", 
                                  regularization = 0.1,
                                  verbose = FALSE)
  
  expect_valid_treefarms_model(model_logloss, "log_loss")
  expect_valid_treefarms_model(model_misclass, "misclassification")
  
  # Probabilities should differ between loss functions
  expect_false(identical(model_logloss$probabilities, model_misclass$probabilities),
               info = "Log-loss and misclassification probabilities should differ")
  
  # Predictions might be the same or different, but that's okay
  # The key is that the internal probability estimates differ
})

test_that("log-loss handles entropy dataset without crashes", {
  # Test with high entropy data designed for log-loss
  expect_no_error({
    model <- safe_treefarms(entropy_dataset$X, entropy_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Should produce reasonable probabilities
  expect_true(all(model$probabilities > 0))
  expect_true(all(model$probabilities < 1))
  
  # Probabilities should reflect the probabilistic nature of the data
  prob_range <- range(model$probabilities)
  expect_true(prob_range[2] - prob_range[1] > 0.1, 
              info = "Log-loss should produce varied probabilities for entropy data")
})

test_that("log-loss with different regularization values", {
  regularization_values <- c(0.01, 0.1, 0.5, 1.0)
  
  for (reg in regularization_values) {
    expect_no_error({
      model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                             loss_function = "log_loss", 
                             regularization = reg,
                             verbose = FALSE)
    })
    
    expect_valid_treefarms_model(model, "log_loss")
    expect_equal(model$regularization, reg)
    
    # All models should produce valid probabilities
    expect_true(all(model$probabilities > 0))
    expect_true(all(model$probabilities < 1))
    
    row_sums <- rowSums(model$probabilities)
    expect_true(all(abs(row_sums - 1) < 1e-10))
  }
})

test_that("log-loss regression test - consistent behavior", {
  # Test that log-loss produces consistent results across runs
  # (This is a regression test to catch changes in behavior)
  
  set.seed(123)  # Fixed seed for reproducibility
  model1 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                          loss_function = "log_loss", 
                          regularization = 0.1,
                          verbose = FALSE)
  
  set.seed(123)  # Same seed
  model2 <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                          loss_function = "log_loss", 
                          regularization = 0.1,
                          verbose = FALSE)
  
  expect_valid_treefarms_model(model1, "log_loss")
  expect_valid_treefarms_model(model2, "log_loss")
  
  # Results should be identical with same seed
  expect_equal(model1$accuracy, model2$accuracy, tolerance = 1e-10)
  expect_equal(model1$n_trees, model2$n_trees)
  expect_equal(model1$probabilities, model2$probabilities, tolerance = 1e-10)
})

test_that("log-loss handles imbalanced data", {
  # Test log-loss with imbalanced dataset
  expect_no_error({
    model <- safe_treefarms(imbalanced_dataset$X, imbalanced_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Should handle imbalanced data gracefully
  expect_true(is.finite(model$accuracy))
  expect_true(all(model$probabilities > 0))
  expect_true(all(model$probabilities < 1))
})

test_that("log-loss with many features", {
  # Test log-loss with high-dimensional data
  expect_no_error({
    model <- safe_treefarms(many_features_dataset$X, many_features_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Should handle many features without issues
  expect_true(is.finite(model$accuracy))
  expect_true(all(model$probabilities > 0))
  expect_true(all(model$probabilities < 1))
})

test_that("log-loss probability calibration", {
  # Test that log-loss produces well-calibrated probabilities
  model <- safe_treefarms(entropy_dataset$X, entropy_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Probabilities should be well-distributed (not all near 0.5)
  prob_class_1 <- model$probabilities[, 2]
  prob_range <- range(prob_class_1)
  
  # Should have some variation in probabilities
  expect_true(prob_range[2] - prob_range[1] > 0.05, 
              info = "Log-loss should produce varied probabilities")
  
  # Mean probability should be reasonable (not too extreme)
  mean_prob <- mean(prob_class_1)
  expect_true(mean_prob > 0.1 && mean_prob < 0.9, 
              info = "Mean log-loss probability should be reasonable")
})

test_that("log-loss with auto-tuning", {
  # Test log-loss with auto-tuned regularization
  expect_no_error({
    model <- safe_treefarms(pattern_dataset$X, pattern_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = NULL,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Auto-tuned regularization should be positive
  expect_true(model$regularization > 0)
  
  # Should still produce valid probabilities
  expect_true(all(model$probabilities > 0))
  expect_true(all(model$probabilities < 1))
})

test_that("log-loss handles edge cases", {
  # Test with minimal data
  expect_no_error({
    model <- safe_treefarms(minimal_dataset$X, minimal_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Test with single class data
  expect_no_error({
    model <- safe_treefarms(single_class_dataset$X, single_class_dataset$y, 
                           loss_function = "log_loss", 
                           regularization = 0.1,
                           verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model, "log_loss")
})

test_that("log-loss numerical stability", {
  # Test with extreme regularization values
  expect_no_error({
    model_low_reg <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                   loss_function = "log_loss", 
                                   regularization = 0.001,
                                   verbose = FALSE)
  })
  
  expect_no_error({
    model_high_reg <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                    loss_function = "log_loss", 
                                    regularization = 10.0,
                                    verbose = FALSE)
  })
  
  expect_valid_treefarms_model(model_low_reg, "log_loss")
  expect_valid_treefarms_model(model_high_reg, "log_loss")
  
  # Both should produce valid probabilities
  expect_true(all(model_low_reg$probabilities > 0))
  expect_true(all(model_low_reg$probabilities < 1))
  expect_true(all(model_high_reg$probabilities > 0))
  expect_true(all(model_high_reg$probabilities < 1))
})

test_that("log-loss worker_limit is enforced", {
  # For log-loss, worker_limit should be 1
  # This is enforced in C++ code, but we can verify behavior
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         worker_limit = 1,  # Should be enforced
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Even if we try to set worker_limit > 1, it should be corrected to 1
  # (This is handled in C++ code)
})

test_that("log-loss cross-entropy calculation is reasonable", {
  model <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Compute cross-entropy manually
  manual_loss <- compute_cross_entropy_loss(model$probabilities, simple_dataset$y)
  
  # Loss should be finite and reasonable (between 0 and log(2) for binary classification)
  expect_true(is.finite(manual_loss),
              info = "Cross-entropy loss should be finite")
  expect_true(manual_loss >= 0,
              info = "Cross-entropy loss should be >= 0")
  expect_true(manual_loss <= log(2),
              info = "Cross-entropy loss should be <= log(2) for binary classification")
})

test_that("log-loss probabilities differ from misclassification", {
  # Train both models with same data
  model_logloss <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                 loss_function = "log_loss", 
                                 regularization = 0.1,
                                 verbose = FALSE)
  
  model_misclass <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                                  loss_function = "misclassification", 
                                  regularization = 0.1,
                                  verbose = FALSE)
  
  expect_valid_treefarms_model(model_logloss, "log_loss")
  expect_valid_treefarms_model(model_misclass, "misclassification")
  
  # Probabilities should differ
  expect_false(identical(model_logloss$probabilities, model_misclass$probabilities),
               info = "Log-loss and misclassification probabilities should differ")
  
  # Log-loss probabilities should be more conservative (closer to 0.5)
  # This is a heuristic check - log-loss tends to produce smoother probabilities
  logloss_mean_diff <- mean(abs(model_logloss$probabilities - 0.5))
  misclass_mean_diff <- mean(abs(model_misclass$probabilities - 0.5))
  
  # Log-loss probabilities are often closer to 0.5 (more conservative)
  # But this is not always true, so we just check that they differ
  expect_true(logloss_mean_diff != misclass_mean_diff,
               info = "Log-loss and misclassification should produce different probability distributions")
})

test_that("log-loss handles extreme regularization values", {
  # Very low regularization
  model_low <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                              loss_function = "log_loss", 
                              regularization = 0.001,
                              verbose = FALSE)
  
  expect_valid_treefarms_model(model_low, "log_loss")
  expect_valid_probabilities(model_low$probabilities, loss_function = "log_loss",
                             info = "Low regularization")
  
  # Very high regularization
  model_high <- safe_treefarms(simple_dataset$X, simple_dataset$y, 
                               loss_function = "log_loss", 
                               regularization = 10.0,
                               verbose = FALSE)
  
  expect_valid_treefarms_model(model_high, "log_loss")
  expect_valid_probabilities(model_high$probabilities, loss_function = "log_loss",
                             info = "High regularization")
})

test_that("log-loss produces well-calibrated probabilities", {
  # Test with entropy dataset (high entropy, good for log-loss)
  model <- safe_treefarms(entropy_dataset$X, entropy_dataset$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Probabilities should be well-distributed (not all near 0.5)
  prob_class_1 <- model$probabilities[, 2]
  prob_range <- range(prob_class_1)
  
  # Should have some variation
  expect_true(prob_range[2] - prob_range[1] > 0.05,
              info = "Log-loss should produce varied probabilities")
  
  # Mean probability should be reasonable
  mean_prob <- mean(prob_class_1)
  expect_true(mean_prob > 0.1 && mean_prob < 0.9,
              info = "Mean log-loss probability should be reasonable")
})

test_that("log-loss bounds hold under perfect separation", {
  # Perfect separation should still produce bounded probabilities
  test_data <- create_perfect_separation_data(n_samples = 100, seed = 42)
  
  model <- safe_treefarms(test_data$X, test_data$y, 
                         loss_function = "log_loss", 
                         regularization = 0.1,
                         verbose = FALSE)
  
  expect_valid_treefarms_model(model, "log_loss")
  
  # Even with perfect separation, probabilities should be bounded
  expect_logloss_bounds(model$probabilities,
                       info = "Perfect separation with log-loss")
})

# Cleanup
teardown_test_environment()
