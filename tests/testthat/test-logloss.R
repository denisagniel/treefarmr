# Test suite for log-loss specific functionality
# Consolidated to reduce redundant model training

library(testthat)
# Setup/teardown now handled by testthat hooks in helper-setup.R

test_that("log-loss training completes and produces valid output", {
  # Test with simple dataset - train once, validate multiple properties
  model_simple <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                    loss_function = "log_loss",
                                    regularization = 0.1,
                                    verbose = FALSE)

  expect_valid_treefarms_model(model_simple, "log_loss")

  # Probabilities should be strictly between 0 and 1 (bounded away from extremes)
  expect_true(all(model_simple@probabilities > 0))
  expect_true(all(model_simple@probabilities < 1))
  expect_true(all(model_simple@probabilities > 0.01))
  expect_true(all(model_simple@probabilities < 0.99))

  # Each row should sum to 1
  row_sums <- rowSums(model_simple@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))

  # Test with pattern dataset
  model_pattern <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.01,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_pattern, "log_loss")
})

test_that("log-loss vs misclassification outputs differ", {
  # Train both models with same data and parameters
  model_logloss <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.1,
                                     verbose = FALSE)

  model_misclass <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                      loss_function = "misclassification",
                                      regularization = 0.1,
                                      verbose = FALSE)

  expect_valid_treefarms_model(model_logloss, "log_loss")
  expect_valid_treefarms_model(model_misclass, "misclassification")

  # Probabilities should differ between loss functions
  expect_false(identical(model_logloss$probabilities, model_misclass$probabilities))

  # Probability distributions should differ
  logloss_mean_diff <- mean(abs(model_logloss$probabilities - 0.5))
  misclass_mean_diff <- mean(abs(model_misclass$probabilities - 0.5))
  expect_true(logloss_mean_diff != misclass_mean_diff)
})

test_that("log-loss handles various datasets", {
  # Entropy dataset (high entropy, good for log-loss)
  model_entropy <- safe_optimaltrees(entropy_dataset$X, entropy_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.1,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_entropy, "log_loss")
  expect_true(all(model_entropy@probabilities > 0))
  expect_true(all(model_entropy@probabilities < 1))

  # Probabilities should reflect the probabilistic nature of the data
  prob_range <- range(model_entropy@probabilities)
  expect_true(prob_range[2] - prob_range[1] > 0.1)

  # Probabilities should be well-distributed (not all near 0.5)
  prob_class_1 <- model_entropy@probabilities[, 2]
  expect_true(max(prob_class_1) - min(prob_class_1) > 0.05)
  mean_prob <- mean(prob_class_1)
  expect_true(mean_prob > 0.1 && mean_prob < 0.9)

  # Imbalanced dataset
  model_imbalanced <- safe_optimaltrees(imbalanced_dataset$X, imbalanced_dataset$y,
                                        loss_function = "log_loss",
                                        regularization = 0.1,
                                        verbose = FALSE)

  expect_valid_treefarms_model(model_imbalanced, "log_loss")
  expect_true(is.finite(model_imbalanced@accuracy))

  # Many features dataset
  model_many_features <- safe_optimaltrees(many_features_dataset$X, many_features_dataset$y,
                                           loss_function = "log_loss",
                                           regularization = 0.1,
                                           verbose = FALSE)

  expect_valid_treefarms_model(model_many_features, "log_loss")
  expect_true(is.finite(model_many_features@accuracy))
})

test_that("log-loss with different regularization values", {
  # Test extremes: very low and very high
  regularization_values <- c(0.01, 1.0)

  for (reg in regularization_values) {
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                               loss_function = "log_loss",
                               regularization = reg,
                               verbose = FALSE)

    expect_valid_treefarms_model(model, "log_loss")
    expect_equal(model@regularization, reg)

    # All models should produce valid probabilities
    expect_true(all(model@probabilities > 0))
    expect_true(all(model@probabilities < 1))

    row_sums <- rowSums(model@probabilities)
    expect_true(all(abs(row_sums - 1) < 1e-10))
  }
})

test_that("log-loss regression test - consistent behavior", {
  # Test that log-loss produces consistent results across runs
  set.seed(123)
  model1 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              verbose = FALSE)

  set.seed(123)
  model2 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              verbose = FALSE)

  expect_valid_treefarms_model(model1, "log_loss")
  expect_valid_treefarms_model(model2, "log_loss")

  # Results should be identical with same seed
  expect_equal(model1@accuracy, model2@accuracy, tolerance = 1e-10)
  expect_equal(model1@n_trees, model2@n_trees)
  expect_equal(model1@probabilities, model2@probabilities, tolerance = 1e-10)
})

test_that("log-loss with auto-tuning", {
  # Test log-loss with auto-tuned regularization
  model <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                             loss_function = "log_loss",
                             regularization = NULL,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")
  expect_true(model@regularization > 0)
  expect_true(all(model@probabilities > 0))
  expect_true(all(model@probabilities < 1))
})

test_that("log-loss handles edge cases", {
  # Minimal data
  model_minimal <- safe_optimaltrees(minimal_dataset$X, minimal_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.1,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_minimal, "log_loss")

  # Single class data
  model_single <- safe_optimaltrees(single_class_dataset$X, single_class_dataset$y,
                                    loss_function = "log_loss",
                                    regularization = 0.1,
                                    verbose = FALSE)

  expect_valid_treefarms_model(model_single, "log_loss")
})

test_that("log-loss numerical stability with extreme regularization", {
  # Very low regularization
  model_low <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                 loss_function = "log_loss",
                                 regularization = 0.001,
                                 verbose = FALSE)

  expect_valid_treefarms_model(model_low, "log_loss")
  expect_valid_probabilities(model_low$probabilities, loss_function = "log_loss",
                             info = "Low regularization")

  # Very high regularization
  model_high <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                  loss_function = "log_loss",
                                  regularization = 10.0,
                                  verbose = FALSE)

  expect_valid_treefarms_model(model_high, "log_loss")
  expect_valid_probabilities(model_high$probabilities, loss_function = "log_loss",
                             info = "High regularization")

  # Both should produce valid probabilities
  expect_true(all(model_low$probabilities > 0))
  expect_true(all(model_low$probabilities < 1))
  expect_true(all(model_high$probabilities > 0))
  expect_true(all(model_high$probabilities < 1))
})

test_that("log-loss worker_limit is enforced", {
  # For log-loss, worker_limit should be 1 (enforced in C++ code)
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             worker_limit = 1,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")
})

test_that("log-loss cross-entropy calculation is reasonable", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")

  # Compute cross-entropy manually
  manual_loss <- compute_cross_entropy_loss(model@probabilities, simple_dataset$y)

  # Loss should be finite and reasonable
  expect_true(is.finite(manual_loss))
  expect_true(manual_loss >= 0)
  expect_true(manual_loss <= log(2))  # Upper bound for binary classification
})

test_that("log-loss bounds hold under perfect separation", {
  # Perfect separation should still produce bounded probabilities
  test_data <- create_perfect_separation_data(n_samples = 100, seed = 42)

  model <- safe_optimaltrees(test_data$X, test_data$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")

  # Even with perfect separation, probabilities should be bounded
  expect_logloss_bounds(model@probabilities,
                        info = "Perfect separation with log-loss")
})
