# Test suite for log-loss specific functionality
# Consolidated to reduce redundant model training

library(testthat)
# Setup/teardown now handled by testthat hooks in helper-setup.R

test_that("log-loss training completes and produces valid output", {
  # Test with simple dataset - train once, validate multiple properties
  model_simple <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                    loss_function = "log_loss",
                                    regularization = 0.1,
                                    compute_probabilities = TRUE,
                                    verbose = FALSE)

  expect_valid_treefarms_model(model_simple, "log_loss")

  # Probabilities should be between 0 and 1
  expect_true(all(model_simple@probabilities >= 0))
  expect_true(all(model_simple@probabilities <= 1))

  # Each row should sum to 1
  row_sums <- rowSums(model_simple@probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-10))

  # Test with pattern dataset
  model_pattern <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.01,
                                     compute_probabilities = TRUE,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_pattern, "log_loss")
})

test_that("log-loss vs misclassification select different trees on calibration signal", {
  # RE-BASELINED 2026-06-30 (loss-normalization fix). The previous version ran
  # both losses on `simple_dataset` (pure noise: random binary X, random y) and
  # asserted their probabilities differ. That only "passed" because the OLD
  # summed-scale log_loss had an ~n-times-too-weak penalty and spuriously
  # over-split the noise. With log_loss now mean-normalized (matching Xu et al.
  # 2026 / the manuscript), BOTH losses correctly select the stump on noise and
  # return the identical base-rate probability -- so the old assertion is no
  # longer a valid invariant. See
  # quality_reports/plans/2026-06-30_loss-normalization-fix.md
  #
  # Probabilities are leaf empirical proportions under BOTH losses, so they can
  # only differ when the two losses select DIFFERENT trees. The genuine,
  # theory-grounded difference is calibration: a split that sharpens within-leaf
  # probabilities WITHOUT changing any leaf's majority class is invisible to
  # misclassification (no prediction changes) but rewarded by log_loss. This DGP
  # constructs exactly that: within feature_1 == 1 the majority is always class
  # 1, but feature_2 separates p = 0.6 from p = 0.95.
  set.seed(7)
  n <- 400
  f1 <- sample(0:1, n, replace = TRUE)
  f2 <- sample(0:1, n, replace = TRUE)
  f3 <- sample(0:1, n, replace = TRUE)
  prob <- ifelse(f1 == 0, 0.1, ifelse(f2 == 0, 0.6, 0.95))
  y <- rbinom(n, 1, prob)
  X <- data.frame(feature_1 = f1, feature_2 = f2, feature_3 = f3)

  model_logloss <- safe_optimaltrees(X, y,
                                     loss_function = "log_loss",
                                     regularization = 0.02,
                                     compute_probabilities = TRUE,
                                     verbose = FALSE)

  model_misclass <- safe_optimaltrees(X, y,
                                      loss_function = "misclassification",
                                      regularization = 0.02,
                                      compute_probabilities = TRUE,
                                      verbose = FALSE)

  expect_valid_treefarms_model(model_logloss, "log_loss")
  expect_valid_treefarms_model(model_misclass, "misclassification")

  # log_loss rewards the calibration split (feature_2) that misclassification
  # ignores, so it yields strictly more distinct probability levels.
  n_levels_logloss <- nrow(unique(model_logloss@probabilities))
  n_levels_misclass <- nrow(unique(model_misclass@probabilities))
  expect_gt(n_levels_logloss, n_levels_misclass)

  # And therefore the probability vectors must differ.
  expect_false(identical(model_logloss@probabilities, model_misclass@probabilities))
})

test_that("log-loss handles various datasets", {
  # Entropy dataset (high entropy, good for log-loss)
  model_entropy <- safe_optimaltrees(entropy_dataset$X, entropy_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.1,
                                     compute_probabilities = TRUE,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_entropy, "log_loss")
  expect_true(all(model_entropy@probabilities >= 0))
  expect_true(all(model_entropy@probabilities <= 1))

  # Probabilities should show some variation
  prob_range <- range(model_entropy@probabilities)
  expect_true(prob_range[2] - prob_range[1] > 0.1)

  # Imbalanced dataset
  model_imbalanced <- safe_optimaltrees(imbalanced_dataset$X, imbalanced_dataset$y,
                                        loss_function = "log_loss",
                                        regularization = 0.1,
                                        compute_probabilities = TRUE,
                                        verbose = FALSE)

  expect_valid_treefarms_model(model_imbalanced, "log_loss")
  expect_true(is.finite(model_imbalanced@accuracy))

  # NOTE: many_features_dataset (p=20) with log-loss causes OOM due to
  # exponential tree enumeration. Tested via core-fitting with misclassification.
  # Log-loss specific behavior adequately tested with p=2-3 datasets above.
})

test_that("log-loss with different regularization values", {
  # Test extremes: very low and very high
  regularization_values <- c(0.01, 1.0)

  for (reg in regularization_values) {
    model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                               loss_function = "log_loss",
                               regularization = reg,
                               compute_probabilities = TRUE,
                               verbose = FALSE)

    expect_valid_treefarms_model(model, "log_loss")
    expect_equal(model@regularization, reg)

    # All models should produce valid probabilities
    expect_true(all(model@probabilities >= 0))
    expect_true(all(model@probabilities <= 1))

    row_sums <- rowSums(model@probabilities)
    expect_true(all(abs(row_sums - 1) < 1e-10))

    # Explicit cleanup
    rm(model)
  }
  gc()  # Force collection after loop
})

test_that("log-loss regression test - consistent behavior", {
  # Test that log-loss produces consistent results across runs
  set.seed(123)
  model1 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              compute_probabilities = TRUE,
                              verbose = FALSE)

  set.seed(123)
  model2 <- safe_optimaltrees(pattern_dataset$X, pattern_dataset$y,
                              loss_function = "log_loss",
                              regularization = 0.1,
                              compute_probabilities = TRUE,
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
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")
  expect_true(model@regularization > 0)
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
})

test_that("log-loss handles edge cases", {
  # Minimal data
  model_minimal <- safe_optimaltrees(minimal_dataset$X, minimal_dataset$y,
                                     loss_function = "log_loss",
                                     regularization = 0.1,
                                     compute_probabilities = TRUE,
                                     verbose = FALSE)

  expect_valid_treefarms_model(model_minimal, "log_loss")

  # Single class data
  model_single <- safe_optimaltrees(single_class_dataset$X, single_class_dataset$y,
                                    loss_function = "log_loss",
                                    regularization = 0.1,
                                    compute_probabilities = TRUE,
                                    verbose = FALSE)

  expect_valid_treefarms_model(model_single, "log_loss")
})

test_that("log-loss numerical stability with extreme regularization", {
  # Very low regularization
  model_low <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                 loss_function = "log_loss",
                                 regularization = 0.001,
                                 compute_probabilities = TRUE,
                                 verbose = FALSE)

  expect_valid_treefarms_model(model_low, "log_loss")
  expect_true(is.matrix(model_low@probabilities))
  expect_true(all(model_low@probabilities >= 0))
  expect_true(all(model_low@probabilities <= 1))

  # Very high regularization
  model_high <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                                  loss_function = "log_loss",
                                  regularization = 10.0,
                                  compute_probabilities = TRUE,
                                  verbose = FALSE)

  expect_valid_treefarms_model(model_high, "log_loss")
  expect_true(is.matrix(model_high@probabilities))
  expect_true(all(model_high@probabilities >= 0))
  expect_true(all(model_high@probabilities <= 1))

  # Row sums should be 1
  expect_true(all(abs(rowSums(model_low@probabilities) - 1) < 1e-10))
  expect_true(all(abs(rowSums(model_high@probabilities) - 1) < 1e-10))
})

test_that("log-loss worker_limit is enforced", {
  # For log-loss, worker_limit should be 1 (enforced in C++ code)
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             worker_limit = 1,
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")
})

test_that("log-loss cross-entropy calculation is reasonable", {
  model <- safe_optimaltrees(simple_dataset$X, simple_dataset$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")

  # Compute cross-entropy manually (clip to avoid log(0))
  probs_clipped <- pmax(model@probabilities, 1e-15)
  probs_clipped <- pmin(probs_clipped, 1 - 1e-15)
  manual_loss <- -mean(simple_dataset$y * log(probs_clipped[, 2]) +
                       (1 - simple_dataset$y) * log(probs_clipped[, 1]))

  # Loss should be finite and non-negative
  expect_true(is.finite(manual_loss))
  expect_true(manual_loss >= 0)
})

test_that("log-loss bounds hold under perfect separation", {
  # Perfect separation should still produce valid probabilities
  test_data <- create_perfect_separation_data(n_samples = 100, seed = 42)

  model <- safe_optimaltrees(test_data$X, test_data$y,
                             loss_function = "log_loss",
                             regularization = 0.1,
                             compute_probabilities = TRUE,
                             verbose = FALSE)

  expect_valid_treefarms_model(model, "log_loss")

  # Probabilities should be valid (in [0, 1], rows sum to 1)
  expect_true(all(model@probabilities >= 0))
  expect_true(all(model@probabilities <= 1))
  expect_true(all(abs(rowSums(model@probabilities) - 1) < 1e-10))
})
