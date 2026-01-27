# Comprehensive Log-Loss Testing Suite
# Tests probability accuracy and runtime performance for all primary log-loss functionality

library(testthat)
library(treefarmr)

# Setup test environment
setup_test_environment()

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
# Accuracy Metrics (from helper-probabilities.R)
# ============================================================================

#' Calculate Mean Absolute Error between predicted and true probabilities
mae_probs <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  mean(abs(pred_probs - true_probs))
}

#' Calculate Root Mean Squared Error
rmse_probs <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  sqrt(mean((pred_probs - true_probs)^2))
}

#' Calculate Brier score (mean squared error)
brier_score <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  mean((pred_probs - true_probs)^2)
}

#' Calculate correlation between predicted and true probabilities
probability_correlation <- function(pred_probs, true_probs) {
  if (is.matrix(pred_probs)) {
    pred_probs <- pred_probs[, 2]
  }
  cor(pred_probs, true_probs)
}

#' Compute cross-entropy loss manually for verification
compute_cross_entropy_loss <- function(probabilities, y_true) {
  p1 <- probabilities[, 2]
  p0 <- probabilities[, 1]
  
  epsilon <- 1e-15
  p1 <- pmax(p1, epsilon)
  p0 <- pmax(p0, epsilon)
  
  loss <- -sum(y_true * log(p1) + (1 - y_true) * log(p0))
  loss <- loss / length(y_true)
  
  return(loss)
}

#' Calculate calibration (mean predicted probability vs true class frequency)
calibration_metric <- function(probabilities, y_true) {
  if (is.matrix(probabilities)) {
    mean_pred_prob <- mean(probabilities[, 2])
  } else {
    mean_pred_prob <- mean(probabilities)
  }
  mean_true_freq <- mean(y_true)
  abs(mean_pred_prob - mean_true_freq)
}

# ============================================================================
# Results Collection and Reporting
# ============================================================================

#' Collect all probability accuracy metrics
collect_accuracy_metrics <- function(pred_probs, true_probs, y_true) {
  list(
    mae = mae_probs(pred_probs, true_probs),
    rmse = rmse_probs(pred_probs, true_probs),
    brier = brier_score(pred_probs, true_probs),
    cross_entropy = compute_cross_entropy_loss(pred_probs, y_true),
    correlation = probability_correlation(pred_probs, true_probs),
    calibration = calibration_metric(pred_probs, y_true)
  )
}

#' Collect runtime metrics from system.time() output
collect_runtime_metrics <- function(timing_result) {
  list(
    elapsed = timing_result["elapsed"],
    user = timing_result["user.self"],
    system = timing_result["sys.self"]
  )
}

#' Validate probabilities are bounded and valid for log-loss
validate_logloss_probabilities <- function(probabilities) {
  # Check bounds (0.01 < p < 0.99 for log-loss)
  all_bounded <- all(probabilities > 0.01) && all(probabilities < 0.99)
  
  # Check rows sum to 1
  row_sums <- rowSums(probabilities)
  all_sum_to_one <- all(abs(row_sums - 1) < 1e-10)
  
  # Check finite
  all_finite <- all(is.finite(probabilities))
  
  list(
    bounded = all_bounded,
    sum_to_one = all_sum_to_one,
    finite = all_finite,
    valid = all_bounded && all_sum_to_one && all_finite
  )
}

#' Print test results in readable format
print_test_results <- function(test_name, accuracy_metrics, runtime_metrics, validation, 
                                additional_info = NULL) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Test:", test_name, "\n")
  cat(rep("=", 70), "\n", sep = "")
  
  cat("\nAccuracy Metrics:\n")
  cat(sprintf("  MAE:              %.6f\n", accuracy_metrics$mae))
  cat(sprintf("  RMSE:             %.6f\n", accuracy_metrics$rmse))
  cat(sprintf("  Brier Score:      %.6f\n", accuracy_metrics$brier))
  cat(sprintf("  Cross-Entropy:   %.6f\n", accuracy_metrics$cross_entropy))
  cat(sprintf("  Correlation:     %.6f\n", accuracy_metrics$correlation))
  cat(sprintf("  Calibration:     %.6f\n", accuracy_metrics$calibration))
  
  cat("\nRuntime Metrics:\n")
  cat(sprintf("  Elapsed Time:    %.3f seconds\n", runtime_metrics$elapsed))
  cat(sprintf("  User Time:       %.3f seconds\n", runtime_metrics$user))
  cat(sprintf("  System Time:     %.3f seconds\n", runtime_metrics$system))
  
  cat("\nValidation:\n")
  cat(sprintf("  Probabilities Bounded:  %s\n", ifelse(validation$bounded, "PASS", "FAIL")))
  cat(sprintf("  Rows Sum to 1:          %s\n", ifelse(validation$sum_to_one, "PASS", "FAIL")))
  cat(sprintf("  All Finite:             %s\n", ifelse(validation$finite, "PASS", "FAIL")))
  cat(sprintf("  Overall Valid:           %s\n", ifelse(validation$valid, "PASS", "FAIL")))
  
  if (!is.null(additional_info)) {
    cat("\nAdditional Info:\n")
    for (key in names(additional_info)) {
      cat(sprintf("  %s: %s\n", key, as.character(additional_info[[key]])))
    }
  }
  
  cat(rep("=", 70), "\n\n", sep = "")
}

# ============================================================================
# Test Case 1: Single Tree Fitting
# ============================================================================

test_that("Single tree fitting with log-loss - probability accuracy and runtime", {
  # Create test dataset
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42)
  
  # Test with different regularization values
  regularization_values <- c(0.01, 0.1, 0.5, 1.0)
  results_list <- list()
  
  for (reg in regularization_values) {
    test_name <- sprintf("fit_tree (reg=%.2f)", reg)
    
    # Time the fitting
    timing <- system.time({
      model <- fit_tree(
        X = data$X,
        y = data$y,
        loss_function = "log_loss",
        regularization = reg,
        verbose = FALSE,
        compute_probabilities = TRUE
      )
    })
    
    # Extract probabilities
    pred_probs <- predict(model, data$X, type = "prob")
    
    # Collect metrics
    accuracy_metrics <- collect_accuracy_metrics(pred_probs, data$true_probs, data$y)
    runtime_metrics <- collect_runtime_metrics(timing)
    validation <- validate_logloss_probabilities(pred_probs)
    
    # Store results
    results_list[[test_name]] <- list(
      accuracy = accuracy_metrics,
      runtime = runtime_metrics,
      validation = validation,
      regularization = reg,
      n_trees = model$n_trees
    )
    
    # Print results
    print_test_results(
      test_name,
      accuracy_metrics,
      runtime_metrics,
      validation,
      list(regularization = reg, n_trees = model$n_trees)
    )
    
    # Validate model
    expect_equal(model$n_trees, 1, info = "fit_tree should return exactly 1 tree")
    expect_equal(model$loss_function, "log_loss")
    expect_true(validation$valid, info = "Probabilities should be valid for log-loss")
  }
  
  # Test with auto-tuning
  test_name <- "fit_tree (auto-tune)"
  timing <- system.time({
    model <- fit_tree(
      X = data$X,
      y = data$y,
      loss_function = "log_loss",
      regularization = NULL,  # Auto-tune
      verbose = FALSE,
      compute_probabilities = TRUE
    )
  })
  
  pred_probs <- predict(model, data$X, type = "prob")
  accuracy_metrics <- collect_accuracy_metrics(pred_probs, data$true_probs, data$y)
  runtime_metrics <- collect_runtime_metrics(timing)
  validation <- validate_logloss_probabilities(pred_probs)
  
  results_list[[test_name]] <- list(
    accuracy = accuracy_metrics,
    runtime = runtime_metrics,
    validation = validation,
    regularization = model$regularization,
    n_trees = model$n_trees
  )
  
  print_test_results(
    test_name,
    accuracy_metrics,
    runtime_metrics,
    validation,
    list(regularization = model$regularization, n_trees = model$n_trees)
  )
  
  expect_equal(model$n_trees, 1)
  expect_equal(model$loss_function, "log_loss")
  expect_true(validation$valid)
})

# ============================================================================
# Test Case 2: Cross-Fitted Single Trees
# ============================================================================

test_that("Cross-fitted single trees with log-loss - probability accuracy and runtime", {
  # Create test dataset
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42)
  
  # Test with different K values
  K_values <- c(3, 5, 10)
  regularization_values <- c(0.1, NULL)  # Fixed and auto-tune
  
  for (K in K_values) {
    for (reg in regularization_values) {
      reg_label <- if (is.null(reg)) "auto" else sprintf("%.2f", reg)
      test_name <- sprintf("cross_fitted_rashomon single_tree=TRUE (K=%d, reg=%s)", K, reg_label)
      
      # Time the cross-fitting
      timing <- system.time({
        result <- cross_fitted_rashomon(
          X = data$X,
          y = data$y,
          K = K,
          loss_function = "log_loss",
          regularization = reg,
          single_tree = TRUE,
          verbose = FALSE,
          seed = 42
        )
      })
      
      # Extract probabilities from each fold and average
      fold_probs_list <- list()
      for (k in 1:K) {
        fold_model <- result$fold_models[[k]]
        fold_probs <- predict(fold_model, data$X, type = "prob")
        fold_probs_list[[k]] <- fold_probs
      }
      
      # Average probabilities across folds
      avg_probs <- Reduce("+", fold_probs_list) / length(fold_probs_list)
      
      # Collect metrics
      accuracy_metrics <- collect_accuracy_metrics(avg_probs, data$true_probs, data$y)
      runtime_metrics <- collect_runtime_metrics(timing)
      validation <- validate_logloss_probabilities(avg_probs)
      
      # Print results
      print_test_results(
        test_name,
        accuracy_metrics,
        runtime_metrics,
        validation,
        list(
          K = K,
          regularization = if (is.null(reg)) result$regularization else reg,
          n_intersecting = result$n_intersecting,
          rashomon_sizes = paste(result$rashomon_sizes, collapse = ", ")
        )
      )
      
      # Validate
      expect_true(all(result$rashomon_sizes == 1), 
                  info = "Each fold should have exactly 1 tree")
      expect_true(validation$valid)
    }
  }
})

# ============================================================================
# Test Case 3: Rashomon Set
# ============================================================================

test_that("Rashomon set with log-loss - probability accuracy and runtime", {
  # Create test dataset
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42)
  
  # Test with different regularization and rashomon_bound_multiplier values
  regularization_values <- c(0.1, NULL)
  rashomon_multipliers <- c(0.01, 0.05, 0.1)
  
  for (reg in regularization_values) {
    for (mult in rashomon_multipliers) {
      reg_label <- if (is.null(reg)) "auto" else sprintf("%.2f", reg)
      test_name <- sprintf("fit_rashomon (reg=%s, mult=%.2f)", reg_label, mult)
      
      # Time the fitting
      timing <- system.time({
        model <- fit_rashomon(
          X = data$X,
          y = data$y,
          loss_function = "log_loss",
          regularization = reg,
          rashomon_bound_multiplier = mult,
          verbose = FALSE,
          compute_probabilities = TRUE
        )
      })
      
      # Extract probabilities from optimal tree (first tree)
      pred_probs <- predict(model, data$X, type = "prob")
      
      # Optionally: ensemble across all trees in rashomon set
      trees <- get_rashomon_trees(model)
      if (length(trees) > 1) {
        # Get probabilities from each tree and average
        tree_probs_list <- list()
        for (i in 1:min(length(trees), 5)) {  # Limit to first 5 trees for efficiency
          # Use the model's predict method which uses the optimal tree
          # For ensemble, we'd need to extract probabilities from each tree individually
          # For now, use the model's predictions
        }
        # For this test, we'll use the optimal tree probabilities
      }
      
      # Collect metrics
      accuracy_metrics <- collect_accuracy_metrics(pred_probs, data$true_probs, data$y)
      runtime_metrics <- collect_runtime_metrics(timing)
      validation <- validate_logloss_probabilities(pred_probs)
      
      # Print results
      print_test_results(
        test_name,
        accuracy_metrics,
        runtime_metrics,
        validation,
        list(
          regularization = if (is.null(reg)) model$regularization else reg,
          rashomon_multiplier = mult,
          n_trees = model$n_trees
        )
      )
      
      # Validate
      expect_true(model$n_trees >= 1, info = "Rashomon set should have at least 1 tree")
      expect_equal(model$loss_function, "log_loss")
      expect_true(validation$valid)
    }
  }
})

# ============================================================================
# Test Case 4: Cross-Fitted Rashomon Set
# ============================================================================

test_that("Cross-fitted rashomon set with log-loss - probability accuracy and runtime", {
  # Create test dataset
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42)
  
  # Test with different K values
  K_values <- c(3, 5, 10)
  regularization_values <- c(0.1, NULL)
  rashomon_multiplier <- 0.05
  
  for (K in K_values) {
    for (reg in regularization_values) {
      reg_label <- if (is.null(reg)) "auto" else sprintf("%.2f", reg)
      test_name <- sprintf("cross_fitted_rashomon single_tree=FALSE (K=%d, reg=%s)", K, reg_label)
      
      # Time the cross-fitting
      timing <- system.time({
        result <- cross_fitted_rashomon(
          X = data$X,
          y = data$y,
          K = K,
          loss_function = "log_loss",
          regularization = reg,
          rashomon_bound_multiplier = rashomon_multiplier,
          single_tree = FALSE,
          verbose = FALSE,
          seed = 42
        )
      })
      
      # Extract probabilities from each fold's rashomon set (use optimal tree from each fold)
      fold_probs_list <- list()
      for (k in 1:K) {
        fold_model <- result$fold_models[[k]]
        fold_probs <- predict(fold_model, data$X, type = "prob")
        fold_probs_list[[k]] <- fold_probs
      }
      
      # Average probabilities across folds
      avg_probs <- Reduce("+", fold_probs_list) / length(fold_probs_list)
      
      # Collect metrics
      accuracy_metrics <- collect_accuracy_metrics(avg_probs, data$true_probs, data$y)
      runtime_metrics <- collect_runtime_metrics(timing)
      validation <- validate_logloss_probabilities(avg_probs)
      
      # Print results
      print_test_results(
        test_name,
        accuracy_metrics,
        runtime_metrics,
        validation,
        list(
          K = K,
          regularization = if (is.null(reg)) result$regularization else reg,
          rashomon_multiplier = rashomon_multiplier,
          n_intersecting = result$n_intersecting,
          rashomon_sizes = paste(result$rashomon_sizes, collapse = ", ")
        )
      )
      
      # Validate
      expect_true(all(result$rashomon_sizes >= 1), 
                  info = "Each fold should have at least 1 tree")
      expect_true(validation$valid)
    }
  }
})

# ============================================================================
# Test Case 5: Intersecting Trees
# ============================================================================

test_that("Intersecting trees from cross-fitted rashomon set - probability accuracy and runtime", {
  # Create test dataset
  data <- simple_ground_truth_dataset(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 50, seed = 42)
  
  # Test with different K values
  K_values <- c(3, 5, 10)
  regularization_values <- c(0.1, NULL)
  rashomon_multiplier <- 0.05
  
  for (K in K_values) {
    for (reg in regularization_values) {
      reg_label <- if (is.null(reg)) "auto" else sprintf("%.2f", reg)
      test_name <- sprintf("intersecting_trees (K=%d, reg=%s)", K, reg_label)
      
      # Time the cross-fitting
      timing <- system.time({
        result <- cross_fitted_rashomon(
          X = data$X,
          y = data$y,
          K = K,
          loss_function = "log_loss",
          regularization = reg,
          rashomon_bound_multiplier = rashomon_multiplier,
          single_tree = FALSE,
          verbose = FALSE,
          seed = 42
        )
      })
      
      # Check if intersecting trees exist
      if (result$n_intersecting > 0) {
        # Extract probabilities from intersecting trees
        # Use the first intersecting tree for prediction
        # The intersecting_trees are stored in result$intersecting_trees
        # We can use the first fold model as a template
        if (length(result$intersecting_trees) > 0) {
          # For prediction, we'll use the first fold model which should contain the intersecting tree
          # In practice, we'd need to construct a model from the intersecting tree
          # For now, we'll use the ensemble approach from the cross-fitted result
          pred_probs <- predict(result, data$X, type = "prob")
        } else {
          # Fallback: use first fold model
          pred_probs <- predict(result$fold_models[[1]], data$X, type = "prob")
        }
      } else {
        # No intersecting trees, skip probability evaluation but report
        pred_probs <- NULL
      }
      
      # Collect metrics if probabilities available
      if (!is.null(pred_probs)) {
        accuracy_metrics <- collect_accuracy_metrics(pred_probs, data$true_probs, data$y)
        validation <- validate_logloss_probabilities(pred_probs)
      } else {
        accuracy_metrics <- NULL
        validation <- NULL
      }
      
      runtime_metrics <- collect_runtime_metrics(timing)
      
      # Print results
      additional_info <- list(
        K = K,
        regularization = if (is.null(reg)) result$regularization else reg,
        rashomon_multiplier = rashomon_multiplier,
        n_intersecting = result$n_intersecting,
        rashomon_sizes = paste(result$rashomon_sizes, collapse = ", ")
      )
      
      if (!is.null(accuracy_metrics)) {
        print_test_results(
          test_name,
          accuracy_metrics,
          runtime_metrics,
          validation,
          additional_info
        )
        expect_true(validation$valid)
      } else {
        cat("\n", rep("=", 70), "\n", sep = "")
        cat("Test:", test_name, "\n")
        cat(rep("=", 70), "\n", sep = "")
        cat("\nNo intersecting trees found.\n")
        for (key in names(additional_info)) {
          cat(sprintf("  %s: %s\n", key, as.character(additional_info[[key]])))
        }
        cat(rep("=", 70), "\n\n", sep = "")
      }
      
      # Validate structure
      expect_true(is.numeric(result$n_intersecting))
      expect_true(result$n_intersecting >= 0)
    }
  }
})

# ============================================================================
# Additional Tests: Multiple Dataset Sizes
# ============================================================================

test_that("Single tree fitting with different dataset sizes", {
  # Test with small, medium, and large datasets
  sizes <- list(
    small = list(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 25, seed = 42),
    medium = list(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 125, seed = 42),
    large = list(probs = c(0.1, 0.3, 0.7, 0.9), n_per_combo = 250, seed = 42)
  )
  
  for (size_name in names(sizes)) {
    size_params <- sizes[[size_name]]
    data <- simple_ground_truth_dataset(
      probs = size_params$probs,
      n_per_combo = size_params$n_per_combo,
      seed = size_params$seed
    )
    
    test_name <- sprintf("fit_tree (%s dataset, n=%d)", size_name, nrow(data$X))
    
    timing <- system.time({
      model <- fit_tree(
        X = data$X,
        y = data$y,
        loss_function = "log_loss",
        regularization = 0.1,
        verbose = FALSE,
        compute_probabilities = TRUE
      )
    })
    
    pred_probs <- predict(model, data$X, type = "prob")
    accuracy_metrics <- collect_accuracy_metrics(pred_probs, data$true_probs, data$y)
    runtime_metrics <- collect_runtime_metrics(timing)
    validation <- validate_logloss_probabilities(pred_probs)
    
    print_test_results(
      test_name,
      accuracy_metrics,
      runtime_metrics,
      validation,
      list(n_samples = nrow(data$X), n_trees = model$n_trees)
    )
    
    expect_true(validation$valid)
  }
})

# Cleanup
teardown_test_environment()



