# Tests for structure-selection utilities relocated from doubletree (2026-07-08):
# select_structure_modal, analyze_structure_diversity, compute_functional_consistency.

test_that("select_structure_modal works", {
  set.seed(123)
  n <- 100
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  # Create M structures (some identical when seeds repeat)
  M <- 5
  structures <- vector("list", M)
  for (m in seq_len(M)) {
    set.seed(100 + (m %% 3))
    model <- fit_tree(X, y, loss_function = "log_loss",
                      regularization = 0.1, store_training_data = TRUE)
    structures[[m]] <- extract_tree_structure(model)
  }

  result <- select_structure_modal(structures)

  expect_type(result, "list")
  expect_true(S7::S7_inherits(result$structure, TreeStructure))
  expect_true(result$frequency > 0 && result$frequency <= 1)
  expect_type(result$hash, "character")
  expect_s3_class(result$counts, "table")
})

test_that("select_structure_modal validates inputs", {
  expect_error(select_structure_modal(NULL), "structures must be a non-empty list")
  expect_error(select_structure_modal(list()), "structures must be a non-empty list")
  expect_error(
    select_structure_modal(list("not a structure")),
    "All elements.*must be TreeStructure"
  )
})

test_that("analyze_structure_diversity works", {
  set.seed(1819)
  n <- 100
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  M <- 5
  structures <- vector("list", M)
  for (m in seq_len(M)) {
    set.seed(100 + m)
    model <- fit_tree(X, y, loss_function = "log_loss",
                      regularization = 0.1, store_training_data = TRUE)
    structures[[m]] <- extract_tree_structure(model)
  }

  diversity <- analyze_structure_diversity(structures)

  expect_type(diversity, "list")
  expect_type(diversity$n_unique, "integer")
  expect_true(diversity$n_unique >= 1 && diversity$n_unique <= M)
  expect_type(diversity$shannon_entropy, "double")
  expect_type(diversity$simpson_index, "double")
  expect_type(diversity$modal_frequency, "double")
  expect_true(diversity$modal_frequency > 0 && diversity$modal_frequency <= 1)
})

test_that("compute_functional_consistency works", {
  set.seed(1617)
  n <- 50
  M <- 3

  # Data with duplicate rows (tied covariates)
  X <- data.frame(
    x1 = rep(c(0, 1), each = 25),
    x2 = rep(c(0, 1), times = 25)
  )

  predictions_a <- matrix(runif(n * M), nrow = n, ncol = M)
  predictions_b <- matrix(runif(n * M), nrow = n, ncol = M)

  result <- compute_functional_consistency(predictions_a, predictions_b, X)

  expect_type(result, "list")
  expect_type(result$max_diff_a, "double")
  expect_type(result$max_diff_b, "double")
  expect_true(result$max_diff_a >= 0)
  expect_true(result$max_diff_b >= 0)
  expect_type(result$n_unique_patterns, "integer")
  expect_type(result$n_groups_with_ties, "integer")
  expect_true(result$n_unique_patterns > 0)
})
