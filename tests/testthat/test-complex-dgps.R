# Tests for complex DGP structures: binary, continuous, regression, adaptive bins
#
# Documents and verifies the fix from 2026-06-10 audit:
#   Old defaults: discretize_method="median", discretize_bins=2
#   New defaults: discretize_method="quantiles", discretize_bins="adaptive"
#
# Sections:
#   1. Binary DGP structural correctness (fast)
#   2. Continuous DGP: new defaults don't crash (fast)
#   3. Symmetric quadratic: structural test (fast, CRITICAL)
#   4. OOS accuracy: continuous DGPs outperform null (slow)
#   5. Old defaults fail on symmetric quadratic (slow, comparative)
#   6. Regression DGPs (fast + slow)
#   7. Adaptive bin scaling formula (fast)

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R
# DGP generators from helper-complex-dgps.R (auto-loaded by testthat)

# Helper: correlation that treats NA/non-finite as 0 (handles constant predictions)
safe_cor <- function(pred, truth) {
  r <- suppressWarnings(cor(pred, truth))
  if (!is.finite(r)) 0 else r
}

# Helper: OOS correlation via single train/test split, averaged over n_seeds
compute_oos_cor <- function(dgp_fn, n_seeds = 3, train_frac = 0.7,
                            regularization = 0.05,
                            loss_function = "misclassification", ...) {
  cors <- vapply(seq_len(n_seeds), function(s) {
    d <- dgp_fn(seed = s)
    n <- nrow(d$X)
    n_train <- floor(train_frac * n)
    set.seed(s * 1000 + 7)
    idx <- sample(seq_len(n))
    idx_train <- idx[seq_len(n_train)]
    idx_test  <- idx[(n_train + 1L):n]

    model <- optimaltrees(
      X = d$X[idx_train, , drop = FALSE],
      y = d$y[idx_train],
      loss_function = loss_function,
      regularization = regularization,
      verbose = FALSE,
      ...
    )

    if (loss_function == "squared_error") {
      pred_test <- predict(model, d$X[idx_test, , drop = FALSE])
    } else {
      pred_test <- predict(model, d$X[idx_test, , drop = FALSE], type = "prob")[, 2L]
    }

    safe_cor(pred_test, d$truth[idx_test])
  }, numeric(1L))

  mean(cors, na.rm = TRUE)
}


# ============================================================================
# Section 1 — Binary DGP structural correctness (fast)
# ============================================================================

test_that("B1 single-split binary DGP: valid model", {
  d <- dgp_b1_single_split(n = 200, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@n_trees, 1L)
})

test_that("B2 XOR binary DGP: valid model", {
  d <- dgp_b2_xor(n = 300, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@n_trees, 1L)
})

test_that("B3 compound logical binary DGP: valid model", {
  d <- dgp_b3_compound(n = 300, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@n_trees, 1L)
})

test_that("B4 noisy interaction binary DGP: valid model", {
  d <- dgp_b4_noisy(n = 300, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@n_trees, 1L)
})


# ============================================================================
# Section 2 — Continuous DGP: new defaults don't crash (fast)
# ============================================================================

test_that("C1 linear continuous DGP: fits with default params", {
  d <- dgp_c1_linear(n = 400, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  meta <- model@discretization_metadata
  expect_false(is.null(meta))
  expect_equal(meta$method, "quantiles")
})

test_that("C2 symmetric quadratic DGP: fits with default params", {
  d <- dgp_c2_symmetric_quadratic(n = 500, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  meta <- model@discretization_metadata
  expect_false(is.null(meta))
  expect_equal(meta$method, "quantiles")
})

test_that("C3 step function DGP: fits with default params", {
  d <- dgp_c3_step(n = 500, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@discretization_metadata$method, "quantiles")
})

test_that("C4 bivariate continuous DGP: fits with default params", {
  d <- dgp_c4_bivariate(n = 400, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@discretization_metadata$method, "quantiles")
})

test_that("C5 DGP4 propensity (mixed continuous/binary): fits with default params", {
  d <- dgp_c5_dgp4_propensity(n = 400, seed = 42)
  model <- safe_optimaltrees(d$X, d$y, regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(model)
  expect_equal(model@discretization_metadata$method, "quantiles")
})


# ============================================================================
# Section 3 — Symmetric quadratic: structural test (fast, CRITICAL)
#
# Documents the core failure mode from 2026-06-10 audit:
#   Old defaults ("median", bins=2) place ONE threshold at x1=median≈0.
#   For truth=plogis(-1+3*x1^2), both halves (x1<=0, x1>0) have the same
#   E[truth] by symmetry → zero gain from splitting → stump.
#
#   New defaults ("quantiles", "adaptive") at n>=404 give n_bins=3 (2 thresholds
#   at ~+/-0.43). Depth-2 tree separates middle (low truth) from tails (high truth).
#
# NOTE: n=600 is used (not n=400) because at n=400, max(2, ceiling(log(400)/3))=2,
#   which places the single quantile threshold at the median — identical to old
#   defaults for this symmetric DGP. The difference only emerges when n>=404 and
#   adaptive gives n_bins=3.
# ============================================================================

test_that("symmetric quadratic: new defaults learn U-shape, old defaults cannot [critical]", {
  d <- dgp_c2_symmetric_quadratic(n = 600, seed = 42)

  # OLD defaults: 1 threshold at median ≈ 0. By symmetry of x1~N(0,1),
  # both halves have identical E[truth] → zero gain → GOSDT returns stump.
  model_old <- optimaltrees(
    d$X, d$y,
    regularization = 0.05,
    discretize_method = "median",
    discretize_bins = 2L,
    verbose = FALSE
  )
  pred_old <- predict(model_old, d$X, type = "prob")[, 2L]
  cor_old <- safe_cor(pred_old, d$truth)

  expect_lt(cor_old, 0.2,
    label = "old defaults (median/2-bin) cannot learn symmetric quadratic")

  # NEW defaults: 2 thresholds at ~+/-0.43.
  # Depth-2 tree: middle (|x1|<0.43, low truth) vs tails (|x1|>0.43, high truth).
  model_new <- optimaltrees(
    d$X, d$y,
    regularization = 0.05,
    verbose = FALSE
    # discretize_method and discretize_bins use defaults: "quantiles" / "adaptive"
  )
  pred_new <- predict(model_new, d$X, type = "prob")[, 2L]
  cor_new <- safe_cor(pred_new, d$truth)

  expect_gt(cor_new, 0.4,
    label = "new defaults (quantiles/adaptive) learn symmetric quadratic")
})


# ============================================================================
# Section 4 — OOS accuracy: continuous DGPs outperform null (slow)
# ============================================================================

test_that("C2 symmetric quadratic: OOS cor > 0.25 with new defaults", {
  skip_slow_tests()
  mean_cor <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05
  )
  expect_gt(mean_cor, 0.25)
})

test_that("C3 step function: OOS cor > 0.20 with new defaults", {
  skip_slow_tests()
  mean_cor <- compute_oos_cor(
    function(seed) dgp_c3_step(n = 500, seed = seed),
    n_seeds = 3, regularization = 0.05
  )
  expect_gt(mean_cor, 0.20)
})

test_that("C5 DGP4 propensity: OOS cor > 0.30 with new defaults (log_loss)", {
  skip_slow_tests()
  # log_loss used here: misclassification gives stumps at reg=0.05 for this
  # 4-feature mixed continuous/binary propensity because per-feature gain is
  # small. log_loss has smoother gradients and correctly exploits weak signals.
  mean_cor <- compute_oos_cor(
    function(seed) dgp_c5_dgp4_propensity(n = 600, seed = seed),
    n_seeds = 5, regularization = 0.05,
    loss_function = "log_loss"
  )
  expect_gt(mean_cor, 0.30)
})


# ============================================================================
# Section 5 — Old defaults fail on symmetric quadratic (slow, comparative)
#
# Directly documents the failure mode from 2026-06-10 audit.
# Old: stump → cor=NA → treated as 0. Mean across seeds < 0.10.
# New: depth-2 tree captures U-shape. Mean across seeds > 0.35.
# ============================================================================

test_that("C2: old defaults fail (cor<0.10), new defaults succeed (cor>0.35) [comparative]", {
  skip_slow_tests()

  # OLD defaults
  mean_old <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05,
    discretize_method = "median", discretize_bins = 2L
  )

  # NEW defaults
  mean_new <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05
  )

  expect_lt(mean_old, 0.10,
    label = "old defaults (median/2-bin) near-zero OOS cor on symmetric quadratic")
  expect_gt(mean_new, 0.35,
    label = "new defaults (quantiles/adaptive) positive OOS cor on symmetric quadratic")
})


# ============================================================================
# Section 6 — Regression DGPs
# ============================================================================

test_that("R1 regression linear: valid model with squared_error", {
  d <- dgp_r1_regression(n = 400, seed = 42)
  model <- safe_optimaltrees(d$X, d$y,
    loss_function = "squared_error",
    regularization = 0.05,
    verbose = FALSE
  )
  expect_valid_treefarms_model(model, "squared_error")
  expect_true(is.numeric(model@predictions))
  expect_null(model@probabilities)
})

test_that("R2 regression quadratic: OOS cor > 0.20 with new defaults", {
  skip_slow_tests()
  mean_cor <- compute_oos_cor(
    function(seed) dgp_r2_regression_quadratic(n = 600, seed = seed),
    n_seeds = 3, regularization = 0.05,
    loss_function = "squared_error"
  )
  expect_gt(mean_cor, 0.20)
})


# ============================================================================
# Section 7 — Adaptive bin scaling formula (fast)
#
# Verifies: n_bins = max(2, ceiling(log(n) / 3)) implemented in discretize.R
# At n=100: max(2, ceiling(4.605/3)) = max(2, 2) = 2 bins, 1 threshold
# At n=500: max(2, ceiling(6.215/3)) = max(2, 3) = 3 bins, 2 thresholds
# At n=2000: max(2, ceiling(7.601/3)) = max(2, 3) = 3 bins, 2 thresholds
# Non-decreasing: bins(100) <= bins(500) <= bins(2000)
# ============================================================================

test_that("adaptive bin formula: n_bins matches max(2, ceiling(log(n)/3))", {
  expected_bins_formula <- function(n) max(2L, ceiling(log(n) / 3))

  n_vec <- c(100L, 500L, 2000L)
  bins_vec <- integer(length(n_vec))

  for (i in seq_along(n_vec)) {
    n_i <- n_vec[[i]]
    d <- dgp_a1_adaptive_bins(n = n_i, seed = 1)
    model <- safe_optimaltrees(d$X, d$y,
      regularization = 0.1,
      verbose = FALSE
    )
    meta <- model@discretization_metadata
    expect_false(is.null(meta),
      info = paste("discretization_metadata should not be NULL at n =", n_i))
    bins_vec[[i]] <- as.integer(meta$n_bins)
    expect_equal(bins_vec[[i]], expected_bins_formula(n_i),
      info = paste("n_bins should match formula at n =", n_i))
  }

  # Non-decreasing: bins should not decrease as n grows
  for (i in seq_len(length(bins_vec) - 1L)) {
    expect_lte(bins_vec[[i]], bins_vec[[i + 1L]],
      label = paste("bins non-decreasing:", n_vec[[i]], "->", n_vec[[i + 1L]]))
  }
})

test_that("adaptive bins: threshold count equals n_bins - 1 for continuous feature", {
  # n=500 → 3 bins → 2 thresholds per continuous feature
  d <- dgp_a1_adaptive_bins(n = 500, seed = 1)
  model <- safe_optimaltrees(d$X, d$y,
    regularization = 0.1,
    verbose = FALSE
  )
  meta <- model@discretization_metadata
  expect_false(meta$all_binary)
  # Each continuous feature should have n_bins - 1 thresholds
  expected_thresholds <- meta$n_bins - 1L
  thresholds_x1 <- meta$features[["x1"]]$thresholds
  expect_equal(length(thresholds_x1), expected_thresholds,
    info = paste("x1 threshold count should be", expected_thresholds))
})
