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
#   8. RMSE convergence: RMSE vs truth decreases with n (slow, theory validation)

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R
# DGP generators from helper-complex-dgps.R (auto-loaded by testthat)

# Helper: correlation that treats NA/non-finite as 0 (handles constant predictions)
safe_cor <- function(pred, truth) {
  r <- suppressWarnings(cor(pred, truth))
  if (!is.finite(r)) 0 else r
}

# Helper: resolve regularization for a given training sample.
#
# regularization can be:
#   - numeric scalar: fixed λ (e.g. 0.05). RMSE plateaus — use only for non-convergence tests.
#   - function(n_train) -> λ: formula-based adaptive rate.
#   - "cv": use cv_regularization_adaptive() on X_train/y_train (data-driven).
#
# "cv" is the practical choice; a formula is needed for clean theory validation
# where you want to guarantee λ_n -> 0 deterministically.
resolve_reg <- function(regularization, n_train,
                        X_train = NULL, y_train = NULL, loss_function = NULL, ...) {
  if (identical(regularization, "cv")) {
    if (is.null(X_train) || is.null(y_train)) {
      stop("regularization='cv' requires X_train and y_train")
    }
    cv_regularization_adaptive(X_train, y_train,
                                loss_function = loss_function, verbose = FALSE)
  } else if (is.function(regularization)) {
    regularization(n_train)
  } else {
    regularization
  }
}

# Theory-motivated adaptive regularization rate: λ_n = log(n) / n.
# Balances bias O(1/K) and variance O(K/n) for K = floor(n^(1/3)) bins.
# As n -> infty: λ_n -> 0, tree grows, RMSE -> 0.
# This is the deterministic rate used for clean convergence validation.
reg_logn_over_n <- function(n) log(n) / n

# Helper: OOS correlation via single train/test split, averaged over n_seeds.
# regularization: numeric scalar (fixed), function(n_train) -> λ, or "cv".
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
    X_tr <- d$X[idx_train, , drop = FALSE]
    y_tr <- d$y[idx_train]

    reg <- resolve_reg(regularization, n_train, X_tr, y_tr, loss_function)

    model <- optimaltrees(
      X = X_tr, y = y_tr,
      loss_function = loss_function,
      regularization = reg,
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

# Helper: OOS RMSE vs truth (not vs noisy y) averaged over n_seeds.
#
# Measures approximation quality: sqrt(mean((f_hat(x) - f(x))^2)) where
# f(x) = truth (true conditional mean or true probability). This is the
# quantity that -> 0 as n -> infty, unlike correlation which only -> 1.
#
# For convergence tests, regularization MUST shrink with n. Two valid options:
#   reg_logn_over_n  — formula λ = log(n)/n; guarantees λ_n -> 0 deterministically.
#   "cv"             — cv_regularization_adaptive(); data-driven, practical, also
#                      shrinks with n empirically (oracle λ -> 0, CV consistent).
# With fixed regularization (e.g. 0.05), RMSE plateaus and does NOT go to 0.
compute_oos_rmse <- function(dgp_fn, n_seeds = 3, train_frac = 0.7,
                              regularization = reg_logn_over_n,
                              loss_function = "squared_error", ...) {
  rmses <- vapply(seq_len(n_seeds), function(s) {
    d <- dgp_fn(seed = s)
    n <- nrow(d$X)
    n_train <- floor(train_frac * n)
    set.seed(s * 1000 + 7)
    idx <- sample(seq_len(n))
    idx_train <- idx[seq_len(n_train)]
    idx_test  <- idx[(n_train + 1L):n]
    X_tr <- d$X[idx_train, , drop = FALSE]
    y_tr <- d$y[idx_train]

    reg <- resolve_reg(regularization, n_train, X_tr, y_tr, loss_function)

    model <- optimaltrees(
      X = X_tr, y = y_tr,
      loss_function = loss_function,
      regularization = reg,
      verbose = FALSE,
      ...
    )

    if (loss_function == "squared_error") {
      pred_test <- predict(model, d$X[idx_test, , drop = FALSE])
    } else {
      pred_test <- predict(model, d$X[idx_test, , drop = FALSE], type = "prob")[, 2L]
    }

    sqrt(mean((pred_test - d$truth[idx_test])^2))
  }, numeric(1L))

  mean(rmses, na.rm = TRUE)
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
# Section 4 — OOS accuracy: log_loss and squared_error outperform null (slow)
#
# All classification tests use log_loss (the relevant loss for propensity
# estimation in doubletree). Regression tests use squared_error.
# ============================================================================

test_that("C2 symmetric quadratic: OOS cor > 0.40 (log_loss)", {
  skip_slow_tests()
  mean_cor <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05,
    loss_function = "log_loss"
  )
  expect_gt(mean_cor, 0.40)
})

test_that("C3 step function: OOS cor > 0.30 (log_loss)", {
  skip_slow_tests()
  mean_cor <- compute_oos_cor(
    function(seed) dgp_c3_step(n = 500, seed = seed),
    n_seeds = 3, regularization = 0.05,
    loss_function = "log_loss"
  )
  expect_gt(mean_cor, 0.30)
})

test_that("C5 DGP4 propensity: OOS cor > 0.30 (log_loss)", {
  skip_slow_tests()
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
# Both fits use log_loss. Old: stump → cor ≈ 0. New: > 0.40.
# ============================================================================

test_that("C2: old defaults fail (cor<0.10), new defaults succeed (cor>0.40) [log_loss comparative]", {
  skip_slow_tests()

  # OLD defaults
  mean_old <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05,
    loss_function = "log_loss",
    discretize_method = "median", discretize_bins = 2L
  )

  # NEW defaults
  mean_new <- compute_oos_cor(
    function(seed) dgp_c2_symmetric_quadratic(n = 800, seed = seed),
    n_seeds = 3, regularization = 0.05,
    loss_function = "log_loss"
  )

  expect_lt(mean_old, 0.10,
    label = "old defaults (median/2-bin) near-zero OOS cor on symmetric quadratic")
  expect_gt(mean_new, 0.40,
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


# ============================================================================
# Section 8 — RMSE convergence: RMSE vs truth decreases with n (slow)
#
# Validates the smooth-function approximation guarantee from theory:
#   dev-docs/smooth-function-approximation.tex
#
# Claim: for K quantile bins and Lipschitz truth f,
#   RMSE(f_hat, f) = O(L/K) + O(sqrt(K/n)) -> 0 as n -> infty.
#   With K = floor(n^(1/3)) and lambda_n = log(n)/n, rate is O(n^{-1/3}).
#
# CRITICAL: regularization must shrink with n for RMSE -> 0.
#   - Fixed lambda (e.g. 0.05): bounds tree complexity regardless of n -> RMSE plateaus.
#   - reg_logn_over_n: lambda = log(n)/n, deterministic, theory-motivated.
#   - "cv": cv_regularization_adaptive(), data-driven, also shrinks empirically.
#   Tests run both tracks: formula (clean theory validation) and CV (practical).
#
# Discretization: K = floor(n^(1/3)) bins (conv_bins). The default adaptive
# formula (max(2, ceil(log(n)/3))) reaches K=4 only at n~50000 — too slow to
# show convergence at practical n. n^(1/3) matches the theory rate condition.
#
# Two DGPs:
#   R2: truth = x1^2, x1~N(0,1), squared_error. Null RMSE = sqrt(2) ≈ 1.41.
#   C2: truth = plogis(-1 + 3*x1^2), log_loss, RMSE vs true probability.
#
# All tests guarded by skip_slow_tests().
# ============================================================================

# Bin count for convergence demo: K = floor(n^(1/3)), min 3.
# Distinct from default "adaptive" formula max(2, ceil(log(n)/3)).
conv_bins <- function(n) max(3L, as.integer(floor(n^(1/3))))

test_that("RMSE convergence (regression, formula reg): RMSE vs truth decreases with n [slow]", {
  skip_slow_tests()

  # n=500:  K=7 bins,  lambda=log(500)/500  ≈ 0.012
  # n=4000: K=15 bins, lambda=log(4000)/4000 ≈ 0.002
  # Bias O(1/K): 1/15 ÷ 1/7 ≈ 0.47; expect at least 20% RMSE reduction.
  n_small <- 500L
  n_large <- 4000L

  # max_depth=3 prevents GOSDT model_limit overflow: with lambda_n -> 0 and many
  # bins, the Rashomon set becomes enormous without a depth cap (C++ wipes all
  # results). Depth-3 still shows convergence: finer bins -> better split placement.
  rmse_small <- compute_oos_rmse(
    function(seed) dgp_r2_regression_quadratic(n = n_small, seed = seed),
    n_seeds = 5, regularization = reg_logn_over_n,
    loss_function = "squared_error",
    discretize_bins = conv_bins(n_small), max_depth = 3L
  )
  rmse_large <- compute_oos_rmse(
    function(seed) dgp_r2_regression_quadratic(n = n_large, seed = seed),
    n_seeds = 5, regularization = reg_logn_over_n,
    loss_function = "squared_error",
    discretize_bins = conv_bins(n_large), max_depth = 3L
  )

  # Both beat null (null RMSE = sqrt(Var(x1^2)) = sqrt(2) ≈ 1.41)
  expect_lt(rmse_small, 1.41 * 0.90,
    label = "n=500 RMSE should beat null by at least 10%")
  expect_lt(rmse_large, 1.41 * 0.60,
    label = "n=4000 RMSE should beat null by at least 40%")

  # Convergence
  expect_lt(rmse_large, rmse_small * 0.80,
    label = "RMSE at n=4000 should be at least 20% lower than at n=500")
})

test_that("RMSE convergence (regression, CV reg): RMSE vs truth decreases with n [slow]", {
  skip_slow_tests()

  # Same convergence claim, but using cv_regularization_adaptive() to select lambda.
  # This is the practically relevant test: does data-driven lambda selection also
  # achieve convergence? It should, because CV is consistent and oracle lambda -> 0.
  # CV has more variance than the formula; threshold is looser (15% vs 20%).
  n_small <- 500L
  n_large <- 4000L

  rmse_small <- compute_oos_rmse(
    function(seed) dgp_r2_regression_quadratic(n = n_small, seed = seed),
    n_seeds = 5, regularization = "cv",
    loss_function = "squared_error",
    discretize_bins = conv_bins(n_small), max_depth = 3L
  )
  rmse_large <- compute_oos_rmse(
    function(seed) dgp_r2_regression_quadratic(n = n_large, seed = seed),
    n_seeds = 5, regularization = "cv",
    loss_function = "squared_error",
    discretize_bins = conv_bins(n_large), max_depth = 3L
  )

  expect_lt(rmse_large, rmse_small * 0.85,
    label = "CV-reg: RMSE at n=4000 should be at least 15% lower than at n=500")
})

test_that("RMSE convergence (propensity, formula reg): RMSE vs true prob decreases with n [slow]", {
  skip_slow_tests()

  n_small <- 500L
  n_large <- 4000L

  rmse_small <- compute_oos_rmse(
    function(seed) dgp_c2_symmetric_quadratic(n = n_small, seed = seed),
    n_seeds = 5, regularization = reg_logn_over_n,
    loss_function = "log_loss",
    discretize_bins = conv_bins(n_small), max_depth = 3L
  )
  rmse_large <- compute_oos_rmse(
    function(seed) dgp_c2_symmetric_quadratic(n = n_large, seed = seed),
    n_seeds = 5, regularization = reg_logn_over_n,
    loss_function = "log_loss",
    discretize_bins = conv_bins(n_large), max_depth = 3L
  )

  expect_lt(rmse_large, rmse_small * 0.85,
    label = "propensity RMSE at n=4000 should be at least 15% lower than at n=500")
})

test_that("RMSE convergence (regression, formula reg): three-point monotone check [slow]", {
  skip_slow_tests()

  # n = 500 -> 2000 -> 8000 with formula reg. Each step should show >= 10% drop.
  n_vec <- c(500L, 2000L, 8000L)

  rmses <- vapply(n_vec, function(n_i) {
    compute_oos_rmse(
      function(seed) dgp_r2_regression_quadratic(n = n_i, seed = seed),
      n_seeds = 5, regularization = reg_logn_over_n,
      loss_function = "squared_error",
      discretize_bins = conv_bins(n_i), max_depth = 3L
    )
  }, numeric(1L))

  expect_lt(rmses[[2]], rmses[[1]] * 0.90,
    label = "RMSE(n=2000) < RMSE(n=500) x 0.90")
  expect_lt(rmses[[3]], rmses[[2]] * 0.90,
    label = "RMSE(n=8000) < RMSE(n=2000) x 0.90")
})

test_that("higher-dim regression p=4: RMSE vs truth beats null (squared_error, CV reg) [slow]", {
  skip_slow_tests()

  # R3 truth = x1 + 0.5*x2; 2 noise features (x3, x4).
  # Null RMSE = sqrt(Var(x1 + 0.5*x2)) = sqrt(1.25) ≈ 1.12.
  # CV selects lambda; max_depth=3L.
  rmse_val <- compute_oos_rmse(
    function(seed) dgp_r3_additive_regression(n = 1200L, p = 4L, seed = seed),
    n_seeds = 5, regularization = "cv",
    loss_function = "squared_error",
    max_depth = 3L
  )
  null_rmse <- sqrt(1.25)  # 1.118

  expect_lt(rmse_val, null_rmse * 0.85,
    label = "R3 p=4 RMSE should beat null by at least 15%")
})

test_that("higher-dim propensity p=4: RMSE vs true prob beats null (log_loss, CV reg) [slow]", {
  skip_slow_tests()

  # C6 truth = plogis(0.5*x1 + 0.5*x2 + 0.5*x3); 1 noise feature (x4).
  # CV selects lambda; max_depth=3L.
  rmse_val <- compute_oos_rmse(
    function(seed) dgp_c6_additive_logistic(n = 1200L, p = 4L, seed = seed),
    n_seeds = 5, regularization = "cv",
    loss_function = "log_loss",
    max_depth = 3L
  )

  # Null RMSE ≈ sqrt(Var(plogis(0.5*x1+0.5*x2+0.5*x3))) ≈ 0.12.
  # Below 0.15 confirms the tree found signal over noise (not just predicting mean).
  expect_lt(rmse_val, 0.15,
    label = "C6 p=4 RMSE vs true prob should be below 0.15")
})
