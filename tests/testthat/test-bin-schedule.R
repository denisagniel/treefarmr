# Tests for the discretization bin-count schedules (2026-07-01 code fix).
#
# The convergence theory requires the per-coordinate grid to refine with n at a
# POLYNOMIAL rate to attain the continuous rate; the previous log schedule was
# too coarse. These tests verify the new "adaptive" (polynomial) default, the
# retained "log" legacy schedule, the "cv" data-driven option, binary invariance,
# and the cap/floor. See quality_reports/plans/2026-07-01_code-fix-discretization-refinement.md

test_that("compute_bin_count: adaptive grows polynomially and beats log", {
  ns <- c(100, 500, 2000, 8000, 50000)
  adaptive <- vapply(ns, function(n) compute_bin_count("adaptive", n), integer(1))
  logbins  <- vapply(ns, function(n) compute_bin_count("log", n), integer(1))

  # Monotone nondecreasing in n.
  expect_true(all(diff(adaptive) >= 0))
  # Polynomial dominates logarithmic for all but the smallest n.
  expect_true(all(adaptive >= logbins))
  expect_gt(adaptive[length(adaptive)], logbins[length(logbins)] * 3)

  # Matches the closed form ceil(n^(1/3)) below the cap.
  expect_equal(compute_bin_count("adaptive", 8000), as.integer(ceiling(8000^(1/3))))
})

test_that("compute_bin_count: respects floor of 2 and an upper cap", {
  # Floor: tiny n still yields at least 2 bins (one threshold).
  expect_gte(compute_bin_count("adaptive", 4), 2L)
  expect_gte(compute_bin_count("log", 4), 2L)

  # Cap: an explicit small cap is honored.
  expect_lte(compute_bin_count("adaptive", 1e6, cap = 10), 10L)

  # Custom exponent must be in (0, 1/2).
  expect_error(compute_bin_count("adaptive", 1000, rho = 0.7), "rho")
})

test_that("compute_bin_count: numeric passthrough and unknown schedule errors", {
  expect_equal(compute_bin_count(6, 1000), 6L)
  expect_error(compute_bin_count(1, 1000), ">= 2")
  expect_error(compute_bin_count("weekly", 1000), "unknown bin schedule")
})

test_that("binary covariates are unaffected by the schedule (no bins created)", {
  set.seed(20260701)
  n <- 300
  X <- data.frame(a = sample(0:1, n, replace = TRUE),
                  b = sample(0:1, n, replace = TRUE))
  # Discretization must detect all-binary and pass through unchanged,
  # regardless of the requested schedule.
  for (sched in list("adaptive", "log", 8)) {
    disc <- discretize_features(X, method = "quantiles", n_bins = sched)
    expect_true(disc$metadata$all_binary)
    expect_equal(ncol(disc$X_binary), 2L)
  }
})

test_that("continuous features get more binary columns under adaptive than log", {
  set.seed(20260701)
  n <- 2000
  X <- data.frame(x = runif(n))
  disc_adaptive <- discretize_features(X, method = "quantiles", n_bins = "adaptive")
  disc_log      <- discretize_features(X, method = "quantiles", n_bins = "log")
  # More bins -> more threshold indicator columns.
  expect_gt(ncol(disc_adaptive$X_binary), ncol(disc_log$X_binary))
})

test_that("'cv' cannot be resolved inside discretize_features (needs y)", {
  X <- data.frame(x = runif(50))
  expect_error(discretize_features(X, n_bins = "cv"), "resolved by the caller")
})

test_that("select_bins_cv returns a sensible bin count from the candidate grid", {
  skip_on_cran()
  set.seed(20260701)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rbinom(n, 1, plogis(3 * (X$x1 - 0.5)))
  res <- select_bins_cv(X, y, loss_function = "log_loss")
  expect_true(res$best_bins %in% res$candidate_bins)
  expect_gte(res$best_bins, 2L)
  expect_length(res$cv_loss, length(res$candidate_bins))
})

test_that("fit_tree accepts 'adaptive', 'log', 'cv', and numeric bins on continuous X", {
  skip_on_cran()
  set.seed(20260701)
  n <- 300
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rbinom(n, 1, plogis(3 * (X$x1 - 0.5)))
  for (sched in list("adaptive", "log", "cv", 6)) {
    m <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.05,
                  discretize_bins = sched)
    expect_s7_class(m, OptimalTreesModel)
    expect_equal(m@n_trees, 1L)
  }
})
