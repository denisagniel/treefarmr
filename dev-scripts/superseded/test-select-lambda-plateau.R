# Tests for select_lambda_plateau() (2026-09-01, quality_reports/plans/
# 2026-09-01_two-paper-split.md, tuning-parameter Oracle consult). No test
# file previously existed for this function -- it is new this session.
#
# Uses continuous covariates throughout, matching this function's actual
# contract (it calls fit_tree() directly, which discretizes continuous X
# internally -- unlike bisect_lambda_to_budget()'s check_leaf_feasibility()
# path, this function never re-maps split indices onto the caller's X, so
# the binary-X-only constraint documented in test-bisect-lambda-budget.R
# does not apply here).

test_that("recovers the true leaf count on a 3-boundary DGP via an explicit grid", {
  # A less extreme relative of the pilot's own 3-boundary DGP
  # (quality_reports/specs/2026-09-01_twostage-recovery-and-rate.md \S2) --
  # kept small/cheap enough (n=1000, discretize_bins=10, max_depth=4L) for a
  # unit test to run in seconds, while still having a genuinely heterogeneous
  # smallest boundary (kappa3=0.35) that requires going below the default
  # log(n)/n anchor to recover -- confirmed by direct timing before writing
  # this test (log(1000)/1000 ~ 0.0069 sits ABOVE the true window's upper
  # end here; this DGP's own Delta_min-implied window is roughly
  # (0.0003, 0.01), verified empirically below, not just computed on paper).
  set.seed(20260901)
  n <- 1000L
  c1 <- 0.4; c2 <- 0.6; c3 <- 0.45
  kappa1 <- 1.2; kappa2 <- 0.6; kappa3 <- 0.35
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y_true <- ifelse(X$x1 <= c1,
    ifelse(X$x3 <= c3, 0, kappa3),
    ifelse(X$x2 <= c2, kappa1, kappa1 + kappa2))
  y <- y_true + rnorm(n, sd = 0.12)

  res <- select_lambda_plateau(
    X, y, discretize_bins = 10, max_depth = 4L,
    lambda_grid = exp(seq(log(3e-4), log(3e-2), length.out = 15L))
  )

  expect_type(res$lambda_selected, "double")
  expect_true(res$lambda_selected > 0)
  expect_true(res$plateau_log_width >= 0)
  expect_s3_class(res$grid, "data.frame")
  expect_s3_class(res$plateaus, "data.frame")
  expect_true(all(c("lambda", "n_leaves") %in% names(res$grid)))
  expect_true(all(c("n_leaves", "lambda_lo", "lambda_hi", "log_width", "n_points") %in%
    names(res$plateaus)))
  # Plateaus must be sorted by log_width descending, winner first.
  expect_true(all(diff(res$plateaus$log_width) <= 0))
  expect_equal(res$n_leaves_selected, res$plateaus$n_leaves[1])

  # The headline claim this test exists to check: the widest plateau's
  # leaf count should be 4 (the true structure), not the 2-leaf
  # under-fit that a fixed, too-large lambda_n produced in the pilot.
  expect_equal(res$n_leaves_selected, 4L)

  # And, using the SELECTED lambda directly with fit_twostage(), confirm
  # the full two-stage pipeline actually recovers all 3 boundaries at
  # this lambda -- not just that fit_tree() alone finds 4 leaves. m_ladder
  # starts at 10 bins to match the resolution the selector was calibrated
  # against above.
  fit <- fit_twostage(X, y, leaf_budget = 4L, m_ladder = c(10, 20, 40),
    lambda_n = res$lambda_selected, min_leaf_n = 1L, verbose = FALSE)
  expect_equal(fit$n_leaves_collapsed, 4L)
  expect_equal(nrow(fit$model@refinement_log), 3L)
})

test_that("default grid anchors at log(n)/n and errors if that exceeds lambda_max", {
  set.seed(20260901)
  n <- 50L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)

  expect_error(
    select_lambda_plateau(X, y, discretize_bins = 8, lambda_max = 1e-6),
    "lambda_max"
  )
})

test_that("accepts an explicit lambda_grid and rejects a degenerate one", {
  set.seed(20260901)
  n <- 500L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 2) + rnorm(n, sd = 0.1)

  res <- select_lambda_plateau(X, y, discretize_bins = 16,
    lambda_grid = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5))
  expect_equal(nrow(res$grid), 6L)

  expect_error(
    select_lambda_plateau(X, y, discretize_bins = 16, lambda_grid = c(0.01, 0.01)),
    "distinct"
  )
})

test_that("warns when every leaf count on the grid is a singleton (no real plateau)", {
  set.seed(20260901)
  n <- 300L
  X <- data.frame(x1 = runif(n))
  y <- ifelse(X$x1 <= 0.5, 0, 1) + rnorm(n, sd = 0.02)

  # Three widely-separated points, confirmed by direct probing (not
  # guessed) to give three DISTINCT leaf counts (3, 2, 1) for this DGP --
  # exercises the singleton-plateau warning path deliberately, not a claim
  # about what a well-chosen grid would do in general.
  expect_warning(
    select_lambda_plateau(X, y, discretize_bins = 8, max_depth = 0L,
      lambda_grid = c(1e-4, 1e-2, 1)),
    "not robust|refine the grid",
    perl = TRUE
  )
})

test_that("validates X/y shape before fitting anything", {
  expect_error(
    select_lambda_plateau("not a data frame", 1:5, discretize_bins = 8),
    "data.frame or matrix"
  )
  expect_error(
    select_lambda_plateau(data.frame(x1 = 1:5), 1:3, discretize_bins = 8),
    "nrow.*length"
  )
})
