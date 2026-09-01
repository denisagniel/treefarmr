# Tests for bisect_lambda_to_budget()'s max_depth handling (2026-09-01 fix,
# quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md §3.2).
# No test file previously existed for this function at all; this covers both
# the new depth-cap logic and a smoke test of the pre-existing bisection path.
#
# Covariates are BINARY throughout, matching this function's actual, documented
# contract: check_leaf_feasibility()/assign_leaf_ids() map tree split feature
# indices onto the SUPPLIED X directly, which only agrees with the fitted tree
# when X is already binary (a tree fit on internally-discretized continuous X
# splits on threshold-indicator features the supplied X doesn't have -- see
# doubletree::estimate_att()'s explicit binary-X requirement, which exists for
# exactly this reason). A first draft of these tests used continuous X and
# happened to pass only because those particular fits degenerated to
# single-leaf stumps (no split ever evaluated, so the mismatch stayed latent);
# switching to a real multi-leaf fit with continuous X reproduced the "split
# references feature index k but X has p columns" error, confirming this is a
# pre-existing constraint of the function, not a bug introduced by this fix.

test_that("default max_depth resolves to depth_required = leaf_budget - 1", {
  set.seed(20260901)
  n <- 300
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, plogis(3 * (X$x1 - 0.5)))

  res <- bisect_lambda_to_budget(X, y, leaf_budget = 4, loss_function = "log_loss")
  expect_equal(res$max_depth, 3L)          # leaf_budget - 1
  expect_equal(res$depth_required, 3L)
  expect_true(res$depth_sufficient)
  expect_false(res$depth_restricted)
  expect_equal(res$leaf_budget, 4L)
  expect_true(res$n_leaves <= 4L)
})

test_that("leaf_budget = 1 clamps depth_required/max_depth to 1, not the disallowed 0", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  res <- bisect_lambda_to_budget(X, y, leaf_budget = 1, loss_function = "log_loss")
  expect_equal(res$max_depth, 1L)
  expect_equal(res$depth_required, 1L)
  expect_true(res$n_leaves <= 1L)
})

test_that("max_depth = 0L is rejected (no longer safely means 'unlimited')", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  expect_error(
    bisect_lambda_to_budget(X, y, leaf_budget = 4, max_depth = 0L),
    "max_depth = 0L"
  )
})

test_that("max_depth below depth_reachable errors unconditionally (mechanically unreachable)", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5), x3 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  # leaf_budget = 8 needs max_depth >= ceiling(log2(8)) = 3 to be reachable at all.
  expect_error(
    bisect_lambda_to_budget(X, y, leaf_budget = 8, max_depth = 2L),
    "mechanically unreachable"
  )
  # depth_restricted = TRUE does NOT rescue this -- reachability is not a tradeoff.
  expect_error(
    bisect_lambda_to_budget(X, y, leaf_budget = 8, max_depth = 2L,
                             depth_restricted = TRUE),
    "mechanically unreachable"
  )
})

test_that("max_depth in [depth_reachable, depth_required) errors without depth_restricted", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5), x3 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  # leaf_budget = 8: depth_reachable = 3, depth_required = 7. max_depth = 3 is
  # reachable but restricted.
  expect_error(
    bisect_lambda_to_budget(X, y, leaf_budget = 8, max_depth = 3L),
    "depth_restricted"
  )
})

test_that("depth_restricted = TRUE accepts a reachable-but-restricted max_depth", {
  # 3-way parity of x1,x2,x3 needs a genuine depth-3 tree to fit losslessly
  # (all 8 combinations map to distinct y values) -- a real, non-degenerate
  # multi-leaf fit, not a stump, so check_leaf_feasibility()'s split-index
  # mapping is actually exercised.
  set.seed(20260901)
  n <- 400
  x1 <- rbinom(n, 1, 0.5); x2 <- rbinom(n, 1, 0.5); x3 <- rbinom(n, 1, 0.5)
  X <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  y <- as.integer(xor(xor(x1, x2), x3))

  res <- bisect_lambda_to_budget(X, y, leaf_budget = 8, max_depth = 3L,
                                  lambda_n = 1e-6, depth_restricted = TRUE,
                                  loss_function = "log_loss")
  expect_equal(res$max_depth, 3L)
  expect_equal(res$depth_required, 7L)
  expect_false(res$depth_sufficient)
  expect_true(res$depth_restricted)
  expect_true(res$feasible)
  expect_true(res$n_leaves <= 8L)
  expect_gt(res$n_leaves, 1L)  # confirms this is a real, non-stump fit
  # certified is well-defined (over the declared, restricted problem) even
  # though depth_sufficient is FALSE -- it is not forced to FALSE.
  expect_type(res$certified, "logical")
})

test_that("depth_restricted = TRUE with max_depth = NULL errors (nothing to restrict)", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  expect_error(
    bisect_lambda_to_budget(X, y, leaf_budget = 4, depth_restricted = TRUE),
    "requires an explicit"
  )
})

test_that("max_depth cannot end up in ... even when forwarded through a wrapper", {
  # max_depth is a formal parameter, so R's own argument matching binds any
  # max_depth = argument to it directly -- confirmed here for the forwarding
  # case (a wrapper collecting max_depth into its own ... and passing that
  # ... straight through), which is the scenario a dots-based guard would
  # otherwise exist to catch.
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5), x3 = rbinom(n, 1, 0.5))
  y <- rbinom(n, 1, 0.5)

  wrapper <- function(...) bisect_lambda_to_budget(X, y, leaf_budget = 8, ...)
  res <- wrapper(max_depth = 3L, depth_restricted = TRUE, loss_function = "log_loss")
  expect_equal(res$max_depth, 3L)
  expect_true(res$depth_restricted)
})

test_that("bisection still recovers the certificate when lambda_n overshoots the budget", {
  # Same 3-way parity DGP: at near-zero lambda_n the solver will use the full
  # 8-leaf exact fit (> leaf_budget = 4), forcing real bisection
  # (used_search = TRUE) down to a feasible <= 4-leaf fit, exercising the
  # pre-existing bisection machinery now routed through an explicit max_depth.
  set.seed(20260901)
  n <- 400
  x1 <- rbinom(n, 1, 0.5); x2 <- rbinom(n, 1, 0.5); x3 <- rbinom(n, 1, 0.5)
  X <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  y <- as.integer(xor(xor(x1, x2), x3))

  res <- bisect_lambda_to_budget(X, y, leaf_budget = 4, lambda_n = 1e-6,
                                  loss_function = "log_loss")
  expect_true(res$used_search)
  expect_true(res$n_leaves <= 4L)
  expect_true(res$feasible)
  expect_equal(res$max_depth, 3L)
  expect_equal(res$depth_required, 3L)
  expect_true(res$depth_sufficient)
})
