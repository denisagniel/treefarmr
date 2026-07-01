# Objective normalization soundness gate (loss-normalization fix, 2026-06-30)
#
# After mean-normalizing log_loss and squared_error in C++ (dataset.cpp), the
# optimized objective must equal  mean_empirical_loss + regularization*(#leaves),
# matching Xu et al. (2026) and the manuscript. These hand-computed checks on tiny
# datasets are the branch-and-bound soundness gate: if normalization had leaked
# into the bounds inconsistently, the optimizer would either report a wrong
# objective or select the wrong tree.
#
# See quality_reports/plans/2026-06-30_loss-normalization-fix.md

# Helper: extract the (single) tree's reported objective and leaf losses.
extract_root <- function(model) {
  trees <- model@trees
  expect_length(trees, 1L)
  trees[[1]]
}

test_that("squared_error objective is mean-normalized: MSE + lambda*(#leaves)", {
  # One binary feature perfectly separates two outcome groups.
  X <- data.frame(f1 = c(0, 0, 1, 1))
  y <- c(1.0, 3.0, 10.0, 12.0)        # group means: 2 and 11
  lambda <- 0.1
  n <- length(y)

  model <- fit_tree(X, y, loss_function = "squared_error", regularization = lambda)
  root <- extract_root(model)

  # Hand-computed optimal (2-leaf) objective:
  #   leaf {0,0}: pred 2, SSE = 1+1 = 2
  #   leaf {1,1}: pred 11, SSE = 1+1 = 2
  #   total SSE = 4 -> MSE = 4/n = 1.0; + lambda*2 = 1.2
  expect_equal(root$model_objective, 1.0 + lambda * 2, tolerance = 1e-5)

  # Each leaf loss must be leaf_SSE / GLOBAL n (= 2/4 = 0.5), so leaf losses sum
  # to MSE. This is the #1 correctness point: divide by global n, not leaf count.
  expect_equal(root$false$loss, 0.5, tolerance = 1e-5)
  expect_equal(root$true$loss, 0.5, tolerance = 1e-5)
  expect_equal(root$false$loss + root$true$loss, sum((y - rep(c(2, 11), each = 2))^2) / n,
               tolerance = 1e-5)

  # The split must beat the stump (mean(y) = 6.5): stump MSE = 21.25, obj 21.35.
  # If the penalty were on the wrong scale, the optimizer could pick the stump.
  expect_lt(root$model_objective, sum((y - mean(y))^2) / n + lambda)
})

test_that("log_loss objective is mean-normalized: mean cross-entropy + lambda*(#leaves)", {
  X <- data.frame(f1 = c(0, 0, 1, 1))
  y <- c(0, 0, 1, 1)                  # f1 perfectly predicts class
  lambda <- 0.1

  model <- fit_tree(X, y, loss_function = "log_loss", regularization = lambda)
  root <- extract_root(model)

  # Perfect separation -> within-leaf entropy ~ 0 (eps-clamped), so the objective
  # is essentially lambda*(#leaves) = 0.2. Tolerance loose enough for eps clamp.
  expect_equal(root$model_objective, lambda * 2, tolerance = 1e-3)
  expect_lt(root$false$loss, 1e-6)
  expect_lt(root$true$loss, 1e-6)
})

test_that("squared_error objective scale is invariant to sample size (mean, not sum)", {
  # Duplicating every row leaves MSE unchanged; a SUMMED objective would double.
  X1 <- data.frame(f1 = c(0, 0, 1, 1))
  y1 <- c(1.0, 3.0, 10.0, 12.0)
  X2 <- data.frame(f1 = rep(c(0, 0, 1, 1), 2))
  y2 <- rep(c(1.0, 3.0, 10.0, 12.0), 2)
  lambda <- 0.1

  o1 <- extract_root(fit_tree(X1, y1, loss_function = "squared_error",
                              regularization = lambda))$model_objective
  o2 <- extract_root(fit_tree(X2, y2, loss_function = "squared_error",
                              regularization = lambda))$model_objective
  expect_equal(o1, o2, tolerance = 1e-5)
})
