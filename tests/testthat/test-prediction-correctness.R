# Prediction correctness verification
#
# Verifies that predict() returns the correct leaf outputs — not just that the
# tree has the right objective (already verified by test-brute-force-optimality.R),
# but that the TRAVERSAL code routes observations to the right leaves and
# returns the correct leaf statistic.
#
# For regression (squared_error): leaf output = mean(y in leaf).
# For classification (log_loss): leaf output = empirical P(y=1 | leaf).
#
# Approach: fit tiny known datasets where the optimal tree structure is
# determined in advance, then verify predict() values against hand-computed
# leaf statistics.

# ── helpers ───────────────────────────────────────────────────────────────────

# Fit a single tree with fixed hyperparameters (no auto-cap guard needed since
# all inputs here are already binary features, ncol(X_binary) <= 4 < 8).
fit_known <- function(X, y, loss, lambda = 0.01, max_depth = 3L) {
  fit_tree(X, y, loss_function = loss, regularization = lambda,
           max_depth = max_depth)
}

# ── regression leaf mean tests ────────────────────────────────────────────────

test_that("squared_error: depth-1 split predicts leaf means", {
  # f1 perfectly separates two groups with very different means.
  # With lambda = 0.2: depth-1 obj = 0.25 + 2*0.2 = 0.65;
  #                    depth-2 obj = 0   + 4*0.2 = 0.80. → depth-1 wins.
  # (Cross: 0.5/4 = 0.125 mean SSE per group; total mean loss = 0.25.)
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L))
  y <- c(1.0, 2.0, 10.0, 11.0)

  m <- fit_known(X, y, "squared_error", lambda = 0.2)
  pred <- predict(m, X)

  expect_equal(length(pred), 4L)
  # Rows 1-2 (f1=0): leaf mean = (1+2)/2 = 1.5
  expect_equal(pred[1], pred[2], tolerance = 1e-5)
  expect_equal(pred[1], 1.5, tolerance = 1e-4)
  # Rows 3-4 (f1=1): leaf mean = (10+11)/2 = 10.5
  expect_equal(pred[3], pred[4], tolerance = 1e-5)
  expect_equal(pred[3], 10.5, tolerance = 1e-4)
})

test_that("squared_error: depth-2 interaction predicts leaf means", {
  # Four groups defined by (f1, f2) with very different means and small within-group noise.
  # Optimal tree: split on f1, then split on f2 in each child → 4 leaves.
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
                  f2 = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L))
  # Group (f1=0,f2=0): y~0; (f1=0,f2=1): y~5; (f1=1,f2=0): y~10; (f1=1,f2=1): y~15
  y <- c(0.0, 0.1, 10.0, 10.1, 5.0, 5.1, 15.0, 15.1)

  m <- fit_known(X, y, "squared_error", lambda = 0.001, max_depth = 3L)
  pred <- predict(m, X)

  expect_equal(length(pred), 8L)
  expect_true(all(is.finite(pred)))

  # Rows in same group must get same prediction
  expect_equal(pred[1], pred[2], tolerance = 1e-5)  # f1=0,f2=0
  expect_equal(pred[5], pred[6], tolerance = 1e-5)  # f1=0,f2=1
  expect_equal(pred[3], pred[4], tolerance = 1e-5)  # f1=1,f2=0
  expect_equal(pred[7], pred[8], tolerance = 1e-5)  # f1=1,f2=1

  # Predictions should be near group means
  expect_equal(pred[1], mean(c(0.0, 0.1)), tolerance = 1e-3)
  expect_equal(pred[5], mean(c(5.0, 5.1)), tolerance = 1e-3)
  expect_equal(pred[3], mean(c(10.0, 10.1)), tolerance = 1e-3)
  expect_equal(pred[7], mean(c(15.0, 15.1)), tolerance = 1e-3)
})

test_that("squared_error: new data routes to same leaf as training data", {
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L))
  y <- c(1.0, 2.0, 10.0, 11.0)
  m <- fit_known(X, y, "squared_error")

  # New observations with the same feature value must get the same prediction
  X_new <- data.frame(f1 = c(0L, 1L, 0L, 1L))
  pred_new <- predict(m, X_new)

  expect_equal(length(pred_new), 4L)
  expect_equal(pred_new[1], 1.5, tolerance = 1e-4)  # f1=0 → group-0 mean
  expect_equal(pred_new[2], 10.5, tolerance = 1e-4) # f1=1 → group-1 mean
  expect_equal(pred_new[3], pred_new[1], tolerance = 1e-5)
  expect_equal(pred_new[4], pred_new[2], tolerance = 1e-5)
})

test_that("squared_error: predictions are all finite and no NAs", {
  set.seed(1)
  n <- 30L
  X <- data.frame(f1 = sample(0L:1L, n, replace = TRUE),
                  f2 = sample(0L:1L, n, replace = TRUE),
                  f3 = sample(0L:1L, n, replace = TRUE))
  y <- rnorm(n, mean = X$f1 * 5 + X$f2 * 3, sd = 0.5)
  m <- fit_known(X, y, "squared_error", lambda = 0.05)
  pred <- predict(m, X)

  expect_equal(length(pred), n)
  expect_true(all(is.finite(pred)))
  expect_false(anyNA(pred))
})

# ── log_loss leaf probability tests ───────────────────────────────────────────

test_that("log_loss: depth-1 split predicts empirical leaf probabilities", {
  # f1 perfectly separates: f1=0 → all y=0; f1=1 → all y=1.
  # Leaf probabilities should be (near) 0 and 1 respectively.
  X <- data.frame(f1 = c(0L, 0L, 0L, 1L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)

  m <- fit_known(X, y, "log_loss", lambda = 0.05)
  prob <- predict(m, X, type = "prob")

  expect_equal(dim(prob), c(6L, 2L))
  expect_true(all(is.finite(prob)))
  expect_true(all(prob >= 0 & prob <= 1))

  # f1=0 rows: P(y=1) should be near 0
  expect_true(all(prob[1:3, 2] < 0.1))
  # f1=1 rows: P(y=1) should be near 1
  expect_true(all(prob[4:6, 2] > 0.9))

  # Rows in same leaf must get identical predictions
  expect_equal(prob[1, 2], prob[2, 2], tolerance = 1e-6)
  expect_equal(prob[1, 2], prob[3, 2], tolerance = 1e-6)
  expect_equal(prob[4, 2], prob[5, 2], tolerance = 1e-6)
  expect_equal(prob[4, 2], prob[6, 2], tolerance = 1e-6)
})

test_that("log_loss: calibrated leaf probability matches empirical fraction", {
  # Mixed leaf: 1 out of 4 observations has y=1 → P(y=1) = 0.25.
  # With large lambda the stump is optimal, so all observations get same prob.
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L))
  y <- c(0L, 1L, 0L, 0L)

  # lambda large enough to force a stump
  m <- fit_known(X, y, "log_loss", lambda = 2.0)
  prob <- predict(m, X, type = "prob")

  # All rows in same leaf (stump) → same probability
  expect_equal(prob[1, 2], prob[2, 2], tolerance = 1e-6)
  expect_equal(prob[1, 2], prob[3, 2], tolerance = 1e-6)
  expect_equal(prob[1, 2], prob[4, 2], tolerance = 1e-6)

  # Empirical P(y=1) = 1/4 = 0.25
  expect_equal(unname(prob[1, 2]), 0.25, tolerance = 0.02)
})

test_that("log_loss: new data with same feature values routes to same leaf", {
  X <- data.frame(f1 = c(0L, 0L, 0L, 1L, 1L, 1L))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  m <- fit_known(X, y, "log_loss", lambda = 0.05)

  X_new <- data.frame(f1 = c(0L, 1L, 0L, 1L))
  prob_new <- predict(m, X_new, type = "prob")
  prob_tr  <- predict(m, X, type = "prob")

  # New f1=0 obs → same prob as training f1=0 obs
  expect_equal(prob_new[1, 2], prob_tr[1, 2], tolerance = 1e-6)
  # New f1=1 obs → same prob as training f1=1 obs
  expect_equal(prob_new[2, 2], prob_tr[4, 2], tolerance = 1e-6)
})

test_that("log_loss: class predictions are consistent with probabilities", {
  X <- data.frame(f1 = c(0L, 0L, 0L, 1L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  m <- fit_known(X, y, "log_loss", lambda = 0.05)

  prob  <- predict(m, X, type = "prob")
  class_pred <- predict(m, X, type = "class")

  # class = 1 iff P(y=1) >= 0.5
  expected_class <- as.integer(prob[, 2] >= 0.5)
  expect_equal(class_pred, expected_class)
})

# ── continuous feature discretization roundtrip ───────────────────────────────

test_that("squared_error: predict on new continuous X applies same cuts", {
  # Two groups separated by x1 = 0 (median). Training bins will create a threshold
  # near 0, so new obs with x1 < 0 and x1 > 0 must route correctly.
  set.seed(42L)
  n <- 40L
  X_tr <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y_tr <- ifelse(X_tr$x1 > 0, 10.0, 1.0) + rnorm(n, 0, 0.1)

  m <- fit_tree(X_tr, y_tr, loss_function = "squared_error",
                regularization = log(n) / n, max_depth = 3L)

  # New data: clearly below and above 0 boundary
  X_te <- data.frame(x1 = c(-2.0, -1.5, -1.0, 1.0, 1.5, 2.0),
                     x2 = c( 0.0,  0.0,  0.0, 0.0, 0.0, 0.0))
  pred <- predict(m, X_te)

  expect_equal(length(pred), 6L)
  expect_true(all(is.finite(pred)))
  # x1 < 0 observations should predict near 1; x1 > 0 should predict near 10
  expect_true(all(pred[1:3] < 5.0))
  expect_true(all(pred[4:6] > 5.0))
})

test_that("log_loss: predict on new continuous X applies same cuts", {
  set.seed(7L)
  n <- 40L
  X_tr <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y_tr <- as.integer(X_tr$x1 > 0)

  m <- fit_tree(X_tr, y_tr, loss_function = "log_loss",
                regularization = log(n) / n, max_depth = 3L)

  X_te <- data.frame(x1 = c(-2.0, -1.5, 1.5, 2.0),
                     x2 = rep(0.0, 4L))
  prob <- predict(m, X_te, type = "prob")

  expect_equal(dim(prob), c(4L, 2L))
  expect_true(all(is.finite(prob)))
  # x1 << 0 → P(y=1) low
  expect_true(all(prob[1:2, 2] < 0.4))
  # x1 >> 0 → P(y=1) high
  expect_true(all(prob[3:4, 2] > 0.6))
})
