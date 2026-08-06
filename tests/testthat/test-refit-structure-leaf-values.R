# Regression tests for refit_structure_on_data() leaf-value correctness.
#
# Bug (found 2026-08-05, fixed same day): the recursive descent tested the FULL
# feature column against the CURRENT node's observation subset,
#     true_idx  <- indices[col == 1]
#     false_idx <- indices[col == 0]
# indexing a length-|indices| vector with a length-n logical mask. At the root
# indices == seq_len(n) so the mask aligns and the result is correct; at every
# deeper node it misaligns, producing NA and duplicated row indices. The NAs were
# then absorbed by `mean(y[indices], na.rm = TRUE)`, so trees of depth >= 2
# returned WRONG leaf values with no error and no warning.
#
# The suite had no test of leaf values below depth 1, which is why it survived.
# These tests are deterministic (no RNG) so the expected values are exact.

# Depth-2 tree: root splits on feature 0; its TRUE branch splits on feature 1.
# Path convention (documented in refit_structure_on_data): false child appends 0,
# true child appends 1.
depth2_structure <- function() {
  list(
    feature = 0L, relation = "==", reference = "",
    false = list(prediction = 0L),
    true  = list(
      feature = 1L, relation = "==", reference = "",
      false = list(prediction = 0L),
      true  = list(prediction = 1L)
    )
  )
}

test_that("refit_structure_on_data partitions every observation exactly once", {
  X <- data.frame(
    f0 = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
    f1 = c(1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
  )
  y <- c(10, 10, 10, 10, 1, 1, 100, 100, 100, 1)

  fit <- refit_structure_on_data(depth2_structure(), X, y,
                                 allow_partial_leaves = TRUE)
  n_per_leaf <- attr(fit, "n_per_leaf")

  # A partition: leaf counts must sum to n. Pre-fix this was 14 for n = 10.
  expect_equal(sum(n_per_leaf), nrow(X))

  expect_equal(unname(n_per_leaf[["0"]]),   sum(X$f0 == 0))
  expect_equal(unname(n_per_leaf[["1-0"]]), sum(X$f0 == 1 & X$f1 == 0))
  expect_equal(unname(n_per_leaf[["1-1"]]), sum(X$f0 == 1 & X$f1 == 1))
})

test_that("refit_structure_on_data computes correct leaf means below depth 1", {
  X <- data.frame(
    f0 = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
    f1 = c(1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
  )
  y <- c(10, 10, 10, 10, 1, 1, 100, 100, 100, 1)

  fit <- refit_structure_on_data(depth2_structure(), X, y,
                                 allow_partial_leaves = TRUE)

  # Depth 1 was always correct (the root mask aligns).
  expect_equal(fit$false$prediction, mean(y[X$f0 == 0]))

  # Depth 2 is the regression. Pre-fix these were 75.25 and 1 respectively.
  expect_equal(fit$true$false$prediction, mean(y[X$f0 == 1 & X$f1 == 0]))
  expect_equal(fit$true$true$prediction,  mean(y[X$f0 == 1 & X$f1 == 1]))
})

test_that("refit_structure_on_data stays correct at depth 3", {
  # Root -> true -> true splits again on feature 2, so the deepest leaves sit at
  # depth 3 and are reached only through two prior misalignment opportunities.
  structure3 <- list(
    feature = 0L, relation = "==", reference = "",
    false = list(prediction = 0L),
    true  = list(
      feature = 1L, relation = "==", reference = "",
      false = list(prediction = 0L),
      true  = list(
        feature = 2L, relation = "==", reference = "",
        false = list(prediction = 0L),
        true  = list(prediction = 1L)
      )
    )
  )

  X <- expand.grid(f0 = c(0, 1), f1 = c(0, 1), f2 = c(0, 1))
  X <- X[rep(seq_len(nrow(X)), each = 3), ]           # 24 rows, every cell filled
  rownames(X) <- NULL
  y <- as.numeric(seq_len(nrow(X)))                   # distinct values

  fit <- refit_structure_on_data(structure3, X, y, allow_partial_leaves = TRUE)
  n_per_leaf <- attr(fit, "n_per_leaf")

  expect_equal(sum(n_per_leaf), nrow(X))
  expect_equal(fit$true$true$false$prediction,
               mean(y[X$f0 == 1 & X$f1 == 1 & X$f2 == 0]))
  expect_equal(fit$true$true$true$prediction,
               mean(y[X$f0 == 1 & X$f1 == 1 & X$f2 == 1]))
})

test_that("refit_structure_on_data leaf assignment matches classification too", {
  # Classification path (y in {0,1}) returns probabilities; the same subsetting
  # bug corrupted P(Y=1) per leaf.
  X <- data.frame(
    f0 = c(0, 0, 1, 1, 1, 1, 1, 1),
    f1 = c(0, 1, 0, 0, 0, 1, 1, 1)
  )
  y <- c(0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L)

  fit <- refit_structure_on_data(depth2_structure(), X, y,
                                 allow_partial_leaves = TRUE)

  p_true_false <- mean(y[X$f0 == 1 & X$f1 == 0])   # = 1
  p_true_true  <- mean(y[X$f0 == 1 & X$f1 == 1])   # = 0
  expect_equal(fit$true$false$probabilities[2], p_true_false)
  expect_equal(fit$true$true$probabilities[2],  p_true_true)
})
