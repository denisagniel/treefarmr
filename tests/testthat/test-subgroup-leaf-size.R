# Subgroup-Aware Minimum Leaf Size Tests
# Covers the solver-side `subgroup_target_index` / `subgroup_min_count_0` /
# `subgroup_min_count_1` floors added 2026-08-20, the raw-row counting
# convention they share with `minimum_captured_points`, and the
# `min_leaf_subgroup_fraction` positivity diagnostic.
# Spec: quality_reports/specs/2026-08-20_leaf-size-subgroup-enforcement.md
#       (global-scholars project)

library(testthat)
# Setup/teardown handled by testthat hooks in helper-setup.R

# ============================================================================
# Fixture: a forced near-degenerate region
# ============================================================================
#
# Four feature cells of 30 rows each, so only 4 distinct feature vectors exist
# among 120 rows. This is deliberate: it makes a raw row count and a
# deduplicated-by-feature-vector count differ by an order of magnitude, which
# is what lets the counting-convention tests below discriminate between them.
#
#   cell  x1 x2   n   y==0  y==1
#   A      0  0  30     25     5
#   B      0  1  30     25     5
#   C      1  0  30      2    28
#   D      1  1  30      1    29
#
# The region x1 == 1 (C + D) is near-degenerate: 60 rows carrying only 3 with
# y == 0. Splitting on x1 is strongly favoured by the objective (training error
# drops from 53/120 to 13/120), so an unconstrained fit produces a leaf whose
# y == 0 fraction is 3/60 = 0.05 -- exactly the "one more draw and this leaf is
# all-treated, so p_hat = 1 and the ATT weight blows up" failure the floor
# exists to prevent. x2 is uninformative by construction (it never reduces
# training error), so once x1 is rejected the fit must collapse to a stump.

degenerate_fixture <- function() {
  cell <- function(x1, x2, n_zero, n_one) {
    data.frame(
      x1 = rep(x1, n_zero + n_one),
      x2 = rep(x2, n_zero + n_one),
      y = c(rep(0, n_zero), rep(1, n_one))
    )
  }
  df <- rbind(
    cell(0, 0, 25, 5),
    cell(0, 1, 25, 5),
    cell(1, 0, 2, 28),
    cell(1, 1, 1, 29)
  )
  list(X = df[, c("x1", "x2")], y = df$y)
}

fx <- degenerate_fixture()

# Number of leaves a fit actually realises on the fixture.
n_realised_leaves <- function(fit, X) {
  length(unique(assign_leaf_ids(fit, X)))
}

# Smallest y == 0 count over the realised leaves.
min_leaf_zero_count <- function(fit, X, y) {
  leaf_id <- assign_leaf_ids(fit, X)
  min(tapply(y == 0, leaf_id, sum))
}

# ============================================================================
# Fixture sanity: the unconstrained fit really is degenerate
# ============================================================================

test_that("fixture is degenerate without the floor (test would be vacuous otherwise)", {
  fit <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                  regularization = 0.05, verbose = FALSE)

  expect_equal(n_realised_leaves(fit, fx$X), 2L)
  expect_equal(min_leaf_zero_count(fit, fx$X, fx$y), 3L)

  # The pre-fix failure mode: a leaf only 3 control units away from p_hat == 1.
  expect_false(
    check_leaf_feasibility(fit, fx$X, m_n = 1, group = fx$y,
                           group_value = 0, m_n_group = 4)$feasible
  )
})

# ============================================================================
# The floor rejects the deficient split
# ============================================================================

test_that("subgroup_min_count_0 rejects a split producing a control-deficient child", {
  fit <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                  regularization = 0.05, verbose = FALSE,
                  subgroup_min_count_0 = 4L)

  # The x1 split is now infeasible and nothing else pays for itself: stump.
  expect_equal(n_realised_leaves(fit, fx$X), 1L)

  # Constraint holds in the final tree, not merely at the rejected candidate.
  expect_gte(min_leaf_zero_count(fit, fx$X, fx$y), 4L)
  expect_true(
    check_leaf_feasibility(fit, fx$X, m_n = 1, group = fx$y,
                           group_value = 0, m_n_group = 4)$feasible
  )
})

test_that("subgroup_min_count_1 applies the same floor to the other arm", {
  # The x1 == 0 child carries 10 rows with y == 1, so a floor of 11 rejects it
  # while a floor of 10 does not. Mechanism is symmetric even though the
  # default is asymmetric (only the control side is load-bearing).
  fit_allowed <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                          regularization = 0.05, verbose = FALSE,
                          subgroup_min_count_1 = 10L)
  fit_rejected <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                           regularization = 0.05, verbose = FALSE,
                           subgroup_min_count_1 = 11L)

  expect_equal(n_realised_leaves(fit_allowed, fx$X), 2L)
  expect_equal(n_realised_leaves(fit_rejected, fx$X), 1L)
})

test_that("subgroup_target_index can be named explicitly instead of resolved from y", {
  # Index 1 is the encoder's 1{y == 1} indicator, which is what the automatic
  # resolution picks; passing it by hand must reproduce the same fit.
  fit_explicit <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                           regularization = 0.05, verbose = FALSE,
                           subgroup_target_index = 1L, subgroup_min_count_0 = 4L)

  expect_equal(n_realised_leaves(fit_explicit, fx$X), 1L)

  # Index 0 is the 1{y == 0} indicator, so the arms swap: a floor on "indicator
  # == 1" is then a floor on control rows and must reject the same split.
  fit_swapped <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                          regularization = 0.05, verbose = FALSE,
                          subgroup_target_index = 0L, subgroup_min_count_1 = 4L)

  expect_equal(n_realised_leaves(fit_swapped, fx$X), 1L)
})

# ============================================================================
# Counting convention: identical to minimum_captured_points (raw rows)
# ============================================================================

test_that("subgroup counts are raw row counts, bracketed at the exact boundary", {
  # Leaf(x1 == 1) holds 3 rows with y == 0 spread over 2 distinct feature
  # vectors (cells C and D). A floor of 3 is therefore satisfiable only if the
  # solver counts raw rows; a deduplicated-by-feature-vector count would see 2
  # and reject. A floor of 4 must reject under either convention.
  fit_at_boundary <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                              regularization = 0.05, verbose = FALSE,
                              subgroup_min_count_0 = 3L)
  fit_past_boundary <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                                regularization = 0.05, verbose = FALSE,
                                subgroup_min_count_0 = 4L)

  expect_equal(n_realised_leaves(fit_at_boundary, fx$X), 2L)
  expect_equal(n_realised_leaves(fit_past_boundary, fx$X), 1L)
})

test_that("minimum_captured_points brackets at the same raw row count", {
  # Companion to the test above, run through the pre-existing total-size floor
  # on the same data. Each child of the x1 split holds 60 raw rows but only 2
  # distinct feature vectors, and the whole dataset has only 4. The check is
  # `min(count, n - count) <= minimum_captured_points`, so raw counting brackets
  # at 60 (59 permits the split, 60 forbids it) whereas deduplicated counting
  # would have forbidden it at any value >= 2. Both floors therefore agree on
  # raw physical row counts -- the convention the spec requires them to share.
  fit_at_boundary <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                              regularization = 0.05, verbose = FALSE,
                              minimum_captured_points = 59L)
  fit_past_boundary <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                                regularization = 0.05, verbose = FALSE,
                                minimum_captured_points = 60L)

  # `minimum_captured_points` is sticky in the solver's process-global
  # Configuration: it is only overwritten when supplied, so the 60 above would
  # otherwise constrain every later fit in this process. That stickiness is
  # pre-existing behaviour, deliberately left untouched by the subgroup work
  # (the three subgroup fields are reset per fit instead), so restore it by hand
  # -- before asserting, so a failing expectation cannot leak it either.
  fit_tree(fx$X, fx$y, loss_function = "misclassification",
           regularization = 0.05, verbose = FALSE,
           minimum_captured_points = 0L)

  expect_equal(n_realised_leaves(fit_at_boundary, fx$X), 2L)
  expect_equal(n_realised_leaves(fit_past_boundary, fx$X), 1L)
})

# ============================================================================
# Backward compatibility: the feature is opt-in
# ============================================================================

test_that("omitting the subgroup settings leaves the search unchanged", {
  fit_default <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                          regularization = 0.05, verbose = FALSE)
  fit_zero_floor <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                             regularization = 0.05, verbose = FALSE,
                             subgroup_min_count_0 = 0L,
                             subgroup_min_count_1 = 0L)

  expect_equal(n_realised_leaves(fit_zero_floor, fx$X),
               n_realised_leaves(fit_default, fx$X))
  expect_equal(fit_zero_floor@min_leaf_subgroup_fraction,
               fit_default@min_leaf_subgroup_fraction)
})

test_that("a subgroup floor does not leak into the next fit in the same session", {
  # The solver's Configuration is a process-global that configure() only
  # overwrites field-by-field, so an omitted field would keep the previous fit's
  # value. That would let an outcome (mu) tree silently inherit a propensity
  # tree's control floor. Fit constrained, then unconstrained, and check the
  # second fit is unaffected.
  constrained <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                          regularization = 0.05, verbose = FALSE,
                          subgroup_min_count_0 = 4L)
  after <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                    regularization = 0.05, verbose = FALSE)

  expect_equal(n_realised_leaves(constrained, fx$X), 1L)
  expect_equal(n_realised_leaves(after, fx$X), 2L)
  expect_equal(after@min_leaf_subgroup_fraction, 3 / 60)
})

test_that("a subgroup floor that cannot be resolved from y is an error, not a silent no-op", {
  y_no_ones <- rep(0, nrow(fx$X))
  expect_error(
    fit_tree(fx$X, y_no_ones, loss_function = "misclassification",
             regularization = 0.05, verbose = FALSE,
             subgroup_min_count_0 = 4L),
    "subgroup_target_index"
  )
})

test_that("an out-of-range explicit subgroup_target_index is an R error, not a solver crash", {
  # subgroup_target_index indexes the encoder's binary label indicators
  # (2 of them for this binary fixture: indices 0 and 1). Index 2 is out of
  # range. Left unchecked at the R level this reaches Task::create_children()
  # on a worker thread and throws an uncaught std::runtime_error there, which
  # escapes the thread function and calls std::terminate() -- a hard R-session
  # abort with no R condition to catch. The fix is an R-side range check before
  # any JSON crosses into C++; this test is the regression guard for it.
  expect_error(
    fit_tree(fx$X, fx$y, loss_function = "misclassification",
             regularization = 0.05, verbose = FALSE,
             subgroup_target_index = 2L, subgroup_min_count_0 = 1L),
    "subgroup_target_index"
  )
  # Negative is equally out of range (not just "too large").
  expect_error(
    fit_tree(fx$X, fx$y, loss_function = "misclassification",
             regularization = 0.05, verbose = FALSE,
             subgroup_target_index = -2L, subgroup_min_count_0 = 1L),
    "subgroup_target_index"
  )
})

test_that("a negative subgroup_min_count_* is an R error, not a silent wraparound", {
  # subgroup_min_count_0/1 are `unsigned int` in C++. A negative R value would
  # wrap to ~4.29e9 rather than erroring, making every candidate split
  # infeasible -- a silent root-only tree with no warning, not a crash. The fix
  # rejects negative values in R before they reach the solver.
  expect_error(
    fit_tree(fx$X, fx$y, loss_function = "misclassification",
             regularization = 0.05, verbose = FALSE,
             subgroup_min_count_0 = -1L),
    "subgroup_min_count_0"
  )
  expect_error(
    fit_tree(fx$X, fx$y, loss_function = "misclassification",
             regularization = 0.05, verbose = FALSE,
             subgroup_min_count_1 = -1L),
    "subgroup_min_count_1"
  )
})

# ============================================================================
# min_leaf_subgroup_fraction diagnostic
# ============================================================================

test_that("min_leaf_subgroup_fraction reports the realised overlap constant", {
  fit_unconstrained <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                                regularization = 0.05, verbose = FALSE)
  fit_constrained <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                              regularization = 0.05, verbose = FALSE,
                              subgroup_min_count_0 = 4L)

  # Unconstrained: the near-degenerate leaf, 3 control rows out of 60.
  expect_equal(fit_unconstrained@min_leaf_subgroup_fraction, 3 / 60)

  # Constrained: the stump, 53 control rows out of 120.
  expect_equal(fit_constrained@min_leaf_subgroup_fraction, 53 / 120)

  # Enforcing the floor must raise the diagnostic, never lower it.
  expect_gt(fit_constrained@min_leaf_subgroup_fraction,
            fit_unconstrained@min_leaf_subgroup_fraction)
})

test_that("min_leaf_subgroup_fraction agrees with the post-fit leaf counts", {
  fit <- fit_tree(fx$X, fx$y, loss_function = "misclassification",
                  regularization = 0.05, verbose = FALSE)

  leaf_counts <- check_leaf_feasibility(fit, fx$X, m_n = 1, group = fx$y,
                                        group_value = 0)$leaf_counts
  expect_equal(fit@min_leaf_subgroup_fraction,
               min(leaf_counts$n_group / leaf_counts$n))
})

test_that("min_leaf_subgroup_fraction is NA where the diagnostic does not apply", {
  set.seed(42)
  X_reg <- data.frame(
    x1 = sample(0:1, 60, replace = TRUE),
    x2 = sample(0:1, 60, replace = TRUE)
  )
  y_reg <- runif(60, 0, 10)

  fit_reg <- fit_tree(X_reg, y_reg, loss_function = "squared_error",
                      regularization = 0.1, verbose = FALSE)

  expect_true(is.na(fit_reg@min_leaf_subgroup_fraction))

  # The property is validated, so NA must survive construction rather than
  # tripping the S7 validator.
  expect_true(is.numeric(fit_reg@min_leaf_subgroup_fraction))
})

test_that("min_leaf_subgroup_fraction() helper degrades to NA rather than erroring", {
  expect_true(is.na(min_leaf_subgroup_fraction(NULL, fx$X, fx$y)))
  expect_true(is.na(min_leaf_subgroup_fraction(list(prediction = 1), NULL, fx$y)))
  expect_true(is.na(min_leaf_subgroup_fraction(list(prediction = 1), fx$X, fx$y[-1])))

  # A single-leaf tree over the whole fixture: 53 of 120 rows have y == 0.
  expect_equal(min_leaf_subgroup_fraction(list(prediction = 1), fx$X, fx$y),
               53 / 120)
})

# ============================================================================
# The mu-tree call path is untouched
# ============================================================================

test_that("log_loss and squared_error fits are unaffected when no floor is set", {
  # The outcome (mu) tree is fitted on control-only data with only
  # `minimum_captured_points`, so it must see no behavioural change at all.
  fit_logloss <- fit_tree(fx$X, fx$y, loss_function = "log_loss",
                          regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(fit_logloss, "log_loss")
  expect_gte(fit_logloss@n_trees, 1L)

  set.seed(7)
  X_reg <- data.frame(
    x1 = sample(0:1, 60, replace = TRUE),
    x2 = sample(0:1, 60, replace = TRUE)
  )
  y_reg <- 2 * X_reg$x1 + rnorm(60, sd = 0.1)
  fit_reg <- fit_tree(X_reg, y_reg, loss_function = "squared_error",
                      regularization = 0.05, verbose = FALSE)
  expect_valid_treefarms_model(fit_reg, "squared_error")
})
