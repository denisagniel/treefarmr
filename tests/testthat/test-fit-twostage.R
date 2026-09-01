# Tests for fit_twostage()'s pure, no-fitting-required budget-resolution
# helpers (Milestone E sub-step E1). fit_twostage() itself (E2/E3/E4) is not
# yet built -- see R/fit_twostage.R and
# quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md's
# Milestone E addendum for the full architecture (Oracle-consulted) these
# helpers implement a piece of.

# ---------------------------------------------------------------------------
# .twostage_resolve_budgets()
# ---------------------------------------------------------------------------

test_that("default depth_budget reproduces Milestone D's leaf_budget=4 benchmark point exactly", {
  res <- .twostage_resolve_budgets(leaf_budget = 4, depth_budget = NULL, n = 2000)
  expect_equal(res$leaf_budget, 4L)
  expect_equal(res$L_A, 7L)                 # 2*4 - 1
  expect_equal(res$depth_required_A, 6L)    # L_A - 1
  expect_equal(res$depth_reachable_A, 3L)   # ceiling(log2(7))
  expect_equal(res$d_0, 2L)                 # ceiling(log2(4))
  expect_equal(res$d_0_source, "default_balanced")
  expect_equal(res$d_A, 4L)                 # min(2*2, 6) -- Milestone D's measured d_A
  expect_true(res$depth_restricted_A)
})

test_that("default depth_budget reproduces Milestone D's leaf_budget=8 benchmark point exactly", {
  res <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = NULL, n = 2000)
  expect_equal(res$L_A, 15L)                # 2*8 - 1
  expect_equal(res$depth_required_A, 14L)
  expect_equal(res$depth_reachable_A, 4L)   # ceiling(log2(15))
  expect_equal(res$d_0, 3L)                 # ceiling(log2(8))
  expect_equal(res$d_A, 6L)                 # min(2*3, 14) -- Milestone D's measured d_A
  expect_true(res$depth_restricted_A)
})

test_that("depth_budget = 'full' gives full compliance over the inflated class, not the staged cap", {
  res <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = "full", n = 2000)
  expect_equal(res$d_0_source, "full")
  expect_true(is.na(res$d_0))
  expect_equal(res$d_A, res$depth_required_A)   # d_A = 14, full compliance
  expect_false(res$depth_restricted_A)
})

test_that("an explicit integer depth_budget is honored as the user's own d_0", {
  res <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = 2, n = 2000)
  expect_equal(res$d_0_source, "user")
  expect_equal(res$d_0, 2L)
  expect_equal(res$d_A, 4L)   # min(2*2, 14)
  expect_true(res$depth_restricted_A)
})

test_that("leaf_budget = 1 never resolves to the disallowed max_depth = 0L", {
  res <- .twostage_resolve_budgets(leaf_budget = 1, depth_budget = NULL, n = 2000)
  expect_equal(res$L_A, 1L)
  expect_equal(res$depth_required_A, 1L)
  expect_equal(res$depth_reachable_A, 1L)
  expect_gte(res$d_A, 1L)
  expect_false(res$depth_restricted_A)   # d_A == depth_required_A == 1 at leaf_budget = 1
})

test_that("d_A is clamped up to depth_reachable_A when the depth_budget-derived value would fall below it", {
  # leaf_budget = 20 -> L_A = 39, depth_reachable_A = ceiling(log2(39)) = 6.
  # An explicit tiny depth_budget = 1 would naively give d_A = min(2, 38) = 2,
  # which is BELOW depth_reachable_A = 6 -- bisect_lambda_to_budget() would
  # reject max_depth < depth_reachable unconditionally. The clamp must lift
  # d_A to depth_reachable_A, not merely warn.
  res <- .twostage_resolve_budgets(leaf_budget = 20, depth_budget = 1, n = 2000)
  expect_equal(res$L_A, 39L)
  expect_equal(res$depth_reachable_A, 6L)
  expect_equal(res$d_A, 6L)
  expect_true(res$depth_restricted_A)
})

test_that("degenerate tiny n does not crash and stays internally consistent", {
  res <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = NULL, n = 3)
  expect_equal(res$L_A, 3L)                 # min(15, A_m=3)
  expect_true(is.finite(res$depth_required_A))
  expect_true(is.finite(res$depth_reachable_A))
  expect_true(is.finite(res$d_A))
  expect_gte(res$d_A, res$depth_reachable_A)
  expect_lte(res$d_A, res$depth_required_A)
})

test_that("invalid leaf_budget/n are rejected with actionable messages", {
  expect_error(.twostage_resolve_budgets(leaf_budget = 0, depth_budget = NULL, n = 100),
               "leaf_budget")
  expect_error(.twostage_resolve_budgets(leaf_budget = -1, depth_budget = NULL, n = 100),
               "leaf_budget")
  expect_error(.twostage_resolve_budgets(leaf_budget = c(4, 8), depth_budget = NULL, n = 100),
               "leaf_budget")
  expect_error(.twostage_resolve_budgets(leaf_budget = 4, depth_budget = NULL, n = 0),
               "n.*nrow")
})

test_that("invalid depth_budget values are rejected, not silently coerced", {
  expect_error(.twostage_resolve_budgets(leaf_budget = 4, depth_budget = "bogus", n = 100),
               "depth_budget")
  expect_error(.twostage_resolve_budgets(leaf_budget = 4, depth_budget = -1, n = 100),
               "depth_budget")
  expect_error(.twostage_resolve_budgets(leaf_budget = 4, depth_budget = c(2, 3), n = 100),
               "depth_budget")
})

# ---------------------------------------------------------------------------
# .twostage_emit_depth_message()
# ---------------------------------------------------------------------------

test_that("the depth-cap message fires only on the default_balanced path", {
  res_default <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = NULL, n = 2000)
  expect_message(.twostage_emit_depth_message(res_default), "STAGED cap")

  res_full <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = "full", n = 2000)
  expect_no_message(.twostage_emit_depth_message(res_full))

  res_user <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = 3, n = 2000)
  expect_no_message(.twostage_emit_depth_message(res_user))
})

test_that("verbose = FALSE suppresses the depth-cap message even on the default path", {
  res_default <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = NULL, n = 2000)
  expect_no_message(.twostage_emit_depth_message(res_default, verbose = FALSE))
})

test_that("the depth-cap message reports the actual resolved numbers, not placeholders", {
  res <- .twostage_resolve_budgets(leaf_budget = 8, depth_budget = NULL, n = 2000)
  msg <- gsub("\\s+", " ", capture_condition(.twostage_emit_depth_message(res))$message)
  expect_match(msg, "log2\\(8\\)\\) = 3")   # d_0
  expect_match(msg, "d_A = 6")
  expect_match(msg, "L_A = 15")
  expect_match(msg, "depth 14")
})

# ---------------------------------------------------------------------------
# .twostage_validate_ladder()
# ---------------------------------------------------------------------------

test_that("a well-formed ladder passes through unchanged (sorted, deduplicated)", {
  res <- .twostage_validate_ladder(m_ladder = c(32, 16, 64, 128), n = 2000, m_n = 1)
  expect_equal(res$m_ladder, c(16L, 32L, 64L, 128L))
  expect_equal(res$dropped_rungs, integer(0))
})

test_that("duplicate rungs are deduplicated", {
  res <- .twostage_validate_ladder(m_ladder = c(16, 32, 32, 16, 64), n = 2000, m_n = 1)
  expect_equal(res$m_ladder, c(16L, 32L, 64L))
})

test_that("a rung below 2 errors unconditionally", {
  expect_error(.twostage_validate_ladder(m_ladder = c(1, 16, 32), n = 2000, m_n = 1),
               ">= 2")
})

test_that("a 1-rung ladder errors -- topology_stable needs two rungs", {
  expect_error(.twostage_validate_ladder(m_ladder = 32, n = 2000, m_n = 1),
               "at least 2")
  # Also catches the case where duplicates collapse a nominally-multi-rung
  # ladder down to one distinct value.
  expect_error(.twostage_validate_ladder(m_ladder = c(32, 32), n = 2000, m_n = 1),
               "at least 2")
})

test_that("rungs the sample cannot support (m_n = o(n/r_n)) are pruned and reported", {
  # n = 200, m_n = 20 -> m_max_supported = floor(200 / 40) = 5. Every
  # requested rung exceeds that, so all get dropped -- and since fewer than
  # 2 rungs then remain, this must error, not silently return an empty/
  # 1-rung ladder.
  expect_error(
    .twostage_validate_ladder(m_ladder = c(16, 32, 64), n = 200, m_n = 20),
    "fewer than 2 rungs remain"
  )
})

test_that("partial pruning keeps the supported rungs and reports the dropped ones", {
  # n = 2000, m_n = 20 -> m_max_supported = floor(2000/40) = 50. 16 and 32
  # survive (<=), 64/128 do not -- 2 rungs remain, so this must succeed
  # (not error), unlike the fully-pruned case above.
  res <- .twostage_validate_ladder(m_ladder = c(16, 32, 64, 128), n = 2000, m_n = 20)
  expect_equal(res$m_max_supported, 50L)
  expect_equal(res$m_ladder, c(16L, 32L))
  expect_equal(res$dropped_rungs, c(64L, 128L))
})

test_that("invalid n/m_n/m_ladder inputs are rejected", {
  expect_error(.twostage_validate_ladder(m_ladder = c(16, 32), n = 0, m_n = 1), "n.*nrow")
  expect_error(.twostage_validate_ladder(m_ladder = c(16, 32), n = 2000, m_n = 0), "m_n")
  expect_error(.twostage_validate_ladder(m_ladder = numeric(0), n = 2000, m_n = 1), "m_ladder")
  expect_error(.twostage_validate_ladder(m_ladder = c(16, NA), n = 2000, m_n = 1), "m_ladder")
})

# ---------------------------------------------------------------------------
# .twostage_run_rung() (Milestone E sub-step E2). Runs a REAL Stage-A fit,
# so these are the first genuinely slow(er) tests in this file -- kept
# small (n in the low hundreds, few bins) and confirmed fast (<6s each) by
# direct, isolated timing before being written, per this session's memory-
# safety convention (§6 of the plan file: prototype small, run one at a
# time, no parallelism).
# ---------------------------------------------------------------------------

test_that(".twostage_run_rung() completes an ordinary rung and populates every 'ok'-path field", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)

  rec <- .twostage_run_rung(
    X, y, m = 8L, leaf_budget = 4L, L_A = 7L, d_A = 4L, depth_restricted_A = TRUE,
    lambda_n = 0.05, m_n = 1L, min_leaf_n = 1L
  )

  expect_equal(rec$status, "ok")
  expect_equal(rec$m, 8L)
  expect_true(rec$n_leaves_A <= 7L)
  expect_true(rec$lambda_case %in% c("i", "ii", "iii", "infeasible"))
  expect_type(rec$feasible_A, "logical")
  expect_type(rec$stage_a_truncated, "logical")
  expect_false(rec$stage_a_truncated)
  expect_type(rec$lambda_binding, "logical")
  expect_true(rec$n_leaves_collapsed <= rec$n_leaves_A)
  expect_equal(rec$budget_slack, 4L - rec$n_leaves_collapsed)
  expect_true(rec$feasible_refined)
  expect_type(rec$local_certified, "logical")
  expect_true(is.finite(rec$local_margin))
  expect_type(rec$local_certified_at_lambda_n, "logical")
  expect_true(is.finite(rec$local_margin_at_lambda_n))
  expect_type(rec$depth_sufficient, "logical")
  expect_type(rec$topology_key, "character")
  expect_s7_class(rec$model, RefinedTreeModel)
})

test_that(".twostage_run_rung() sets max_leaves = NULL on the refined model, never L_A", {
  # Q5.1: capping max_leaves at L_A would let certify_local_optimality()'s
  # `add` perturbation silently ignore a genuinely improving split that WAS
  # in Stage A's own search space -- double-counting what lambda already
  # penalizes. Confirm the composition directly, not just by comment.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)

  rec <- .twostage_run_rung(
    X, y, m = 8L, leaf_budget = 4L, L_A = 7L, d_A = 4L, depth_restricted_A = TRUE,
    lambda_n = 0.05, m_n = 1L, min_leaf_n = 1L
  )
  expect_equal(rec$status, "ok")
  expect_equal(length(rec$model@max_leaves), 0L)   # NULL, per the S7 integer(0) gotcha
  expect_equal(rec$model@max_depth, 4L)            # d_A IS a real solver-enforced bound
})

test_that(".twostage_run_rung() runs the local certificate at both lambda and lambda_n when a real search occurred", {
  # Forces used_search = TRUE (lambda_n deliberately overshoots leaf_budget
  # = 3), so the returned fit's lambda != lambda_n -- Q5.2's case (i), where
  # the two certificate evaluations are genuinely different claims.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)

  rec <- .twostage_run_rung(
    X, y, m = 8L, leaf_budget = 3L, L_A = 3L, d_A = 4L, depth_restricted_A = TRUE,
    lambda_n = 1e-6, m_n = 1L, min_leaf_n = 1L
  )
  expect_equal(rec$status, "ok")
  expect_gt(rec$lambda, 1e-6)          # bisection raised it, confirming case (i)
  expect_equal(rec$lambda_case, "i")
  expect_type(rec$local_certified, "logical")
  expect_type(rec$local_certified_at_lambda_n, "logical")
  expect_true(is.finite(rec$local_margin))
  expect_true(is.finite(rec$local_margin_at_lambda_n))
})

test_that(".twostage_run_rung() reports collapse_unsupported without erroring (the ordinary case)", {
  # Same fixture/lambda as test-stage-b.R's dedicated "genuine cross-
  # coordinate structure" test, routed through bisect_lambda_to_budget() at
  # a leaf_budget large enough that lambda_n itself is accepted (no search),
  # reproducing the exact same fit.
  set.seed(20260901)
  n <- 500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)

  rec <- .twostage_run_rung(
    X, y, m = 16L, leaf_budget = 8L, L_A = 8L, d_A = 3L, depth_restricted_A = TRUE,
    lambda_n = 0.01, m_n = 1L, min_leaf_n = 1L
  )
  expect_equal(rec$status, "collapse_unsupported")
  expect_null(rec$model)
  # Stage A itself completed -- those fields are populated even on this refusal.
  expect_false(is.na(rec$n_leaves_A))
  expect_false(is.na(rec$n_fits))
  # Everything Stage-B-dependent stays untouched (NA), not silently zero.
  expect_true(is.na(rec$n_leaves_collapsed))
  expect_true(is.na(rec$local_certified))
})

test_that(".twostage_run_rung() reports stage_b_binary_split without erroring", {
  set.seed(20260901)
  n <- 300
  x_bin <- rbinom(n, 1, 0.5)
  x_cont <- runif(n)
  y <- ifelse(x_bin == 1, 10, 0) + rnorm(n, sd = 0.01)
  X <- data.frame(x_bin = x_bin, x_cont = x_cont)

  rec <- .twostage_run_rung(
    X, y, m = 4L, leaf_budget = 2L, L_A = 2L, d_A = 1L, depth_restricted_A = FALSE,
    lambda_n = 0.1, m_n = 1L, min_leaf_n = 1L
  )
  expect_equal(rec$status, "stage_b_binary_split")
  expect_null(rec$model)
})

test_that(".twostage_run_rung() reports stage_a_deadline without ever starting a fit", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)

  t0 <- Sys.time()
  rec <- .twostage_run_rung(
    X, y, m = 8L, leaf_budget = 4L, L_A = 7L, d_A = 4L, depth_restricted_A = TRUE,
    lambda_n = 0.05, m_n = 1L, min_leaf_n = 1L,
    deadline = Sys.time() - 1
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(rec$status, "stage_a_deadline")
  expect_null(rec$model)
  expect_true(is.na(rec$n_leaves_A))   # Stage A never even ran
  expect_lt(elapsed, 2)                # confirms no fit was attempted
})

# ---------------------------------------------------------------------------
# .refined_leaf_floor_ok() -- the group-aware refined-tree feasibility
# re-check (Oracle consult, Q5.3): Stage A's `feasible` describes the
# discretized Stage-A tree, not the object fit_twostage() returns, and
# refine_tree_cuts()'s min_leaf_n floor has NO group awareness at all.
# ---------------------------------------------------------------------------

test_that(".refined_leaf_floor_ok() passes when both the total and group floors hold", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.05,
                   discretize_bins = 8L, max_depth = 4L)
  refined <- refine_tree(fit, X, y, min_leaf_n = 1L, max_depth = 4L, max_leaves = NULL)

  expect_true(.refined_leaf_floor_ok(refined, X, group = NULL, group_value = 0,
                                       m_n = 1L, m_n_group = 1L))
  # A group vector that is roughly balanced across leaves should also pass
  # a modest group floor.
  group <- rbinom(n, 1, 0.5)
  expect_true(.refined_leaf_floor_ok(refined, X, group = group, group_value = 0,
                                       m_n = 1L, m_n_group = 1L))
})

test_that(".refined_leaf_floor_ok() catches a group-floor violation the total floor misses", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.05,
                   discretize_bins = 8L, max_depth = 4L)
  refined <- refine_tree(fit, X, y, min_leaf_n = 1L, max_depth = 4L, max_leaves = NULL)

  # Total floor: trivially satisfied at m_n = 1. Group floor: group = 1
  # everywhere means group == 0 (the required subgroup) has ZERO members in
  # every leaf -- the total floor cannot see this at all, since it never
  # looks at group membership.
  group_all_one <- rep(1L, n)
  expect_true(.refined_leaf_floor_ok(refined, X, group = NULL, group_value = 0,
                                       m_n = 1L, m_n_group = 1L))
  expect_false(.refined_leaf_floor_ok(refined, X, group = group_all_one, group_value = 0,
                                        m_n = 1L, m_n_group = 1L))
})

test_that(".refined_leaf_floor_ok() catches an ordinary total-floor violation", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.05,
                   discretize_bins = 8L, max_depth = 4L)
  refined <- refine_tree(fit, X, y, min_leaf_n = 1L, max_depth = 4L, max_leaves = NULL)

  expect_false(.refined_leaf_floor_ok(refined, X, group = NULL, group_value = 0,
                                        m_n = n, m_n_group = n))   # impossibly large floor
})

# ---------------------------------------------------------------------------
# fit_twostage() (Milestone E sub-step E3): the m-ladder loop, stop
# conditions, cross-rung compare_topology(), result assembly, and the
# certified/certified_full_class split. Every fixture DGP below timed and
# verified (<20s each, most well under 5s) in isolation before being
# written, per this session's memory-safety convention.
# ---------------------------------------------------------------------------

test_that("fit_twostage() succeeds end to end: 2 stable rungs, certified and certified_full_class both TRUE", {
  # leaf_budget = 2 is small enough that the default staged depth cap
  # happens to COINCIDE with full compliance (d_0 = ceiling(log2(2)) = 1,
  # d_A = min(2*1, depth_required_A = 2) = 2 = depth_required_A) --
  # confirming certified_full_class is reachable, not just certified.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)

  res <- fit_twostage(X, y, leaf_budget = 2, lambda_n = 0.05, m_ladder = c(8, 16),
                       time_budget = 60, fit_time_limit = 20, depth_budget = 2)

  expect_s3_class(res, "optimaltrees_twostage_fit")
  expect_true(res$certified)
  expect_true(res$certified_full_class)
  expect_true(res$depth_sufficient)
  expect_true(is.na(res$reason))
  expect_equal(res$stop_reason, "topology_stable")
  expect_equal(res$n_rungs_completed, 2L)
  expect_equal(res$m_used, 16L)              # the finer of the two stable rungs
  expect_s7_class(res$model, RefinedTreeModel)
  expect_equal(nrow(res$ladder_topologies), 2L)
  expect_true(isTRUE(res$ladder_topologies$topology_stable_vs_prev[[2]]))
  expect_equal(res$ladder_topologies$compared_to_m[[2]], 8L)
})

test_that("fit_twostage() reports certified TRUE but certified_full_class FALSE under the default depth cap", {
  # The core distinction the whole Milestone-E design resolved this session
  # (decision #2's actual mechanics): the DEFAULT depth_budget = NULL cap
  # restricts below full compliance at leaf_budget = 4 (unlike the
  # leaf_budget = 2 test above), so depth_sufficient/certified_full_class
  # must be FALSE even though every other check passes.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)

  res <- expect_message(
    fit_twostage(X, y, leaf_budget = 4, lambda_n = 0.05, m_ladder = c(8, 16),
                 time_budget = 60, fit_time_limit = 20),
    "STAGED cap"
  )
  expect_true(res$certified)
  expect_false(res$certified_full_class)
  expect_false(res$depth_sufficient)
  expect_true(is.na(res$reason))
  expect_equal(res$d_0_source, "default_balanced")
})

test_that("fit_twostage() with verbose = FALSE suppresses the depth-cap message", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)

  expect_no_message(
    fit_twostage(X, y, leaf_budget = 4, lambda_n = 0.05, m_ladder = c(8, 16),
                 time_budget = 60, fit_time_limit = 20, verbose = FALSE)
  )
})

test_that("fit_twostage() stops at budget_exhausted without attempting a single fit", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)

  t0 <- Sys.time()
  res <- suppressMessages(fit_twostage(
    X, y, leaf_budget = 2, lambda_n = 0.05, m_ladder = c(8, 16),
    time_budget = 5, fit_time_limit = 10, depth_budget = 2
  ))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_equal(res$stop_reason, "budget_exhausted")
  expect_equal(res$n_rungs_completed, 0L)
  expect_false(res$certified)
  expect_equal(res$reason, "any_rung_completed")
  expect_null(res$model)
  expect_true(is.na(res$m_used))
  expect_lt(elapsed, 2)   # confirms no fit was ever attempted
})

test_that("fit_twostage() escalates a stage_b_binary_split refusal exactly once, then stops", {
  set.seed(1)
  n <- 300
  x_bin <- rbinom(n, 1, 0.5)
  x_cont <- runif(n)
  y <- ifelse(x_bin == 1, 10, 0) + rnorm(n, sd = 0.01)
  X <- data.frame(x_bin = x_bin, x_cont = x_cont)

  res <- suppressMessages(fit_twostage(
    X, y, leaf_budget = 2, lambda_n = 0.1, m_ladder = c(4, 8, 16),
    time_budget = 30, fit_time_limit = 10, depth_budget = 1
  ))

  expect_equal(res$stop_reason, "stage_b_binary_split")
  expect_equal(res$n_rungs_completed, 0L)
  # Escalated exactly once (m=4 -> m=8) and then stopped -- m=16 was never
  # attempted, confirming this is NOT retried indefinitely across the
  # whole ladder the way collapse_unsupported is.
  expect_equal(res$ladder_topologies$m, c(4L, 8L))
  expect_true(all(res$ladder_topologies$status == "stage_b_binary_split"))
})

test_that("fit_twostage() rejects loss_function other than squared_error", {
  n <- 50
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  expect_error(
    fit_twostage(X, y, leaf_budget = 2, loss_function = "misclassification"),
    "squared_error"
  )
})

test_that("fit_twostage() rejects worker_limit != 1", {
  n <- 50
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  expect_error(fit_twostage(X, y, leaf_budget = 2, worker_limit = 2), "worker_limit")
})

test_that("fit_twostage() rejects an all-binary X before spending any Stage-A time", {
  n <- 50
  Xb <- data.frame(a = rbinom(n, 1, 0.5), b = rbinom(n, 1, 0.5))
  y <- rnorm(n)
  expect_error(fit_twostage(Xb, y, leaf_budget = 2), "continuous covariate")
})

test_that("fit_twostage() rejects non-numeric X columns", {
  n <- 50
  Xf <- data.frame(x1 = factor(sample(letters[1:3], n, replace = TRUE)))
  y <- rnorm(n)
  expect_error(fit_twostage(Xf, y, leaf_budget = 2), "numeric")
})

test_that("fit_twostage() rejects mismatched nrow(X)/length(y)", {
  n <- 50
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n - 1)
  expect_error(fit_twostage(X, y, leaf_budget = 2), "nrow")
})

# ---------------------------------------------------------------------------
# print/summary/predict S3 methods (Milestone E sub-step E4).
#
# IMPORTANT, discovered during this sub-step and fixed in R/zzz.R: NAMESPACE's
# declarative S3method() registration for these three methods does NOT take
# effect under `pkgload::load_all()` (the mechanism `devtools::test()` uses)
# -- confirmed empirically that generic dispatch (`print(res)`) silently
# falls through to `print.default` in that environment, even though a REAL
# `R CMD INSTALL` + `library()` load dispatches correctly (verified directly:
# `methods("print")` lists the method after install, and `print(res)`
# produces the expected formatted output). `.onLoad()` now explicitly
# re-registers these three methods via `registerS3method()`, which fixes the
# INSTALLED-package case but does not change `load_all()`'s own behavior
# (a pkgload limitation, not a bug in this package). These tests therefore
# call the S3 methods DIRECTLY BY NAME (exercising the exact same function
# bodies real dispatch would call) rather than through generic `print()`/
# `summary()`/`predict()`, so they pass reliably under `devtools::test()`.
# ---------------------------------------------------------------------------

test_that("print.optimaltrees_twostage_fit() reports the full disclosure on a successful fit", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(X, y, leaf_budget = 4, lambda_n = 0.05,
                                        m_ladder = c(8, 16),
                                        time_budget = 60, fit_time_limit = 20))

  out <- capture.output(print.optimaltrees_twostage_fit(res))
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "certified\\s+TRUE")
  expect_match(txt, "certified_full_class\\s+FALSE")
  expect_match(txt, "depth_sufficient = FALSE BY DESIGN, not a fit failure")
  expect_match(txt, "STABLE")
  expect_match(txt, "inside the validated staged range")
  expect_match(txt, "LOCAL ONLY")
})

test_that("print.optimaltrees_twostage_fit() correctly frames a leaf_budget SMALLER than {4,8}", {
  # Regression test for a real messaging bug caught while manually verifying
  # this sub-step: the original disclosure text assumed "outside the range"
  # always meant LARGER (and therefore more expensive) than 8, and
  # incorrectly told the user to "expect materially worse" cost for
  # leaf_budget = 2 -- backwards, since a smaller budget should be at least
  # as tractable, not worse.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(X, y, leaf_budget = 2, lambda_n = 0.05,
                                        m_ladder = c(8, 16),
                                        time_budget = 60, fit_time_limit = 20,
                                        depth_budget = 2))

  out <- capture.output(print.optimaltrees_twostage_fit(res))
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "smaller than the validated staged range")
  expect_match(txt, "at least as tractable")
  expect_no_match(txt, "expect materially worse")
})

test_that("print.optimaltrees_twostage_fit() correctly frames a leaf_budget LARGER than {4,8}", {
  # Constructs a SYNTHETIC result (real model, hand-edited leaf_budget) to
  # exercise the >8 disclosure branch without an actual leaf_budget = 20
  # Stage-A search (never benchmarked in this repo -- see Milestone D --
  # and potentially very slow; the print method's formatting logic is a
  # pure function of the list's fields, so this tests it directly without
  # needing the real, expensive computation).
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(X, y, leaf_budget = 4, lambda_n = 0.05,
                                        m_ladder = c(8, 16),
                                        time_budget = 60, fit_time_limit = 20))
  res$leaf_budget <- 20L

  out <- capture.output(print.optimaltrees_twostage_fit(res))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "OUTSIDE the validated staged range")
  expect_match(txt, "expect materially worse")
  expect_no_match(txt, "at least as tractable")
})

test_that("print.optimaltrees_twostage_fit() aborts branch is unreached when no rung completed", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(
    X, y, leaf_budget = 20, lambda_n = 0.05, m_ladder = c(8, 16),
    time_budget = 5, fit_time_limit = 10   # exhausted before any fit -- fast
  ))

  out <- capture.output(print.optimaltrees_twostage_fit(res))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "No rung completed")
  expect_match(txt, "any_rung_completed")
})

test_that("summary.optimaltrees_twostage_fit() prints the ladder trace in addition to print()'s output", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(X, y, leaf_budget = 2, lambda_n = 0.05,
                                        m_ladder = c(8, 16),
                                        time_budget = 60, fit_time_limit = 20,
                                        depth_budget = 2))

  out <- capture.output(summary.optimaltrees_twostage_fit(res))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "certified")             # print()'s own content
  expect_match(txt, "Ladder trace:")
  expect_match(txt, "topology_stable_vs_prev")   # a ladder_topologies column name
})

test_that("predict.optimaltrees_twostage_fit() delegates to the winning RefinedTreeModel", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(X, y, leaf_budget = 4, lambda_n = 0.05,
                                        m_ladder = c(8, 16),
                                        time_budget = 60, fit_time_limit = 20))

  preds_via_fit   <- predict.optimaltrees_twostage_fit(res, X)
  preds_via_model <- predict(res$model, X)
  expect_equal(preds_via_fit, preds_via_model)
})

test_that("predict.optimaltrees_twostage_fit() aborts cleanly when no rung completed", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05)
  res <- suppressMessages(fit_twostage(
    X, y, leaf_budget = 2, lambda_n = 0.05, m_ladder = c(8, 16),
    time_budget = 5, fit_time_limit = 10, depth_budget = 2
  ))

  expect_null(res$model)
  expect_error(predict.optimaltrees_twostage_fit(res, X), "no rung completed")
})
