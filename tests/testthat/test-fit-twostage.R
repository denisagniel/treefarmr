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
