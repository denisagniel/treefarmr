# Tests for R/fit_tree.R's `...` splice handling around the arguments this
# function owns unconditionally (model_limit, single_tree) -- regression
# coverage for the "formal argument matched by multiple actual arguments"
# defect class fixed 2026-09-29 (r-reviewer review,
# quality_reports/reviews/2026-09-29_stage-b-fit-tree-fixes.md, Issues 2-3).

test_that("fit_tree() accepts an explicit model_limit= without an argument-matching error", {
  # Regression: `dots` was READ for model_limit (to respect a user override
  # or fall back to the dimensionality heuristic) but never stripped from the
  # `...` splice into optimaltrees(), so the documented override path --
  # "Override by passing model_limit=0 (unlimited) or model_limit=<value>
  # explicitly" -- crashed with "formal argument 'model_limit' matched by
  # multiple actual arguments" on every call that used it.
  set.seed(1)
  X <- data.frame(a = rbinom(80, 1, .5), b = rbinom(80, 1, .5))
  y <- as.numeric(X$a == X$b)
  expect_no_error(fit_tree(X, y, regularization = 0.1, model_limit = 0))
  expect_no_error(fit_tree(X, y, regularization = 0.1, model_limit = 1000))
})

test_that("fit_tree() rejects single_tree= with an actionable message, not a raw argument-matching error", {
  # Same defect class as model_limit above: single_tree is a formal of
  # optimaltrees() but not of fit_tree(), so an explicit single_tree= from a
  # caller lands in `...` and collides with fit_tree()'s own hardcoded
  # single_tree = TRUE. Rather than merely stripping it (a caller asking for
  # single_tree = FALSE from a function whose entire contract is exactly one
  # tree has a clear, wrong intent that deserves pointing at fit_rashomon(),
  # not a silently-overridden request).
  set.seed(1)
  X <- data.frame(a = rbinom(80, 1, .5))
  y <- as.numeric(X$a)
  expect_error(fit_tree(X, y, single_tree = FALSE), "fit_rashomon")
})
