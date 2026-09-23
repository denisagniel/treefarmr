# Regression tests for fitted regression leaf-value precision, via the actual
# C++ Model/encoder path (fit_tree() -> Rcpp -> Model constructor -> to_json ->
# predict()) -- NOT refit_structure_on_data(), a separate pure-R code path
# already covered by test-refit-structure-leaf-values.R.
#
# Bug (found 2026-09-09, fixed same day): a fitted leaf's control-mean value was
# silently rounded to ~6 significant figures. Two independent, stacked causes:
#   (1) C++: model.cpp cast the leaf mean to `float` then serialized it via
#       std::to_string(float), which formats with a fixed 6 decimal places (not
#       significant figures) -- catastrophic for small-magnitude leaves.
#   (2) R: treefarms.R/auto_tune.R built the fit's CSV payload via
#       apply(data_df, 1L, function(r) paste(as.character(r), collapse=",")),
#       which forces the whole row (including the unrelated y column) through
#       as.matrix.data.frame's homogeneous-type coercion -- silently downgrading
#       to 7-sig-fig character formatting the moment any column (e.g. a
#       factor-encoded feature) is non-numeric.
# Both are fixed: (1) model.cpp keeps a `double` prediction member serialized at
# full round-trip precision; (2) the CSV payload is now built column-wise, with
# sprintf("%.17g", .) for numeric columns. regression_targets_/target_values
# were also widened from float to double throughout the C++ encoder/dataset
# (Oracle-reviewed 2026-09-09; see quality_reports/plans/2026-09-09_optimaltrees-
# leaf-value-precision-complete-fix.md), so nothing between R's double `y` and
# the fitted leaf value narrows precision anymore. Tolerance below is ~1e-10,
# not exactly 0, only to allow for legitimate floating-point summation order
# effects in the C++ accumulation -- not for any known remaining precision
# floor.

test_that("fitted leaf value matches mean(y) exactly for a small-magnitude leaf", {
  # Small-magnitude values are exactly the case the old std::to_string(float)
  # bug destroyed (leading zeros ate into the fixed 6 decimal places).
  X <- data.frame(f1 = c(0, 0, 0, 0, 1, 1, 1, 1), f2 = c(0, 1, 0, 1, 0, 1, 0, 1))
  y0 <- c(-0.0591234567, -0.0587654321, -0.0592345678, -0.0588765432)
  y1 <- c(1.234567891, 1.234567892, 1.234567893, 1.234567894)
  y <- c(y0, y1)

  m <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01, max_depth = 1L)
  pred <- predict(m, X)

  expect_equal(pred[X$f1 == 0], rep(mean(y0), 4), tolerance = 1e-10)
  expect_equal(pred[X$f1 == 1], rep(mean(y1), 4), tolerance = 1e-10)
})

test_that("fitted leaf value matches mean(y) exactly when X has a non-numeric column", {
  # Regression guard for the R-side CSV floor (cause 2 above): a factor-encoded
  # feature used to silently truncate the unrelated y column to 7 sig figs via
  # apply(data_df, 1L, ...)'s as.matrix.data.frame coercion.
  X <- data.frame(f1 = factor(c(0, 0, 0, 0, 1, 1, 1, 1)), f2 = c(0, 1, 0, 1, 0, 1, 0, 1))
  y0 <- c(-0.0512345671, -0.0517654329, -0.0518345672, -0.0511765431)
  y1 <- c(3.234567891, 3.234567892, 3.234567893, 3.234567894)
  y <- c(y0, y1)

  m <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01, max_depth = 1L)
  pred <- predict(m, X)

  expect_equal(pred[X$f1 == 0], rep(mean(y0), 4), tolerance = 1e-10)
  expect_equal(pred[X$f1 == 1], rep(mean(y1), 4), tolerance = 1e-10)
})

test_that("fitted leaf value matches mean(y) across multiple leaves at depth 2", {
  X <- data.frame(
    f1 = c(0, 0, 0, 0, 1, 1, 1, 1),
    f2 = c(0, 0, 1, 1, 0, 0, 1, 1)
  )
  y <- c(
    -0.0134567891, -0.0135678912,   # f1=0, f2=0
     0.0987654321,  0.0986543219,   # f1=0, f2=1
     5.123456789,   5.123456780,    # f1=1, f2=0
    -2.345678912,  -2.345678901     # f1=1, f2=1
  )

  m <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.001, max_depth = 2L)
  pred <- predict(m, X)

  for (grp in list(c(1, 2), c(3, 4), c(5, 6), c(7, 8))) {
    expect_equal(pred[grp], rep(mean(y[grp]), 2), tolerance = 1e-10)
  }
})

test_that("a logical feature column does not corrupt the CSV payload", {
  # Regression guard for a real bug the 2026-09-10 r-review caught in this same
  # fix: is.numeric(TRUE) is FALSE, so the column-wise CSV builder's original
  # dispatch (is.numeric(col) else as.character(col)) sent logical columns down
  # the character branch, emitting "TRUE"/"FALSE" instead of "1"/"0" -- unlike
  # the old apply()/as.matrix() path, which coerced a logical+numeric frame to
  # a double matrix first. Fixed by routing is.logical() through the numeric
  # branch before the fallback is.numeric() check.
  X <- data.frame(f1 = c(TRUE, TRUE, FALSE, FALSE), f2 = c(0, 1, 0, 1))
  y <- c(-0.0512345671, -0.0517654329, 3.234567891, 3.234567892)

  m <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01, max_depth = 1L)
  pred <- predict(m, X)

  expect_equal(pred[X$f1], rep(mean(y[X$f1]), 2), tolerance = 1e-10)
  expect_equal(pred[!X$f1], rep(mean(y[!X$f1]), 2), tolerance = 1e-10)
})
