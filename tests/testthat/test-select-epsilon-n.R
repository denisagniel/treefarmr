# Tests for select_epsilon_n: the theory-justified fixed Rashomon tolerance
# epsilon_n = c * log(n)/n = o(n^{-1/2}) (doubletree margin-resolution corollary).

test_that("select_epsilon_n returns c * log(n)/n", {
  expect_equal(select_epsilon_n(1000), log(1000) / 1000)
  expect_equal(select_epsilon_n(500, c = 2), 2 * log(500) / 500)
  expect_equal(select_epsilon_n(1000, c = 1), select_epsilon_n(1000))
})

test_that("select_epsilon_n is o(n^{-1/2}): ratio to n^{-1/2} -> 0", {
  # epsilon_n / n^{-1/2} = log(n)/sqrt(n), which must decrease toward 0.
  ns <- c(100, 1000, 10000, 100000)
  ratio <- vapply(ns, function(n) select_epsilon_n(n) / n^(-1 / 2), numeric(1))
  # strictly decreasing
  expect_true(all(diff(ratio) < 0))
  # and heading to 0
  expect_lt(ratio[length(ratio)], ratio[1])
  expect_lt(select_epsilon_n(1e6) / 1e6^(-1 / 2), 0.02)
})

test_that("select_epsilon_n scales linearly in c", {
  expect_equal(select_epsilon_n(1000, c = 3), 3 * select_epsilon_n(1000, c = 1))
})

test_that("select_epsilon_n validates inputs", {
  expect_error(select_epsilon_n(1), "n.*>= 2")
  expect_error(select_epsilon_n(c(100, 200)), "single number")
  expect_error(select_epsilon_n("100"), "single number")
  expect_error(select_epsilon_n(1000, c = 0), "positive")
  expect_error(select_epsilon_n(1000, c = -1), "positive")
})
