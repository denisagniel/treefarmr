# Persistence: models are plain (S7) R objects, so base saveRDS/readRDS is the
# supported round-trip. The former bespoke save_optimaltrees/load_optimaltrees
# machinery was removed 2026-07-08 (broken for the S7 return types, zero callers,
# redundant with saveRDS). This test guards that the native path works.

test_that("OptimalTreesModel round-trips via saveRDS/readRDS", {
  set.seed(1)
  X <- data.frame(x1 = rbinom(80, 1, 0.5), x2 = rbinom(80, 1, 0.5))
  y <- rbinom(80, 1, 0.5)
  m <- optimaltrees(X, y, loss_function = "log_loss", regularization = 0.1)

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(m, tf)
  m2 <- readRDS(tf)

  expect_true(S7::S7_inherits(m2, OptimalTreesModel))
  expect_equal(m2@n_trees, m@n_trees)
  expect_equal(predict(m2, X, type = "prob"), predict(m, X, type = "prob"))
})

test_that("CFRashomon round-trips via saveRDS/readRDS", {
  set.seed(2)
  X <- data.frame(x1 = rbinom(80, 1, 0.5), x2 = rbinom(80, 1, 0.5))
  y <- rbinom(80, 1, 0.5)
  cfr <- cross_fitted_rashomon(X, y, K = 2, loss_function = "log_loss",
                               regularization = 0.1)

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(cfr, tf)
  cfr2 <- readRDS(tf)

  expect_true(S7::S7_inherits(cfr2, CFRashomon))
  expect_equal(cfr2@n_intersecting, cfr@n_intersecting)
})
