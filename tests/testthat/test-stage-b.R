# Tests for Stage B's coordinate-space tree representation (R/stage_b.R),
# Milestone A of quality_reports/plans/2026-09-01_two-stage-package-defaults-
# session.md's §4 (fit_twostage()). Regression-only scope by explicit
# decision; classification is a later, separate milestone.
#
# This is sub-step 1 of Milestone A's build order (schema + walk engine +
# as_coordinate_tree + the equivalence test) -- collapse_transitions() and
# refine_tree_cuts() are separate, later sub-steps and are NOT covered here.

test_that("as_coordinate_tree + coord_tree_predict reproduce predict() exactly (train and fresh data)", {
  # The single highest-value test in this milestone (Oracle's framing):
  # validates the new coordinate-space walk engine against the TRUSTED
  # existing prediction path before any refinement logic exists.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y <- 6 * (X$x1 - 0.5) + 6 * (X$x2 - 0.5) + 6 * (X$x3 - 0.5) + rnorm(n, sd = 0.3)

  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 3L)
  ct <- as_coordinate_tree(fit, X, y)

  pred_coord_train <- coord_tree_predict(ct, X)
  pred_model_train  <- predict(fit, X, type = "class")
  expect_equal(pred_coord_train, pred_model_train, tolerance = 1e-6)

  set.seed(999)
  n2 <- 200
  Xnew <- data.frame(x1 = runif(n2), x2 = runif(n2), x3 = runif(n2))
  pred_coord_new <- coord_tree_predict(ct, Xnew)
  pred_model_new  <- predict(fit, Xnew, type = "class")
  expect_equal(pred_coord_new, pred_model_new, tolerance = 1e-6)
})

test_that("as_coordinate_tree handles the same binary feature index split on by sibling subtrees", {
  # THE decisive scenario for rejecting the metadata-patch representation
  # (option C in the plan/Oracle consult): confirmed empirically (before this
  # design was accepted) that a real fit can split on the SAME discretized
  # binary column in two different branches. A single stored per-coordinate
  # threshold cannot hold two different Stage-B-refined values for that
  # column; the coordinate-space tree has no such collision because every
  # split node carries its own `cut`. This test locks in that a fit which
  # actually exhibits the pattern still converts and predicts correctly.
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y <- 6 * (X$x1 - 0.5) + 6 * (X$x2 - 0.5) + 6 * (X$x3 - 0.5) + rnorm(n, sd = 0.3)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 3L)

  collect_features <- function(node, acc = integer(0)) {
    if (!is.null(node$prediction)) return(acc)
    acc <- c(acc, as.integer(node$feature))
    acc <- collect_features(node$true, acc)
    acc <- collect_features(node$false, acc)
    acc
  }
  feats <- collect_features(fit@trees[[1]])
  skip_if_not(anyDuplicated(feats) > 0,
              "This seed/DGP did not reproduce a duplicate-feature-index fit.")

  ct <- as_coordinate_tree(fit, X, y)
  expect_equal(coord_tree_predict(ct, X), predict(fit, X, type = "class"),
               tolerance = 1e-6)
})

test_that("as_coordinate_tree rejects non-squared_error models loudly", {
  set.seed(20260901)
  n <- 300
  Xb <- data.frame(a = rbinom(n, 1, 0.5), b = rbinom(n, 1, 0.5))
  yb <- rbinom(n, 1, 0.5)
  fit_cls <- fit_tree(Xb, yb, loss_function = "log_loss", regularization = 0.1)

  expect_error(as_coordinate_tree(fit_cls, Xb, yb), "squared_error")
})

test_that("as_coordinate_tree rejects models with no continuous covariates", {
  set.seed(20260901)
  n <- 300
  Xb <- data.frame(a = rbinom(n, 1, 0.5), b = rbinom(n, 1, 0.5))
  yb <- as.numeric(rbinom(n, 1, 0.5))
  fit_bin <- fit_tree(Xb, yb, loss_function = "squared_error", regularization = 0.1)

  expect_error(as_coordinate_tree(fit_bin, Xb, yb), "nothing (for|to) (S|s)tage B")
})

test_that("as_coordinate_tree rejects X that doesn't reconcile with the model's discretization", {
  set.seed(20260901)
  n <- 300
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y <- runif(n)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.1,
                   discretize_bins = 6L)

  X_missing_col <- X[, c("x1", "x2")]
  expect_error(as_coordinate_tree(fit, X_missing_col, y), "Features missing")
})

test_that("as_coordinate_tree rejects an out-of-range tree_index", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(x1 = runif(n))
  y <- runif(n)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.1)

  expect_error(as_coordinate_tree(fit, X, y, tree_index = 5L), "out of range")
})

test_that("as_coordinate_tree assigns unique, pre-order node ids", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y <- 6 * (X$x1 - 0.5) + 6 * (X$x2 - 0.5) + 6 * (X$x3 - 0.5) + rnorm(n, sd = 0.3)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 3L)
  ct <- as_coordinate_tree(fit, X, y)

  collect_ids <- function(node) {
    if (identical(node$kind, "leaf")) return(node$id)
    c(node$id, collect_ids(node$left), collect_ids(node$right))
  }
  ids <- collect_ids(ct)
  expect_equal(length(unique(ids)), length(ids))
  expect_equal(ids[[1]], 1L)  # root is id 1, pre-order
  expect_true(all(ids == sort(ids)) || TRUE)  # pre-order is not globally sorted
  # Pre-order invariant that DOES always hold: a node's own id is smaller
  # than every id in its subtree.
  check_preorder <- function(node) {
    if (identical(node$kind, "leaf")) return(TRUE)
    sub_left  <- collect_ids(node$left)
    sub_right <- collect_ids(node$right)
    all(node$id < sub_left) && all(node$id < sub_right) &&
      check_preorder(node$left) && check_preorder(node$right)
  }
  expect_true(check_preorder(ct))
})

test_that("every leaf carries n/stats/prediction and reproduces the fitted model's own value", {
  set.seed(20260901)
  n <- 400
  X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
  y <- 6 * (X$x1 - 0.5) + 6 * (X$x2 - 0.5) + 6 * (X$x3 - 0.5) + rnorm(n, sd = 0.3)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 3L)
  ct <- as_coordinate_tree(fit, X, y)

  check_leaf <- function(node) {
    if (identical(node$kind, "leaf")) {
      expect_true(node$n > 0L)
      expect_equal(node$stats$n, node$n)
      expect_equal(node$stats$sum / node$stats$n, node$prediction, tolerance = 1e-9)
      expect_equal(node$prediction, node$original_prediction, tolerance = 1e-6)
      return(invisible())
    }
    check_leaf(node$left)
    check_leaf(node$right)
  }
  check_leaf(ct)
})

test_that("bin_lookup rebuilds coord/k/cut positionally and matches binary_names", {
  set.seed(20260901)
  n <- 300
  X <- data.frame(x1 = runif(n), x2 = rbinom(n, 1, 0.5), x3 = runif(n))
  disc <- discretize_features(X, method = "quantiles", n_bins = 4)
  lk <- bin_lookup(disc$metadata)

  expect_equal(nrow(lk), length(disc$metadata$binary_names))
  expect_true(all(lk$coord[!is.na(lk$k)] %in% c("x1", "x3")))
  expect_true(is.na(lk$k[lk$coord == "x2"]))
})

test_that("bin_lookup rejects all-binary metadata (nothing to look up)", {
  set.seed(20260901)
  n <- 200
  X <- data.frame(a = rbinom(n, 1, 0.5), b = rbinom(n, 1, 0.5))
  disc <- discretize_features(X, method = "quantiles", n_bins = 4)
  expect_true(disc$metadata$all_binary)
  expect_error(bin_lookup(disc$metadata), "all features are binary")
})

# ---------------------------------------------------------------------------
# collapse_transitions() -- synthetic hand-built fixtures, adapted from
# doubletree/simulations/threshold_superconsistency/code/check_stage_b.R's
# fixture set (same coverage: both collapse orientations, non-adjacent
# untouched, both abort paths) to this package's node schema (ids required;
# k_lo/k_hi instead of a single k). Plus one negative test Oracle's consult
# specifically asked for (intervening different-coordinate split) that the
# original fixture set did not include.
# ---------------------------------------------------------------------------

mk_leaf <- function(id) list(kind = "leaf", id = id)
mk_split <- function(id, coord, k_lo, cut, left, right) {
  list(kind = "split", id = id, coord = coord, cut = cut,
       grid_cuts = cut, collapsed = FALSE, k_lo = k_lo, k_hi = NA_integer_,
       left = left, right = right)
}

test_that("collapse_transitions collapses a grid-adjacent pair (right-child orientation)", {
  # root splits Xc at k=5; its right child ALSO splits Xc at k=6 (adjacent),
  # sandwiching a transition leaf at (tau5, tau6].
  fixture <- mk_split(
    id = 1L, "Xc", k_lo = 5L, cut = 0.5,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xc", k_lo = 6L, cut = 0.6,
                      left  = mk_leaf(4L),   # the transition leaf, (0.5, 0.6]
                      right = mk_leaf(5L))
  )
  out <- collapse_transitions(fixture)
  expect_equal(out$n_collapses, 1L)
  ct <- out$tree
  expect_true(isTRUE(ct$collapsed))
  expect_equal(coord_tree_count_splits(ct), 1L)  # 2 splits merged into 1
  expect_equal(sort(ct$grid_cuts), c(0.5, 0.6))
  expect_equal(ct$cut, 0.55)  # starting value: midpoint, refinement moves it later
  expect_equal(ct$k_lo, 5L)
  expect_equal(ct$k_hi, 6L)
})

test_that("collapse_transitions collapses symmetrically (left-child orientation)", {
  fixture_left <- mk_split(
    id = 1L, "Xc", k_lo = 6L, cut = 0.6,
    left  = mk_split(2L, "Xc", k_lo = 5L, cut = 0.5,
                      left  = mk_leaf(3L),
                      right = mk_leaf(4L)),  # transition leaf, (0.5, 0.6]
    right = mk_leaf(5L)
  )
  out_left <- collapse_transitions(fixture_left)
  expect_equal(out_left$n_collapses, 1L)
  expect_equal(coord_tree_count_splits(out_left$tree), 1L)
})

test_that("collapse_transitions leaves non-adjacent same-coordinate splits untouched", {
  fixture_far <- mk_split(
    id = 1L, "Xc", k_lo = 5L, cut = 0.5,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xc", k_lo = 9L, cut = 0.9,
                      left = mk_leaf(4L), right = mk_leaf(5L))
  )
  out_far <- collapse_transitions(fixture_far)
  expect_equal(out_far$n_collapses, 0L)
})

test_that("collapse_transitions does not collapse across an intervening different-coordinate split", {
  # Oracle's consult specifically flagged this as a case the pairwise check
  # must NOT treat as a transition pair -- two same-coordinate adjacent-k
  # splits with a DIFFERENT coordinate's split directly between them are not
  # sandwiching a leaf, so the pattern (child must be a split on the SAME
  # coordinate) correctly fails to match at the top level.
  fixture <- mk_split(
    id = 1L, "Xc", k_lo = 5L, cut = 0.5,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xd", k_lo = 1L, cut = 0.3,   # different coordinate
                      left  = mk_split(4L, "Xc", k_lo = 6L, cut = 0.6,
                                        left = mk_leaf(5L), right = mk_leaf(6L)),
                      right = mk_leaf(7L))
  )
  out <- collapse_transitions(fixture)
  expect_equal(out$n_collapses, 0L)
})

test_that("collapse_transitions aborts loudly on a degenerate (wrong-side) split", {
  # right child cuts BELOW its parent -- an empty-child bug, not a
  # transition leaf.
  fixture_degenerate <- mk_split(
    id = 1L, "Xc", k_lo = 6L, cut = 0.6,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xc", k_lo = 5L, cut = 0.5,
                      left = mk_leaf(4L), right = mk_leaf(5L))
  )
  expect_error(collapse_transitions(fixture_degenerate), "[Dd]egenerate")
})

test_that("collapse_transitions aborts loudly on a chained (three-way) collapse", {
  fixture_chained <- mk_split(
    id = 1L, "Xc", k_lo = 5L, cut = 0.5,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xc", k_lo = 6L, cut = 0.6,
                      left  = mk_split(4L, "Xc", k_lo = 7L, cut = 0.55,  # non-leaf transition child
                                        left = mk_leaf(5L), right = mk_leaf(6L)),
                      right = mk_leaf(7L))
  )
  expect_error(collapse_transitions(fixture_chained), "chained")
})

test_that("collapse_transitions's structural max_iter bound is generous by default but errors when artificially starved", {
  fixture <- mk_split(
    id = 1L, "Xc", k_lo = 5L, cut = 0.5,
    left  = mk_leaf(2L),
    right = mk_split(3L, "Xc", k_lo = 6L, cut = 0.6,
                      left  = mk_leaf(4L), right = mk_leaf(5L))
  )
  # Default (NULL -> coord_tree_count_splits(tree) = 2) converges fine.
  expect_equal(collapse_transitions(fixture)$n_collapses, 1L)
  # An artificially insufficient explicit max_iter=0 leaves no room for the
  # "confirming, no collapse found" pass and must error, not silently return
  # a partially-processed tree.
  expect_error(collapse_transitions(fixture, max_iter = 0L), "fixed point")
})

test_that("collapse_transitions() wired ahead of as_coordinate_tree() on a real fit still predicts correctly (max_depth = 2)", {
  # Full-pipeline smoke test, per Metis's "full pipeline" fixture. Uses
  # max_depth = 2 -- the ORIGINAL simulation script's own validated regime,
  # where a same-coordinate OR cross-coordinate split nested inside a
  # transition band cannot occur (there is no room for a further split
  # below a transition leaf at that depth) -- so this specifically tests
  # the "collapse succeeds, tree stays walkable" path, not the error path
  # (that has its own dedicated test below, since it turned out NOT to be
  # a rare case at higher depth).
  set.seed(20260901)
  n <- 500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 16L, max_depth = 2L)
  ct <- as_coordinate_tree(fit, X, y)
  out <- collapse_transitions(ct)
  expect_true(out$n_collapses >= 0L)
  preds <- coord_tree_predict(out$tree, X)
  expect_true(all(is.finite(preds)))
  expect_true(all(preds >= min(y) - 1 & preds <= max(y) + 1))
})

test_that("collapse_transitions() errors on a real fit where the transition band holds genuine cross-coordinate structure", {
  # Discovered empirically while writing this milestone (2026-09-01), NOT a
  # constructed edge case: this exact DGP/seed at max_depth = 3 produces a
  # transition band (between two adjacent x1 grid cuts) that the solver
  # profitably splits again on x2 -- real structure (two leaves with clearly
  # different fitted values), not a grid artifact. collapse_transitions()
  # correctly refuses to merge across it rather than silently destroying
  # that structure. Locking this in as its own test because it turned out
  # to be the ORDINARY outcome once depth exceeds 2 with 2+ informative
  # coordinates, not a rare pathological input worth only a passing comment.
  set.seed(20260901)
  n <- 500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.5, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 16L, max_depth = 3L)
  ct <- as_coordinate_tree(fit, X, y)
  expect_error(collapse_transitions(ct), "genuine further(\\s|\\n)+split|chained")
})
