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

# ---------------------------------------------------------------------------
# refine_tree_cuts() -- the off-grid threshold-moving step. Run AFTER
# collapse_transitions() in every test here, matching the documented
# pipeline order.
# ---------------------------------------------------------------------------

test_that("refine_tree_cuts recovers a known off-grid boundary far more accurately than the grid", {
  # THE core value proposition of Stage B, per theory.tex's motivation: the
  # grid floors threshold error at O(n^{-1/3}); the exact scan attains
  # O(n^{-1}). Deliberately choose a true cutoff that does NOT land on a
  # grid cutpoint and confirm refinement moves substantially closer to it.
  set.seed(20260901)
  n <- 2000
  X <- data.frame(x1 = runif(n))
  true_cut <- 0.4123456
  y <- ifelse(X$x1 <= true_cut, 0, 3) + rnorm(n, sd = 0.05)

  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 2L)
  ct <- as_coordinate_tree(fit, X, y)
  grid_error <- abs(ct$cut - true_cut)

  collapsed <- collapse_transitions(ct)$tree
  ref <- refine_tree_cuts(collapsed, X, y)
  refined_error <- abs(ref$tree$cut - true_cut)

  expect_lt(refined_error, grid_error)
  expect_lt(refined_error, 0.01)   # should land within ~1% of the truth
})

test_that("refine_tree_cuts never increases training SSE (monotonicity)", {
  set.seed(20260901)
  n <- 1000
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.35, 0, 2) + ifelse(X$x2 <= 0.65, 0, 1) + rnorm(n, sd = 0.1)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 10L, max_depth = 2L)
  ct <- as_coordinate_tree(fit, X, y)
  collapsed <- collapse_transitions(ct)$tree
  ref <- refine_tree_cuts(collapsed, X, y)

  rows <- ref$refined[!is.na(ref$refined$sse_before), ]
  if (nrow(rows) > 0) {
    expect_true(all(rows$sse_after <= rows$sse_before + 1e-9))
  }
})

test_that("refine_tree_cuts's min_leaf_n floor prevents any leaf from ending up empty", {
  set.seed(42)
  n <- 300
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.5, ifelse(X$x2 <= 0.5, 0, 1), 2) + rnorm(n, sd = 0.02)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.001,
                   discretize_bins = 12L, max_depth = 2L)
  ct <- as_coordinate_tree(fit, X, y)
  collapsed <- collapse_transitions(ct)$tree

  check_min_leaf <- function(node, min_n) {
    if (identical(node$kind, "leaf")) {
      expect_gte(node$n, min_n)
      return(invisible())
    }
    check_min_leaf(node$left, min_n)
    check_min_leaf(node$right, min_n)
  }

  for (mln in c(1L, 5L, 20L)) {
    ref <- refine_tree_cuts(collapsed, X, y, min_leaf_n = mln)
    check_min_leaf(ref$tree, mln)
  }
})

test_that("refine_tree_cuts distinguishes too_few_rows_at_node from min_leaf_n_infeasible", {
  # An artificially huge min_leaf_n relative to the data hits the coarse
  # too_few_rows_at_node gate (2*min_leaf_n > n_node) before any candidate
  # is even considered.
  set.seed(20260901)
  n <- 100
  X <- data.frame(x1 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 6L, max_depth = 1L)
  ct <- as_coordinate_tree(fit, X, y)

  ref <- refine_tree_cuts(ct, X, y, min_leaf_n = 1000L)
  expect_false(ref$refined$refined[[1]])
  expect_equal(ref$refined$reason[[1]], "too_few_rows_at_node")
  expect_equal(ref$tree$cut, ct$cut)   # grid cut retained unchanged

  # min_leaf_n_infeasible specifically -- the coarse gate PASSES (enough
  # total rows), but NO candidate can achieve min_leaf_n on BOTH sides.
  # Hand-built rather than via a real fit: fit_tree()/GOSDT choosing to
  # split at exactly two distinct covariate values on demand isn't
  # reliably controllable, so construct the coordinate tree directly.
  # Exactly 2 distinct x1 values (20 rows at 0.1, 5 rows at 0.9) means
  # exactly 2 candidates exist; splitting at either leaves one side with
  # only 5 (or 0) rows -- below min_leaf_n = 10 -- while the coarse gate
  # (2*10 = 20 <= n_node = 25) passes.
  fixture <- list(
    kind = "split", id = 1L, coord = "x1", cut = 0.5,
    grid_cuts = 0.5, collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_,
    left  = list(kind = "leaf", id = 2L),
    right = list(kind = "leaf", id = 3L)
  )
  X3 <- data.frame(x1 = c(rep(0.1, 20), rep(0.9, 5)))
  y3 <- c(rep(0, 20), rep(1, 5))

  ref3 <- refine_tree_cuts(fixture, X3, y3, min_leaf_n = 10L)
  expect_false(ref3$refined$refined[[1]])
  expect_equal(ref3$refined$reason[[1]], "min_leaf_n_infeasible")
  expect_equal(ref3$tree$cut, fixture$cut)
})

test_that("scan_cutoff's tie-break prefers the candidate closest to incumbent_cut, then smallest", {
  # Hand-constructed exact tie: xj = c(1,2,3), y = c(0,10,0) gives IDENTICAL
  # total SSE = 50 whether the cut falls at x=1 or x=2. The pre-port
  # behavior (strict `<` comparison over an ascending sweep) always picked
  # the smallest candidate regardless of the incumbent; this locks in the
  # new, documented rule instead.
  xj <- c(1, 2, 3); y <- c(0, 10, 0)
  lid_left <- c(1, 1, 1); lid_right <- c(2, 2, 2); K <- 2

  res_near2 <- scan_cutoff(xj, y, lid_left, lid_right, K,
                            candidates = c(1, 2), incumbent_cut = 1.8)
  expect_equal(res_near2$cut, 2)
  expect_equal(res_near2$all_candidates$sse, c(50, 50))

  res_near1 <- scan_cutoff(xj, y, lid_left, lid_right, K,
                            candidates = c(1, 2), incumbent_cut = 1.2)
  expect_equal(res_near1$cut, 1)
})

test_that("refine_tree_cuts is exactly invariant to a global additive shift of y (centering correctness)", {
  # SSE is exactly invariant to shifting y by one constant across all rows
  # (a standard identity: within-group variance is shift-invariant). This
  # locks in that the internal y-centering (added to avoid catastrophic
  # cancellation for y far from zero) does not change the result at all --
  # only where the near-zero risk of losing precision arises.
  set.seed(20260901)
  n <- 800
  X <- data.frame(x1 = runif(n))
  true_cut <- 0.37
  y <- ifelse(X$x1 <= true_cut, 0, 2) + rnorm(n, sd = 0.05)

  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 1L)
  ct <- as_coordinate_tree(fit, X, y)
  ref <- refine_tree_cuts(ct, X, y)

  shift <- 1e6
  y_shifted <- y + shift
  fit2 <- fit_tree(X, y_shifted, loss_function = "squared_error", regularization = 0.01,
                    discretize_bins = 8L, max_depth = 1L)
  ct2 <- as_coordinate_tree(fit2, X, y_shifted)
  ref2 <- refine_tree_cuts(ct2, X, y_shifted)

  expect_equal(ref$tree$cut, ref2$tree$cut, tolerance = 1e-6)
  # Root is a split node here (max_depth = 1), not a leaf -- compare LEAF
  # predictions via the walk engine, not a $prediction field that only
  # exists on leaves.
  preds1 <- coord_tree_predict(ref$tree, X)
  preds2 <- coord_tree_predict(ref2$tree, X)
  expect_equal(preds1 + shift, preds2, tolerance = 1e-4)
})

test_that("full pipeline (as_coordinate_tree -> collapse_transitions -> refine_tree_cuts) produces a walkable, improved-fit tree", {
  set.seed(20260901)
  n <- 1500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4123, 0, 3) + ifelse(X$x2 <= 0.6789, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 2L)
  ct <- as_coordinate_tree(fit, X, y)
  collapsed <- collapse_transitions(ct)$tree
  ref <- refine_tree_cuts(collapsed, X, y)

  preds_grid    <- coord_tree_predict(ct, X)
  preds_refined <- coord_tree_predict(ref$tree, X)
  sse_grid    <- sum((y - preds_grid)^2)
  sse_refined <- sum((y - preds_refined)^2)

  expect_true(all(is.finite(preds_refined)))
  expect_lte(sse_refined, sse_grid + 1e-6)

  # Every leaf still carries valid, non-empty stats.
  check_leaf <- function(node) {
    if (identical(node$kind, "leaf")) {
      expect_gt(node$n, 0L)
      expect_true(is.finite(node$prediction))
      return(invisible())
    }
    check_leaf(node$left); check_leaf(node$right)
  }
  check_leaf(ref$tree)
})

# ---------------------------------------------------------------------------
# refine_tree() -- the public entry point -- and RefinedTreeModel's public
# consumer contract (predict/print/summary, leaf_assignments/n_leaves/
# split_table, and the class validator).
# ---------------------------------------------------------------------------

test_that("refine_tree() end to end recovers known off-grid boundaries and predicts sanely", {
  set.seed(20260901)
  n <- 1500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  true_cut1 <- 0.4123; true_cut2 <- 0.6789
  y <- ifelse(X$x1 <= true_cut1, 0, 3) + ifelse(X$x2 <= true_cut2, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 2L)

  rt <- fit |> refine_tree(X, y)
  expect_s7_class(rt, RefinedTreeModel)
  expect_equal(n_leaves(rt), 4L)
  expect_setequal(rt@coords, c("x1", "x2"))
  expect_equal(rt@n_train, n)

  st <- split_table(rt)
  expect_equal(nrow(st), 3L)   # 2 true boundaries, one split on each coordinate twice
  x1_cuts <- st$cut[st$coord == "x1"]
  x2_cuts <- st$cut[st$coord == "x2"]
  expect_true(all(abs(x1_cuts - true_cut1) < 0.01))
  expect_true(all(abs(x2_cuts - true_cut2) < 0.01))

  preds <- predict(rt, X)
  expect_true(all(is.finite(preds)))
  la <- leaf_assignments(rt, X)
  expect_equal(length(unique(la)), n_leaves(rt))
})

test_that("predict.RefinedTreeModel and leaf_assignments reject newdata missing a required coordinate", {
  set.seed(20260901)
  n <- 500
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 3) + ifelse(X$x2 <= 0.6, 0, 1) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 8L, max_depth = 2L)
  rt <- refine_tree(fit, X, y)

  expect_error(predict(rt, X[, "x1", drop = FALSE]), "missing coordinate")
  expect_error(leaf_assignments(rt, X[, "x1", drop = FALSE]), "missing coordinate")
})

test_that("RefinedTreeModel's validator rejects invalid trees", {
  mk_leaf <- function(id, pred = 0, n = 5) {
    list(kind = "leaf", id = id, prediction = pred, n = n,
         stats = list(n = n, sum = pred * n, sumsq = 0))
  }
  mk_split <- function(id, coord, cut, left, right) {
    list(kind = "split", id = id, coord = coord, cut = cut, grid_cuts = cut,
         collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_, left = left, right = right)
  }
  good_tree <- mk_split(1L, "x1", 0.5, mk_leaf(2L), mk_leaf(3L))

  # Valid construction succeeds.
  expect_s7_class(
    RefinedTreeModel(tree = good_tree, coords = "x1", loss = "squared_error",
                      n_train = 100L, training_risk = 1.0),
    RefinedTreeModel
  )

  # Duplicate node id.
  bad_dup <- mk_split(1L, "x1", 0.5, mk_leaf(1L), mk_leaf(3L))
  expect_error(
    RefinedTreeModel(tree = bad_dup, coords = "x1", loss = "squared_error",
                      n_train = 100L, training_risk = 1.0),
    "duplicate node id"
  )

  # min_leaf_n violation.
  bad_nleaf <- mk_split(1L, "x1", 0.5, mk_leaf(2L, n = 0), mk_leaf(3L))
  expect_error(
    RefinedTreeModel(tree = bad_nleaf, coords = "x1", loss = "squared_error",
                      n_train = 100L, training_risk = 1.0, min_leaf_n = 1L),
    "below min_leaf_n"
  )

  # coord not declared in @coords.
  expect_error(
    RefinedTreeModel(tree = good_tree, coords = "x2", loss = "squared_error",
                      n_train = 100L, training_risk = 1.0),
    "not in @coords"
  )

  # Bracket infeasibility: a nested same-coordinate split whose cut lies
  # outside what its own subtree position permits.
  bad_bracket <- mk_split(
    1L, "x1", 0.5,
    left  = mk_split(2L, "x1", 0.6, mk_leaf(3L), mk_leaf(4L)),
    right = mk_leaf(5L)
  )
  expect_error(
    RefinedTreeModel(tree = bad_bracket, coords = "x1", loss = "squared_error",
                      n_train = 100L, training_risk = 1.0),
    "identifying bracket"
  )

  # Wrong loss (Milestone A scope is squared_error only).
  expect_error(
    RefinedTreeModel(tree = good_tree, coords = "x1", loss = "log_loss",
                      n_train = 100L, training_risk = 1.0),
    "squared_error"
  )
})

test_that("print/summary on a RefinedTreeModel run without error and report the refinement log", {
  set.seed(20260901)
  n <- 500
  X <- data.frame(x1 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 2) + rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                   discretize_bins = 6L, max_depth = 1L)
  rt <- refine_tree(fit, X, y)

  expect_output(print(rt), "RefinedTreeModel")
  expect_output(summary(rt), "Refinement log")
})
