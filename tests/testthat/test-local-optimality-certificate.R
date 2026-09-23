# Tests for R/local_optimality_certificate.R (Milestone B, 2026-09-01).
# Design: quality_reports/plans/2026-09-01_two-stage-package-defaults-
# session.md, Milestone B section (full Oracle consult: three framing
# corrections -- MOVE is not vacuous, "beats every perturbation by margin
# > 0" is mathematically unattainable, and a fourth perturbation class
# (RECOORD) was missing from the plan's original "move/add/delete" prose).

# ---------------------------------------------------------------------------
# Test helpers: hand-build small coordinate-space trees directly, bypassing
# fit_tree()/refine_tree() entirely, so every scenario below is an exact,
# fully-controlled construction rather than something inferred from a real
# solver fit.
# ---------------------------------------------------------------------------

mk_leaf_lc <- function(id, y, idx) {
  yl <- y[idx]
  list(kind = "leaf", id = id, prediction = mean(yl), n = length(idx),
       stats = list(n = length(idx), sum = sum(yl), sumsq = sum(yl * yl)),
       original_prediction = mean(yl))
}

mk_split_lc <- function(id, coord, cut, left, right, collapsed = FALSE) {
  list(kind = "split", id = id, coord = coord, cut = cut, grid_cuts = cut,
       collapsed = collapsed, k_lo = 1L, k_hi = NA_integer_,
       left = left, right = right)
}

# One split on `coord` at `cut`, two leaves.
mk_stump_split <- function(X, y, coord, cut) {
  idx_l <- which(X[[coord]] <= cut)
  idx_r <- which(X[[coord]] > cut)
  mk_split_lc(1L, coord, cut, mk_leaf_lc(2L, y, idx_l), mk_leaf_lc(3L, y, idx_r))
}

mk_rtm <- function(tree, coords, X, y, lambda = 0.1, min_leaf_n = 1L,
                    max_depth = NULL, max_leaves = NULL) {
  RefinedTreeModel(
    tree = tree, coords = coords, loss = "squared_error",
    n_train = as.integer(nrow(X)),
    training_risk = coord_tree_training_sse(tree, X, y),
    lambda = lambda, min_leaf_n = as.integer(min_leaf_n),
    max_depth = max_depth, max_leaves = max_leaves
  )
}

# ---------------------------------------------------------------------------
# Test 1: DELETE closed form matches brute-force rebuild
# ---------------------------------------------------------------------------

test_that("perturb_delete_deltas()'s closed form matches a brute-force merge+recompute", {
  set.seed(20260901)
  for (rep in 1:20) {
    n <- sample(30:80, 1)
    X <- data.frame(x1 = runif(n), x2 = runif(n))
    y <- rnorm(n, sd = 2) + 3 * X$x1
    cut <- runif(1, min(X$x1) + 1e-6, max(X$x1) - 1e-6)
    tree <- mk_stump_split(X, y, "x1", cut)
    lambda <- runif(1, 0.01, 1)
    rtm <- mk_rtm(tree, "x1", X, y, lambda = lambda)
    lambda_sse <- .objective_lambda_sse(rtm)

    d <- perturb_delete_deltas(rtm, lambda_sse = lambda_sse)
    expect_equal(nrow(d), 1L)

    # Brute force: merge into a single leaf and recompute the same delta
    # directly from coord_tree_training_sse(), not from the closed form.
    merged <- mk_leaf_lc(1L, y, seq_len(n))
    sse_before <- coord_tree_training_sse(tree, X, y)
    sse_after  <- coord_tree_training_sse(merged, X, y)
    brute_delta <- (sse_after - sse_before) - lambda_sse

    expect_equal(d$delta[[1L]], brute_delta, tolerance = 1e-8)
  }
})

# ---------------------------------------------------------------------------
# Test 2: stump with n < 2*min_leaf_n-style vacuity (no splits exist at all)
# ---------------------------------------------------------------------------

test_that("a stump certifies FALSE with reason vacuous_no_feasible_perturbations", {
  set.seed(1)
  n <- 50L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  stump <- mk_leaf_lc(1L, y, seq_len(n))
  rtm <- mk_rtm(stump, character(0), X, y, lambda = 0.1)

  cert <- certify_local_optimality(rtm, X, y, perturbations = c("move", "recoord", "delete"))
  expect_false(cert$certified)
  expect_identical(cert$reason, "vacuous_no_feasible_perturbations")
})

test_that("min_leaf_n blocking every candidate also certifies vacuous", {
  set.seed(2)
  n <- 10L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  tree <- mk_stump_split(X, y, "x1", median(X$x1))
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.1, min_leaf_n = 1L)

  # min_leaf_n way above what any candidate could satisfy for move; delete
  # doesn't depend on min_leaf_n, so exclude it to isolate the vacuous path.
  cert <- certify_local_optimality(rtm, X, y, perturbations = "move", min_leaf_n = 100L)
  expect_false(cert$certified)
  expect_identical(cert$reason, "vacuous_no_feasible_perturbations")
})

# ---------------------------------------------------------------------------
# Test 3: deliberately mis-placed cut -> improving_move
# ---------------------------------------------------------------------------

test_that("a deliberately mis-placed cut is caught by MOVE, with the correct worst$node_id", {
  set.seed(3)
  n <- 300L
  X <- data.frame(x1 = runif(n))
  y <- ifelse(X$x1 <= 0.4, 0, 5) + rnorm(n, sd = 0.05)

  # Deliberately off the true jump at 0.4.
  bad_cut <- 0.3
  tree <- mk_stump_split(X, y, "x1", bad_cut)
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.1)

  cert <- certify_local_optimality(rtm, X, y, perturbations = "move")
  expect_false(cert$certified)
  expect_identical(cert$reason, "improving_move")
  expect_identical(cert$worst$node_id, 1L)
  expect_lt(cert$margin, 0)
})

# ---------------------------------------------------------------------------
# Test 4: wrong-coordinate split at a prunable node -> improving_recoord
# ---------------------------------------------------------------------------

test_that("a split on the wrong coordinate at a prunable node is caught by RECOORD", {
  set.seed(4)
  n <- 300L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x2 <= 0.5, 0, 5) + rnorm(n, sd = 0.05)   # signal is in x2, not x1

  tree <- mk_stump_split(X, y, "x1", median(X$x1))       # split on the WRONG coordinate
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.1)

  cert <- certify_local_optimality(rtm, X, y, perturbations = "recoord")
  expect_false(cert$certified)
  expect_identical(cert$reason, "improving_recoord")
  expect_identical(cert$worst$coord, "x2")
})

# ---------------------------------------------------------------------------
# Test 5: zero-signal split with lambda > 0 -> improving_delete
# ---------------------------------------------------------------------------

test_that("a zero-signal split (no real mean difference) is caught by DELETE when lambda > 0", {
  set.seed(5)
  n <- 200L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)   # no real signal on x1 at all
  tree <- mk_stump_split(X, y, "x1", median(X$x1))
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.5)

  cert <- certify_local_optimality(rtm, X, y, perturbations = "delete")
  expect_false(cert$certified)
  expect_identical(cert$reason, "improving_delete")
  expect_lt(cert$margin, 0)
})

# ---------------------------------------------------------------------------
# Test 6: strong unmodelled signal -> improving_add at small lambda,
# certified at large lambda
# ---------------------------------------------------------------------------

test_that("strong unmodelled signal triggers improving_add at small lambda, passes at large lambda", {
  set.seed(6)
  n <- 400L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x2 <= 0.5, 0, 10) + rnorm(n, sd = 0.1)   # entirely unmodelled by a stump
  stump <- mk_leaf_lc(1L, y, seq_len(n))

  rtm_small <- mk_rtm(stump, character(0), X, y, lambda = 0.01)
  cert_small <- certify_local_optimality(rtm_small, X, y, perturbations = "add")
  expect_false(cert_small$certified)
  expect_identical(cert_small$reason, "improving_add")

  rtm_large <- mk_rtm(stump, character(0), X, y, lambda = 100)
  cert_large <- certify_local_optimality(rtm_large, X, y, perturbations = "add")
  expect_true(cert_large$certified)
})

# ---------------------------------------------------------------------------
# Test 7: saturated tree at max_leaves -> all ADDs infeasible, none scored
# ---------------------------------------------------------------------------

test_that("a tree at max_leaves marks every ADD infeasible, not an improving failure", {
  set.seed(7)
  n <- 300L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x2 <= 0.5, 0, 10) + rnorm(n, sd = 0.1)   # real signal exists...
  tree <- mk_stump_split(X, y, "x1", median(X$x1))       # ...but this tree has 2 leaves already
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.01, max_leaves = 2L)

  cert <- certify_local_optimality(rtm, X, y, perturbations = "add")
  expect_identical(nrow(cert$by_kind), 1L)
  expect_equal(cert$by_kind$n_infeasible[[1L]], cert$by_kind$n_evaluated[[1L]])
  expect_true(is.na(cert$by_kind$min_delta[[1L]]))
  expect_false(cert$certified)
  expect_identical(cert$reason, "vacuous_no_feasible_perturbations")
})

# ---------------------------------------------------------------------------
# Test 8: lambda = 0, uncapped -> lambda_zero_uncapped
# ---------------------------------------------------------------------------

test_that("lambda = 0 with no depth/leaf cap reports lambda_zero_uncapped, not improving_add", {
  set.seed(8)
  n <- 200L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  tree <- mk_stump_split(X, y, "x1", median(X$x1))
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.1)   # nonzero at construction

  cert <- certify_local_optimality(rtm, X, y, lambda = 0, perturbations = "add")
  expect_false(cert$certified)
  expect_identical(cert$reason, "lambda_zero_uncapped")
})

test_that("lambda = 0 with a binding max_leaves does NOT short-circuit to lambda_zero_uncapped", {
  set.seed(9)
  n <- 200L
  X <- data.frame(x1 = runif(n))
  y <- rnorm(n)
  tree <- mk_stump_split(X, y, "x1", median(X$x1))
  rtm <- mk_rtm(tree, "x1", X, y, lambda = 0.1, max_leaves = 2L)

  cert <- certify_local_optimality(rtm, X, y, lambda = 0, perturbations = "add",
                                    max_leaves = 2L)
  expect_false(identical(cert$reason, "lambda_zero_uncapped"))
})

# ---------------------------------------------------------------------------
# Test 9: MOVE fixed-point regression (Oracle correction C1: MOVE is NOT
# vacuous). Guards against a future "MOVE is redundant, refine_tree_cuts()
# already handles it" refactor.
# ---------------------------------------------------------------------------

test_that("MOVE detects a deliberately staled cut a single root-first pass would miss", {
  # Real two-coordinate fit + Milestone A refinement, so the whole tree is
  # genuinely correctly refined -- then artificially re-stale ONE split's
  # cut back to an inferior (but still bracket-feasible) value, simulating
  # exactly the failure mode a non-fixed-point pass could produce.
  #
  # Deliberately stale a PRUNABLE (leaf-parent) split, not the root: staling
  # an ANCESTOR changes which rows reach its descendants, which can make a
  # descendant's OWN (unchanged) cut fall outside its newly-recomputed
  # candidate set -- a genuine, and correctly detected, inconsistency, but
  # not what THIS test is isolating. A prunable node has no descendants at
  # all, so staling it cannot cascade into invalidating anything else.
  set.seed(10)
  n <- 600L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- ifelse(X$x1 <= 0.5, ifelse(X$x2 <= 0.5, 0, 3), ifelse(X$x2 <= 0.5, 6, 9)) +
    rnorm(n, sd = 0.05)
  fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.02,
                   max_depth = 2L, discretize_bins = 16L)
  rt <- refine_tree(fit, X, y)
  skip_if(n_leaves(rt) < 3L, "fit did not recover a multi-level tree at this seed")

  paths <- coord_tree_split_paths(rt@tree)
  prunable_path <- NULL
  for (p in paths) {
    node <- coord_tree_node_at(rt@tree, p)
    if (identical(node$left$kind, "leaf") && identical(node$right$kind, "leaf")) {
      prunable_path <- p
      break
    }
  }
  skip_if(is.null(prunable_path), "no prunable split found at this seed")

  target <- coord_tree_node_at(rt@tree, prunable_path)
  # Restrict candidate values to the ROWS THAT ACTUALLY REACH this node
  # (not the full dataset) -- perturb_move_deltas()'s own candidate set is
  # built the same way, and a value absent from this subset can never be a
  # legitimate cut for this node regardless of how "stale" it is meant to
  # look.
  node_idx <- coord_tree_rows_at(rt@tree, X, prunable_path)
  coord_vals <- sort(unique(X[[target$coord]][node_idx]))
  stale_cut <- coord_vals[[max(2L, floor(length(coord_vals) * 0.1))]]
  br <- node_bracket(rt@tree, prunable_path)
  if (isTRUE(all.equal(stale_cut, target$cut, tolerance = 1e-6)) ||
      !(stale_cut > br$lo && stale_cut < br$hi)) {
    skip("could not construct a usable stale cut at this seed")
  }
  target$cut <- stale_cut
  stale_tree <- coord_tree_set_node_at(rt@tree, prunable_path, target)

  stale_model <- RefinedTreeModel(
    tree = stale_tree, coords = rt@coords, loss = rt@loss,
    n_train = rt@n_train, training_risk = coord_tree_training_sse(stale_tree, X, y),
    lambda = rt@lambda, min_leaf_n = rt@min_leaf_n
  )

  cert <- certify_local_optimality(stale_model, X, y, perturbations = "move")
  expect_false(cert$certified)
  expect_identical(cert$reason, "improving_move")
  expect_identical(cert$worst$node_id, target$id)
})

# ---------------------------------------------------------------------------
# Test 10: certified is never NA across a fuzz loop
# ---------------------------------------------------------------------------

test_that("certified is never NA across a fuzz loop of random trees/data", {
  set.seed(11)
  for (rep in 1:60) {
    n <- sample(20:200, 1)
    X <- data.frame(x1 = runif(n), x2 = runif(n))
    y <- rnorm(n, sd = sample(c(0.1, 1, 5), 1))

    depth <- sample(0:2, 1)
    if (depth == 0L) {
      tree <- mk_leaf_lc(1L, y, seq_len(n))
      coords <- character(0)
    } else if (depth == 1L) {
      coord <- sample(c("x1", "x2"), 1)
      cut <- runif(1, min(X[[coord]]) + 1e-6, max(X[[coord]]) - 1e-6)
      tree <- mk_stump_split(X, y, coord, cut)
      coords <- coord
    } else {
      cut1 <- runif(1, min(X$x1) + 1e-6, max(X$x1) - 1e-6)
      idx_l <- which(X$x1 <= cut1); idx_r <- which(X$x1 > cut1)
      if (length(idx_l) < 4L || length(idx_r) < 4L) next
      cut2 <- runif(1, min(X$x2[idx_l]) + 1e-9, max(X$x2[idx_l]) - 1e-9)
      idx_ll <- idx_l[X$x2[idx_l] <= cut2]; idx_lr <- idx_l[X$x2[idx_l] > cut2]
      if (length(idx_ll) < 1L || length(idx_lr) < 1L) next
      left_sub <- mk_split_lc(2L, "x2", cut2, mk_leaf_lc(3L, y, idx_ll), mk_leaf_lc(4L, y, idx_lr))
      tree <- mk_split_lc(1L, "x1", cut1, left_sub, mk_leaf_lc(5L, y, idx_r))
      coords <- c("x1", "x2")
    }

    lambda <- sample(c(0, 0.01, 0.1, 1, 10), 1)
    min_leaf_n <- sample(1:3, 1)
    rtm <- tryCatch(
      mk_rtm(tree, coords, X, y, lambda = lambda, min_leaf_n = min_leaf_n),
      error = function(e) NULL
    )
    if (is.null(rtm)) next   # construction itself rejected (e.g. bracket infeasibility); not this test's concern

    cert <- certify_local_optimality(rtm, X, y)
    expect_false(is.na(cert$certified),
                 info = sprintf("rep %d: certified was NA (lambda=%s, min_leaf_n=%s, depth=%s)",
                                 rep, lambda, min_leaf_n, depth))
    expect_true(is.logical(cert$certified))
  }
})

# ---------------------------------------------------------------------------
# Test 11: same partition, different split order/orientation ->
# topology_stable = TRUE, structure_identical = FALSE
# ---------------------------------------------------------------------------

test_that("compare_topology: same partition via different split order is stable but not structure-identical", {
  set.seed(12)
  n <- 400L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)

  cut1 <- 0.5; cut2 <- 0.5
  # V1: split x1 first, then x2 within each side.
  idx_l <- which(X$x1 <= cut1); idx_r <- which(X$x1 > cut1)
  v1 <- mk_split_lc(
    1L, "x1", cut1,
    mk_split_lc(2L, "x2", cut2,
                mk_leaf_lc(3L, y, idx_l[X$x2[idx_l] <= cut2]),
                mk_leaf_lc(4L, y, idx_l[X$x2[idx_l] > cut2])),
    mk_split_lc(5L, "x2", cut2,
                mk_leaf_lc(6L, y, idx_r[X$x2[idx_r] <= cut2]),
                mk_leaf_lc(7L, y, idx_r[X$x2[idx_r] > cut2]))
  )
  # V2: split x2 first, then x1 within each side -- SAME 4 quadrants.
  idx_b <- which(X$x2 <= cut2); idx_t <- which(X$x2 > cut2)
  v2 <- mk_split_lc(
    1L, "x2", cut2,
    mk_split_lc(2L, "x1", cut1,
                mk_leaf_lc(3L, y, idx_b[X$x1[idx_b] <= cut1]),
                mk_leaf_lc(4L, y, idx_b[X$x1[idx_b] > cut1])),
    mk_split_lc(5L, "x1", cut1,
                mk_leaf_lc(6L, y, idx_t[X$x1[idx_t] <= cut1]),
                mk_leaf_lc(7L, y, idx_t[X$x1[idx_t] > cut1]))
  )

  m1 <- mk_rtm(v1, c("x1", "x2"), X, y)
  m2 <- mk_rtm(v2, c("x1", "x2"), X, y)

  cmp <- compare_topology(m1, m2, X)
  expect_true(cmp$comparable)
  expect_true(cmp$topology_stable)
  expect_identical(cmp$refinement, "identical")
  expect_false(cmp$structure_identical)
})

# ---------------------------------------------------------------------------
# Test 12: cuts jittered within a gap -> stable; across a data point -> unstable
# ---------------------------------------------------------------------------

test_that("compare_topology: cuts jittered within the same inter-observation gap are stable", {
  set.seed(13)
  n <- 100L
  X <- data.frame(x1 = sort(runif(n)))   # sorted for easy gap inspection
  y <- rnorm(n)

  # Pick a gap between two adjacent observed values.
  i <- 50L
  gap_lo <- X$x1[[i]]; gap_hi <- X$x1[[i + 1L]]
  cut_a <- gap_lo + 0.25 * (gap_hi - gap_lo)
  cut_b <- gap_lo + 0.75 * (gap_hi - gap_lo)
  expect_true(cut_a != cut_b)   # genuinely different numeric cuts

  m_a <- mk_rtm(mk_stump_split(X, y, "x1", cut_a), "x1", X, y)
  m_b <- mk_rtm(mk_stump_split(X, y, "x1", cut_b), "x1", X, y)

  cmp <- compare_topology(m_a, m_b, X)
  expect_true(cmp$comparable)
  expect_true(cmp$topology_stable)
})

test_that("compare_topology: cuts jittered ACROSS a data point are unstable", {
  set.seed(14)
  n <- 100L
  X <- data.frame(x1 = sort(runif(n)))
  y <- rnorm(n)

  i <- 50L
  cut_before <- X$x1[[i]] - 1e-9        # just below the i-th point
  cut_after  <- X$x1[[i]] + 1e-9        # just above it: crosses exactly one observation

  m_before <- mk_rtm(mk_stump_split(X, y, "x1", cut_before), "x1", X, y)
  m_after  <- mk_rtm(mk_stump_split(X, y, "x1", cut_after),  "x1", X, y)

  cmp <- compare_topology(m_before, m_after, X)
  expect_true(cmp$comparable)
  expect_false(cmp$topology_stable)
})

# ---------------------------------------------------------------------------
# Test 13: one extra split at rung m2 -> curr_refines_prev
# ---------------------------------------------------------------------------

test_that("compare_topology: an extra split at the later rung reports curr_refines_prev", {
  set.seed(15)
  n <- 300L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)

  cut1 <- 0.5
  idx_l <- which(X$x1 <= cut1); idx_r <- which(X$x1 > cut1)
  prev <- mk_stump_split(X, y, "x1", cut1)

  cut2 <- 0.5
  curr <- mk_split_lc(
    1L, "x1", cut1,
    mk_split_lc(2L, "x2", cut2,
                mk_leaf_lc(3L, y, idx_l[X$x2[idx_l] <= cut2]),
                mk_leaf_lc(4L, y, idx_l[X$x2[idx_l] > cut2])),
    mk_leaf_lc(5L, y, idx_r)
  )

  m_prev <- mk_rtm(prev, "x1", X, y)
  m_curr <- mk_rtm(curr, c("x1", "x2"), X, y)

  cmp <- compare_topology(m_prev, m_curr, X)
  expect_true(cmp$comparable)
  expect_false(cmp$topology_stable)
  expect_identical(cmp$refinement, "curr_refines_prev")
  expect_equal(cmp$n_leaves_curr, cmp$n_leaves_prev + 1L)
})

# ---------------------------------------------------------------------------
# Test 14: X mismatch -> comparable = FALSE, topology_stable NOT a bare FALSE
# ---------------------------------------------------------------------------

test_that("compare_topology: a missing coordinate in X reports comparable = FALSE, not a bare topology_stable = FALSE", {
  set.seed(16)
  n <- 100L
  X_full <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)

  m1 <- mk_rtm(mk_stump_split(X_full, y, "x1", 0.5), "x1", X_full, y)
  m2 <- mk_rtm(mk_stump_split(X_full, y, "x2", 0.5), "x2", X_full, y)

  X_missing_x2 <- X_full[, "x1", drop = FALSE]
  cmp <- compare_topology(m1, m2, X_missing_x2)
  expect_false(cmp$comparable)
  expect_identical(cmp$reason, "X_mismatch")
  expect_true(is.na(cmp$topology_stable))   # NOT identical(cmp$topology_stable, FALSE)
})

# ---------------------------------------------------------------------------
# Additional coverage: input_mismatch, tree_leaf_boxes(), canonical_box_signature()
# ---------------------------------------------------------------------------

test_that("certify_local_optimality reports input_mismatch when X lacks a coordinate the model uses", {
  n <- 50L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)
  rtm <- mk_rtm(mk_stump_split(X, y, "x1", 0.5), "x1", X, y)

  cert <- certify_local_optimality(rtm, X[, "x2", drop = FALSE], y)
  expect_false(cert$certified)
  expect_identical(cert$reason, "input_mismatch")
})

test_that("tree_leaf_boxes() omits trivially-unconstrained coordinates and bounds correctly", {
  n <- 100L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)
  tree <- mk_stump_split(X, y, "x1", 0.5)

  boxes <- tree_leaf_boxes(tree, c("x1", "x2"))
  expect_true(all(boxes$coord == "x1"))   # x2 never split on -> omitted entirely
  expect_equal(nrow(boxes), 2L)           # one row per leaf
  left_box <- boxes[boxes$leaf_id == 2L, ]
  right_box <- boxes[boxes$leaf_id == 3L, ]
  expect_equal(left_box$hi, 0.5)
  expect_equal(right_box$lo, 0.5)
})

test_that("canonical_box_signature() agrees with canonical_partition() in the ordinary (non-collinear) case", {
  set.seed(17)
  n <- 200L
  X <- data.frame(x1 = runif(n), x2 = runif(n))
  y <- rnorm(n)
  t1 <- mk_stump_split(X, y, "x1", 0.5)
  t2 <- mk_stump_split(X, y, "x1", 0.5)   # identical tree

  expect_identical(canonical_partition(t1, X)$key, canonical_partition(t2, X)$key)
  expect_identical(canonical_box_signature(t1, X), canonical_box_signature(t2, X))
})
