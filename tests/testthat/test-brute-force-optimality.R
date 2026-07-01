# Brute-force optimality verification
#
# For small fully-binary datasets (n <= 20, p <= 4), enumerate EVERY possible
# decision tree up to a given depth and compute its exact penalized objective.
# The GOSDT result must equal the brute-force minimum.
#
# This is the strongest correctness check we can run without access to the
# solver's internal proof: if the brute-force minimum disagrees with GOSDT,
# either (a) the objective normalization is wrong, or (b) GOSDT found a
# suboptimal tree. Both are bugs. If they agree on several DGPs, we have
# strong evidence the solver is globally optimal on these problem sizes.
#
# Scope: binary (0/1) features only; no discretization involved.
# All three losses: misclassification, log_loss, squared_error.

# ── brute-force engine ───────────────────────────────────────────────────────

# Leaf prediction and loss for each loss function.
leaf_loss <- function(y, loss) {
  n <- length(y)
  if (n == 0L) return(0)
  switch(loss,
    misclassification = {
      # Return raw count of misclassified points (caller divides by n_global).
      # majority vote prediction: misclassify the minority class.
      min(sum(y == 0L), sum(y == 1L))
    },
    log_loss = {
      if (n == 0L) return(0)
      eps <- 1e-12
      p1 <- sum(y) / n
      p0 <- 1 - p1
      p1c <- max(eps, min(1 - eps, p1))
      p0c <- max(eps, min(1 - eps, p0))
      -(p1c * log(p1c) + p0c * log(p0c)) * n   # summed entropy at leaf
    },
    squared_error = {
      if (n == 0L) return(0)
      mu <- mean(y)
      sum((y - mu)^2)                     # SSE at leaf
    },
    stop("unknown loss: ", loss)
  )
}

# Recursive tree enumerator.
# Returns the minimum penalized objective over all trees of depth <= max_depth
# rooted at the subproblem (rows, depth).
# Objective = (sum of leaf losses) / n_global + lambda * n_leaves
# where leaf losses are in the matching scale (see leaf_loss above).
min_tree_obj <- function(X, y, rows, lambda, loss, max_depth, n_global) {
  y_sub <- y[rows]
  n_sub <- length(rows)

  # Stump objective (always available)
  stump_obj <- leaf_loss(y_sub, loss) / n_global + lambda

  if (max_depth == 0L || n_sub <= 1L) return(stump_obj)

  best <- stump_obj

  for (j in seq_len(ncol(X))) {
    # Feature j is binary {0,1}; one possible split: j == 1
    left  <- rows[X[rows, j] == 1]  # true branch
    right <- rows[X[rows, j] == 0]  # false branch

    # Skip degenerate splits
    if (length(left) == 0L || length(right) == 0L) next

    left_best  <- min_tree_obj(X, y, left,  lambda, loss, max_depth - 1L, n_global)
    right_best <- min_tree_obj(X, y, right, lambda, loss, max_depth - 1L, n_global)

    # Both sub-objectives already include lambda per sub-leaf; the split adds no
    # additional leaf (the internal node is free). But wait: each sub-call
    # includes lambda for ITS leaves. The total for this split is just the sum.
    split_obj <- left_best + right_best

    if (split_obj < best) best <- split_obj
  }

  best
}

brute_force_obj <- function(X, y, lambda, loss, max_depth = 4L) {
  stopifnot(is.data.frame(X), all(unlist(X) %in% c(0, 1)))
  min_tree_obj(X, y, seq_len(nrow(X)), lambda, loss, max_depth, nrow(X))
}

# Pull the penalized objective from a fitted model.
gosdt_obj <- function(model) {
  model@trees[[1]]$model_objective
}

# ── helper: check one DGP / loss combination ─────────────────────────────────

check_optimality <- function(X, y, lambda, loss, max_depth = 3L, label = "") {
  bf  <- brute_force_obj(X, y, lambda, loss, max_depth)
  m   <- fit_tree(X, y, loss_function = loss, regularization = lambda,
                  max_depth = as.integer(max_depth))
  got <- gosdt_obj(m)
  expect_equal(got, bf, tolerance = 1e-4,
               label = paste0("GOSDT objective [", label, "]"),
               expected.label = "brute-force minimum")
}

# ── test cases ────────────────────────────────────────────────────────────────

test_that("misclassification: GOSDT matches brute force on separable DGP", {
  # f1 perfectly predicts y: one depth-1 split is globally optimal
  X <- data.frame(f1 = c(0, 0, 0, 1, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  check_optimality(X, y, lambda = 0.05, loss = "misclassification",
                   label = "separable depth-1")
})

test_that("misclassification: GOSDT matches brute force when stump is optimal", {
  # y is pure noise uncorrelated with X: stump always wins for any lambda > 0
  set.seed(1)
  X <- data.frame(f1 = c(0,0,1,1,0,0,1,1),
                  f2 = c(0,1,0,1,0,1,0,1))
  y <- c(0L,1L,0L,1L,1L,0L,1L,0L)  # no signal
  check_optimality(X, y, lambda = 0.1, loss = "misclassification",
                   label = "noise-stump")
})

test_that("misclassification: GOSDT matches brute force on depth-2 XOR DGP", {
  # XOR: the optimal tree requires depth 2
  X <- data.frame(f1 = c(0, 0, 1, 1, 0, 0, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1, 0, 1))
  y <- as.integer(X$f1 != X$f2)
  # lambda small enough that the depth-2 tree beats the stump
  check_optimality(X, y, lambda = 0.05, loss = "misclassification",
                   max_depth = 3L, label = "XOR depth-2")
})

test_that("misclassification: GOSDT matches brute force on 4-feature DGP", {
  # n=16, p=4: larger but still exhaustively checkable
  set.seed(42)
  f1 <- rep(c(0, 1), 8); f2 <- rep(c(0, 0, 1, 1), 4)
  f3 <- rep(c(0, 0, 0, 0, 1, 1, 1, 1), 2); f4 <- rep(0:1, each = 8)
  X  <- data.frame(f1 = f1, f2 = f2, f3 = f3, f4 = f4)
  y  <- as.integer((f1 + f2 + f3) >= 2)  # majority vote on first 3 features
  check_optimality(X, y, lambda = 0.05, loss = "misclassification",
                   max_depth = 3L, label = "4-feature majority")
})

test_that("squared_error: GOSDT matches brute force on clean regression DGP", {
  # f1 partitions y into two well-separated groups
  X <- data.frame(f1 = c(0, 0, 0, 1, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1))
  y <- c(1.0, 2.0, 1.5, 10.0, 11.0, 10.5)
  check_optimality(X, y, lambda = 0.05, loss = "squared_error",
                   label = "regression depth-1")
})

test_that("squared_error: GOSDT matches brute force on depth-2 regression DGP", {
  # f1 and f2 interact: best tree uses both
  X <- data.frame(f1 = c(0, 0, 1, 1, 0, 0, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1, 0, 1))
  y <- c(1, 5, 5, 1, 1, 5, 5, 1)  # interaction: y = 1 iff f1 == f2
  check_optimality(X, y, lambda = 0.04, loss = "squared_error",
                   max_depth = 3L, label = "regression depth-2 interaction")
})

test_that("squared_error: stump is optimal when lambda is large", {
  X <- data.frame(f1 = c(0, 0, 1, 1), f2 = c(0, 1, 0, 1))
  y <- c(1.0, 3.0, 10.0, 12.0)
  # lambda = 5: cost of any split (lambda * 2 = 10) >> MSE gain (~21.25 -> 1)
  # Actually split wins at lambda=5 (21.25 -> 1.0+10=11 < 5.25+5=10.25 stump)
  # so use lambda=15 to force stump
  check_optimality(X, y, lambda = 15.0, loss = "squared_error",
                   label = "regression forced stump")
})

test_that("log_loss: GOSDT matches brute force on separable DGP", {
  X <- data.frame(f1 = c(0, 0, 0, 1, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  check_optimality(X, y, lambda = 0.05, loss = "log_loss",
                   label = "log_loss separable")
})

test_that("log_loss: GOSDT matches brute force on calibration DGP", {
  # The DGP from test-logloss.R: f2 sharpens calibration within f1==1 group.
  # Replicate with a small n version that's still fast for brute force.
  set.seed(7)
  n <- 16
  f1 <- c(rep(0, 8), rep(1, 8))
  f2 <- rep(c(0, 0, 1, 1), 4)
  y  <- c(rep(0L, 6), rep(1L, 2),  # f1==0: mostly 0
          rep(1L, 4), rep(1L, 4))   # f1==1: all 1 (simplify for small n)
  X  <- data.frame(f1 = f1, f2 = f2)
  check_optimality(X, y, lambda = 0.05, loss = "log_loss",
                   max_depth = 3L, label = "log_loss calibration")
})

test_that("log_loss: stump is optimal when lambda is large", {
  X <- data.frame(f1 = c(0, 0, 1, 1))
  y <- c(0L, 0L, 1L, 1L)
  check_optimality(X, y, lambda = 2.0, loss = "log_loss",
                   label = "log_loss forced stump")
})

# ── scale invariance (sanity) ─────────────────────────────────────────────────

test_that("squared_error objective is invariant to row duplication", {
  X1 <- data.frame(f1 = c(0, 0, 1, 1))
  y1 <- c(1.0, 3.0, 10.0, 12.0)
  X2 <- X1[rep(1:4, 2), , drop = FALSE]; rownames(X2) <- NULL
  y2 <- rep(y1, 2)
  lambda <- 0.1

  o1 <- gosdt_obj(fit_tree(X1, y1, loss_function = "squared_error",
                           regularization = lambda))
  o2 <- gosdt_obj(fit_tree(X2, y2, loss_function = "squared_error",
                           regularization = lambda))
  expect_equal(o1, o2, tolerance = 1e-5,
               label = "n=4 objective", expected.label = "n=8 objective (duplicate rows)")
})

# ── depth constraint enforcement ──────────────────────────────────────────────

# Helper: recursively compute max depth of a tree (list with $feature key at splits)
tree_actual_depth <- function(node, d = 0L) {
  if (is.null(node$feature)) return(d)
  max(tree_actual_depth(node$false, d + 1L), tree_actual_depth(node$true, d + 1L))
}

test_that("max_depth=1 produces depth-1 trees on a depth-2-optimal interaction", {
  # XOR/2-way-AND dataset: depth-1 cannot separate, depth-2 is optimal.
  # With max_depth=1, GOSDT must produce a stump (depth 1).
  X <- data.frame(f1 = c(0, 0, 1, 1, 0, 0, 1, 1),
                  f2 = c(0, 1, 0, 1, 0, 1, 0, 1))
  y <- as.integer(X$f1 == 1L & X$f2 == 1L)  # AND

  m1 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.001, max_depth = 1L)
  m2 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.001, max_depth = 2L)
  m3 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.001, max_depth = 3L)

  expect_lte(tree_actual_depth(m1@trees[[1]]), 1L)
  expect_lte(tree_actual_depth(m2@trees[[1]]), 2L)
  expect_lte(tree_actual_depth(m3@trees[[1]]), 3L)

  # Depth-2 objective should be strictly better than depth-1 on this data
  expect_lt(gosdt_obj(m2), gosdt_obj(m1) - 0.01,
            label = "depth-2 objective strictly better than depth-1 on AND data")
})

test_that("max_depth=2 produces depth-2 trees on a depth-3-optimal 3-way interaction", {
  # 3-way AND: depth-3 splits x1, x2, x3 in sequence.
  # With max_depth=2, GOSDT can only achieve depth-2 (or less).
  n <- 32
  x1 <- rep(c(0L, 1L), 16L); x2 <- rep(c(0L, 0L, 1L, 1L), 8L)
  x3 <- rep(c(rep(0L, 4L), rep(1L, 4L)), 4L)
  X <- data.frame(f1 = x1, f2 = x2, f3 = x3)
  y <- as.integer(x1 == 1L & x2 == 1L & x3 == 1L)

  m2 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.0001, max_depth = 2L)
  m3 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.0001, max_depth = 3L)
  m4 <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.0001, max_depth = 4L)

  expect_lte(tree_actual_depth(m2@trees[[1]]), 2L)
  expect_lte(tree_actual_depth(m3@trees[[1]]), 3L)
  expect_lte(tree_actual_depth(m4@trees[[1]]), 4L)

  # Depth-3 objective should be strictly better than depth-2 on this data
  expect_lt(gosdt_obj(m3), gosdt_obj(m2) - 0.05,
            label = "depth-3 objective strictly better than depth-2 on 3-way AND")
  # Depth-4 should match depth-3 (no improvement beyond depth-3)
  expect_equal(gosdt_obj(m4), gosdt_obj(m3), tolerance = 1e-4,
               label = "depth-4 objective matches depth-3 (3-way AND is fully captured)")
})
