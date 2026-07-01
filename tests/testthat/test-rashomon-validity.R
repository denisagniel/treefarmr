# Rashomon set validity verification
#
# Verifies two properties of the enumerated Rashomon set:
# (1) BOUND VALIDITY: every enumerated tree satisfies the radius constraint,
#     i.e., model_objective <= optimal * (1 + multiplier) + tol
# (2) OPTIMUM CORRECTNESS: the best tree in the set matches the globally
#     optimal objective from brute-force enumeration (same check as
#     test-brute-force-optimality.R, but now through fit_rashomon).
#
# Note: model_objective is NOT the same as the penalized objective in the
# brute-force test. GOSDT's model_objective = summed_mean_loss + lambda * n_leaves
# which equals the brute-force objective under mean-normalized losses.
# We reuse the same brute-force helpers here.

# ── brute-force helpers (same as test-brute-force-optimality.R) ──────────────

leaf_loss_bf <- function(y, loss) {
  n <- length(y)
  if (n == 0L) return(0)
  switch(loss,
    log_loss = {
      eps <- 1e-12
      p1 <- sum(y) / n; p0 <- 1 - p1
      -(max(eps, min(1 - eps, p1)) * log(max(eps, min(1 - eps, p1))) +
        max(eps, min(1 - eps, p0)) * log(max(eps, min(1 - eps, p0)))) * n
    },
    squared_error = { mu <- mean(y); sum((y - mu)^2) },
    stop("unsupported loss: ", loss)
  )
}

min_tree_obj_bf <- function(X, y, rows, lambda, loss, max_depth, n_global) {
  y_sub <- y[rows]; n_sub <- length(rows)
  stump <- leaf_loss_bf(y_sub, loss) / n_global + lambda
  if (max_depth == 0L || n_sub <= 1L) return(stump)
  best <- stump
  for (j in seq_len(ncol(X))) {
    left  <- rows[X[rows, j] == 1]; right <- rows[X[rows, j] == 0]
    if (!length(left) || !length(right)) next
    lo <- min_tree_obj_bf(X, y, left,  lambda, loss, max_depth - 1L, n_global)
    ro <- min_tree_obj_bf(X, y, right, lambda, loss, max_depth - 1L, n_global)
    if (lo + ro < best) best <- lo + ro
  }
  best
}

brute_force_min <- function(X, y, lambda, loss, max_depth = 4L)
  min_tree_obj_bf(X, y, seq_len(nrow(X)), lambda, loss, max_depth, nrow(X))

# ── validity helpers ──────────────────────────────────────────────────────────

check_rashomon_validity <- function(m, multiplier, label = "") {
  objs <- sapply(m@trees, function(t) t$model_objective)
  opt  <- min(objs)

  # All trees must satisfy the radius bound
  bound <- opt * (1 + multiplier)
  within <- all(objs <= bound + 1e-5)
  expect_true(within,
    label = paste0("Rashomon bound [", label, "]: all objs <= opt*(1+eps)"))

  # The set must contain at least the optimal tree (minimum objective)
  expect_true(m@n_trees >= 1L,
    label = paste0("Rashomon non-empty [", label, "]"))

  opt
}

# ── tests ─────────────────────────────────────────────────────────────────────

test_that("log_loss Rashomon: bound valid and optimum matches brute force", {
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L))
  y <- as.integer(X$f1 != X$f2)  # XOR: needs depth-2 tree
  lambda <- 0.05; mult <- 0.3

  m <- fit_rashomon(X, y, loss_function = "log_loss",
                    regularization = lambda,
                    rashomon_bound_multiplier = mult,
                    max_depth = 3L)

  opt_got <- check_rashomon_validity(m, mult, "log_loss XOR")

  # Optimum must match brute-force
  opt_bf <- brute_force_min(X, y, lambda, "log_loss", max_depth = 3L)
  expect_equal(opt_got, opt_bf, tolerance = 1e-4,
               label = "Rashomon optimum [log_loss XOR]",
               expected.label = "brute-force minimum")
})

test_that("squared_error Rashomon: bound valid and optimum matches brute force", {
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L))
  y <- c(1, 5, 5, 1, 1, 5, 5, 1)  # interaction
  lambda <- 0.04; mult <- 0.3

  m <- fit_rashomon(X, y, loss_function = "squared_error",
                    regularization = lambda,
                    rashomon_bound_multiplier = mult,
                    max_depth = 3L)

  opt_got <- check_rashomon_validity(m, mult, "squared_error interaction")

  opt_bf <- brute_force_min(X, y, lambda, "squared_error", max_depth = 3L)
  expect_equal(opt_got, opt_bf, tolerance = 1e-4,
               label = "Rashomon optimum [squared_error interaction]",
               expected.label = "brute-force minimum")
})

test_that("Rashomon set grows with larger multiplier", {
  X <- data.frame(f1 = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L))
  y <- as.integer(c(0,0,1,1,0,0,1,1,0,1,0,1))
  lambda <- 0.05

  m_tight <- fit_rashomon(X, y, loss_function = "log_loss",
                           regularization = lambda,
                           rashomon_bound_multiplier = 0.05,
                           max_depth = 3L)
  m_wide  <- fit_rashomon(X, y, loss_function = "log_loss",
                           regularization = lambda,
                           rashomon_bound_multiplier = 0.5,
                           max_depth = 3L)

  # Wider radius must contain at least as many trees
  expect_gte(m_wide@n_trees, m_tight@n_trees)

  # Both sets must be valid under their own multiplier
  check_rashomon_validity(m_tight, 0.05, "tight")
  check_rashomon_validity(m_wide,  0.5,  "wide")
})

test_that("additive Rashomon bound: trees within additive radius", {
  X <- data.frame(f1 = c(0L, 0L, 0L, 1L, 1L, 1L))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  lambda <- 0.05; adder <- 0.2

  m <- fit_rashomon(X, y, loss_function = "log_loss",
                    regularization = lambda,
                    rashomon_bound_adder = adder,
                    rashomon_bound_multiplier = 0,  # use adder, not multiplier
                    max_depth = 3L)

  objs <- sapply(m@trees, function(t) t$model_objective)
  opt  <- min(objs)
  bound <- opt + adder

  expect_true(all(objs <= bound + 1e-5),
              label = "Additive bound: all objs <= opt + adder")
  expect_true(m@n_trees >= 1L)
})

test_that("Rashomon optimum equals single-tree optimum", {
  # The best tree in the Rashomon set must equal the single-tree fit
  X <- data.frame(f1 = c(0L, 0L, 0L, 1L, 1L, 1L),
                  f2 = c(0L, 1L, 0L, 1L, 0L, 1L))
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  lambda <- 0.05

  m_single <- fit_tree(X, y, loss_function = "log_loss",
                        regularization = lambda, max_depth = 3L)
  m_rash   <- fit_rashomon(X, y, loss_function = "log_loss",
                            regularization = lambda,
                            rashomon_bound_multiplier = 0.05,
                            max_depth = 3L)

  obj_single <- m_single@trees[[1]]$model_objective
  obj_rash   <- min(sapply(m_rash@trees, function(t) t$model_objective))

  expect_equal(obj_rash, obj_single, tolerance = 1e-4,
               label = "Rashomon optimum", expected.label = "single-tree optimum")
})
