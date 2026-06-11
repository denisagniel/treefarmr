# Helper: DGP generators for complex-structure tests
#
# Each function returns list(X, y, truth) where:
#   X     = data.frame of features
#   y     = observed outcome (binary integer for classification, numeric for regression)
#   truth = true probability (classification) or conditional mean (regression)
#
# DGP taxonomy:
#   B1-B5  Binary-only features (no discretization needed)
#   C1-C5  Continuous features (core test of new quantiles/adaptive defaults)
#   R1-R2  Regression (squared_error loss)
#   A1     Adaptive bin scaling (2 continuous cols, n is required arg)

# ---- Category B: Binary-only ------------------------------------------------

dgp_b1_single_split <- function(n = 200, seed = 1) {
  set.seed(seed)
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE)
  )
  truth <- as.numeric(X$x1)
  y <- as.integer(truth)
  list(X = X, y = y, truth = truth)
}

dgp_b2_xor <- function(n = 300, seed = 1) {
  set.seed(seed)
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE)
  )
  truth <- as.numeric(X$x1 != X$x2)
  y <- as.integer(truth)
  list(X = X, y = y, truth = truth)
}

dgp_b3_compound <- function(n = 300, seed = 1) {
  set.seed(seed)
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE),
    x4 = sample(0:1, n, replace = TRUE),
    x5 = sample(0:1, n, replace = TRUE)
  )
  truth <- as.numeric((X$x1 & X$x2) | (X$x3 & !X$x4))
  y <- as.integer(truth)
  list(X = X, y = y, truth = truth)
}

dgp_b4_noisy <- function(n = 500, seed = 1) {
  set.seed(seed)
  X <- data.frame(
    x1 = sample(0:1, n, replace = TRUE),
    x2 = sample(0:1, n, replace = TRUE),
    x3 = sample(0:1, n, replace = TRUE),
    x4 = sample(0:1, n, replace = TRUE),
    x5 = sample(0:1, n, replace = TRUE)
  )
  truth <- 0.1 + 0.7 * X$x1 * X$x2
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

dgp_b5_highdim <- function(n = 500, p = 20, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(sample(0:1, n * p, replace = TRUE), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- 0.2 + 0.4 * X$x1 * X$x2 + 0.2 * X$x3
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# ---- Category C: Continuous features ----------------------------------------

dgp_c1_linear <- function(n = 400, seed = 1) {
  set.seed(seed)
  x1 <- runif(n, -1, 1)
  X <- data.frame(x1 = x1, x2 = sample(0:1, n, replace = TRUE))
  truth <- plogis(2 * x1 - 1.5)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# KEY TEST DGP: symmetric quadratic. Old defaults (median) cannot learn this.
# Truth = plogis(-1 + 3*x1^2): symmetric around x1=0.
# Median split at x1=0 has zero information (both halves have same E[truth]).
# Quantile thresholds at ~+/-0.43 (n>=404) create middle vs tail groups,
# correctly capturing the U-shape. See audit: AUDIT_COMPLEX_STRUCTURES_2026-06-10.md
dgp_c2_symmetric_quadratic <- function(n = 800, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n, 0, 1)
  X <- data.frame(x1 = x1, x2 = sample(0:1, n, replace = TRUE))
  truth <- plogis(-1 + 3 * x1^2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

dgp_c3_step <- function(n = 500, seed = 1) {
  set.seed(seed)
  x1 <- runif(n, 0, 1)
  X <- data.frame(x1 = x1, x2 = sample(0:1, n, replace = TRUE))
  truth <- 0.1 + 0.6 * as.numeric(x1 > 0.33) + 0.2 * as.numeric(x1 > 0.67)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

dgp_c4_bivariate <- function(n = 600, seed = 1) {
  set.seed(seed)
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  X <- data.frame(x1 = x1, x2 = x2)
  truth <- plogis(x1 + x2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# DGP4 propensity from doubletree simulations: x1, x3, x4 continuous; x2 binary.
dgp_c5_dgp4_propensity <- function(n = 600, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n, 0, 1)
  x2 <- sample(0:1, n, replace = TRUE)
  x3 <- rnorm(n, 0, 1)
  x4 <- rnorm(n, 0, 1)
  X <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  truth <- plogis(-0.5 + 0.3 * x1 + 0.4 * x3 + 0.2 * x4 + 0.2 * x1 * x3)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# ---- Category R: Regression -------------------------------------------------

dgp_r1_regression <- function(n = 400, seed = 1) {
  set.seed(seed)
  x1 <- runif(n, -1, 1)
  X <- data.frame(x1 = x1, x2 = sample(0:1, n, replace = TRUE))
  truth <- 2 * x1
  y <- truth + rnorm(n, 0, 0.5)
  list(X = X, y = y, truth = truth)
}

# Symmetric quadratic for regression: same structural issue as C2 but MSE loss.
dgp_r2_regression_quadratic <- function(n = 600, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n, 0, 1)
  X <- data.frame(x1 = x1, x2 = sample(0:1, n, replace = TRUE))
  truth <- x1^2
  y <- truth + rnorm(n, 0, 0.3)
  list(X = X, y = y, truth = truth)
}

# ---- Category A: Adaptive bin scaling ---------------------------------------

# n is a required arg. Returns 2 continuous features.
# Used to verify adaptive formula: n_bins = max(2, ceiling(log(n)/3)).
dgp_a1_adaptive_bins <- function(n, seed = 1) {
  set.seed(seed)
  X <- data.frame(
    x1 = rnorm(n, 0, 1),
    x2 = runif(n, 0, 1)
  )
  truth <- plogis(X$x1 + X$x2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# ---- Category HD: Higher-dimensional (variable p) ---------------------------

# C6: Additive logistic, p continuous features.
# Signal in first min(3, p) features (equal weight 0.5 each); rest are noise.
# Design: log_loss is the appropriate loss for this propensity-style DGP.
dgp_c6_additive_logistic <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  n_signal <- min(3L, p)
  signal <- Reduce(`+`, lapply(paste0("x", seq_len(n_signal)),
                               function(col) 0.5 * X[[col]]))
  truth <- plogis(signal)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# R3: Additive regression, p continuous features.
# Signal only in x1 + 0.5*x2; remaining features are pure noise.
# Demonstrates that regularization suppresses irrelevant features regardless of p.
dgp_r3_additive_regression <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- X$x1 + 0.5 * X$x2
  y <- truth + rnorm(n, 0, 1)
  list(X = X, y = y, truth = truth)
}
