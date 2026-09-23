# Shared DGP definitions for server simulation scripts.
# Source this file at the top of each simulation script:
#   source(file.path(dirname(normalizePath(sys.frames()[[1]]$ofile)), "dgps.R"))
# Or with explicit path:
#   source("<absolute path to simulations/dgps.R>")
#
# DGPs used in optimaltrees standalone paper simulations:
#   R2: Quadratic regression (symmetric, squared_error)
#   C2: Symmetric quadratic propensity (log_loss)
#   R3: Additive regression, variable p (squared_error)
#   C6: Additive logistic, variable p (log_loss)
#   C8: Overlap stress propensity (log_loss)

# R2: Symmetric quadratic regression.
# truth = x1^2, x1 ~ N(0,1). Null RMSE = sqrt(Var(x1^2)) = sqrt(2) ≈ 1.414.
# Key structural challenge: symmetric around x1=0, median split has zero information.
dgp_r2 <- function(n, p = 2, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- X$x1^2
  y <- truth + rnorm(n, 0, 0.3)
  list(X = X, y = y, truth = truth, null_rmse = sqrt(2))
}

# C2: Symmetric quadratic propensity.
# truth = plogis(-1 + 3*x1^2), x1 ~ N(0,1).
# Null RMSE ≈ 0.135 (computed numerically below).
dgp_c2 <- function(n, p = 2, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- plogis(-1 + 3 * X$x1^2)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# R3: Additive regression with variable number of features.
# truth = x1 + 0.5*x2; remaining p-2 features are pure noise.
# Null RMSE = sqrt(Var(x1 + 0.5*x2)) = sqrt(1.25) ≈ 1.118.
dgp_r3 <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- X$x1 + 0.5 * X$x2
  y <- truth + rnorm(n, 0, 1)
  list(X = X, y = y, truth = truth, null_rmse = sqrt(1.25))
}

# C6: Additive logistic with variable number of features.
# truth = plogis(0.5*x1 + 0.5*x2 + 0.5*x3); remaining p-3 features are noise.
# Signal capped at 3 features regardless of p.
dgp_c6 <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  n_signal <- min(3L, p)
  signal <- rowSums(sapply(seq_len(n_signal), function(j) 0.5 * X[[j]]))
  truth <- plogis(signal)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# C8: Propensity overlap stress (DML use case).
# truth = plogis(x1 + 2*x2*(x2 > 1)), x1, x2, ... ~ N(0,1).
# Creates near-zero/near-one propensities in tails of x2.
# P(x2 > 1) ≈ 0.16; in that region truth approaches 1.
# Signal features: x1, x2. Noise features: x3, ..., xp.
dgp_c8 <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- plogis(X$x1 + 2 * X$x2 * (X$x2 > 1))
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# R4: Pure interaction regression (requires depth >= 2).
# truth = 2 * (x1 > 0) * (x2 > 0) — four-quadrant step function.
# No main effects: depth-1 stump can only capture E[truth|x1>0] - E[truth|x1<0] = 0.
# A depth-2 tree can perfectly represent the function.
# Null RMSE = sqrt(Var(2*(x1>0)*(x2>0))) = sqrt(0.25) = 0.5.
dgp_r4 <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- 2 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0)
  y <- truth + rnorm(n, 0, 0.5)
  list(X = X, y = y, truth = truth, null_rmse = 0.5)
}

# C4: Pure interaction classification (requires depth >= 2).
# truth = plogis(3 * (x1 > 0) * (x2 > 0) - 0.75) — four-quadrant propensity.
# Depth-1 tree predicts same value for all x1>0 observations regardless of x2.
# Depth-2 tree can separate the four quadrants.
dgp_c4 <- function(n, p = 4, seed = 1) {
  set.seed(seed)
  X <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))
  truth <- plogis(3 * as.numeric(X$x1 > 0) * as.numeric(X$x2 > 0) - 0.75)
  y <- rbinom(n, 1, truth)
  list(X = X, y = y, truth = truth)
}

# Compute null RMSE for C2 (predicting marginal mean of truth)
null_rmse_c2 <- local({
  set.seed(1L)
  x <- rnorm(1e5)
  truth <- plogis(-1 + 3 * x^2)
  sqrt(mean((truth - mean(truth))^2))
})

# Compute null RMSE for C6 (p=4 version)
null_rmse_c6 <- local({
  set.seed(1L)
  X <- matrix(rnorm(1e5 * 3L), ncol = 3L)
  truth <- plogis(0.5 * X[, 1] + 0.5 * X[, 2] + 0.5 * X[, 3])
  sqrt(mean((truth - mean(truth))^2))
})

# Compute null RMSE for C8 (p=4 version)
null_rmse_c8 <- local({
  set.seed(1L)
  x1 <- rnorm(1e5); x2 <- rnorm(1e5)
  truth <- plogis(x1 + 2 * x2 * (x2 > 1))
  sqrt(mean((truth - mean(truth))^2))
})

# Compute null RMSE for C4
null_rmse_c4 <- local({
  set.seed(1L)
  x1 <- rnorm(1e5); x2 <- rnorm(1e5)
  truth <- plogis(3 * as.numeric(x1 > 0) * as.numeric(x2 > 0) - 0.75)
  sqrt(mean((truth - mean(truth))^2))
})
