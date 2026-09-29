# ============================================================
# DGP for the grid-resolution / threshold-rate study
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 3, "Data generating process")
# Purpose: one jump boundary (X1) plus one smooth ("cosmetic") boundary per
#          side (X2), so a fixed 4-leaf depth-2 tree has one genuinely
#          discontinuous split and two budget-forced ones.
# Inputs: none
# Outputs: none (function definitions only; source this file)
# ============================================================
#
# Follows simulations/dgps.R's convention:
#   dgp_<name>(n, p, seed) -> list(X, y, truth, ...)
# extended (per spec Section 9) with the DGP parameter set, so every
# downstream consumer of a generated sample can assert it was drawn under
# the parameters its ground truth was computed for rather than assuming it.
#
# This file deliberately does NOT reuse simulations/sim_rate_estimation.R's or
# adaptive_bins_simple.R's bin schedule / regularization conventions: those
# belong to the abandoned growing-complexity program (spec Section 0). The
# grid resolution m_n is an explicit per-configuration design variable here.

# ---- 0 DGP parameter set --------------------------------------------------

#' Seed the RNG for this study, pinning the generator explicitly
#'
#' \strong{`set.seed(seed)` alone is NOT reproducible across execution
#' contexts.} With `kind = NULL` (the default) `set.seed()` keeps whatever
#' generator is currently active, so the same integer seed yields a DIFFERENT
#' stream depending on ambient state. That is not hypothetical: running these
#' replications under `furrr` with `furrr_options(seed = TRUE)` puts each worker
#' on `"L'Ecuyer-CMRG"`, and the identical seed then produced visibly different
#' data and different fitted thresholds than the sequential run -- caught by
#' comparing a parallel slice against already-recorded sequential results, which
#' is the only reason it did not silently corrupt a 4,800-cell run.
#'
#' Pinning all three components makes a replication's data a function of its
#' seed ALONE: sequential or parallel, any backend, any session that happens to
#' have called `RNGkind()`. The pinned values are R's own post-3.6 defaults, so
#' this reproduces every result recorded before it existed (verified, not
#' assumed).
seed_study_rng <- function(seed) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
            sample.kind = "Rejection")
}

#' Canonical DGP parameters for this study
#'
#' Every quantity spec Section 3 fixes, in one place, with the
#' favorable-regime (Regime F) values as defaults. `delta1`, `sigma`, and `p`
#' are the manipulated quantities in the stress regimes (S1, S2); `t1`,
#' `a_L`, `b_L`, `b_R` are fixed across every regime in the design.
#'
#' The two smooth pieces (spec Section 3, as revised 2026-09-28) are
#'   x1 <= t1 : g_L(x2) = a_L + b_L * x2^2
#'   x1 >  t1 : g_R(x2) = (a_L + delta1) + b_R * (1 - x2)^2     [MIRRORED]
#' The right piece is mirrored in x2 for a derived reason, not a stylistic
#' one: for `a + b*x2^2` under Uniform(0,1) the population-optimal single
#' cutpoint is `(1 + sqrt(17))/8` for EVERY `(a, b)` (see
#' `optimal_poly_cut()` in ground_truth.R), so a non-mirrored right piece can
#' never place `t2*_R` anywhere other than `t2*_L`. Mirroring maps the same
#' maximizer through `x2 -> 1 - x2`, giving `t2*_R = 1 - (1 + sqrt(17))/8`.
#'
#' Each side is returned as a coefficient triple `c(a0, a1, a2)` of
#' `a0 + a1*x2 + a2*x2^2` (`left_coef`, `right_coef`). Expanding the mirrored
#' piece, `(a_L + delta1) + b_R*(1 - x2)^2` has triple
#' `c(a_L + delta1 + b_R, -2*b_R, b_R)`. Every ground-truth quantity (optimal
#' cutpoint, leaf mean, population risk) is a closed-form polynomial integral
#' that does not care where the coefficients came from, so the triple form is
#' what the machinery consumes -- which is also what makes it reusable across
#' Regime S1's `delta1` ladder and any later change of smooth form without a
#' rewrite.
dgp_params_jump_cosmetic <- function(delta1 = 5, sigma = 0.5, t1 = 0.5,
                                      a_L = 0, b_L = 3, b_R = 3) {
  stopifnot(
    is.numeric(delta1), length(delta1) == 1L, is.finite(delta1),
    is.numeric(sigma), length(sigma) == 1L, sigma > 0,
    is.numeric(t1), length(t1) == 1L, t1 > 0, t1 < 1,
    is.numeric(a_L), length(a_L) == 1L, is.finite(a_L),
    is.numeric(b_L), length(b_L) == 1L, is.finite(b_L),
    is.numeric(b_R), length(b_R) == 1L, is.finite(b_R)
  )
  list(
    delta1 = delta1, sigma = sigma, t1 = t1,
    a_L = a_L, b_L = b_L, b_R = b_R,
    # Vertical offset of the right piece (its value at x2 = 1, where the
    # mirrored quadratic's own vertex sits).
    offset_right = a_L + delta1,
    left_coef  = c(a_L, 0, b_L),
    right_coef = c(a_L + delta1 + b_R, -2 * b_R, b_R)
  )
}

#' Evaluate a quadratic coefficient triple a0 + a1*x + a2*x^2
poly_eval <- function(coef, x) coef[[1L]] + coef[[2L]] * x + coef[[3L]] * x^2

# ---- 1 True regression function ------------------------------------------

#' gamma_0(x1, x2), exactly as spec Section 3 writes it
#'
#' Evaluated through `params$left_coef` / `params$right_coef`, which for the
#' spec's parameterization ARE `a_L + b_L*x2^2` and
#' `(a_L + delta1) + (b_L + b_R)*x2^2` (see `dgp_params_jump_cosmetic()`).
#'
#' @param x1,x2 Numeric vectors of equal length (or length-1, recycled).
#' @param params A `dgp_params_jump_cosmetic()` list.
#' @return Numeric vector of the same length as `x1`/`x2`.
gamma_0_jump_cosmetic <- function(x1, x2, params = dgp_params_jump_cosmetic()) {
  ifelse(x1 <= params$t1,
         poly_eval(params$left_coef, x2),
         poly_eval(params$right_coef, x2))
}

#' Jump of gamma_0 across x1 = t1, as a function of x2
#'
#' `gamma_0(t1+, x2) - gamma_0(t1-, x2)`. For the JUMP boundary's ground truth
#' (`t1* = t1` exactly) to follow from the change-point argument spec Section 3
#' cites, this must be bounded away from zero on the whole support: a jump that
#' vanishes at some x2 means gamma_0 is CONTINUOUS across x1 = t1 there, and
#' the "exactly by construction, no estimation needed" claim no longer covers
#' the whole boundary.
#'
#' For the revised (mirrored) form with `b_L = b_R = b` this reduces to
#' `delta1 + b*(1 - 2*x2)`, ranging over `[delta1 - b, delta1 + b]` -- bounded
#' away from zero iff `delta1 > b`. Regime S1's most severe point
#' (`delta1 = 2.5 < b_R = 3`) deliberately violates that, which is why this is
#' computed generically from the coefficient triples rather than from a
#' closed-form specialized to `delta1 > b_R`.
gamma_0_jump_profile <- function(x2, params = dgp_params_jump_cosmetic()) {
  poly_eval(params$right_coef, x2) - poly_eval(params$left_coef, x2)
}

#' Points in [0,1] where the jump across x1 = t1 vanishes
#'
#' Exact real roots of the quadratic `right_coef - left_coef` restricted to
#' `[0,1]`. Returns `numeric(0)` when the jump never vanishes on the support
#' (the condition Regime F satisfies and Regime S1's last rung breaks).
gamma_0_jump_zeros <- function(params = dgp_params_jump_cosmetic()) {
  d <- params$right_coef - params$left_coef
  c0 <- d[[1L]]; c1 <- d[[2L]]; c2 <- d[[3L]]
  tiny <- .Machine$double.eps^0.5
  roots <- if (abs(c2) < tiny) {
    if (abs(c1) < tiny) {
      # Jump is a nonzero constant (no roots) or identically zero (no jump at
      # all, which is a different DGP and must not be silently treated as
      # "no roots").
      if (abs(c0) < tiny) {
        cli::cli_abort(
          "gamma_0_jump_zeros: the two smooth pieces are identical, so there \\
           is no jump at x1 = t1 at all -- this is not the DGP spec Section 3 \\
           describes."
        )
      }
      numeric(0)
    } else {
      -c0 / c1
    }
  } else {
    disc <- c1^2 - 4 * c2 * c0
    if (disc < 0) numeric(0) else (-c1 + c(-1, 1) * sqrt(disc)) / (2 * c2)
  }
  sort(unique(roots[roots >= 0 & roots <= 1]))
}

# ---- 2 Sampler -----------------------------------------------------------

#' Draw one sample from the jump + cosmetic DGP
#'
#' X1, ..., Xp i.i.d. Uniform(0,1); gamma_0 depends on (X1, X2) only, so the
#' p - 2 extra coordinates are pure noise (spec Section 4, Regime S2).
#'
#' @param n Sample size.
#' @param p Number of covariates (>= 2). Default 2 (Regimes F and Q2).
#' @param delta1 Jump-size design parameter. Default 5 (Regime F). Regime S1
#'   ladders this down through {4.5, 3.5, 2.5}.
#' @param seed Integer seed for this replication's data. Defaults to the
#'   study's own base seed so sourcing this file and calling the function
#'   with no seed still yields working data (interactive-entry M1).
#' @param params A `dgp_params_jump_cosmetic()` list. `delta1` overrides its
#'   `delta1` field, so callers can vary the jump size without rebuilding the
#'   whole parameter list.
#' @return list(X = data.frame with columns x1..xp, y, truth = gamma_0(X),
#'   params, seed, n, p).
dgp_jump_cosmetic <- function(n, p = 2, delta1 = 5, seed = 20260928L,
                               params = dgp_params_jump_cosmetic()) {
  if (!is.numeric(n) || length(n) != 1L || n < 2) {
    cli::cli_abort("dgp_jump_cosmetic: {.arg n} must be a single integer >= 2.")
  }
  if (!is.numeric(p) || length(p) != 1L || p < 2) {
    cli::cli_abort(
      "dgp_jump_cosmetic: {.arg p} must be >= 2 -- gamma_0 depends on both \\
       x1 and x2, so p < 2 is not a coarser version of this DGP, it is a \\
       different one."
    )
  }
  n <- as.integer(n)
  p <- as.integer(p)
  # Rebuild every DERIVED field from the (possibly overridden) delta1 rather
  # than trusting the ones already in `params`: a stale offset/coefficient
  # triple is exactly how a Regime S1 rung would silently inherit Regime F's
  # ground truth.
  params <- dgp_params_jump_cosmetic(delta1 = delta1, sigma = params$sigma,
                                     t1 = params$t1, a_L = params$a_L,
                                     b_L = params$b_L, b_R = params$b_R)

  seed_study_rng(seed)
  X <- as.data.frame(matrix(stats::runif(n * p), nrow = n, ncol = p))
  colnames(X) <- paste0("x", seq_len(p))

  truth <- gamma_0_jump_cosmetic(X$x1, X$x2, params)
  y <- truth + stats::rnorm(n, 0, params$sigma)

  list(X = X, y = y, truth = truth, params = params,
       seed = as.integer(seed), n = n, p = p)
}
