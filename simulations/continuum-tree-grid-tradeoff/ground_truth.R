# ============================================================
# Closed-form ground truth (f*, t1*, t2*_L, t2*_R) for the
# grid-resolution / threshold-rate study
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 3, "Ground truth, and how it is computed to negligible error")
# Purpose: everything the excess-risk and threshold-error metrics measure
#          against, computed exactly (closed-form polynomial integrals) rather
#          than estimated, plus the brute-force cross-checks that make
#          "negligible error" a measured claim instead of an assumed one.
# Inputs: dgp.R (sourced by the caller, or by this file's sibling scripts)
# Outputs: none (function definitions only; source this file)
# ============================================================
#
# Every function here takes the DGP parameter set as an argument. Nothing is
# hardcoded to the favorable regime: `delta1` varies across Regime S1, so a
# ground-truth quantity baked to delta1 = 2 would be silently wrong there
# (spec Section 3's topology-verification clause is explicit that f* must be
# recomputed per delta1, never reused).

# ---- 1 Optimal single cutpoint for one smooth piece -----------------------
#
# Spec Section 3: for g(x2) = a + b*x2^2 on [0,1] under X2 ~ Uniform(0,1),
# the population-SSE-optimal single cutpoint t for a 2-piece constant
# approximation has closed-form leaf means
#   c1(t) = (1/t) * int_0^t g = a + b*t^2/3
#   c2(t) = (1/(1-t)) * int_t^1 g = a + b*(1 + t + t^2)/3
# (both elementary, since int_0^t x^2 dx = t^3/3 and
#  int_t^1 x^2 dx = (1 - t^3)/3, and (1 - t^3)/(3(1 - t)) = (1+t+t^2)/3).
#
# Writing SSE(t) = int_0^t (g - c1)^2 + int_t^1 (g - c2)^2 and expanding, the
# cross terms cancel against the leaf-mean definitions, leaving
#   S(t) = int_0^1 g^2 - t*c1(t)^2 - (1-t)*c2(t)^2
# with int_0^1 g^2 = a^2 + 2ab/3 + b^2/5. That is a polynomial in t, exactly
# as the spec says.
#
# Implemented once, for a general quadratic coefficient triple
# coef = c(a0, a1, a2) meaning a0 + a1*x + a2*x^2, with the spec's own
# (a1 == 0) case as a thin named alias. The extra generality costs nothing and
# is what lets a revised smooth piece reuse this machinery unchanged.

#' Raw moments int_lo^hi x^j dx for j = 0..4
poly_moments_01 <- function(lo, hi) {
  vapply(0:4, function(j) (hi^(j + 1) - lo^(j + 1)) / (j + 1), numeric(1))
}

#' int_lo^hi g and int_lo^hi g^2 for g(x) = a0 + a1*x + a2*x^2
poly_integrals <- function(coef, lo = 0, hi = 1) {
  m <- poly_moments_01(lo, hi)
  a0 <- coef[[1L]]; a1 <- coef[[2L]]; a2 <- coef[[3L]]
  list(
    i1 = a0 * m[[1L]] + a1 * m[[2L]] + a2 * m[[3L]],
    i2 = a0^2 * m[[1L]] + 2 * a0 * a1 * m[[2L]] +
      (a1^2 + 2 * a0 * a2) * m[[3L]] + 2 * a1 * a2 * m[[4L]] +
      a2^2 * m[[5L]]
  )
}

#' Population SSE of the best 2-piece constant fit to a quadratic, at cutpoint t
#'
#' @param t Cutpoint(s) in [0, 1] (vectorized).
#' @param coef Coefficient triple `c(a0, a1, a2)`.
#' @return Numeric vector of the same length as `t`.
poly_split_sse <- function(t, coef) {
  a0 <- coef[[1L]]; a1 <- coef[[2L]]; a2 <- coef[[3L]]
  # c1(t) = (1/t) int_0^t g ; c2(t) = (1/(1-t)) int_t^1 g, both reduced so no
  # 0/0 arises at t = 0 or t = 1.
  c1 <- a0 + a1 * t / 2 + a2 * t^2 / 3
  c2 <- a0 + a1 * (1 + t) / 2 + a2 * (1 + t + t^2) / 3
  poly_integrals(coef)$i2 - t * c1^2 - (1 - t) * c2^2
}

#' Spec-faithful alias: SSE for g(x) = a + b*x^2
quadratic_split_sse <- function(t, a, b) poly_split_sse(t, c(a, 0, b))

#' Population-optimal single cutpoint for a quadratic piece, by 1-D minimization
#'
#' @param coef Coefficient triple `c(a0, a1, a2)`.
#' @param tol `optimize()` tolerance. Spec Section 3 asks for 1e-10; note
#'   `optimize()` cannot in practice resolve the location of a smooth interior
#'   minimum below about `.Machine$double.eps^0.5` (~1.5e-8), so 1e-10 means
#'   "iterate to the achievable floor", which is still two orders of magnitude
#'   below the 1e-6 agreement tolerance the cross-check demands and six below
#'   the smallest statistical effect (O(n^-1/2) at n = 500 is ~4e-2).
#' @return list(t, sse, analytic_t, n_optima).
#'   `analytic_t` is the exact stationary point in the pure-quadratic
#'   (`a1 == 0`) case, `NA` otherwise -- a third, fully independent value used
#'   only for cross-checking.
#'   `n_optima` counts distinct near-global minima found on a dense scan; more
#'   than one means the cutpoint is not uniquely identified even though a
#'   minimizer exists.
optimal_poly_cut <- function(coef, tol = 1e-10) {
  if (!is.numeric(coef) || length(coef) != 3L || any(!is.finite(coef))) {
    cli::cli_abort("optimal_poly_cut: {.arg coef} must be 3 finite numbers.")
  }
  # A piece with no x-dependence is NOT a degenerate edge case to paper over
  # with an NA: it is fit exactly by a single constant, so EVERY cutpoint
  # attains the same population SSE (zero) and there is no t* to converge to.
  # Any threshold-error metric computed against one would be meaningless.
  # Fail loudly (interactive-entry F1).
  if (max(abs(coef[2:3])) < .Machine$double.eps^0.5) {
    cli::cli_abort(c(
      "optimal_poly_cut: the smooth piece {.val {coef}} is constant in x.",
      "x" = "Every cutpoint attains the same population SSE (zero), so the \\
             optimal cutpoint is NOT IDENTIFIED -- there is no t* for a \\
             threshold-error metric to target.",
      "i" = "This is a property of the DGP parameters, not a numerical \\
             failure: check whether the smooth piece was meant to vary in x2."
    ), class = "continuum_tree_cut_not_identified")
  }
  o <- stats::optimize(poly_split_sse, interval = c(0, 1), coef = coef,
                        tol = tol)
  # Dense scan for a second, comparably good minimum. `optimize()` returns one
  # local minimum with no statement about uniqueness, and a tied pair of
  # optima is a real possibility for a quadratic with a linear term (e.g.
  # coef = c(0, -3, 3) has two exactly symmetric optima) -- silently reporting
  # one of them as "the" t* would be a fabricated ground truth.
  scan_t <- seq(1e-6, 1 - 1e-6, length.out = 20001L)
  scan_s <- poly_split_sse(scan_t, coef)
  near <- scan_s <= o$objective + 1e-10 * max(1, abs(o$objective))
  # Count contiguous runs of near-optimal points: one run = one optimum.
  n_optima <- if (!any(near)) 1L else sum(diff(c(FALSE, near)) == 1L)

  # Analytic stationary point, available whenever the quadratic's vertex sits
  # at an endpoint of [0,1]. Decomposing S(t) = Var(g) - Explained(t) for the
  # vertex-at-0 case g = a + b*x^2, with Explained(t) = t*(c1-gbar)^2 +
  # (1-t)*(c2-gbar)^2 and gbar = a + b/3, gives
  #   Explained(t) = (b^2/9) * t*(1-t)*(1+t)^2,
  # so t* maximizes h(t) = t + t^2 - t^3 - t^4 and solves
  #   h'(t) = 1 + 2t - 3t^2 - 4t^3 = -(t+1)(4t^2 - t - 1) = 0
  # whose only root in (0,1) is (1 + sqrt(17))/8.
  #
  # NOTE (a real, load-bearing consequence, not a curiosity): Explained(t)
  # depends on (a, b) ONLY through the multiplicative factor b^2/9. The
  # MAXIMIZER is therefore the same for every intercept a and every nonzero
  # curvature b. Two smooth pieces of the form a + b*x2^2 can never have
  # DIFFERENT optimal cutpoints, whatever (a, b) are chosen -- which is
  # precisely why spec Section 3 (as revised 2026-09-28) mirrors the right
  # piece instead of re-tuning its curvature.
  #
  # Vertex at 1 (the mirrored piece, g = A + B*(1-x)^2, coefficient triple
  # c(A+B, -2B, B)): the reflection x -> 1 - x maps Uniform(0,1) to itself and
  # a cutpoint t to 1 - t, so the maximizer is 1 - (1 + sqrt(17))/8 exactly.
  t_star_vertex0 <- (1 + sqrt(17)) / 8
  vertex <- if (abs(coef[[3L]]) < .Machine$double.eps^0.5) {
    NA_real_
  } else {
    -coef[[2L]] / (2 * coef[[3L]])
  }
  analytic_t <- if (is.na(vertex)) {
    NA_real_
  } else if (abs(vertex) < 1e-12) {
    t_star_vertex0
  } else if (abs(vertex - 1) < 1e-12) {
    1 - t_star_vertex0
  } else {
    NA_real_
  }
  list(t = o$minimum, sse = o$objective, analytic_t = analytic_t,
       vertex = vertex, n_optima = n_optima)
}

#' Spec-faithful alias: optimal cutpoint for g(x) = a + b*x^2
optimal_quadratic_cut <- function(a, b, tol = 1e-10) {
  optimal_poly_cut(c(a, 0, b), tol = tol)
}

# ---- 2 Ground truth: t1*, t2*_L, t2*_R, f* --------------------------------

#' Closed-form ground truth for the jump + cosmetic DGP
#'
#' @param params A `dgp_params_jump_cosmetic()` list.
#' @param tol Passed to `optimal_quadratic_cut()`.
#' @return list with
#'   `t1_star`     -- the JUMP boundary. Exactly `params$t1`, by construction:
#'                    for a single discontinuity of a function that is
#'                    continuous on each side, under a continuous design
#'                    density, the population-risk-minimizing single-threshold
#'                    split is the true break point (change-point literature,
#'                    `chan1993` / `seijosen2011`, cited by spec Section 3).
#'                    No estimation, no numerical minimization.
#'   `t2_star_L`, `t2_star_R` -- the COSMETIC boundaries, from
#'                    `optimal_quadratic_cut()` on each side's own smooth
#'                    piece: g_L(x2) = a_L + b_L*x2^2 on the left, and on the
#'                    right g_R(x2) = (a_L + delta1) + (b_L + b_R)*x2^2.
#'   `topology`    -- the depth-2 tree shape f* is asserted to have.
#'   `leaf_means`, `risk` -- exact population leaf means of f* and its
#'                    population risk E[(gamma_0 - f*)^2].
ground_truth_jump_cosmetic <- function(params = dgp_params_jump_cosmetic(),
                                        tol = 1e-10) {
  left  <- optimal_poly_cut(params$left_coef,  tol = tol)
  right <- optimal_poly_cut(params$right_coef, tol = tol)

  topology <- depth2_topology("x1", params$t1, "x2", left$t, "x2", right$t)
  risk <- population_tree_risk(topology, params)

  list(
    params = params,
    t1_star = params$t1,
    t2_star_L = left$t,
    t2_star_R = right$t,
    t2_star_L_analytic = left$analytic_t,
    t2_star_R_analytic = right$analytic_t,
    topology = topology,
    leaf_means = risk$leaf_means,
    leaf_rects = risk$leaf_rects,
    risk = risk$risk
  )
}

# ---- 3 Exact population moments of gamma_0 over a rectangle ---------------

#' Exact integrals of gamma_0 and gamma_0^2 over an axis-aligned rectangle
#'
#' The Lebesgue measure on the rectangle IS the Uniform(0,1)^2 measure
#' restricted to it (density 1), so these integrals are already the
#' probability-weighted ones -- no Jacobian.
#'
#' gamma_0 is piecewise in x1 (break at `t1`) and a quadratic in x2 on each
#' piece, so the rectangle integral splits into the x1 < t1 and x1 > t1 slabs
#' and each is an elementary polynomial integral in x2 (`poly_integrals()`).
#'
#' @param rect list(x1lo, x1hi, x2lo, x2hi).
#' @param params A `dgp_params_jump_cosmetic()` list.
#' @return list(area, I1 = int gamma_0, I2 = int gamma_0^2).
gamma0_rect_moments <- function(rect, params) {
  w_left  <- max(0, min(rect$x1hi, params$t1) - rect$x1lo)
  w_right <- max(0, rect$x1hi - max(rect$x1lo, params$t1))
  pl <- poly_integrals(params$left_coef,  rect$x2lo, rect$x2hi)
  pr <- poly_integrals(params$right_coef, rect$x2lo, rect$x2hi)

  list(area = (w_left + w_right) * (rect$x2hi - rect$x2lo),
       I1 = w_left * pl$i1 + w_right * pr$i1,
       I2 = w_left * pl$i2 + w_right * pr$i2)
}

# ---- 4 Population risk of an arbitrary depth-2, 4-leaf tree --------------

#' Build a depth-2 topology descriptor
#'
#' @param root_coord,root_cut Root split.
#' @param left_coord,left_cut Split inside the root's `<=` child.
#' @param right_coord,right_cut Split inside the root's `>` child.
depth2_topology <- function(root_coord, root_cut, left_coord, left_cut,
                             right_coord, right_cut) {
  valid <- c("x1", "x2")
  bad <- setdiff(c(root_coord, left_coord, right_coord), valid)
  if (length(bad)) {
    cli::cli_abort("depth2_topology: coordinate(s) {.val {bad}} not in {.val {valid}}.")
  }
  list(root  = list(coord = root_coord,  cut = root_cut),
       left  = list(coord = left_coord,  cut = left_cut),
       right = list(coord = right_coord, cut = right_cut))
}

#' The four leaf rectangles of a depth-2 topology over [0,1]^2
#'
#' Leaves are returned in the fixed order (root-left/child-left,
#' root-left/child-right, root-right/child-left, root-right/child-right), so
#' leaf index is stable across calls and comparable across topologies of the
#' same shape.
depth2_leaf_rects <- function(topology) {
  apply_cut <- function(rect, coord, cut, side) {
    lo <- paste0(coord, "lo"); hi <- paste0(coord, "hi")
    if (identical(side, "left")) {
      rect[[hi]] <- min(rect[[hi]], cut)
    } else {
      rect[[lo]] <- max(rect[[lo]], cut)
    }
    rect
  }
  full <- list(x1lo = 0, x1hi = 1, x2lo = 0, x2hi = 1)
  out <- list()
  for (s1 in c("left", "right")) {
    r1 <- apply_cut(full, topology$root$coord, topology$root$cut, s1)
    child <- topology[[s1]]
    for (s2 in c("left", "right")) {
      out[[length(out) + 1L]] <- apply_cut(r1, child$coord, child$cut, s2)
    }
  }
  out
}

#' Exact population risk E[(gamma_0 - tau)^2] of a depth-2 tree
#'
#' Leaf predictions are the population conditional means of gamma_0 over each
#' leaf rectangle (the risk-minimizing values for a fixed partition), so this
#' is the population risk of the BEST tree with that partition -- exactly what
#' spec Section 2's f* is the argmin of over partitions.
#'
#' A leaf of zero area (a cut that lands outside its parent's range, e.g. a
#' child splitting the same coordinate as its parent on the wrong side)
#' contributes no mass and no risk; that is a legitimate 3-leaf member of the
#' at-most-4-leaf class, not an error.
#'
#' @return list(risk, leaf_means, leaf_areas, leaf_rects).
population_tree_risk <- function(topology, params) {
  rects <- depth2_leaf_rects(topology)
  mom <- lapply(rects, gamma0_rect_moments, params = params)
  areas <- vapply(mom, `[[`, numeric(1), "area")
  I1 <- vapply(mom, `[[`, numeric(1), "I1")
  I2 <- vapply(mom, `[[`, numeric(1), "I2")
  has_mass <- areas > 0
  means <- rep(NA_real_, length(areas))
  means[has_mass] <- I1[has_mass] / areas[has_mass]
  risk <- sum(I2[has_mass] - I1[has_mass]^2 / areas[has_mass])
  list(risk = risk, leaf_means = means, leaf_areas = areas, leaf_rects = rects)
}

# ---- 5 Population risk of the best tree of a GIVEN depth-2 shape ---------

#' All eight depth-2 shapes over two coordinates
#'
#' (root coord) x (left-child coord) x (right-child coord), each in {x1, x2}.
#' Used by the topology verification: rather than checking the handful of
#' alternatives spec Section 3 names, this enumerates the whole depth-2
#' shape space, so "no alternative topology achieves lower population risk"
#' is checked against every shape the estimator can actually return.
depth2_shapes <- function() {
  g <- expand.grid(root = c("x1", "x2"), left = c("x1", "x2"),
                    right = c("x1", "x2"), stringsAsFactors = FALSE)
  g[order(g$root, g$left, g$right), c("root", "left", "right")]
}

#' Minimize population risk over the three cutpoints of one depth-2 shape
#'
#' Coarse grid to localize (the risk surface is piecewise smooth in the cuts,
#' with kinks where a cut crosses `t1`, so a purely local optimizer started
#' anywhere can stall on the wrong side of a kink), then Nelder-Mead from the
#' grid argmin. Extra starts can be supplied via `extra_starts` -- the
#' verification script passes the constructed ground truth's own cuts, so a
#' shape that CONTAINS f* cannot be reported as worse than f* merely because
#' the optimizer missed it.
#'
#' @param shape One row of `depth2_shapes()` (or a list with root/left/right).
#' @param params A `dgp_params_jump_cosmetic()` list.
#' @param n_grid Points per coordinate in the coarse grid (default 25 ->
#'   25^3 = 15,625 exact risk evaluations per shape).
#' @param extra_starts Optional list of length-3 numeric vectors of cuts
#'   (root, left, right) to seed the local search from in addition to the
#'   grid argmin.
#' @return list(risk, cuts, topology, grid_risk).
best_risk_for_shape <- function(shape, params, n_grid = 25L,
                                 extra_starts = list()) {
  shape_topology <- function(cuts) {
    depth2_topology(shape$root, cuts[[1L]], shape$left, cuts[[2L]],
                     shape$right, cuts[[3L]])
  }
  obj <- function(cuts) {
    if (any(!is.finite(cuts)) || any(cuts <= 0) || any(cuts >= 1)) return(Inf)
    population_tree_risk(shape_topology(cuts), params)$risk
  }

  # Interior grid: endpoints excluded because a cut at 0 or 1 empties a leaf,
  # which is already covered (as a zero-area leaf) by any interior cut of the
  # corresponding 3-leaf shape.
  gr <- seq(0, 1, length.out = n_grid + 2L)[-c(1L, n_grid + 2L)]
  grid <- as.matrix(expand.grid(gr, gr, gr))
  grid_risk <- apply(grid, 1L, obj)
  best_i <- which.min(grid_risk)

  starts <- c(list(as.numeric(grid[best_i, ])), extra_starts)
  best <- list(risk = grid_risk[[best_i]], cuts = as.numeric(grid[best_i, ]))
  for (s in starts) {
    o <- stats::optim(s, obj, method = "Nelder-Mead",
                       control = list(reltol = 1e-14, maxit = 5000))
    if (o$value < best$risk) best <- list(risk = o$value, cuts = o$par)
  }
  best$topology  <- shape_topology(best$cuts)
  best$grid_risk <- min(grid_risk)
  best
}

# ---- 6 Brute-force cross-checks (spec Section 3's "negligible error") ----

#' Brute-force optimal cutpoint for a quadratic piece, by grid + quadrature
#'
#' Deliberately shares NO algebra with `poly_split_sse()`: the leaf means and
#' the SSE are both obtained by numerical quadrature (midpoint rule on a fine
#' mesh, accumulated with `cumsum()` so all candidates are scored in one
#' vectorized pass) directly from g evaluated pointwise.
#'
#' Resolution note, stated rather than glossed: with `n_candidates` equally
#' spaced candidates the grid spacing is ~1/n_candidates, so the grid argmin
#' can differ from the true optimum by up to half that (~5e-6 at the spec's
#' 100,001 candidates) for purely grid reasons. `refine_optimal_cut_numeric()`
#' below closes that gap; the objective-VALUE agreement, which the grid does
#' resolve to quadrature precision, is reported separately.
#'
#' @param coef Coefficient triple `c(a0, a1, a2)`.
#' @param n_candidates Candidate cutpoints, equally spaced in (0,1).
#' @param n_mesh Midpoint-rule mesh size for the quadrature.
#' @return list(t, sse, n_candidates, n_mesh, grid_spacing).
brute_force_optimal_cut <- function(coef, n_candidates = 100001L,
                                     n_mesh = 4e6L) {
  n_mesh <- as.integer(n_mesh)
  xs <- (seq_len(n_mesh) - 0.5) / n_mesh
  gs <- poly_eval(coef, xs)
  cs1 <- cumsum(gs)
  cs2 <- cumsum(gs * gs)
  tot1 <- cs1[[n_mesh]]
  tot2 <- cs2[[n_mesh]]

  cand <- seq(0, 1, length.out = n_candidates + 2L)[-c(1L, n_candidates + 2L)]
  k <- findInterval(cand, xs)   # mesh points at or below each candidate
  if (any(k < 1L) || any(k >= n_mesh)) {
    cli::cli_abort(
      "brute_force_optimal_cut: mesh of {n_mesh} points is too coarse for \\
       {n_candidates} candidates -- some candidate leaves a side with no \\
       mesh point, so its numerical SSE would be undefined."
    )
  }
  sL1 <- cs1[k]; sL2 <- cs2[k]
  sR1 <- tot1 - sL1; sR2 <- tot2 - sL2
  nR <- n_mesh - k
  sse <- ((sL2 - sL1^2 / k) + (sR2 - sR1^2 / nR)) / n_mesh

  i <- which.min(sse)
  list(t = cand[[i]], sse = sse[[i]], n_candidates = n_candidates,
       n_mesh = n_mesh, grid_spacing = cand[[2L]] - cand[[1L]])
}

#' Population SSE at cutpoint t by adaptive quadrature (`integrate()`)
#'
#' A second, independent brute-force objective: `integrate()` is handed the
#' raw g, so nothing in `poly_split_sse()`'s derivation is reused. Exact to
#' machine precision here because g and (g - c)^2 are polynomials of degree
#' <= 4, which Gauss-Kronrod integrates exactly.
numeric_split_sse <- function(t, coef) {
  g <- function(x) poly_eval(coef, x)
  c1 <- stats::integrate(g, 0, t)$value / t
  c2 <- stats::integrate(g, t, 1)$value / (1 - t)
  stats::integrate(function(x) (g(x) - c1)^2, 0, t)$value +
    stats::integrate(function(x) (g(x) - c2)^2, t, 1)$value
}

#' Refine a brute-force cutpoint on a fine local grid of `integrate()` values
#'
#' Closes the coarse grid's ~5e-6 spacing-limited resolution so the
#' brute-force/closed-form comparison can be made at the spec's 1e-6
#' tolerance on the LOCATION, not only on the objective value.
#'
#' @param t0 Starting cutpoint (the coarse grid argmin).
#' @param coef Coefficient triple.
#' @param half_width Half-width of the local grid (default 1e-5, one coarse
#'   grid step at 100,001 candidates).
#' @param n_points Points in the local grid (default 2001 -> 1e-8 spacing).
refine_optimal_cut_numeric <- function(t0, coef, half_width = 1e-5,
                                        n_points = 2001L) {
  cand <- seq(max(t0 - half_width, 1e-9), min(t0 + half_width, 1 - 1e-9),
               length.out = n_points)
  sse <- vapply(cand, numeric_split_sse, numeric(1), coef = coef)
  i <- which.min(sse)
  if (i == 1L || i == n_points) {
    cli::cli_abort(
      "refine_optimal_cut_numeric: the refined minimum sits on the edge of \\
       the local grid around {t0} (half width {half_width}) -- the coarse \\
       argmin was not within one step of the true optimum, so this \\
       refinement is not a valid cross-check. Widen `half_width`."
    )
  }
  list(t = cand[[i]], sse = sse[[i]], spacing = cand[[2L]] - cand[[1L]])
}

#' Population mean of gamma_0 over a rectangle by nested `integrate()`
#'
#' Independent cross-check on `gamma0_rect_moments()`'s closed form. The outer
#' x1 integral is split at `t1` when the rectangle straddles the jump, so
#' `integrate()` never sees a discontinuity inside a single panel.
gamma0_rect_mean_numeric <- function(rect, params) {
  inner <- function(x1v) {
    vapply(x1v, function(x1) {
      stats::integrate(
        function(x2) gamma_0_jump_cosmetic(rep(x1, length(x2)), x2, params),
        rect$x2lo, rect$x2hi
      )$value
    }, numeric(1))
  }
  breaks <- sort(unique(c(rect$x1lo, rect$x1hi,
                           params$t1[params$t1 > rect$x1lo & params$t1 < rect$x1hi])))
  num <- sum(vapply(seq_len(length(breaks) - 1L), function(i) {
    stats::integrate(inner, breaks[[i]], breaks[[i + 1L]])$value
  }, numeric(1)))
  num / ((rect$x1hi - rect$x1lo) * (rect$x2hi - rect$x2lo))
}

#' Leaf index (1..4) of each point of a coordinate sample under a depth-2 tree
#'
#' Same fixed leaf ordering as `depth2_leaf_rects()`, so indices line up with
#' `population_tree_risk()`'s `leaf_means` / `leaf_areas`.
#'
#' @param topology A `depth2_topology()`.
#' @param x1,x2 Coordinate vectors of equal length.
#' @return Integer vector in 1..4.
depth2_leaf_index <- function(topology, x1, x2) {
  coord_vals <- list(x1 = x1, x2 = x2)
  root_left <- coord_vals[[topology$root$coord]] <= topology$root$cut
  leaf <- integer(length(root_left))
  for (s in seq_along(c("left", "right"))) {
    side <- c("left", "right")[[s]]
    in_side <- if (identical(side, "left")) root_left else !root_left
    child <- topology[[side]]
    child_left <- coord_vals[[child$coord]] <= child$cut
    leaf[in_side & child_left]  <- (s - 1L) * 2L + 1L
    leaf[in_side & !child_left] <- (s - 1L) * 2L + 2L
  }
  leaf
}

#' Predictions of a depth-2 tree with FIXED (given) leaf means
#'
#' Used to evaluate f* on the fixed Monte Carlo sample. This is NOT the same as
#' refitting leaf means on that sample: f* carries its exact POPULATION leaf
#' means, and substituting the sample means would make the reference risk
#' systematically (if slightly) too small, biasing every excess risk upward by
#' the same O(1/N_MC) amount. Small, but free to get right.
depth2_tree_predict <- function(topology, leaf_means, x1, x2) {
  if (length(leaf_means) != 4L || any(!is.finite(leaf_means))) {
    cli::cli_abort(
      "depth2_tree_predict: {.arg leaf_means} must be 4 finite values; got \\
       {.val {leaf_means}}. A non-finite leaf mean means the topology has a \\
       zero-mass leaf, which cannot be evaluated pointwise."
    )
  }
  leaf_means[depth2_leaf_index(topology, x1, x2)]
}

#' Monte Carlo leaf means and risk of a depth-2 tree on a fixed X sample
#'
#' The brute-force Monte Carlo counterpart to `population_tree_risk()`: leaf
#' means are REFIT on the sample, so this is the sample-optimal tree with that
#' partition -- the right object for cross-checking `population_tree_risk()`,
#' and deliberately NOT the right object for evaluating f* (see
#' `depth2_tree_predict()`).
#'
#' @param topology A `depth2_topology()`.
#' @param x1,x2 Coordinates of the evaluation sample.
#' @param g0 `gamma_0` evaluated on that same sample.
#' @return list(risk, leaf_means, leaf_se, leaf_counts). `leaf_se` is the
#'   Monte Carlo standard error of each leaf mean, so a closed-form/MC
#'   comparison can be judged against the MC noise it actually carries rather
#'   than against a tolerance the sample size cannot support.
mc_tree_risk <- function(topology, x1, x2, g0) {
  leaf <- depth2_leaf_index(topology, x1, x2)
  counts <- tabulate(leaf, nbins = 4L)
  means <- rep(NA_real_, 4L)
  ses <- rep(NA_real_, 4L)
  risk <- 0
  for (k in seq_len(4L)) {
    if (counts[[k]] == 0L) next
    gk <- g0[leaf == k]
    means[[k]] <- mean(gk)
    ses[[k]] <- stats::sd(gk) / sqrt(counts[[k]])
    risk <- risk + sum((gk - means[[k]])^2)
  }
  list(risk = risk / length(g0), leaf_means = means, leaf_se = ses,
       leaf_counts = counts)
}
