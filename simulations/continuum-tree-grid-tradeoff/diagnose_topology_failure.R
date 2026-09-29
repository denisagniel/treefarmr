# ============================================================
# Diagnosis of the ground-truth / topology-verification failure
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
# Purpose: ground_truth_gate.R reports that the spec's constructed 4-leaf
#          topology is NOT the population-best one. This script isolates WHY,
#          separates the parameter-value causes from the functional-form cause,
#          and measures which candidate repairs would clear the gate -- so the
#          spec revision decision is made on numbers, not on guesses.
#          It changes nothing: dgp.R stays faithful to the APPROVED spec.
# Inputs: dgp.R, ground_truth.R
# Outputs: stdout report; results/topology_failure_diagnosis.rds
# ============================================================

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}
source(file.path(STUDY_DIR, "dgp.R"))
source(file.path(STUDY_DIR, "ground_truth.R"))

# ---- 0 A parameter set built directly from two coefficient triples --------
#
# The approved spec's parameterization is a special case; this lets the same
# machinery score candidate revisions without editing dgp.R.
params_from_coefs <- function(t1, left_coef, right_coef, sigma = 0.5,
                               label = "custom") {
  list(label = label, t1 = t1, sigma = sigma,
       left_coef = left_coef, right_coef = right_coef,
       a_L = left_coef[[1L]], b_L = left_coef[[3L]],
       a_right = right_coef[[1L]], b_right = right_coef[[3L]],
       delta1 = right_coef[[1L]] - left_coef[[1L]],
       b_R = right_coef[[3L]] - left_coef[[3L]])
}

CONSTRUCTED_SHAPE <- "root=x1 | left=x2 | right=x2"

#' Score one candidate DGP: cutpoints, jump profile, and the topology gate
score_candidate <- function(params, n_grid = 25L) {
  cut_of <- function(coef) {
    if (max(abs(coef[2:3])) < .Machine$double.eps^0.5) {
      return(list(t = NA_real_, identified = FALSE, n_optima = NA_integer_))
    }
    o <- optimal_poly_cut(coef)
    list(t = o$t, identified = TRUE, n_optima = o$n_optima)
  }
  cl <- cut_of(params$left_coef)
  cr <- cut_of(params$right_coef)

  x2 <- seq(0, 1, length.out = 100001L)
  jump <- gamma_0_jump_profile(x2, params)
  jump_ok <- !(min(jump) < 0 && max(jump) > 0) && min(abs(jump)) > 1e-8

  # A 4-leaf topology needs both child cuts; where a side's cut is not
  # identified, reuse the other side's (any value gives the same risk there,
  # which is exactly the degeneracy being diagnosed).
  tL <- if (cl$identified) cl$t else cr$t
  tR <- if (cr$identified) cr$t else cl$t
  constructed <- depth2_topology("x1", params$t1, "x2", tL, "x2", tR)
  risk_constructed <- population_tree_risk(constructed, params)$risk

  shapes <- depth2_shapes()
  starts <- list(c(params$t1, tL, tR), c(tL, params$t1, params$t1),
                  c(params$t1, params$t1, params$t1), c(tL, tL, tR))
  rows <- lapply(seq_len(nrow(shapes)), function(i) {
    shape <- as.list(shapes[i, ])
    b <- best_risk_for_shape(shape, params, n_grid = n_grid,
                              extra_starts = starts)
    data.frame(shape = sprintf("root=%s | left=%s | right=%s",
                                shape$root, shape$left, shape$right),
               risk = b$risk, cut_root = b$cuts[[1L]],
               cut_left = b$cuts[[2L]], cut_right = b$cuts[[3L]],
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab <- tab[order(tab$risk), ]
  other <- tab[tab$shape != CONSTRUCTED_SHAPE, ]
  i <- which.min(other$risk)

  list(params = params, t2_L = cl, t2_R = cr,
       jump_min_abs = min(abs(jump)), jump_range = range(jump),
       jump_ok = jump_ok, risk_constructed = risk_constructed,
       best_other_shape = other$shape[[i]], best_other_risk = other$risk[[i]],
       margin = other$risk[[i]] - risk_constructed, shape_tab = tab)
}

report_candidate <- function(s) {
  p <- s$params
  cat(sprintf("\n### %s\n", p$label))
  cat(sprintf("  t1 = %g ; left g(x2) = %g + %g*x2 + %g*x2^2 ; right g(x2) = %g + %g*x2 + %g*x2^2\n",
              p$t1, p$left_coef[[1L]], p$left_coef[[2L]], p$left_coef[[3L]],
              p$right_coef[[1L]], p$right_coef[[2L]], p$right_coef[[3L]]))
  cat(sprintf("  t2*_L = %s (identified: %s, optima: %s)\n",
              format(s$t2_L$t, digits = 10), s$t2_L$identified, s$t2_L$n_optima))
  cat(sprintf("  t2*_R = %s (identified: %s, optima: %s)\n",
              format(s$t2_R$t, digits = 10), s$t2_R$identified, s$t2_R$n_optima))
  cat(sprintf("  t2*_L == t2*_R ? %s\n",
              if (!s$t2_L$identified || !s$t2_R$identified) {
                "undefined (at least one side's cutpoint is not identified)"
              } else if (isTRUE(all.equal(s$t2_L$t, s$t2_R$t, tolerance = 1e-6))) {
                "YES (degenerate pair -- the two cosmetic boundaries coincide)"
              } else {
                "no (distinct pair, as the design intends)"
              }))
  cat(sprintf("  jump across x1=t1 over x2: [%.4f, %.4f], min |jump| = %.6f -> %s\n",
              s$jump_range[[1L]], s$jump_range[[2L]], s$jump_min_abs,
              if (s$jump_ok) "OK (never vanishes)" else "VANISHES inside the support"))
  cat(sprintf("  R(constructed) = %.10f ; best other shape = %.10f (%s)\n",
              s$risk_constructed, s$best_other_risk, s$best_other_shape))
  cat(sprintf("  topology gate margin = %+.6e -> %s\n", s$margin,
              if (s$margin > 1e-9) "PASS (constructed is strictly best)" else
                if (abs(s$margin) <= 1e-9) "FAIL (exact TIE: f* not unique)" else
                  "FAIL (constructed is NOT f*)"))
  invisible(s)
}

cat("==========================================================\n")
cat("Topology-failure diagnosis: continuum-tree-grid-tradeoff\n")
cat("==========================================================\n")

# ---- 1 Is the equal-cutpoint degeneracy a parameter choice or structural? --
#
# Analytically (see optimal_poly_cut()'s comment), for g(x2) = a + b*x2^2 the
# explained variance of a single split is (b^2/9)*t*(1-t)*(1+t)^2: the
# maximizer does not depend on a or b at all. If that is right, NO choice of
# (a_L, b_L, b_R, delta1) in the spec's functional form can make t2*_L and
# t2*_R differ -- so the spec's stated design goal ("deliberately given
# different curvature so they land at two different, non-symmetric
# locations") is unreachable by re-tuning parameters.

cat("\n--- 1 Curvature/intercept invariance of the optimal cutpoint ---\n")
cat("  For g(x2) = a + b*x2^2 on [0,1] under Uniform(0,1):\n")
cat("    a          b          t*(a,b)            t* - (1+sqrt(17))/8\n")
inv_grid <- expand.grid(a = c(-5, 0, 2, 100), b = c(-3, -1.5, 0.25, 3, 50))
inv_t <- vapply(seq_len(nrow(inv_grid)), function(i) {
  optimal_poly_cut(c(inv_grid$a[[i]], 0, inv_grid$b[[i]]))$t
}, numeric(1))
for (i in seq_len(nrow(inv_grid))) {
  cat(sprintf("    %-10g %-10g %.12f     %+.3e\n", inv_grid$a[[i]],
              inv_grid$b[[i]], inv_t[[i]], inv_t[[i]] - (1 + sqrt(17)) / 8))
}
cat(sprintf("  spread of t* across all %d (a, b) pairs: %.3e\n",
            nrow(inv_grid), diff(range(inv_t))))
cat("  => the optimal cutpoint is INVARIANT to both intercept and curvature.\n")
cat("     t2*_L and t2*_R are therefore EQUAL for every parameter setting of\n")
cat("     the spec's functional form; this is structural, not a bad delta1/b_R.\n")

# ---- 2 Score the approved spec and a ladder of candidate repairs ----------

candidates <- list(
  # As approved. b_L + b_R = 0 -> right side constant in x2; jump 2 - 3*x2^2
  # changes sign at x2 = sqrt(2/3).
  params_from_coefs(0.5, c(0, 0, 3), c(2, 0, 0), label =
    "APPROVED SPEC: a_L=0, b_L=3, b_R=-3, delta1=2"),
  # Parameter-only repair attempt A: keep the spec's form, pick b_R so the
  # right side is not flat AND the jump never vanishes (needs b_R > -delta1).
  params_from_coefs(0.5, c(0, 0, 3), c(2, 0, 2), label =
    "repair attempt A (params only): b_R = -1  -> b_right = 2, jump = 2 - x2^2"),
  params_from_coefs(0.5, c(0, 0, 3), c(2, 0, 1.5), label =
    "repair attempt B (params only): b_R = -1.5 -> b_right = 1.5, jump = 2 - 1.5*x2^2"),
  # Form repair C: give the right side a MIRRORED quadratic in x2,
  # (a_L + delta1) + b_R2*(1 - x2)^2, i.e. coef (a+b, -2b, b). Still a
  # quadratic (spec Section 10's closed-form-integral limitation is untouched)
  # but its optimal cutpoint is 1 - (1+sqrt(17))/8, so the cosmetic pair is
  # genuinely non-symmetric. Jump = delta1 + b_R2*(1-x2)^2 - b_L*x2^2, whose
  # minimum over [0,1] is delta1 - b_L, so delta1 > b_L is required.
  params_from_coefs(0.5, c(0, 0, 1), c(3, -2, 1), label =
    "repair attempt C (mirrored right piece): b_L=1, b_R2=1, delta1=2, jump = 3 - 2*x2"),
  params_from_coefs(0.5, c(0, 0, 3), c(8, -6, 3), label =
    "repair attempt D (mirrored right piece): b_L=3, b_R2=3, delta1=5, jump = 8 - 6*x2")
)

scored <- lapply(candidates, score_candidate)
invisible(lapply(scored, report_candidate))

# ---- 3 The winning alternative topology, examined ------------------------

cat("\n--- 3 What the winning alternative topology under the APPROVED spec does ---\n")
spec_params <- candidates[[1L]]
spec_score <- scored[[1L]]
win <- spec_score$shape_tab[spec_score$shape_tab$shape == spec_score$best_other_shape, ]
# Tighten the winner's cuts: the shape scan's Nelder-Mead start comes from a
# 25-point grid, so report a properly converged optimum before interpreting
# the cut locations.
obj <- function(cuts) {
  if (any(cuts <= 0) || any(cuts >= 1)) return(Inf)
  population_tree_risk(depth2_topology("x2", cuts[[1L]], "x1", cuts[[2L]],
                                        "x2", cuts[[3L]]), spec_params)$risk
}
o <- stats::optim(c(win$cut_root, win$cut_left, win$cut_right), obj,
                   method = "Nelder-Mead",
                   control = list(reltol = 1e-15, maxit = 20000))
o <- stats::optim(o$par, obj, method = "Nelder-Mead",
                   control = list(reltol = 1e-15, maxit = 20000))
jump_zero <- sqrt(-spec_params$delta1 / spec_params$b_R)
cat(sprintf("  shape %s, risk = %.12f\n", spec_score$best_other_shape, o$value))
cat(sprintf("    x2 cut at root        = %.10f\n", o$par[[1L]]))
cat(sprintf("    x1 cut in left child  = %.10f   (true jump location t1 = %g)\n",
            o$par[[2L]], spec_params$t1))
cat(sprintf("    x2 cut in right child = %.10f   (jump vanishes at x2 = %.10f)\n",
            o$par[[3L]], jump_zero))
cat("  Reading: it spends the X1 split only on the LOW-x2 band, where the two\n")
cat("  sides of the jump are far apart, and in the HIGH-x2 band -- where the\n")
cat("  jump has shrunk to nothing and reversed sign -- it drops the X1 split\n")
cat("  entirely and buys a second X2 split instead. That is only profitable\n")
cat("  BECAUSE the jump vanishes inside the support, which is the same defect\n")
cat("  section 1's jump-profile check flags.\n")

saveRDS(list(invariance = cbind(inv_grid, t_star = inv_t), scored = scored,
             winner_cuts = o$par, winner_risk = o$value),
        file.path(STUDY_DIR, "results", "topology_failure_diagnosis.rds"))

cat("\n==========================================================\n")
cat("Summary of the topology gate across candidates:\n")
for (s in scored) {
  cat(sprintf("  %-70s margin %+.4e  %s\n",
              substr(s$params$label, 1L, 70L), s$margin,
              if (s$margin > 1e-9) "PASS" else "FAIL"))
}
cat("==========================================================\n")
