# ============================================================
# Ground-truth verification for the grid-resolution / threshold-rate study
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 3, "Ground truth, and how it is computed to negligible
#        error", including the MANDATORY one-time topology verification)
# Purpose: prove, numerically, that every closed-form ground-truth quantity
#          this study measures against is right, and that the constructed
#          4-leaf topology really is f*. Nothing downstream may run until this
#          script reports PASS on every check.
# Inputs: dgp.R, ground_truth.R, results/mc_eval_sample.rds
# Outputs: stdout report; results/ground_truth_verification.rds
# ============================================================

# ---- 0 Setup -------------------------------------------------------------

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}
source(file.path(STUDY_DIR, "dgp.R"))
source(file.path(STUDY_DIR, "ground_truth.R"))
source(file.path(STUDY_DIR, "mc_eval_sample.R"))

# Tolerances, all from spec Section 3 / this task's stated gates.
TOL_CUT        <- 1e-6    # closed-form vs REFINED brute-force optimal cutpoint
TOL_EXACT      <- 1e-9    # closed form vs deterministic quadrature (authoritative)
# Spec Section 3, "Excess-risk evaluation" (revised 2026-09-28): the leaf-mean
# MC check is stated in Monte Carlo standard-error units, NOT as an absolute
# threshold -- a leaf mean's own s.e. at N_MC = 1e7 is ~2-4e-4, i.e. already
# above a naive 1e-4 absolute gate before any error exists. Gated at 4 s.e.
# (a conventional 4-sigma band) rather than the literal 1 s.e. the spec quotes
# as an observed value, because a 1-s.e. gate would fail ~32% of the time by
# chance alone even on an exactly correct closed form.
TOL_MC_SE      <- 4
TOL_TIE        <- 1e-9    # margin below this is a tie, not a strict win

DELTA1 <- 5               # this task's scope: favorable Regime F only
params <- dgp_params_jump_cosmetic(delta1 = DELTA1)

checks <- list()
record <- function(name, passed, detail) {
  checks[[length(checks) + 1L]] <<- list(name = name, passed = passed,
                                          detail = detail)
  cat(sprintf("[%s] %s\n      %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}

cat("=========================================================\n")
cat("Ground-truth verification: continuum-tree-grid-tradeoff\n")
cat(sprintf("Regime F, delta1 = %g, sigma = %g, t1 = %g\n",
            params$delta1, params$sigma, params$t1))
cat("=========================================================\n\n")

# ---- 1 Identifiability audit of the two smooth pieces --------------------
#
# gamma_0 restricted to each side of the jump:
#   x1 <= t1 : g_L(x2) = a_L + b_L*x2^2
#   x1 >  t1 : g_R(x2) = (a_L + delta1) + (b_L + b_R)*x2^2
# A cosmetic boundary's t2* exists only if that side's curvature is nonzero:
# a flat piece is fit exactly by a single constant, so every cutpoint attains
# the same (zero) population SSE and no t2* is identified.

cat("--- 1 Smooth-piece identifiability and jump profile ---\n")
fmt_coef <- function(coef) {
  sprintf("%g %+g*x2 %+g*x2^2", coef[[1L]], coef[[2L]], coef[[3L]])
}
cat(sprintf("  left  piece  g_L(x2) = a_L + b_L*x2^2          = %s\n",
            fmt_coef(params$left_coef)))
cat(sprintf("  right piece  g_R(x2) = (a_L+delta1) + b_R*(1-x2)^2 = %s\n",
            fmt_coef(params$right_coef)))
cat(sprintf("  parameters: a_L = %g, b_L = %g, b_R = %g, delta1 = %g, t1 = %g, sigma = %g\n",
            params$a_L, params$b_L, params$b_R, params$delta1, params$t1,
            params$sigma))

# A side's cosmetic cutpoint is identified only if that side actually varies
# in x2: a constant piece is fit exactly by one constant, so every cutpoint
# attains the same (zero) SSE and no t2* exists.
identified_of <- function(coef) max(abs(coef[2:3])) >= .Machine$double.eps^0.5
left_identified  <- identified_of(params$left_coef)
right_identified <- identified_of(params$right_coef)

record("t2*_L is identified (left smooth piece varies in x2)", left_identified,
       sprintf("max |non-constant coefficients| = %.10g", max(abs(params$left_coef[2:3]))))
record("t2*_R is identified (right smooth piece varies in x2)", right_identified,
       sprintf("max |non-constant coefficients| = %.10g", max(abs(params$right_coef[2:3]))))

# The JUMP boundary's ground truth (t1* = t1 exactly, no estimation) rests on
# the change-point argument spec Section 3 cites. That argument needs a real
# discontinuity along the whole boundary: if
# gamma_0(t1+, x2) - gamma_0(t1-, x2) vanishes anywhere on [0,1], gamma_0 is
# CONTINUOUS across x1 = t1 at that x2 and the boundary is not uniformly a
# jump. For the revised form with b_L = b_R = b the jump is
# delta1 + b*(1 - 2*x2) on [delta1 - b, delta1 + b], so the condition is
# delta1 > b -- checked here numerically AND exactly (roots of the quadratic).
x2_scan <- seq(0, 1, length.out = 100001L)
jump_scan <- gamma_0_jump_profile(x2_scan, params)
jump_zeros <- gamma_0_jump_zeros(params)
cat(sprintf("  jump(x2) = gamma_0(t1+,x2) - gamma_0(t1-,x2) over x2 in [0,1]: [%.6f, %.6f]\n",
            min(jump_scan), max(jump_scan)))
cat(sprintf("  exact roots of jump(x2) in [0,1]: %s\n",
            if (length(jump_zeros) == 0L) "none" else
              paste(sprintf("%.10f", jump_zeros), collapse = ", ")))
record("the jump at x1 = t1 never vanishes on the x2 support",
       length(jump_zeros) == 0L && min(abs(jump_scan)) > 1e-8,
       sprintf("min |jump| on a 100001-point scan = %.6f; %d exact root(s) in [0,1]; condition delta1 > b_R is %g > %g = %s",
               min(abs(jump_scan)), length(jump_zeros), params$delta1,
               params$b_R, params$delta1 > params$b_R))
cat("\n")

# ---- 2 t2* : closed form vs optimize() vs analytic root vs brute force ----

cat("--- 2 Cosmetic-boundary cutpoints ---\n")

verify_cut <- function(label, coef) {
  cf <- optimal_poly_cut(coef, tol = 1e-10)
  bf <- brute_force_optimal_cut(coef, n_candidates = 100001L, n_mesh = 4e6L)
  rf <- refine_optimal_cut_numeric(bf$t, coef)

  cat(sprintf("  %s:  g(x2) = %s   (vertex at x2 = %g)\n", label,
              fmt_coef(coef), cf$vertex))
  cat(sprintf("    optimize(tol=1e-10) on closed-form S(t)   t = %.12f   S = %.12f\n",
              cf$t, cf$sse))
  cat(sprintf("    analytic root                            t = %.12f\n", cf$analytic_t))
  cat(sprintf("    brute force, %d-pt grid (RAW)         t = %.12f   S = %.12f   (grid spacing %.3g)\n",
              bf$n_candidates, bf$t, bf$sse, bf$grid_spacing))
  cat(sprintf("    brute force, integrate() refinement      t = %.12f   S = %.12f   (grid spacing %.3g)\n",
              rf$t, rf$sse, rf$spacing))

  d_analytic <- abs(cf$t - cf$analytic_t)
  d_coarse   <- abs(cf$t - bf$t)
  d_refined  <- abs(cf$t - rf$t)
  d_sse      <- abs(cf$sse - bf$sse)

  # F5 (own flag from the pre-revision pass): report what optimize() ACTUALLY
  # achieved, not the tol that was requested. The analytic root is the only
  # exactly-known value available, so |t_optimize - t_analytic| IS the achieved
  # precision; ~1.5e-08 is optimize()'s structural floor for a smooth interior
  # minimum (golden-section/parabolic cannot beat ~sqrt(machine eps)).
  cat(sprintf("    optimize() achieved precision = %.3e  (requested tol 1e-10; structural floor ~%.1e)\n",
              d_analytic, .Machine$double.eps^0.5))

  record(sprintf("%s: the minimizer is unique", label), cf$n_optima == 1L,
         sprintf("%d near-global minimum/minima found on a 20001-point scan",
                 cf$n_optima))
  record(sprintf("%s: optimize() agrees with the analytic root", label),
         d_analytic < TOL_CUT,
         sprintf("|t_optimize - t_analytic| = %.3e  (tol %.0e) -- this is also the ACHIEVED precision, vs the 1e-10 requested",
                 d_analytic, TOL_CUT))
  record(sprintf("%s: closed-form objective agrees with numerical quadrature", label),
         d_sse < TOL_EXACT,
         sprintf("|S_closed(t*) - S_bruteforce(t_raw)| = %.3e  (tol %.0e)",
                 d_sse, TOL_EXACT))
  # F3: the RAW grid argmin is reported as its own line but is NOT the gate --
  # its 1e-5 spacing bounds the achievable agreement at 5e-6, above the spec's
  # 1e-6 tolerance, for purely grid reasons. The gate is the integrate()-based
  # local refinement at 1e-8 spacing.
  record(sprintf("%s: RAW brute-force grid argmin is within its own grid resolution (informational, NOT the 1e-6 gate)", label),
         d_coarse <= bf$grid_spacing / 2 + TOL_EXACT,
         sprintf("|t_closed - t_raw| = %.3e, half grid spacing = %.3e -- spacing-limited, as expected",
                 d_coarse, bf$grid_spacing / 2))
  record(sprintf("%s: REFINED brute-force argmin agrees with closed form (the 1e-6 gate)", label),
         d_refined < TOL_CUT,
         sprintf("|t_closed - t_refined| = %.3e  (tol %.0e), at %.0e refinement spacing",
                 d_refined, TOL_CUT, rf$spacing))

  list(t = cf$t, sse = cf$sse, analytic = cf$analytic_t, vertex = cf$vertex,
       raw = bf$t, refined = rf$t, achieved_precision = d_analytic)
}

cut_L <- verify_cut("t2*_L (left)", params$left_coef)

if (right_identified) {
  cut_R <- verify_cut("t2*_R (right)", params$right_coef)
} else {
  cat("  t2*_R (right): SKIPPED -- not identified (see check 1).\n")
  record("t2*_R: closed form verified against brute force", FALSE,
         "not attempted: the quantity does not exist for these DGP parameters")
  # A placeholder cut is still needed to build a 4-leaf topology for the
  # topology comparison in section 4. Any value gives the same population
  # risk (that is exactly the degeneracy), so the left side's t2* is reused
  # and this is labelled, not hidden.
  cut_R <- list(t = cut_L$t, sse = NA_real_, analytic = NA_real_,
                vertex = NA_real_, raw = NA_real_, refined = NA_real_,
                achieved_precision = NA_real_)
}

record("t2*_L and t2*_R are DISTINCT (the design's matched-but-not-identical cosmetic pair)",
       right_identified && abs(cut_L$t - cut_R$t) > 1e-3,
       sprintf("t2*_L = %.10f, t2*_R = %.10f, |difference| = %.6f",
               cut_L$t, cut_R$t, abs(cut_L$t - cut_R$t)))
record("t2*_L + t2*_R == 1 (exact mirror relation implied by the revised right piece)",
       right_identified && abs(cut_L$t + cut_R$t - 1) < TOL_CUT,
       sprintf("t2*_L + t2*_R - 1 = %.3e  (tol %.0e)",
               cut_L$t + cut_R$t - 1, TOL_CUT))
cat("\n")

# ---- 3 f* leaf means: closed form vs nested integrate() vs 1e7 MC ---------

cat("--- 3 f* leaf means and population risk ---\n")

topology_star <- depth2_topology("x1", params$t1, "x2", cut_L$t, "x2", cut_R$t)
risk_star <- population_tree_risk(topology_star, params)

mc_eval <- load_mc_eval_sample()
assert_mc_eval_matches(mc_eval, params)
mc_star <- mc_tree_risk(topology_star, mc_eval$x1, mc_eval$x2, mc_eval$gamma0)

# f* evaluated with its EXACT population leaf means (not refit on the sample).
# This is the reference risk every replication's excess risk is measured
# against, so it is computed here, once, with the same machinery one_sim.R uses.
fstar_pred <- depth2_tree_predict(topology_star, risk_star$leaf_means,
                                   mc_eval$x1, mc_eval$x2)
mc_risk_fstar <- mean((mc_eval$gamma0 - fstar_pred)^2)

num_means <- vapply(risk_star$leaf_rects, gamma0_rect_mean_numeric, numeric(1),
                     params = params)

leaf_labels <- c("x1<=t1 & x2<=t2L", "x1<=t1 & x2>t2L",
                 "x1>t1 & x2<=t2R",  "x1>t1 & x2>t2R")
cat("  leaf                  area      closed form     nested integrate()      MC (N=1e7)      MC s.e.\n")
for (k in seq_len(4L)) {
  cat(sprintf("  %-20s %8.6f  %14.10f  %18.10f  %14.10f  %11.3e\n",
              leaf_labels[[k]], risk_star$leaf_areas[[k]],
              risk_star$leaf_means[[k]], num_means[[k]],
              mc_star$leaf_means[[k]], mc_star$leaf_se[[k]]))
}

d_num <- max(abs(risk_star$leaf_means - num_means))
record("f* leaf means: closed form agrees with nested integrate() (AUTHORITATIVE exact check)",
       d_num < TOL_EXACT,
       sprintf("max |closed - numeric| = %.3e  (tol %.0e)", d_num, TOL_EXACT))

# F4 / spec Section 3 "Excess-risk evaluation" (revised): the MC comparison is
# stated in MC standard-error units, not as an absolute threshold. It is a
# sanity check on the MC harness, not on the closed form (the nested
# integrate() check above is the authoritative one). The raw absolute
# difference is printed for the record but is NOT a gate.
d_mc <- abs(risk_star$leaf_means - mc_star$leaf_means)
mc_z <- max(d_mc / mc_star$leaf_se, na.rm = TRUE)
cat(sprintf("  max |closed - MC| = %.3e  (for the record; a leaf mean's own MC s.e. here is %.1e-%.1e, so an absolute 1e-4 gate is below MC noise and is NOT used)\n",
            max(d_mc), min(mc_star$leaf_se[mc_star$leaf_se > 0]),
            max(mc_star$leaf_se)))
record("f* leaf means: closed form agrees with 1e7-draw Monte Carlo within MC noise",
       mc_z < TOL_MC_SE,
       sprintf("max |closed - MC| / (that leaf's own MC s.e.) = %.2f  (gate %g s.e.)",
               mc_z, TOL_MC_SE))

# R(f*) itself is a mean over all 1e7 draws, so its MC s.e. is small enough
# that an absolute comparison IS meaningful here (unlike the per-leaf means).
risk_star_se <- stats::sd((mc_eval$gamma0 - fstar_pred)^2) / sqrt(mc_eval$n_mc)
d_risk <- abs(risk_star$risk - mc_risk_fstar)
record("R(f*): closed form agrees with 1e7-draw Monte Carlo within MC noise",
       d_risk < TOL_MC_SE * risk_star_se,
       sprintf("closed = %.10f, MC = %.10f, |diff| = %.3e, MC s.e. = %.3e -> %.2f s.e.  (gate %g s.e.)",
               risk_star$risk, mc_risk_fstar, d_risk, risk_star_se,
               d_risk / risk_star_se, TOL_MC_SE))
cat("\n")

# ---- 4 Topology verification (MANDATORY, spec Section 3) -----------------
#
# Enumerate every depth-2 shape over (x1, x2) and minimize exact population
# risk over its three cutpoints. The constructed topology must be the STRICT
# unique minimizer; a tie means f* is not unique, so "topology match" is not a
# well-defined diagnostic and the threshold-error metrics have no single
# target.

cat("--- 4 Topology verification over all 8 depth-2 shapes ---\n")

shapes <- depth2_shapes()
constructed_cuts <- c(params$t1, cut_L$t, cut_R$t)
# E[gamma_0 | x2] = (left_coef + right_coef)/2, since X1 is Uniform(0,1) and
# t1 = 0.5 splits it into equal halves -- weight each side by its x1 mass.
marginal_coef <- params$t1 * params$left_coef + (1 - params$t1) * params$right_coef
best_x2_single <- optimal_poly_cut(marginal_coef)$t

shape_rows <- list()
for (i in seq_len(nrow(shapes))) {
  shape <- as.list(shapes[i, ])
  starts <- list(constructed_cuts,
                  c(best_x2_single, params$t1, params$t1),
                  c(params$t1, params$t1, params$t1),
                  c(best_x2_single, best_x2_single, best_x2_single))
  b <- best_risk_for_shape(shape, params, n_grid = 25L, extra_starts = starts)
  shape_rows[[i]] <- data.frame(
    shape = sprintf("root=%s | left=%s | right=%s", shape$root, shape$left, shape$right),
    risk = b$risk, cut_root = b$cuts[[1L]], cut_left = b$cuts[[2L]],
    cut_right = b$cuts[[3L]], stringsAsFactors = FALSE
  )
}
shape_tab <- do.call(rbind, shape_rows)
shape_tab <- shape_tab[order(shape_tab$risk), ]

cat(sprintf("  constructed f* topology  risk = %.12f   (cuts %.8f / %.8f / %.8f)\n\n",
            risk_star$risk, constructed_cuts[[1L]], constructed_cuts[[2L]],
            constructed_cuts[[3L]]))
cat("  best risk attainable by each depth-2 shape (ascending):\n")
cat("  shape                              risk            cut_root    cut_left    cut_right\n")
for (i in seq_len(nrow(shape_tab))) {
  cat(sprintf("  %-34s %.12f  %10.7f  %10.7f  %10.7f\n",
              shape_tab$shape[[i]], shape_tab$risk[[i]], shape_tab$cut_root[[i]],
              shape_tab$cut_left[[i]], shape_tab$cut_right[[i]]))
}

constructed_shape <- "root=x1 | left=x2 | right=x2"
other <- shape_tab[shape_tab$shape != constructed_shape, ]
best_other_i <- which.min(other$risk)
best_other <- other$risk[[best_other_i]]
margin <- best_other - risk_star$risk

# Cross-check the best ALTERNATIVE shape's closed-form risk against the same
# 1e7 MC sample before drawing any conclusion from it. A topology-verification
# verdict that rests on an unchecked optimizer output would be exactly the
# kind of unaudited number this whole gate exists to prevent.
best_other_topology <- depth2_topology(
  sub(".*root=([^ ]+).*", "\\1", other$shape[[best_other_i]]),
  other$cut_root[[best_other_i]],
  sub(".*left=([^ ]+).*", "\\1", other$shape[[best_other_i]]),
  other$cut_left[[best_other_i]],
  sub(".*right=([^ ]+).*", "\\1", other$shape[[best_other_i]]),
  other$cut_right[[best_other_i]]
)
mc_other <- mc_tree_risk(best_other_topology, mc_eval$x1, mc_eval$x2,
                          mc_eval$gamma0)
mc_other_se <- stats::sd((mc_eval$gamma0 -
  depth2_tree_predict(best_other_topology,
                       population_tree_risk(best_other_topology, params)$leaf_means,
                       mc_eval$x1, mc_eval$x2))^2) / sqrt(mc_eval$n_mc)
cat(sprintf("\n  best alternative shape, closed form = %.12f   MC (N=1e7, leaf means refit) = %.12f   |diff| = %.3e\n",
            best_other, mc_other$risk, abs(best_other - mc_other$risk)))
record("best alternative shape's closed-form risk agrees with 1e7-draw Monte Carlo",
       abs(best_other - mc_other$risk) < TOL_MC_SE * mc_other_se,
       sprintf("closed = %.10f, MC = %.10f, |diff| = %.3e, MC s.e. = %.3e -> %.2f s.e.  (gate %g s.e.)",
               best_other, mc_other$risk, abs(best_other - mc_other$risk),
               mc_other_se, abs(best_other - mc_other$risk) / mc_other_se,
               TOL_MC_SE))

record("constructed topology has STRICTLY lower population risk than every other depth-2 shape",
       margin > TOL_TIE,
       sprintf("R(constructed) = %.12f, best alternative shape = %.12f (%s), margin = %+.6e  (need > %.0e)",
               risk_star$risk, best_other, other$shape[[best_other_i]], margin,
               TOL_TIE))
cat("\n")

# ---- 5 Verdict -----------------------------------------------------------

cat("=========================================================\n")
failed <- Filter(function(c) !c$passed, checks)
cat(sprintf("%d of %d checks passed.\n", length(checks) - length(failed),
            length(checks)))
if (length(failed)) {
  cat("\nFAILED CHECKS:\n")
  for (f in failed) cat(sprintf("  - %s\n      %s\n", f$name, f$detail))
} else {
  cat("\nGATE PASSED. Ground truth for this Delta1 is verified; downstream\n")
  cat("replication machinery may run against it.\n")
  cat(sprintf("  t1*      = %.12f  (exact, by construction)\n", params$t1))
  cat(sprintf("  t2*_L    = %.12f\n", cut_L$t))
  cat(sprintf("  t2*_R    = %.12f\n", cut_R$t))
  cat(sprintf("  R(f*)    = %.12f  (closed form)\n", risk_star$risk))
  cat(sprintf("  R(f*)    = %.12f  (on the fixed N_MC = 1e7 sample -- the\n", mc_risk_fstar))
  cat("             reference every replication's excess risk subtracts)\n")
}
cat("=========================================================\n")

saveRDS(
  list(params = params, checks = checks, cut_L = cut_L, cut_R = cut_R,
       topology_star = topology_star, risk_star = risk_star,
       mc_star = mc_star, mc_risk_fstar = mc_risk_fstar,
       shape_tab = shape_tab, right_identified = right_identified,
       jump_zeros = jump_zeros, passed = length(failed) == 0L),
  file.path(STUDY_DIR, "results", "ground_truth_verification.rds")
)

if (length(failed) && !interactive() && sys.nframe() == 0L) {
  quit(status = 1L)
}
