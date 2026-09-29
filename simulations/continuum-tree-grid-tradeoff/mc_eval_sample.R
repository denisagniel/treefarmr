# ============================================================
# Fixed Monte Carlo evaluation sample for excess-risk integration
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 3, "Excess-risk evaluation"; Section 9, reproducibility)
# Purpose: draw ONE fixed N_MC = 1e7 sample of X, once, and evaluate gamma_0
#          on it exactly, so MC-integration noise is a constant, documented
#          offset shared by every replication and configuration rather than an
#          extra source of across-config variance.
# Inputs: dgp.R (sourced below)
# Outputs: simulations/continuum-tree-grid-tradeoff/results/mc_eval_sample.rds
# ============================================================
#
# Run as a script (`Rscript mc_eval_sample.R`) to (re)generate the sample, or
# source it and call `load_mc_eval_sample()` to read the saved one.

# ---- 0 Setup -------------------------------------------------------------

STUDY_DIR <- if (requireNamespace("rprojroot", quietly = TRUE) &&
                  !is.null(getOption("continuum_tree_study_dir"))) {
  getOption("continuum_tree_study_dir")
} else {
  # Working default so sourcing this file interactively from either the
  # package root or the study directory resolves (interactive-entry M1).
  if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
    "simulations/continuum-tree-grid-tradeoff"
  } else {
    "."
  }
}

source(file.path(STUDY_DIR, "dgp.R"))

# N_MC = 1e7 per spec Section 3: gives ~3e-4 relative MC-integration error on
# the risk functionals, several orders below the smallest excess-risk
# difference the study reads (O(sqrt(log n / n)) at n = 16000 is ~2e-2).
N_MC <- 1e7

# One fixed seed, drawn once and reused verbatim across every replication and
# configuration (spec Section 9). YYYYMMDD form per
# .claude/rules/r-code-conventions.md sec 1; distinct from the per-replication
# seed stream so the evaluation sample can never collide with training data.
MC_EVAL_SEED <- 20260928L

MC_EVAL_PATH <- file.path(STUDY_DIR, "results", "mc_eval_sample.rds")

# ---- 1 Generate ----------------------------------------------------------

#' Draw the fixed evaluation sample and evaluate gamma_0 on it
#'
#' @param n_mc Sample size. Default `N_MC`.
#' @param p Number of covariates. gamma_0 ignores coordinates beyond x1, x2,
#'   so only x1 and x2 are stored -- the noise coordinates of Regime S2 carry
#'   no information about gamma_0 and a fitted tree that splits on one is
#'   evaluated through its leaf means, which this sample supplies via x1/x2.
#'   Kept as an explicit argument (rather than assumed) so a later S2 run that
#'   DOES need noise coordinates on the evaluation sample fails loudly here
#'   instead of silently reusing a p = 2 sample.
#' @param seed Default `MC_EVAL_SEED`.
#' @param params A `dgp_params_jump_cosmetic()` list.
build_mc_eval_sample <- function(n_mc = N_MC, p = 2L, seed = MC_EVAL_SEED,
                                  params = dgp_params_jump_cosmetic()) {
  if (p != 2L) {
    cli::cli_abort(c(
      "build_mc_eval_sample: only {.code p = 2} is implemented.",
      "i" = "Regime S2 (p in 5, 8) needs an evaluation sample carrying the \\
             noise coordinates a fitted tree may split on; reusing the p = 2 \\
             sample there would silently evaluate the wrong function."
    ))
  }
  # Generator pinned, not inherited -- see seed_study_rng() in dgp.R for why
  # bare set.seed() is not reproducible across execution contexts.
  seed_study_rng(seed)
  x1 <- stats::runif(n_mc)
  x2 <- stats::runif(n_mc)
  list(
    x1 = x1, x2 = x2,
    gamma0 = gamma_0_jump_cosmetic(x1, x2, params),
    n_mc = n_mc, p = as.integer(p), seed = as.integer(seed), params = params
  )
}

#' Read the saved evaluation sample, erroring if it is absent
load_mc_eval_sample <- function(path = MC_EVAL_PATH) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "load_mc_eval_sample: {.file {path}} does not exist.",
      "i" = "Run {.code Rscript mc_eval_sample.R} once to generate it. It is \\
             deliberately not regenerated on demand: a silently re-drawn \\
             sample would break the spec's requirement that every \\
             replication integrate against the SAME sample."
    ))
  }
  readRDS(path)
}

#' Assert a saved evaluation sample matches the DGP parameters in use
#'
#' gamma_0 stored in the sample depends on (delta1, a_L, b_L, b_R, t1). A
#' parameter change must invalidate the stored gamma_0 loudly, not be absorbed
#' into a wrong excess-risk number (interactive-entry F1).
assert_mc_eval_matches <- function(mc_eval, params) {
  fields <- c("delta1", "t1", "a_L", "b_L", "b_R")
  diffs <- fields[vapply(fields, function(f) {
    !isTRUE(all.equal(mc_eval$params[[f]], params[[f]]))
  }, logical(1))]
  if (length(diffs)) {
    cli::cli_abort(c(
      "assert_mc_eval_matches: the saved evaluation sample was built under \\
       different DGP parameters ({.field {diffs}}).",
      "i" = "Its stored gamma_0 is therefore the wrong function; regenerate \\
             the sample rather than reusing it."
    ))
  }
  invisible(TRUE)
}

# ---- 2 Export ------------------------------------------------------------

if (!interactive() && sys.nframe() == 0L) {
  t0 <- Sys.time()
  dir.create(dirname(MC_EVAL_PATH), recursive = TRUE, showWarnings = FALSE)
  mc_eval <- build_mc_eval_sample()
  # compress = FALSE: 1e7 i.i.d. uniforms are incompressible, so compression
  # costs minutes and saves nothing.
  saveRDS(mc_eval, MC_EVAL_PATH, compress = FALSE)
  cat(sprintf(
    "mc_eval_sample: N_MC = %.0f, p = %d, seed = %d\n  written to %s (%.1f MB) in %.1f s\n",
    mc_eval$n_mc, mc_eval$p, mc_eval$seed, MC_EVAL_PATH,
    file.size(MC_EVAL_PATH) / 1024^2,
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  cat(sprintf(
    "  E[gamma_0] = %.10f   E[gamma_0^2] = %.10f   Var[gamma_0] = %.10f\n",
    mean(mc_eval$gamma0), mean(mc_eval$gamma0^2),
    mean(mc_eval$gamma0^2) - mean(mc_eval$gamma0)^2
  ))
}
