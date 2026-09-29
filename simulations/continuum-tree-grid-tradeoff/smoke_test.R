# ============================================================
# Smoke test: exactly ONE replication, every returned field printed
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
# Purpose: confirm the whole one_sim() path runs end to end at a single
#          (n, m_n) and expose every field it returns for inspection, plus
#          wall-clock time for that one call.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R, one_sim.R,
#         results/mc_eval_sample.rds, results/ground_truth_verification.rds
# Outputs: stdout report; results/smoke_test.rds
# ============================================================

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}

suppressMessages(devtools::load_all(".", quiet = TRUE))
source(file.path(STUDY_DIR, "dgp.R"))
source(file.path(STUDY_DIR, "ground_truth.R"))
source(file.path(STUDY_DIR, "mc_eval_sample.R"))
source(file.path(STUDY_DIR, "one_sim.R"))

SMOKE_N     <- 2000L
SMOKE_M     <- 64L
SMOKE_DELTA <- 5      # Regime F, spec Section 3 as revised 2026-09-28

params <- dgp_params_jump_cosmetic(delta1 = SMOKE_DELTA)
mc_eval <- prepare_mc_eval(load_mc_eval_sample())
gt <- study_ground_truth(
  params, mc_eval,
  verification_path = file.path(STUDY_DIR, "results",
                                 "ground_truth_verification.rds")
)

cat("=========================================================\n")
cat("Smoke test: one_sim(), 1 replication\n")
cat(sprintf("  n = %d, m_n = %d, delta1 = %g, p = 2\n", SMOKE_N, SMOKE_M,
            SMOKE_DELTA))
cat(sprintf("  ground truth: t1* = %.10f, t2*_L = %.10f, t2*_R = %.10f\n",
            gt$t1_star, gt$t2_star_L, gt$t2_star_R))
cat(sprintf("  R(f*) = %.10f (closed form), %.10f (on the fixed N_MC sample)\n",
            gt$risk, gt$fstar_risk_mc))
cat("=========================================================\n\n")

t0 <- Sys.time()
res <- one_sim(n = SMOKE_N, m_n = SMOKE_M, delta1 = SMOKE_DELTA, p = 2,
                ground_truth = gt, mc_eval = mc_eval, rep = 1L)
wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat("--- every returned field ---\n")
for (nm in names(res)) {
  v <- res[[nm]]
  cat(sprintf("  %-22s %s\n", nm,
              if (is.numeric(v) && !is.na(v)) format(v, digits = 10) else
                format(v)))
}
cat(sprintf("\n  wall-clock time for this ONE one_sim() call: %.3f s\n", wall))
cat(sprintf("  (internal breakdown: dgp %.3f + stage A %.3f + stage B %.3f + eval %.3f = total %.3f)\n",
            res$secs_dgp, res$secs_stage_a, res$secs_stage_b, res$secs_eval,
            res$secs_total))

saveRDS(list(result = res, wall = wall, params = params),
        file.path(STUDY_DIR, "results", "smoke_test.rds"))
