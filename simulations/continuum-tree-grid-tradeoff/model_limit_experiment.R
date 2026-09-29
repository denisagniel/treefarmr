# ============================================================
# Bounded experiment: does an explicit model_limit = 0 speed up Stage A?
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
# Purpose: the 10-rep timing pilot measured Stage-A cost 2-3 orders of
#          magnitude above spec Section 8's assumption, and flagged that
#          fit_tree()'s dimensionality heuristic imposes model_limit = 1e6 at
#          p*(m_n-1) > 100 estimated binary features -- even though that same
#          function's own comment says model_limit should be disabled for
#          single-tree extraction, which is exactly this study's case (L = 4).
#          This tests whether overriding it helps, on IDENTICAL seeds.
# Scope: 3 cells x 5 replications = 15 calls. Nothing else.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R, one_sim.R,
#         results/timing_pilot_reps10.rds (the model_limit-default baseline)
# Outputs: results/model_limit_experiment.rds; stdout report.
#          Writes NOTHING that the timing pilot produced.
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

CELLS <- data.frame(n   = c(16000L, 4000L, 16000L),
                     m_n = c(  128L,  128L,    32L))
N_REPS <- 5L
DELTA1 <- 5
P      <- 2L
OUT_PATH <- file.path(STUDY_DIR, "results", "model_limit_experiment.rds")
BASELINE_PATH <- file.path(STUDY_DIR, "results", "timing_pilot_reps10.rds")

params  <- dgp_params_jump_cosmetic(delta1 = DELTA1)
mc_eval <- prepare_mc_eval(load_mc_eval_sample())
gt <- study_ground_truth(
  params, mc_eval,
  verification_path = file.path(STUDY_DIR, "results",
                                 "ground_truth_verification.rds")
)

baseline <- readRDS(BASELINE_PATH)

cat("=========================================================\n")
cat("model_limit = 0 experiment: 3 cells x 5 reps = 15 calls\n")
cat("  baseline (model_limit left to fit_tree()'s heuristic) read from\n")
cat(sprintf("  %s -- NOT modified.\n", BASELINE_PATH))
cat("=========================================================\n\n")

rows <- list()
for (ci in seq_len(nrow(CELLS))) {
  for (rp in seq_len(N_REPS)) {
    nn <- CELLS$n[[ci]]; mm <- CELLS$m_n[[ci]]
    # Seeds are a pure function of (n, m_n, delta1, p, rep), so the baseline
    # row for this cell/rep and this run's row use the SAME data by
    # construction. Asserted rather than assumed -- the whole comparison is
    # void if the two runs see different draws.
    want_seed <- sim_seed(nn, mm, DELTA1, P, rp)
    b <- baseline[baseline$n == nn & baseline$m_n == mm & baseline$rep == rp, ]
    if (nrow(b) != 1L) {
      stop(sprintf("baseline has %d rows for n=%d m_n=%d rep=%d; expected 1",
                   nrow(b), nn, mm, rp))
    }
    if (b$seed != want_seed) {
      stop(sprintf("seed mismatch at n=%d m_n=%d rep=%d: baseline %d, derived %d",
                   nn, mm, rp, b$seed, want_seed))
    }
    r <- one_sim(n = nn, m_n = mm, delta1 = DELTA1, p = P,
                  ground_truth = gt, mc_eval = mc_eval, rep = rp,
                  model_limit = 0)
    rows[[length(rows) + 1L]] <- r
    cat(sprintf("  n=%5d m_n=%3d rep=%d  model_limit=0: %7.2fs (stage A %7.2fs)  |  default: %7.2fs (stage A %7.2fs)  ratio %.2fx\n",
                nn, mm, rp, r$secs_total, r$secs_stage_a,
                b$secs_total, b$secs_stage_a,
                b$secs_stage_a / r$secs_stage_a))
    saveRDS(do.call(rbind, rows), OUT_PATH, compress = FALSE)
  }
}
res <- do.call(rbind, rows)

# ---- Timing comparison ---------------------------------------------------

cat("\n--- wall clock, model_limit = 0 vs default, same seeds ---\n")
cat("                        model_limit = 0            fit_tree() default         ratio (default/ml0)\n")
cat("  cell              mean_tot  max_tot  mean_A    mean_tot  max_tot  mean_A     tot     stageA\n")
cmp_rows <- list()
for (ci in seq_len(nrow(CELLS))) {
  nn <- CELLS$n[[ci]]; mm <- CELLS$m_n[[ci]]
  e <- res[res$n == nn & res$m_n == mm, ]
  b <- baseline[baseline$n == nn & baseline$m_n == mm &
                  baseline$rep <= N_REPS, ]
  cmp_rows[[ci]] <- data.frame(
    n = nn, m_n = mm, n_reps = nrow(e),
    ml0_mean_total = mean(e$secs_total), ml0_max_total = max(e$secs_total),
    ml0_mean_stage_a = mean(e$secs_stage_a),
    def_mean_total = mean(b$secs_total), def_max_total = max(b$secs_total),
    def_mean_stage_a = mean(b$secs_stage_a),
    ratio_total = mean(b$secs_total) / mean(e$secs_total),
    ratio_stage_a = mean(b$secs_stage_a) / mean(e$secs_stage_a)
  )
  cat(sprintf("  n=%5d m_n=%3d  %8.2f %8.2f %8.2f  %10.2f %8.2f %8.2f  %7.2fx %7.2fx\n",
              nn, mm, mean(e$secs_total), max(e$secs_total),
              mean(e$secs_stage_a), mean(b$secs_total), max(b$secs_total),
              mean(b$secs_stage_a),
              mean(b$secs_total) / mean(e$secs_total),
              mean(b$secs_stage_a) / mean(e$secs_stage_a)))
}
cmp <- do.call(rbind, cmp_rows)
cat("\n  (baseline columns use the FIRST 5 replications of each cell from the\n")
cat("   10-rep pilot -- the same 5 seeds this run used, not the 10-rep means.)\n")

# ---- Correctness comparison ---------------------------------------------
#
# model_limit is a cap on the number of models the search may enumerate;
# removing the cap should not change the returned optimum. Verified field by
# field on identical seeds rather than assumed -- a difference here would
# itself be a finding (it would mean the 1e6 cap was silently truncating the
# search and the recorded pilot results are not the certified optimum).

cat("\n--- correctness fields, model_limit = 0 vs default, same seeds ---\n")
check_cols <- c("status", "n_leaves", "lambda", "certified", "feasible",
                 "gap", "any_truncated", "used_search", "n_fits",
                 "solver_status_max", "bins_exact", "n_splits",
                 "root_coord", "left_coord", "right_coord", "topology_match",
                 "t1_hat", "t2L_hat", "t2R_hat", "risk_fhat", "excess_risk",
                 "reason_root", "reason_left", "reason_right",
                 "grid_cut_root", "grid_cut_left", "grid_cut_right")
diffs <- list()
for (i in seq_len(nrow(res))) {
  e <- res[i, ]
  b <- baseline[baseline$n == e$n & baseline$m_n == e$m_n &
                  baseline$rep == e$rep, ]
  for (cl in check_cols) {
    same <- if (is.numeric(e[[cl]]) && is.numeric(b[[cl]])) {
      isTRUE(all.equal(e[[cl]], b[[cl]], tolerance = 0))
    } else {
      identical(e[[cl]], b[[cl]])
    }
    if (!same) {
      diffs[[length(diffs) + 1L]] <- data.frame(
        n = e$n, m_n = e$m_n, rep = e$rep, field = cl,
        model_limit_0 = format(e[[cl]], digits = 15),
        default = format(b[[cl]], digits = 15), stringsAsFactors = FALSE
      )
    }
  }
}
if (length(diffs) == 0L) {
  cat(sprintf("  IDENTICAL on all %d checked field(s) x %d replication(s) = %d comparisons,\n",
              length(check_cols), nrow(res), length(check_cols) * nrow(res)))
  cat("  at tolerance 0 (bit-exact). Removing the cap changed nothing about the\n")
  cat("  returned fit, which is what a pure search-space cap should do.\n")
} else {
  cat(sprintf("  %d DIFFERENCE(S) FOUND -- this is itself a finding:\n", length(diffs)))
  print(do.call(rbind, diffs), row.names = FALSE)
}

cat("\n--- warnings ---\n")
cat(sprintf("  model_limit = 0:  %d of %d replications warned\n",
            sum(res$n_warnings > 0L), nrow(res)))
base5 <- baseline[baseline$rep <= N_REPS &
                    paste(baseline$n, baseline$m_n) %in%
                      paste(CELLS$n, CELLS$m_n), ]
cat(sprintf("  default:          %d of %d replications warned\n",
            sum(base5$n_warnings > 0L), nrow(base5)))

cat("\n=========================================================\n")
best <- max(cmp$ratio_stage_a)
cat(sprintf("Largest Stage-A speedup observed: %.2fx  (cell n=%d, m_n=%d)\n",
            best, cmp$n[[which.max(cmp$ratio_stage_a)]],
            cmp$m_n[[which.max(cmp$ratio_stage_a)]]))
cat(sprintf("Smallest: %.2fx\n", min(cmp$ratio_stage_a)))
cat("=========================================================\n")

saveRDS(list(results = res, comparison = cmp, diffs = diffs,
             cells = CELLS, n_reps = N_REPS, params = params),
        file.path(STUDY_DIR, "results", "model_limit_experiment_full.rds"))
