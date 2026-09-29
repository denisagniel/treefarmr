# ============================================================
# Timing / sanity pilot: 9 configurations x n_reps replications
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 8, "Recommended staging" step 1 -- a REDUCED grid, used to
#        size the real pilot; this is not the Regime-F pilot itself)
# Purpose: real per-fit wall-clock cost and its scaling in (n, m_n), plus a
#          first, deliberately underpowered look at certified rate,
#          topology-match rate, and excess risk.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R, one_sim.R,
#         results/mc_eval_sample.rds, results/ground_truth_verification.rds
# Outputs: results/timing_pilot.rds (written incrementally), stdout report
# ============================================================
#
# Usage: Rscript timing_pilot.R [n_reps]
# Written incrementally (one save per completed call) so a long run that is
# interrupted still leaves every finished replication on disk.

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

# Reduced grid for the timing read (this task's scope). The FULL Regime-F grid
# is n(6) x m_n(7) at 500 reps and is explicitly NOT run here.
PILOT_N      <- c(500L, 4000L, 16000L)
PILOT_M      <- c(8L, 32L, 128L)
PILOT_DELTA1 <- 5      # Regime F, spec Section 3 as revised 2026-09-28
PILOT_P      <- 2L

args <- commandArgs(trailingOnly = TRUE)
N_REPS <- if (length(args) >= 1L) as.integer(args[[1L]]) else 10L
OUT_PATH <- file.path(STUDY_DIR, "results",
                       sprintf("timing_pilot_reps%d.rds", N_REPS))

params <- dgp_params_jump_cosmetic(delta1 = PILOT_DELTA1)
mc_eval <- prepare_mc_eval(load_mc_eval_sample())
gt <- study_ground_truth(
  params, mc_eval,
  verification_path = file.path(STUDY_DIR, "results",
                                 "ground_truth_verification.rds")
)

grid <- expand.grid(rep = seq_len(N_REPS), m_n = PILOT_M, n = PILOT_N)
grid <- grid[order(grid$n, grid$m_n, grid$rep), c("n", "m_n", "rep")]

# Resume from a partial run. Learned the hard way: the first 10-rep run was
# killed by the OS partway through the last (and by far the most expensive)
# configuration, and although every completed replication WAS on disk, the run
# had no way to pick up where it left off. Because each cell's seed is a pure
# function of (n, m_n, delta1, p, rep), a resumed replication is bit-identical
# to the one the interrupted run would have produced -- verified by re-running
# one cell in a fresh session and comparing every field.
rows <- list()
done_key <- character(0)
if (file.exists(OUT_PATH)) {
  prev <- readRDS(OUT_PATH)
  keep <- prev$delta1 == PILOT_DELTA1 & prev$p == PILOT_P
  prev <- prev[keep, , drop = FALSE]
  if (nrow(prev) > 0L) {
    rows <- split(prev, seq_len(nrow(prev)))
    done_key <- sprintf("%d|%d|%d", prev$n, prev$m_n, prev$rep)
    cat(sprintf("Resuming: %d replication(s) already on disk at %s.\n",
                nrow(prev), OUT_PATH))
  }
}
grid_key <- sprintf("%d|%d|%d", grid$n, grid$m_n, grid$rep)
todo <- which(!grid_key %in% done_key)

cat("=========================================================\n")
cat(sprintf("Timing pilot: %d configs x %d reps = %d one_sim() calls (%d still to run)\n",
            length(PILOT_N) * length(PILOT_M), N_REPS, nrow(grid),
            length(todo)))
cat(sprintf("  n in {%s}, m_n in {%s}, delta1 = %g, p = %d\n",
            paste(PILOT_N, collapse = ", "), paste(PILOT_M, collapse = ", "),
            PILOT_DELTA1, PILOT_P))
cat(sprintf("  writing to %s\n", OUT_PATH))
cat("=========================================================\n\n")

t_pilot <- Sys.time()
for (k in seq_along(todo)) {
  i <- todo[[k]]
  g <- grid[i, ]
  t0 <- Sys.time()
  r <- one_sim(n = g$n, m_n = g$m_n, delta1 = PILOT_DELTA1, p = PILOT_P,
                ground_truth = gt, mc_eval = mc_eval, rep = g$rep)
  rows[[length(rows) + 1L]] <- r
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  [%3d/%3d] n=%5d m_n=%3d rep=%2d  %7.2fs  status=%-24s topology_match=%-5s excess=%s\n",
              k, length(todo), g$n, g$m_n, g$rep, el, r$status,
              r$topology_match, format(r$excess_risk, digits = 4)))
  # Incremental save after EVERY call: a 90-call run that is killed at call 90
  # must not lose the first 89 (this happened, and this save is why it did not).
  saveRDS(do.call(rbind, rows), OUT_PATH, compress = FALSE)
}
res <- do.call(rbind, rows)
res <- res[order(res$n, res$m_n, res$rep), ]
total_secs <- as.numeric(difftime(Sys.time(), t_pilot, units = "secs"))

# Summary tables live in summarize_pilot.R, not here: running them only at the
# END of a ~2-hour job means an interrupted run yields no tables at all even
# though every replication is already saved. Called as a separate step so the
# tables can always be regenerated from the saved table alone.
cat(sprintf("\nDone. %d replication(s) in this invocation, %.1f s (%.2f h).\n",
            length(todo), total_secs, total_secs / 3600))
cat(sprintf("Summarize with:\n  Rscript %s %s\n",
            file.path(STUDY_DIR, "summarize_pilot.R"), OUT_PATH))

saveRDS(list(results = res, total_secs = total_secs, n_reps = N_REPS,
             params = params, grid = grid),
        file.path(STUDY_DIR, "results",
                   sprintf("timing_pilot_reps%d_full.rds", N_REPS)))
