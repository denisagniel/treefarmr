# ============================================================
# Supplementary timing check at m_n = 64 (the new grid ceiling)
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 8, resourcing revision: m_n capped at 64)
# Purpose: the 10-rep pilot measured m_n in {8, 32, 128} but never 64, which is
#          now the grid CEILING and therefore the cost driver for the whole
#          committed run. Measure it directly, at every n, instead of
#          interpolating between 32 and 128 -- and capture the memory
#          high-water mark, since memory (not cores) is what killed the last
#          long run and is what caps the furrr worker count.
# Scope: 6 n values x 3 replications = 18 calls, sequential. Nothing else.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R, one_sim.R
# Outputs: results/timing_check_m64.rds; stdout report
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

CHECK_N   <- c(500L, 1000L, 2000L, 4000L, 8000L, 16000L)
CHECK_M   <- 64L
CHECK_REP <- 3L
DELTA1    <- 5
P         <- 2L

params  <- dgp_params_jump_cosmetic(delta1 = DELTA1)
mc_eval <- prepare_mc_eval(load_mc_eval_sample())
gt <- study_ground_truth(
  params, mc_eval,
  verification_path = file.path(STUDY_DIR, "results",
                                 "ground_truth_verification.rds")
)

# Resident set size of THIS process, in MB. gc()'s numbers cover R's own heap
# only; the branch-and-bound solver allocates outside it, so the figure that
# matters for sizing parallel workers has to come from the OS.
proc_rss_mb <- function() {
  as.numeric(system(sprintf("ps -o rss= -p %d", Sys.getpid()), intern = TRUE)) / 1024
}

cat("=========================================================\n")
cat(sprintf("Timing check at m_n = %d: %d n values x %d reps = %d calls\n",
            CHECK_M, length(CHECK_N), CHECK_REP,
            length(CHECK_N) * CHECK_REP))
cat(sprintf("  RSS after loading the fixed evaluation sample: %.0f MB\n",
            proc_rss_mb()))
cat("=========================================================\n\n")

rows <- list()
rss_peak <- proc_rss_mb()
for (nn in CHECK_N) {
  for (rp in seq_len(CHECK_REP)) {
    r <- one_sim(n = nn, m_n = CHECK_M, delta1 = DELTA1, p = P,
                  ground_truth = gt, mc_eval = mc_eval, rep = rp)
    rss <- proc_rss_mb()
    rss_peak <- max(rss_peak, rss)
    r$rss_mb <- rss
    rows[[length(rows) + 1L]] <- r
    cat(sprintf("  n=%5d m_n=%3d rep=%d  %7.2fs (stage A %7.2fs)  status=%s  topology_match=%s  RSS %.0f MB\n",
                nn, CHECK_M, rp, r$secs_total, r$secs_stage_a, r$status,
                r$topology_match, rss))
  }
}
res <- do.call(rbind, rows)

cat("\n--- mean seconds per call at m_n = 64, by n ---\n")
cat("        n   mean_total    max_total     mean_A   mean_stageB   mean_eval\n")
for (nn in CHECK_N) {
  e <- res[res$n == nn, ]
  cat(sprintf("  %7d   %10.2f   %10.2f  %9.2f   %11.3f   %9.3f\n",
              nn, mean(e$secs_total), max(e$secs_total),
              mean(e$secs_stage_a), mean(e$secs_stage_b), mean(e$secs_eval)))
}

cat(sprintf("\n  peak process RSS across all %d calls: %.0f MB\n", nrow(res),
            rss_peak))
cat(sprintf("  status: %s | topology_match: %d/%d | bins_exact: %d/%d\n",
            paste(unique(res$status), collapse = ","),
            sum(res$topology_match), nrow(res),
            sum(res$bins_exact), nrow(res)))

# Projected cost of the committed run, from THESE numbers plus the recorded
# 10-rep pilot for the coarser columns. Reported so the worker count and the
# go/no-go are decided on measurements, not on interpolation.
cat("\n--- projected serial cost of the committed 100-rep run ---\n")
pilot <- readRDS(file.path(STUDY_DIR, "results", "timing_pilot_reps10.rds"))
m64 <- vapply(CHECK_N, function(nn) mean(res$secs_total[res$n == nn]),
               numeric(1))
names(m64) <- as.character(CHECK_N)
cat("  m_n=64 column measured here; m_n in {4,8,16,32} approximated from the\n")
cat("  pilot's m_n=8 and m_n=32 measurements (log-linear in m_n, interpolated\n")
cat("  in n where the pilot has no point) -- an ESTIMATE, labelled as such.\n")
pil_mean <- function(nn, mm) {
  z <- pilot$secs_total[pilot$n == nn & pilot$m_n == mm]
  if (length(z) == 0L) NA_real_ else mean(z)
}
for (mm in c(8L, 32L)) {
  vals <- vapply(CHECK_N, pil_mean, numeric(1), mm = mm)
  cat(sprintf("    pilot m_n=%-3d : %s\n", mm,
              paste(sprintf("n=%d:%s", CHECK_N,
                             ifelse(is.na(vals), "--",
                                    sprintf("%.1fs", vals))), collapse = "  ")))
}
cat(sprintf("    measured m_n=64: %s\n",
            paste(sprintf("n=%d:%.1fs", CHECK_N, m64), collapse = "  ")))

saveRDS(list(results = res, rss_peak_mb = rss_peak, m64_mean = m64),
        file.path(STUDY_DIR, "results", "timing_check_m64.rds"))
cat("\n=========================================================\n")
