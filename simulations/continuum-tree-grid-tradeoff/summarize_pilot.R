# ============================================================
# Summary tables for a timing-pilot run
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 5 metrics; Section 8 staging)
# Purpose: turn a (possibly incomplete) timing_pilot.R results table into the
#          reported tables. Split out of timing_pilot.R deliberately: putting
#          the summary only at the END of a ~2-hour run means an interrupted
#          run yields no tables at all even though every finished replication
#          is already on disk. That happened once (call 90/90 was killed), so
#          this is a fix, not a refactor for its own sake.
# Inputs: results/timing_pilot_reps<N>.rds (the incremental table)
# Outputs: stdout report
# ============================================================
#
# Usage: Rscript summarize_pilot.R results/timing_pilot_reps10.rds

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1L) args[[1L]] else {
  file.path(STUDY_DIR, "results", "timing_pilot_reps10.rds")
}
if (!file.exists(path)) stop("no such results file: ", path)
res <- readRDS(path)
if (is.list(res) && !is.data.frame(res) && !is.null(res$results)) {
  res <- res$results
}

# Cell-level completeness is reported, never assumed: a config with fewer reps
# than the rest must be visible in the tables, not averaged over silently.
cell_n <- tapply(rep(1L, nrow(res)), list(res$n, res$m_n), sum)

agg <- function(f, fn) {
  out <- tapply(f, list(sprintf("n=%d", res$n), sprintf("m_n=%d", res$m_n)), fn)
  out[order(as.integer(sub("n=", "", rownames(out)))),
      order(as.integer(sub("m_n=", "", colnames(out)))), drop = FALSE]
}
show_tab <- function(tab, fmt = "%9.2f") {
  cat(sprintf("  %-10s", ""))
  cat(sprintf("%12s", colnames(tab)), sep = "")
  cat("\n")
  for (r in rownames(tab)) {
    cat(sprintf("  %-10s", r))
    cat(sprintf("%12s", sprintf(fmt, tab[r, ])), sep = "")
    cat("\n")
  }
}
mean_ <- function(z) mean(z, na.rm = TRUE)
med_  <- function(z) stats::median(z, na.rm = TRUE)
se_   <- function(z) stats::sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z)))

cat("=========================================================\n")
cat(sprintf("Timing pilot summary: %s\n", basename(path)))
cat(sprintf("  %d replications across %d configurations\n", nrow(res),
            length(unique(paste(res$n, res$m_n)))))
cat("=========================================================\n")

cat("\n--- replications completed per config ---\n")
show_tab(agg(rep(1L, nrow(res)), sum), "%9.0f")

cat(sprintf("\nTOTAL one_sim() wall clock summed over all calls: %.1f s (%.2f h)\n",
            sum(res$secs_total), sum(res$secs_total) / 3600))
cat(sprintf("Per-call: mean %.2f s, median %.2f s, max %.2f s, min %.2f s\n",
            mean(res$secs_total), stats::median(res$secs_total),
            max(res$secs_total), min(res$secs_total)))
cat(sprintf("Mean split: stage A %.2f s, stage B %.2f s, MC eval %.2f s, dgp %.3f s\n",
            mean_(res$secs_stage_a), mean_(res$secs_stage_b),
            mean_(res$secs_eval), mean_(res$secs_dgp)))

cat("\n--- mean SECONDS per one_sim() call ---\n")
show_tab(agg(res$secs_total, mean_))
cat("\n--- max SECONDS per call (cost heterogeneity matters for job sizing) ---\n")
show_tab(agg(res$secs_total, function(z) max(z, na.rm = TRUE)))
cat("\n--- mean STAGE-A seconds (the dominant term) ---\n")
show_tab(agg(res$secs_stage_a, mean_))
cat("\n--- mean STAGE-B seconds ---\n")
show_tab(agg(res$secs_stage_b, mean_), "%9.3f")
cat("\n--- mean MC-EVAL seconds (1e7-draw excess-risk integration) ---\n")
show_tab(agg(res$secs_eval, mean_), "%9.3f")

cat("\n--- status counts (every replication is reported; none dropped) ---\n")
print(table(res$status))
cat("\n--- grid resolution realised exactly as requested (bins_exact) ---\n")
show_tab(agg(res$bins_exact, mean_), "%9.3f")
cat("\n--- certified rate ---\n")
show_tab(agg(res$certified, mean_), "%9.3f")
cat("\n--- feasible rate ---\n")
show_tab(agg(res$feasible, mean_), "%9.3f")
cat("\n--- depth_sufficient rate (expected 0: max_depth=2 < leaf_budget-1=3 by design) ---\n")
show_tab(agg(res$depth_sufficient, mean_), "%9.3f")
cat("\n--- any_truncated rate (expected 0: no time limit set) ---\n")
show_tab(agg(res$any_truncated, mean_), "%9.3f")
cat("\n--- n_leaves (mean) and n_fits (mean) ---\n")
show_tab(agg(res$n_leaves, mean_), "%9.2f")
show_tab(agg(res$n_fits, mean_), "%9.2f")
cat("\n--- topology-match rate ---\n")
show_tab(agg(res$topology_match, mean_), "%9.3f")

cat("\n--- mean excess risk ---\n")
show_tab(agg(res$excess_risk, mean_), "%9.5f")
cat("\n--- s.e. of that mean (over reps) -- compare against the means above ---\n")
show_tab(agg(res$excess_risk, se_), "%9.5f")

cat("\n--- median |t1_hat - t1*|  (JUMP boundary; Q2 predicts O(n^-1)) ---\n")
show_tab(agg(res$t1_err, med_), "%9.6f")
cat("\n--- median |t2L_hat - t2*_L|  (COSMETIC left; Q2 predicts O(n^-1/2)) ---\n")
show_tab(agg(res$t2L_err, med_), "%9.6f")
cat("\n--- median |t2R_hat - t2*_R|  (COSMETIC right; Q2 predicts O(n^-1/2)) ---\n")
show_tab(agg(res$t2R_err, med_), "%9.6f")
cat("\n--- threshold-error exclusion counts (node absent from fitted topology) ---\n")
for (nm in c("t1_err", "t2L_err", "t2R_err")) {
  cat(sprintf("  %-8s %d of %d replications have no such node\n", nm,
              sum(is.na(res[[nm]])), nrow(res)))
}

cat("\n--- Stage-B `reason` counts per node ---\n")
for (nm in c("reason_root", "reason_left", "reason_right")) {
  tb <- table(res[[nm]], useNA = "ifany")
  cat(sprintf("  %-14s %s\n", nm,
              paste(sprintf("%s=%d", names(tb), as.integer(tb)), collapse = "  ")))
}
cat("\n--- Stage-B: fraction of nodes whose cut actually MOVED off the grid ---\n")
for (nm in c("moved_root", "moved_left", "moved_right")) {
  cat(sprintf("  %-14s %.3f\n", nm, mean_(res[[nm]])))
}
cat("\n--- warnings captured (never discarded) ---\n")
cat(sprintf("  replications with >=1 warning: %d of %d\n",
            sum(res$n_warnings > 0L), nrow(res)))
uw <- unique(res$first_warning[!is.na(res$first_warning)])
for (w in uw) {
  cat(sprintf("  [%d reps] %s\n", sum(res$first_warning == w, na.rm = TRUE),
              substr(w, 1L, 150L)))
}

reps_per_cell <- unique(as.integer(cell_n))
cat(sprintf("\nCAVEAT: %s replications per configuration. This is a TIMING and\n",
            paste(reps_per_cell, collapse = "/")))
cat("SANITY check only. The excess-risk and threshold-error tables above are\n")
cat("far too noisy to read as evidence on Q1 or Q2 -- the s.e. table is the\n")
cat("same order as the means it accompanies. Spec Section 8 calls for 300-750\n")
cat("reps per configuration for the real run.\n")
cat("=========================================================\n")
