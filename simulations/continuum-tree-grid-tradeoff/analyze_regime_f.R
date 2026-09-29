# ============================================================
# Analysis of the Regime F 100-rep run (Q1 and Q2 reads)
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 5 metrics, Section 6 success criteria SC1-SC3, Section 8)
# Purpose: turn results/regime_f_100reps.rds into the reported tables and a
#          FIRST READ on SC1/SC2/SC3. This rung is 100 reps; the success
#          criteria are pre-committed against 500 (Q1) / 750 (Q2), so nothing
#          here confirms or refutes them -- the point is whether the pattern is
#          readable at 100 and therefore whether 500/750 is warranted.
# Inputs: results/regime_f_100reps.rds
# Outputs: stdout report; results/regime_f_100reps_analysis.rds
# ============================================================

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}
args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1L) args[[1L]] else {
  file.path(STUDY_DIR, "results", "regime_f_100reps.rds")
}
res <- readRDS(path)
if (is.list(res) && !is.data.frame(res)) res <- res$results

RF_N    <- sort(unique(res$n))
Q1_REPS <- 100L   # replications 1..100 span the whole m_n grid
# Q1 tables use replications 1..100 ONLY, at every m_n. Cells at
# m_n in {16,32,64} also carry the Q2 block's replications 101..200, and folding
# those in would give the middle of the m_n grid twice the precision of its
# ends -- which is exactly the comparison SC1 is about, so it must not be
# confounded by unequal replication.
q1 <- res[res$rep <= Q1_REPS, , drop = FALSE]
q2 <- res[res$m_n %in% c(16L, 32L, 64L), , drop = FALSE]

tab <- function(d, f, fn) {
  out <- tapply(f, list(sprintf("n=%d", d$n), sprintf("m_n=%d", d$m_n)), fn)
  out[order(as.integer(sub("n=", "", rownames(out)))),
      order(as.integer(sub("m_n=", "", colnames(out)))), drop = FALSE]
}
show <- function(t, fmt = "%10.5f") {
  cat(sprintf("  %-10s", ""));  cat(sprintf("%12s", colnames(t)), sep = ""); cat("\n")
  for (r in rownames(t)) {
    cat(sprintf("  %-10s", r))
    cat(sprintf("%12s", sprintf(fmt, t[r, ])), sep = ""); cat("\n")
  }
}
mean_ <- function(z) mean(z, na.rm = TRUE)
med_  <- function(z) stats::median(z, na.rm = TRUE)
se_   <- function(z) stats::sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z)))

cat("=========================================================\n")
cat(sprintf("Regime F analysis: %s\n", basename(path)))
cat(sprintf("  %d rows total | Q1 block (reps 1-%d): %d rows | Q2 block (m_n in 16/32/64, all reps): %d rows\n",
            nrow(res), Q1_REPS, nrow(q1), nrow(q2)))
cat("=========================================================\n")

cat("\n### replications per cell, Q1 block (should be 100 everywhere)\n")
show(tab(q1, rep(1L, nrow(q1)), sum), "%10.0f")
cat("\n### replications per cell, Q2 block (200 where the Q2 grid overlaps)\n")
show(tab(q2, rep(1L, nrow(q2)), sum), "%10.0f")

# ---- 1 Solver / certificate health, EVERY configuration ------------------

cat("\n=== 1 Certificate and topology health (all configurations) ===\n")
cat("\n--- status counts (every replication reported; none dropped) ---\n")
print(table(res$status))
if (any(res$status != "ok")) {
  cat("\nNON-OK replications, in full:\n")
  bad <- res[res$status != "ok", c("n", "m_n", "rep", "seed", "status",
                                    "error_class", "error_message")]
  print(bad, row.names = FALSE)
}
cat("\n--- bins_exact rate (grid resolution realised as requested) ---\n")
show(tab(res, res$bins_exact, mean_), "%10.3f")
cat("\n--- certified rate ---\n")
show(tab(res, res$certified, mean_), "%10.3f")
cat("\n--- feasible rate ---\n")
show(tab(res, res$feasible, mean_), "%10.3f")
cat("\n--- depth_sufficient rate (0 expected by design: max_depth=2 < L-1=3) ---\n")
show(tab(res, res$depth_sufficient, mean_), "%10.3f")
cat("\n--- any_truncated rate (0 expected: no time limit) ---\n")
show(tab(res, res$any_truncated, mean_), "%10.3f")
cat("\n--- mean n_leaves / mean n_fits ---\n")
show(tab(res, res$n_leaves, mean_), "%10.2f")
show(tab(res, res$n_fits, mean_), "%10.2f")
cat("\n--- topology-match rate ---\n")
show(tab(res, res$topology_match, mean_), "%10.3f")
cat("\n--- per-node match rates (root / left / right) ---\n")
show(tab(res, res$match_root, mean_), "%10.3f")
show(tab(res, res$match_left, mean_), "%10.3f")
show(tab(res, res$match_right, mean_), "%10.3f")

# ---- 2 Q1: excess risk ---------------------------------------------------

cat("\n=== 2 Q1: excess risk vs the population-best 4-leaf tree f* ===\n")
cat(sprintf("\n--- mean excess risk (%d reps/cell) ---\n", Q1_REPS))
m_exc <- tab(q1, q1$excess_risk, mean_)
show(m_exc, "%10.5f")
cat("\n--- s.e. of that mean ---\n")
s_exc <- tab(q1, q1$excess_risk, se_)
show(s_exc, "%10.5f")
cat("\n--- relative s.e. (s.e. / mean) -- how readable each cell is ---\n")
show(s_exc / m_exc, "%10.3f")
cat("\n--- median excess risk (less sensitive to the occasional bad draw) ---\n")
show(tab(q1, q1$excess_risk, med_), "%10.5f")

# SC1: "flat past some m_n^dagger(n)", operationalised as spec Section 6 does --
# within 10% RELATIVE of the finest tested m_n (here 64, the resized ceiling).
finest <- max(as.integer(sub("m_n=", "", colnames(m_exc))))
cat(sprintf("\n--- SC1 read: excess risk relative to the finest tested m_n = %d ---\n",
            finest))
cat("    value shown = mean_excess(m_n) / mean_excess(finest) - 1\n")
rel <- m_exc / m_exc[, sprintf("m_n=%d", finest)] - 1
show(rel, "%10.3f")

cat("\n--- SC1/SC2 read: m_n^dagger(n) = smallest m_n within 10% of m_n=64,\n")
cat("    with every finer m_n also within 10% (so it is a flattening POINT,\n")
cat("    not an isolated crossing) ---\n")
ms <- as.integer(sub("m_n=", "", colnames(m_exc)))
dag <- vapply(seq_len(nrow(rel)), function(i) {
  ok <- abs(rel[i, ]) <= 0.10
  hit <- which(vapply(seq_along(ok), function(k) all(ok[k:length(ok)]), logical(1)))
  if (length(hit) == 0L) NA_integer_ else ms[[min(hit)]]
}, integer(1))
names(dag) <- rownames(rel)
cat("          n     m_n^dagger    sqrt(n)   m_dagger/sqrt(n)\n")
for (i in seq_along(dag)) {
  nn <- RF_N[[i]]
  cat(sprintf("  %9d   %10s %10.1f   %16s\n", nn,
              ifelse(is.na(dag[[i]]), "none<=64", dag[[i]]), sqrt(nn),
              ifelse(is.na(dag[[i]]), "--", sprintf("%.3f", dag[[i]] / sqrt(nn)))))
}
if (sum(!is.na(dag)) >= 3L) {
  ok <- !is.na(dag)
  fit <- stats::lm(log(dag[ok]) ~ log(RF_N[ok]))
  cat(sprintf("\n  log(m_n^dagger) ~ log(n) slope = %.3f (s.e. %.3f); SC2's target is 0.5 (sqrt(n))\n",
              stats::coef(fit)[[2L]], summary(fit)$coefficients[2L, 2L]))
} else {
  cat("\n  Too few resolved m_n^dagger values to fit a growth slope.\n")
}

# ---- 3 Q2: threshold errors ---------------------------------------------

cat("\n=== 3 Q2: threshold error by boundary type ===\n")
bt <- list(`JUMP t1 (target slope -1)` = "t1_err",
            `COSMETIC left t2L (target -0.5)` = "t2L_err",
            `COSMETIC right t2R (target -0.5)` = "t2R_err")
slopes <- list()
for (lab in names(bt)) {
  cl <- bt[[lab]]
  cat(sprintf("\n--- median |%s| ---\n", cl))
  tt <- tab(q2, q2[[cl]], med_)
  show(tt, "%10.6f")
  cat(sprintf("--- excluded (node absent from the fitted topology), fraction ---\n"))
  show(tab(q2, is.na(q2[[cl]]), mean_), "%10.3f")
  sl <- vapply(colnames(tt), function(cn) {
    y <- tt[, cn]; keep <- is.finite(y) & y > 0
    if (sum(keep) < 3L) return(NA_real_)
    stats::coef(stats::lm(log(y[keep]) ~ log(RF_N[keep])))[[2L]]
  }, numeric(1))
  slopes[[lab]] <- sl
  cat("--- log-log slope of median error on n, per m_n ---\n")
  cat(sprintf("    %s\n", paste(sprintf("%s: %+.3f", names(sl), sl), collapse = "   ")))
}

cat("\n--- SC3 read: slope side by side (target: jump -1, cosmetic -0.5) ---\n")
sl_tab <- do.call(rbind, slopes)
cat(sprintf("  %-34s%s\n", "boundary",
            paste(sprintf("%12s", colnames(sl_tab)), collapse = "")))
for (r in rownames(sl_tab)) {
  cat(sprintf("  %-34s%s\n", r,
              paste(sprintf("%12s", sprintf("%+.3f", sl_tab[r, ])), collapse = "")))
}

cat("\n--- Stage-B `reason` breakdown, per node, over all replications ---\n")
for (nm in c("reason_root", "reason_left", "reason_right")) {
  tb <- table(res[[nm]], useNA = "ifany")
  cat(sprintf("  %-14s %s\n", nm,
              paste(sprintf("%s=%d", names(tb), as.integer(tb)), collapse = "  ")))
}
cat("\n--- Stage-B `reason` != 'refined' rate, by cell (root node) ---\n")
show(tab(res, res$reason_root != "refined", mean_), "%10.3f")
cat("\n--- fraction of nodes whose cut actually MOVED off the grid ---\n")
for (nm in c("moved_root", "moved_left", "moved_right")) {
  cat(sprintf("  %-14s overall %.3f\n", nm, mean_(res[[nm]])))
}
show(tab(res, res$moved_root, mean_), "%10.3f")

# ---- 4 Cost --------------------------------------------------------------

cat("\n=== 4 Realised cost ===\n")
cat(sprintf("  summed one_sim() time: %.2f CPU-h over %d calls\n",
            sum(res$secs_total) / 3600, nrow(res)))
cat(sprintf("  per call: mean %.2f s, median %.2f s, max %.2f s\n",
            mean(res$secs_total), stats::median(res$secs_total),
            max(res$secs_total)))
cat("\n--- mean seconds per call, by cell ---\n")
show(tab(res, res$secs_total, mean_), "%10.2f")

cat("\n--- warnings captured (never discarded) ---\n")
cat(sprintf("  replications with >=1 warning: %d of %d\n",
            sum(res$n_warnings > 0L), nrow(res)))
uw <- unique(res$first_warning[!is.na(res$first_warning)])
for (w in uw) cat(sprintf("  [%d] %s\n", sum(res$first_warning == w, na.rm = TRUE),
                          substr(w, 1L, 130L)))

saveRDS(list(mean_excess = m_exc, se_excess = s_exc, rel_excess = rel,
             m_dagger = dag, slopes = sl_tab, n_rows = nrow(res)),
        file.path(STUDY_DIR, "results", "regime_f_100reps_analysis.rds"))

cat("\n=========================================================\n")
cat(sprintf("CAVEAT: this is the %d-rep rung. SC1/SC2/SC3 are pre-committed\n", Q1_REPS))
cat("against 500 (Q1) / 750 (Q2) replications. Nothing above confirms or\n")
cat("refutes them; read the relative-s.e. table to judge what is readable.\n")
cat("=========================================================\n")
