# ============================================================
# Regime F at the resized grid, 100 replications per configuration
# Study: continuum-tree-grid-tradeoff
# Spec:  quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md
#        (Section 8 as revised 2026-09-28: m_n capped at 64, Regime F only)
# Purpose: the "100" rung of the 1 -> 10 -> 100 -> full staging. NOT the full
#          500/750-rep run the spec's success criteria SC1-SC3 are pre-committed
#          against; this rung exists to see whether the pattern is readable at
#          100 and therefore whether 500/750 is warranted.
# Inputs: dgp.R, ground_truth.R, mc_eval_sample.R, one_sim.R,
#         results/mc_eval_sample.rds, results/ground_truth_verification.rds
# Outputs: results/regime_f_100reps.rds (written incrementally, resumable)
# ============================================================
#
# Usage: Rscript regime_f_run.R [n_workers]
#
# Q1-F grid: n(6) x m_n{4,8,16,32,64}  = 30 configs, replications   1..100
# Q2-F grid: n(6) x m_n{16,32,64}      = 18 configs, replications 101..200
#
# WHY Q2 USES REPLICATIONS 101-200 RATHER THAN 1-100: Q2-F's grid is a strict
# SUBSET of Q1-F's (same n values, m_n in {16,32,64} vs {4,8,16,32,64}), and one
# one_sim() call already returns both Q1's excess risk and Q2's threshold
# errors. Running Q2 at replications 1-100 would therefore re-run 1,800 calls
# whose seeds -- being a pure function of (n, m_n, delta1, p, rep) -- are
# identical to Q1's, reproducing 1,800 bit-identical rows and buying nothing.
# Using 101-200 keeps the committed 4,800-call budget while making every call
# informative: Q2's cells end up with 200 replications (100 shared with Q1 plus
# 100 fresh), which is the right direction anyway since the spec gives Q2 MORE
# replication than Q1 at full scale (750 vs 500) precisely because a
# log-log slope needs more power than a level comparison.

STUDY_DIR <- if (dir.exists("simulations/continuum-tree-grid-tradeoff")) {
  "simulations/continuum-tree-grid-tradeoff"
} else {
  "."
}
STUDY_ABS <- normalizePath(STUDY_DIR)
PKG_ABS   <- normalizePath(".")

suppressMessages(devtools::load_all(".", quiet = TRUE))
library(furrr)
source(file.path(STUDY_DIR, "dgp.R"))
source(file.path(STUDY_DIR, "ground_truth.R"))
source(file.path(STUDY_DIR, "mc_eval_sample.R"))
source(file.path(STUDY_DIR, "one_sim.R"))

RF_N       <- c(500L, 1000L, 2000L, 4000L, 8000L, 16000L)
RF_M_Q1    <- c(4L, 8L, 16L, 32L, 64L)
RF_M_Q2    <- c(16L, 32L, 64L)
RF_REPS    <- 100L
DELTA1     <- 5
P          <- 2L
OUT_PATH   <- file.path(STUDY_DIR, "results", "regime_f_100reps.rds")

# Worker count. The binding constraint is MEMORY, not cores: timing_check_m64.R
# measured a 1.28 GB peak RSS per process at m_n = 64 (mostly the 229 MB fixed
# evaluation sample plus the solver's own allocation), and the 10-rep pilot was
# already killed once by the OS with ~62 MB free. On this 16 GB machine
# (10 logical cores: 8 performance + 2 efficiency) 6 workers x ~1.3 GB leaves
# ~8 GB for the OS and page cache. Going to 8-10 workers would fit the core
# count but not the memory budget, and a second OOM kill costs more than the
# 25% of throughput it would buy.
args <- commandArgs(trailingOnly = TRUE)
N_WORKERS <- if (length(args) >= 1L) as.integer(args[[1L]]) else 6L

# ---- Work table ----------------------------------------------------------

work <- rbind(
  expand.grid(rep = seq_len(RF_REPS), m_n = RF_M_Q1, n = RF_N,
               KEEP.OUT.ATTRS = FALSE),
  expand.grid(rep = RF_REPS + seq_len(RF_REPS), m_n = RF_M_Q2, n = RF_N,
               KEEP.OUT.ATTRS = FALSE)
)
work <- work[, c("n", "m_n", "rep")]
# Cheapest-first (m_n dominates cost, n second). Two reasons, both practical:
# broad coverage lands early, and a crash in the expensive tail loses the least
# completed work.
work <- work[order(work$m_n, work$n, work$rep), ]
rownames(work) <- NULL
work$key <- sprintf("%d|%d|%d", work$n, work$m_n, work$rep)

# Seeds are checked for collisions across the WHOLE table before anything runs.
# At 4,800 cells a single collision would silently make two replications share
# data, which is exactly the kind of thing that is invisible in the results.
work$seed <- mapply(sim_seed, work$n, work$m_n, DELTA1, P, work$rep)
if (anyDuplicated(work$seed)) {
  dup <- work[work$seed %in% work$seed[duplicated(work$seed)], ]
  stop(sprintf("seed collision across %d of %d cells; e.g.\n%s",
               nrow(dup), nrow(work),
               paste(utils::capture.output(print(head(dup, 6))), collapse = "\n")))
}

# ---- Resume --------------------------------------------------------------

rows <- list()
done_key <- character(0)
if (file.exists(OUT_PATH)) {
  prev <- readRDS(OUT_PATH)
  prev <- prev[prev$delta1 == DELTA1 & prev$p == P, , drop = FALSE]
  if (nrow(prev) > 0L) {
    rows <- split(prev, seq_len(nrow(prev)))
    done_key <- sprintf("%d|%d|%d", prev$n, prev$m_n, prev$rep)
  }
}
todo <- work[!work$key %in% done_key, , drop = FALSE]

cat("=========================================================\n")
cat(sprintf("Regime F, %d reps/config, resized grid (m_n <= %d)\n", RF_REPS,
            max(RF_M_Q1)))
cat(sprintf("  Q1-F: n(%d) x m_n(%d) = %d configs, reps 1..%d\n",
            length(RF_N), length(RF_M_Q1), length(RF_N) * length(RF_M_Q1),
            RF_REPS))
cat(sprintf("  Q2-F: n(%d) x m_n(%d) = %d configs, reps %d..%d\n",
            length(RF_N), length(RF_M_Q2), length(RF_N) * length(RF_M_Q2),
            RF_REPS + 1L, 2L * RF_REPS))
cat(sprintf("  total cells %d | already done %d | to run %d\n",
            nrow(work), length(done_key), nrow(todo)))
cat(sprintf("  workers: %d (memory-bound; see the comment in this file)\n",
            N_WORKERS))
cat("=========================================================\n\n")

if (nrow(todo) == 0L) {
  cat("Nothing to do.\n")
} else {

# ---- Worker setup --------------------------------------------------------
#
# Each worker loads the package, the study files, and the 229 MB evaluation
# sample ONCE and caches them in its own global environment. Caching matters:
# passing `mc_eval` as a future global would ship 240 MB per chunk per worker,
# which would dominate the run. Persistent multisession workers keep this state
# across future_map() calls within one plan().
worker_setup <- function(pkg_abs, study_abs, delta1) {
  if (!isTRUE(get0(".ctg_ready", envir = globalenv(), ifnotfound = FALSE))) {
    suppressMessages(devtools::load_all(pkg_abs, quiet = TRUE))
    for (f in c("dgp.R", "ground_truth.R", "mc_eval_sample.R", "one_sim.R")) {
      source(file.path(study_abs, f))
    }
    mc <- prepare_mc_eval(
      load_mc_eval_sample(file.path(study_abs, "results", "mc_eval_sample.rds"))
    )
    pr <- dgp_params_jump_cosmetic(delta1 = delta1)
    g <- study_ground_truth(
      pr, mc,
      verification_path = file.path(study_abs, "results",
                                     "ground_truth_verification.rds")
    )
    assign(".ctg_mc", mc, envir = globalenv())
    assign(".ctg_gt", g, envir = globalenv())
    assign(".ctg_ready", TRUE, envir = globalenv())
  }
  invisible(TRUE)
}

run_cell <- function(n, m_n, rep, pkg_abs, study_abs, delta1, p) {
  worker_setup(pkg_abs, study_abs, delta1)
  one_sim(n = n, m_n = m_n, delta1 = delta1, p = p, rep = rep,
           ground_truth = get(".ctg_gt", envir = globalenv()),
           mc_eval = get(".ctg_mc", envir = globalenv()))
}

future::plan(future::multisession, workers = N_WORKERS)
# Registered and torn down explicitly, on every exit path: leaving 6 worker R
# processes alive after an error would hold ~8 GB.
on.exit({
  future::plan(future::sequential)
  cat("\nfurrr backend torn down (plan(sequential)).\n")
}, add = TRUE)

# Chunk = 4 x workers, so the incremental save lands every few minutes even in
# the expensive tail, without serialising the table too often in the cheap head.
CHUNK <- 4L * N_WORKERS
chunks <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / CHUNK))
t_run <- Sys.time()
n_done_this_run <- 0L

for (ci in seq_along(chunks)) {
  idx <- chunks[[ci]]
  blk <- todo[idx, , drop = FALSE]
  out <- furrr::future_pmap(
    list(n = blk$n, m_n = blk$m_n, rep = blk$rep),
    run_cell,
    pkg_abs = PKG_ABS, study_abs = STUDY_ABS, delta1 = DELTA1, p = P,
    # seed = TRUE gives each worker an L'Ecuyer stream. That is SAFE here only
    # because dgp.R's seed_study_rng() pins the generator explicitly -- a bare
    # set.seed() inherits the ambient generator, so under an L'Ecuyer worker the
    # identical seed yields a different stream and different data. That was a
    # real, observed failure (see seed_study_rng()'s docs); it is caught by the
    # post-run cross-check against the 10-rep pilot's overlapping cells, which
    # must match bit-for-bit.
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = c("worker_setup", "one_sim")
    )
  )
  rows <- c(rows, out)
  n_done_this_run <- n_done_this_run + length(idx)
  saveRDS(do.call(rbind, rows), OUT_PATH, compress = FALSE)

  el <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))
  rate <- el / n_done_this_run
  bad <- vapply(out, function(z) !identical(z$status, "ok"), logical(1))
  cat(sprintf("  chunk %3d/%3d  cells %5d  n=%5d..%5d m_n=%3d..%3d  elapsed %6.1f min  %.2f s/cell  ETA %6.1f min%s\n",
              ci, length(chunks), n_done_this_run, min(blk$n), max(blk$n),
              min(blk$m_n), max(blk$m_n), el / 60, rate,
              rate * (nrow(todo) - n_done_this_run) / 60,
              if (any(bad)) sprintf("  [%d NON-OK]", sum(bad)) else ""))
}

res <- do.call(rbind, rows)
res <- res[order(res$n, res$m_n, res$rep), ]
saveRDS(res, OUT_PATH, compress = FALSE)
total <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))
cat(sprintf("\nDone. %d cell(s) this invocation in %.1f min (%.2f h) on %d workers.\n",
            n_done_this_run, total / 60, total / 3600, N_WORKERS))
cat(sprintf("Summed one_sim() CPU time: %.2f h -> parallel speedup %.2fx\n",
            sum(res$secs_total) / 3600, sum(res$secs_total) / total))
cat(sprintf("Rows on disk: %d at %s\n", nrow(res), OUT_PATH))
}
