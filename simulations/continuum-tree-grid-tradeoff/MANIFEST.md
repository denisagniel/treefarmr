# MANIFEST — continuum-tree-grid-tradeoff

**Spec:** `quality_reports/specs/2026-09-28_continuum-tree-grid-tradeoff.md` (stem:
`2026-09-28_continuum-tree-grid-tradeoff`, archetype: decision, status: APPROVED,
with a post-approval DGP revision and a post-pilot resourcing revision — both
recorded inline in that spec, not here).

**Status as of 2026-09-29: Regime F, 100-replication rung, IN PROGRESS (92% done,
resumable).** Not yet run: the full 500/750-replication commitment for Regime F, and
Regimes S1/S2 (deferred pending Regime F's own result — see spec Section 8).

## Files

Core, reusable pipeline (in dependency order):

- `dgp.R` — `dgp_jump_cosmetic(n, p, seed, params)`: the DGP (mirrored-quadratic
  jump/cosmetic design), following `simulations/dgps.R`'s `dgp_*()` convention.
- `ground_truth.R` — closed-form `t1*`, `t2*_L`, `t2*_R`, `f*`'s leaf means, and the
  exhaustive 8-shape topology verification, all parameterized by
  `(a_L, b_L, b_R, delta1)` (not hardcoded), so it is reusable for Regime S1's `Δ1`
  values without modification.
- `ground_truth_gate.R` — **mandatory correctness gate.** Re-verifies the closed
  forms against brute-force/`integrate()`/1e7-draw Monte Carlo numerics and the
  8-shape topology check; must be re-run (and must pass) for any new `Δ1` (or other
  DGP parameter) before trusting `one_sim()` output at that setting. Named to avoid
  this repo's `verify_*.R` gitignore pattern (renamed from `verify_ground_truth.R`
  during the same session that wrote it — see session notes,
  `quality_reports/session_notes/2026-09-29.md`); functionally unchanged.
- `mc_eval_sample.R` — generates/loads the single fixed `N_MC = 1e7` Monte Carlo
  evaluation sample (seed `20260928`) used by every replication's excess-risk
  calculation; keyed to the DGP parameter set via `assert_mc_eval_matches()` so a
  DGP revision invalidates it loudly, not silently. The `results/mc_eval_sample.rds`
  it produces is gitignored (`results/` — 240 MB, regenerable from this script's
  fixed seed in ~1-2s for γ₀, the X draws take longer but are identical every time).
- `one_sim.R` — one replication: `dgp_jump_cosmetic()` → `bisect_lambda_to_budget()`
  (Stage A, explicit `discretize_bins = m_n`) → `refine_tree()` (Stage B) → returns
  one row with every field the spec's Section 5 metrics need (excess risk, topology
  match, `certified`/`feasible`/`depth_sufficient`/`gap`/`any_truncated`, Stage-B
  `reason` per node, fitted thresholds with `NA` where the topology doesn't match).
  Never drops a replication silently on any status.

Diagnostic / one-off scripts (kept, not scratch — each documents a real finding this
session, not a throwaway check):

- `diagnose_topology_failure.R` — the script that found the originally-approved DGP's
  topology was not `f*` (see the spec's Section 3 revision note).
- `smoke_test.R`, `timing_pilot.R`, `timing_check_m64.R`, `model_limit_experiment.R`,
  `summarize_pilot.R` — the staged verification sequence (1 rep → 10-rep timing pilot
  → targeted `m_n=64` timing check → `model_limit=0` experiment, ruled out) that
  produced the resourcing revision in the spec's Section 8.
- `regime_f_run.R` — the currently-in-progress 100-rep Regime-F runner (`furrr`
  parallel, incremental save, resume-by-skipping-completed-cells). **Re-running this
  script resumes from wherever `results/regime_f_100reps.rds` left off** — it will
  skip the 4,416 `(n, m_n, rep)` cells already on disk and finish the remaining ~384.
- `analyze_regime_f.R` — reads `results/regime_f_100reps.rds` and produces the Q1/Q2
  summary tables (certified/topology-match rates, mean excess risk by cell, median
  threshold error by boundary type) reported in session notes; safe to re-run at any
  point, including on the partial 100-rep table.

## Results (`results/`, gitignored — regenerate via the scripts above)

- `ground_truth_verification.rds`, `topology_failure_diagnosis.rds` — DGP correctness
  evidence.
- `mc_eval_sample.rds` — the fixed evaluation sample (240 MB).
- `smoke_test.rds`, `timing_pilot_reps1(_full).rds`, `timing_pilot_reps10(_full).rds`,
  `rerun_n16000_m128_rep10.rds` — staged verification outputs.
- `timing_check_m64.rds` — targeted `m_n=64` timing (the number the resourcing
  revision's `m_n=64` cost estimate is based on).
- `model_limit_experiment(_full).rds`, `.log` — the ruled-out `model_limit=0` check.
- `regime_f_100reps.rds`, `.log` — the in-progress 100-rep Regime-F run (4,416/4,800
  cells as of this manifest; resumable).

## Next steps (see spec Section 8 and session notes for full context)

1. Finish the 100-rep Regime-F run (`Rscript regime_f_run.R` resumes it).
2. Run `analyze_regime_f.R` for a first (still underpowered) read on SC1/SC2/SC3.
3. Decide, from that read: push to the full 500/750-rep commitment, or revisit the
   `m_n` grid further, before deciding anything about Regimes S1/S2 or cluster
   resourcing.
