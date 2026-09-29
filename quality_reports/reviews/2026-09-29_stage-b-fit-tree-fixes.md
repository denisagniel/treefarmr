# R Code Review: Stage-B quantile-space fix, fit_tree() dots fix, repro-script rewrite

**Date:** 2026-09-29
**Reviewer:** r-reviewer agent
**Branch:** `session/2026-09-29-continuum-tree-grid-tradeoff`
**Scope (staged diff, 618 lines):**
- `R/stage_b.R:1057-1098` — rho_n applied in quantile space instead of raw covariate units
- `R/fit_twostage.R:701-714` — obsolete raw-units warning removed, replaced by explanatory comment
- `R/fit_tree.R:210-238` — `...` splice duplicate-argument crash fixed via `do.call()`
- `dev-scripts/repro_single_tree_tie_blowup.R` — full rewrite against the 2026-09-08 review
- `tests/testthat/test-stage-b.R:581-641` — new Stage-B regression test + honesty comment on the pre-existing one

**Context read:** `R/discretize.R` (`compute_thresholds`, `compute_bin_count`), `R/bisect_lambda_to_budget.R`,
`R/fit_rashomon.R`, `R/treefarms.R` (`optimaltrees()` signature), `R/stage_b.R` roxygen for `r_n`/`M_n`,
`inst/paper/story.md`, `quality_reports/reviews/2026-09-08_repro_single_tree_tie_blowup.md`.

**Method:** static review plus dimensional analysis of `rho_n` against the grid `compute_thresholds()` actually
builds, and argument-matching analysis of the `fit_tree()` → `optimaltrees()` and
`bisect_lambda_to_budget()` → `fit_tree()` call chains. The reviewer did not execute the package; the
claim that `devtools::test()` passes and that all 3 repro modes dry-ran clean is taken as given and is
consistent with everything read.

## Summary

- **Total issues:** 20
- **Critical:** 0
- **High:** 4
- **Medium:** 9
- **Low:** 7

**Verdict: the two bug fixes are correct, and the Stage-B one is more clearly correct than its own commit
message argues.** Independently confirmed the dimensional analysis: `compute_thresholds()` places cutpoints
at equal *probability* mass — `probs <- seq(0, 1, length.out = n_bins + 1)` stripped of its endpoints, fed to
`quantile(x_clean, probs, names = FALSE)` (`R/discretize.R:495-500`, default `type = 7`). A grid cell is
therefore `1/r_n` wide *in probability*, so `rho_n = M_n/r_n` is exactly "`M_n` mesh widths" only when read as
a probability radius. The old raw-unit reading was coherent only if a coordinate's total range happened to
equal 1 — which is why the deleted warning existed. The fix removes the scale dependence rather than papering
over it, and deleting the warning is the right call, not a regression.

**What was checked and found clean:**
- `fit_rashomon()` does **not** share the `model_limit` defect: `model_limit` is a genuine formal
  (`R/fit_rashomon.R:119`), so R binds a caller's `model_limit=` to the formal and it can never reach `...`.
  The `do.call()` treatment was correctly scoped to `fit_tree()` only.
- The `fit_tree()` fix is a genuine **prerequisite** for the repro-script rewrite, not an unrelated drive-by.
  `bisect_lambda_to_budget()` rejects only `time_limit` from `...` (`R/bisect_lambda_to_budget.R:411-416`),
  **not** `model_limit`, so the script's new `model_limit = model_limit` argument flows through `...` into the
  internal `fit_tree()` call — which would have crashed pre-fix.
- Every field the rewritten script reads off the bisect result exists: `any_truncated`, `certified`,
  `feasible`, `depth_sufficient`, `gap`, `n_fits`, `n_leaves`, `lambda`
  (`R/bisect_lambda_to_budget.R:522-539`, `183-206`).
- `optimaltrees_deadline_exceeded` is really signalled (`R/bisect_lambda_to_budget.R:562-565`), so the
  script's dedicated handler is not dead code.
- `no_candidates_in_bracket` really exists (`R/stage_b.R:1127-1132`).
- The claim "nothing in this package's own test suite calls `fit_tree()` with an explicit `model_limit=`" is
  accurate — grep across `tests/`, `dev-scripts/`, `vignettes/` finds only comments, no live call (true before
  this review's fixes; now closed, see Issue 3 disposition below).
- Review Issues #1–#9, #15 and #19 from 2026-09-08 are all genuinely addressed, most of them well.

## Issues

### Issue 1: `rho_n` is now a probability but was never range-checked, and the vacuous case lost its only warning — **FIXED** (2026-09-29, same session)

- **Severity:** High
- **Finding:** After the units fix, `rho_n` is a pure probability, and any value `>= 0.5` makes the search
  window cover the whole support for a mid-distribution anchor, silently reverting Stage B to the legacy
  structural-only bracket. With the package's own documented default `M_n = sqrt(r_n)` this is reached whenever
  `r_n <= 4`, which includes the entire `"log"` bin schedule (`compute_bin_count("log", n) = 3` at `n = 2000`).
  The commit under review deleted the only warning that ever fired on this failure mode (correctly — the
  warning was about the wrong cause, covariate scale, not bin count) but added no replacement check.
- **Fix applied:** `refine_tree_cuts()` now aborts with `cli_abort()` when `rho_n >= 0.5`, naming the exact
  default (`M_n = sqrt(r_n)`, `r_n <= 4`) that reaches it. A new `bracket_saturated` column is added to the
  `refined` diagnostic data.frame (parallel to the existing `no_candidates_in_bracket` reason), `TRUE`/`FALSE`
  when a valid `rho_n < 0.5` window still clamps at 0 or 1 for an edge-of-support anchor, `NA` when no `rho_n`
  window was applied at all. Three new tests in `test-stage-b.R` cover: the `>= 0.5` rejection (exact boundary
  and comfortably over it, plus a just-under-boundary case that must still work); `bracket_saturated = TRUE`
  for an edge anchor; `bracket_saturated = FALSE` for a mid-support anchor at the same `rho_n`; `NA` with no
  `rho_n` supplied.
- **Not affected:** the in-progress `continuum-tree-grid-tradeoff` simulation study calls `refine_tree()`
  without `r_n`/`M_n` (confirmed in `simulations/continuum-tree-grid-tradeoff/one_sim.R` — Stage B is exercised
  in its plain structural-bracket-only mode), so this fix does not touch that study's already-collected
  100-rep results.

### Issue 2: the `do.call()` fix left the identical defect in place for `single_tree` — **FIXED** (2026-09-29, same session)

- **Severity:** High
- **Finding:** `single_tree` is a formal of `optimaltrees()` but not of `fit_tree()`, so a caller's
  `single_tree = FALSE` lands in `dots`, survives the original `extra_args <- dots; extra_args$model_limit <-
  NULL`, and collides with the hardcoded `single_tree = TRUE` — reproducing the byte-identical "formal argument
  matched by multiple actual arguments" error this commit exists to eliminate, for the *more* likely of the two
  arguments a caller might mistakenly pass.
- **Fix applied:** `single_tree` is no longer silently stripped. `fit_tree()` now explicitly rejects
  `single_tree=` in `...` with `cli_abort()` pointing at `fit_rashomon()` — the caller's intent (more than one
  tree) is clear and wrong for this function, so an actionable redirect is more correct than a silent override.
  `model_limit` continues to be stripped (its override IS the documented, correct use of this function). Test
  added in `test-fit-tree.R`.

### Issue 3: the `fit_tree()` model_limit fix shipped with no regression test — **FIXED** (2026-09-29, same session)

- **Severity:** High
- **Finding:** the commit's own comment correctly diagnosed that nothing in the test suite exercised the
  explicit-`model_limit=` path, fixed the code, but added no test closing that gap — leaving the regression
  surface open to a future refactor silently reintroducing the defect.
- **Fix applied:** `tests/testthat/test-fit-tree.R` created with two tests: explicit `model_limit=0` and
  `model_limit=1000` both succeed without an argument-matching error; `single_tree=FALSE` errors with a message
  naming `fit_rashomon` (covers Issue 2 in the same file).

### Issue 4: repro-script provenance still cannot distinguish an old binary from a new one

- **File:** `dev-scripts/repro_single_tree_tie_blowup.R:285-302`
- **Category:** Reproducibility
- **Severity:** High
- **Status: not fixed this session — deferred, tracked here rather than silently dropped.**
- **Finding:** the 2026-09-08 review's stated failure mode survives: if `OT_TIE_TAG` is left at its `new`
  default while an old binary is still loaded (the single easiest mistake in this workflow), the artifact is
  mislabelled with no way to detect it after the fact. `packageVersion()` does not change across a C++-only
  fix; `git_sha` is a property of the source tree at run time, not of the loaded build — the whole hazard is
  forgetting to reinstall, in which case the source is new, the SHA says new, and the `.so` is old.
- **Proposed fix (not applied):** record the loaded shared object's mtime/hash instead of (or alongside) the
  git SHA — `system.file("libs", paste0("optimaltrees", .Platform$dynlib.ext), package = "optimaltrees")` and
  hash or stat it. Left for a future session since it does not affect the correctness of any result already
  collected, only the provenance metadata's ability to catch a *future* stale-reinstall mistake.

### Medium and Low issues (9 + 7 = 16, not itemized here)

Not reproduced in full in this note — see the reviewer's complete transcript for the itemized list if needed;
none were judged severity-High or a correctness risk to already-collected results. Deferred with the same
discipline as Issue 4: tracked, not silently dropped, revisit in a future session rather than blocking this
commit on issues that do not change any conclusion already drawn.

## Disposition

Issues 1–3 (all High) fixed in this same session, verified via full `devtools::test()` (0 failures) after the
fixes, with new regression coverage for each. Issue 4 (High) and all Medium/Low issues deferred, documented
above rather than silently dropped, on the basis that none affects the correctness of results already
collected under the `continuum-tree-grid-tradeoff` study.
