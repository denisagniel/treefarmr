# R Code Review: treefarms.R, auto_tune.R, test-leaf-value-precision.R
**Date:** 2026-09-10
**Reviewer:** r-reviewer agent
**Scope:** R-side hunks of the leaf-value precision fix only
(`R/treefarms.R:953-964`, `R/auto_tune.R:139-149`,
`tests/testthat/test-leaf-value-precision.R:1-77`). The companion C++ fix
(`model.cpp`/`encoder.cpp`/`dataset.cpp`) is explicitly out of scope per the
review request.
**Context read:** `quality_reports/plans/2026-09-09_optimaltrees-leaf-value-precision-complete-fix.md`,
`.claude/rules/r-code-conventions.md`.
**Method:** static review. No code was executed; claims that would need a run to
confirm are marked **[verify]**.

## Summary
- **Total issues:** 12
- **Critical:** 0
- **High:** 2 (Issue 1 — silent encoding change for logical columns; Issue 2 —
  precision-critical logic duplicated, one copy untested)
- **Medium:** 4 (Issues 3-6)
- **Low:** 6 (Issues 7-12)

**Verdict: the fix is correct and well-motivated, and the change is a net
improvement — but it is not yet fully guarded.** `sprintf("%.17g", .)` is the
right formatter (17 significant digits is the round-trip-exact width for IEEE-754
binary64, and `%g` strips trailing zeros so binary 0/1 features stay one
character wide, avoiding payload bloat). Replacing `apply(data_df, 1L, ...)` also
removes a second, unstated hazard: the old path's dependence on
`as.matrix.data.frame`'s `format()`-mediated numeric conversion. The WHY-comments
in all three files are exemplary and exactly what this project's conventions ask
for.

Three things hold it back from PR quality: (a) the new per-column `is.numeric()`
dispatch silently reclassifies **logical** columns from numeric to character,
which the old whole-frame coercion did not (Issue 1); (b) the builder is
copy-pasted into two functions that have *different* input-normalization
preconditions, and only one copy is tested (Issue 2); (c) the new regression
tests pin the ~7-significant-figure catastrophic failure but do **not** pin the
17-digit round-trip contract the fix was written to establish — every test value
has ≤10 significant digits, so all eight assertions would also pass under
`%.15g` or plain `as.character()` (Issue 4).

---

## Issues

### Issue 1: Logical columns silently change encoding from `1`/`0` to `TRUE`/`FALSE`
- **File:** `R/treefarms.R:960-962`; `R/auto_tune.R:145-147`
- **Category:** Domain Correctness
- **Severity:** High
- **Rationale:** The replaced code dispatched on the *frame's* coerced type;
  the new code dispatches per column on `is.numeric()`, and
  `is.numeric(TRUE)` is `FALSE`. For a frame of numeric + logical columns,
  `as.matrix.data.frame` applied the coercion hierarchy and produced a
  **double** matrix, so `TRUE` reached the CSV as `"1"`. Post-fix it reaches the
  CSV as `"TRUE"`.

  Reachable through the documented API: `discretize_features()` short-circuits
  on `all_binary` (`discretize.R:56-59`), and `is_binary()`
  (`discretize.R:622-626`) is `all(x_clean %in% c(0, 1))`, `TRUE` for
  `c(TRUE, FALSE)` — so a logical column passes through **unconverted**.

  Downstream, the tree's `reference` field is parsed numerically in at least
  three places — `tree_structure.R:224`, `stage_b.R:180`, `tree_averaging.R:394`
  — so a `"TRUE"` reference either aborts or produces `NA`.

  Blast radius differs by entry point: `optimaltrees()`/`fit_tree()`'s logical
  **`y`** is safe (normalized at `treefarms.R:773-775` before the builder runs);
  only logical **X columns** regress there. `auto_tune_optimaltrees()`
  (`auto_tune.R:47-48`) has **neither** the `y` normalization nor the
  binary-feature validation before its copy of the builder — so a logical
  **`y`** there becomes `"TRUE"`/`"FALSE"` and `treefarms.R:397`'s
  `suppressWarnings(as.numeric(node$prediction))` turns that into a **silent
  `NA` prediction**.

  The existing suite cannot catch this: no test uses a logical column, so the
  green 2123 is not evidence against it.

### Issue 2: Precision-critical builder duplicated verbatim; the `auto_tune.R` copy has zero test coverage and different preconditions
- **File:** `R/treefarms.R:953-964` and `R/auto_tune.R:139-149`
- **Category:** Function Design
- **Severity:** High
- **Rationale:** (1) Drift risk on a numerical contract — the next edit will
  plausibly patch one site and not the other. (2) Asymmetric preconditions —
  `treefarms.R`'s copy is preceded by outcome normalization and binary-feature
  validation; `auto_tune.R`'s copy is preceded by neither. (3) Coverage —
  `fit_tree()` delegates to `optimaltrees()`, so all three new tests exercise
  only the `treefarms.R` copy.
- **Status:** not fixed this pass (deferred — see commit/session notes).

### Issue 3: `do.call(paste, c(formatted_cols, sep = ","))` breaks on a feature column named `sep` or `collapse`
- **File:** `R/treefarms.R:963`; `R/auto_tune.R:148`
- **Category:** Domain Correctness / Polish
- **Severity:** Medium
- **Rationale:** `formatted_cols` inherits `names(data_df)`, and `do.call()`
  matches list element names against the callee's formals. A feature named
  `sep` produces `formal argument "sep" matched by multiple actual arguments`;
  `collapse` changes the result shape instead of erroring, and is the more
  dangerous of the two. `unname()` + explicit `list()` closes it at zero cost.
- **Status:** fixed this pass.

### Issue 4: The regression tests do not pin the round-trip contract they were written to protect
- **File:** `tests/testthat/test-leaf-value-precision.R`
- **Category:** Tests / Domain Correctness
- **Severity:** Medium
- **Rationale:** every outcome literal has ≤10 significant digits, so all eight
  assertions would also pass under `%.15g` or `as.character()`. The tests guard
  the catastrophic ~7-sig-fig failure, not the 17-digit round-trip contract the
  header claims (lines 22-23).
- **Status:** not fixed this pass (deferred — requires the Issue 2 extraction
  to test the payload directly; the full-mantissa fixture variant is a smaller
  follow-up on its own).

### Issue 5: Tests conflate structure recovery with leaf-value precision
- **Severity:** Medium. **Status:** not fixed this pass (deferred).

### Issue 6: No guard against separator characters in column names or non-numeric levels
- **Severity:** Medium. **Status:** not fixed this pass (deferred — bundled
  with Issue 2's extraction).

### Issues 7-12 (comment scoping, locale documentation, magic tolerance,
superseded-pattern audit, `class`-column collision, missing contract doc)
- **Severity:** Low each. **Status:** not fixed this pass (deferred).

---

## What is right (recorded so a future reader does not re-litigate it)

1. `%.17g` is the correct formatter — 17 significant digits is the minimum
   round-trip-exact width for IEEE-754 binary64, and unlike `as.character()` it
   is invariant across R versions.
2. No payload bloat for binary features — `%g` strips trailing zeros.
3. Column-wise is also faster than the code it replaced.
4. The factor test genuinely discriminates the R-side fix (traced through
   `is_binary()`/`discretize_features()`'s `all_binary` fast path — confirmed
   the factor column reaches the builder as non-numeric, exactly the case the
   fix targets).
5. Comment quality is above the project's bar — WHY-comments throughout, citing
   root cause, commit, and plan.

## Resolution (2026-09-10, same day)

Fixed before commit: **Issue 1** (logical-column dispatch — added an
`is.logical()` branch ahead of `is.numeric()` in both `treefarms.R` and
`auto_tune.R`, formatting logical columns via `sprintf("%.17g", as.numeric(col))`
so they reach C++ as `"1"`/`"0"`, matching the pre-fix behavior) and **Issue 3**
(`unname()` + explicit `list(sep = ",")` in the `do.call(paste, ...)` call, both
files). A regression test for the logical-column case was added to
`test-leaf-value-precision.R`.

**Deferred, not fixed this pass** (tracked for next session, not silently
dropped): Issue 2 (extract the duplicated builder into one shared, validated
helper — carries Issues 3's guard, 6, 11, 12 with it if done), Issue 4 (add an
exact, non-tolerance round-trip test once the helper exists), Issue 5
(structural pre-assertions in the multi-leaf test), Issues 7-10 and 12 (comment
scoping, locale documentation, magic-tolerance constant, superseded-pattern
audit of `structure_selection.R`/`treefarms_isolated.R`). None of these are
correctness regressions in the current diff — Issue 1 was the one genuine
regression, and it is fixed.

**Gate assessment after fixes: commit-ready (≥80).** PR-quality (90) still
requires Issue 2's extraction and Issue 4's exactness test.
