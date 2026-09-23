# R Code Review: repro_single_tree_tie_blowup.R

**File:** `optimaltrees/dev-scripts/repro_single_tree_tie_blowup.R` (195 lines)
**Date:** 2026-09-08
**Reviewer:** r-reviewer agent
**Standard applied:** dev/debugging tooling under `dev-scripts/` — reproducibility, correctness, and non-misleading output. Package-API rigor (roxygen, exports, tests) intentionally **not** applied.

## Summary

- **Total issues:** 22
- **Critical:** 3 (the harness cannot currently substantiate the conclusion it prints)
- **High:** 5 (blocks the old-vs-new comparison this script exists to perform)
- **Medium:** 9
- **Low:** 5

**Overall assessment.** The script is well-organized, well-commented, and the DGP reuse is clean and genuinely read-only. Its `stopifnot()` on `DGP_DIR`, the explicit `worker_limit = 1L`, the `%||%`-free core paths, and the correct use of `vapply()` over `sapply()` all show care. The header comment is unusually good: it states the setting, the modes, and the tie-count arithmetic, and the arithmetic checks out (`T(d) = d·T(d-1)²` with T(3)=12, T(4)=576, T(5)=1658880 — verified, and `Reduce(function(t, k) k * t^2, seq_len(d))` computes exactly that).

The problems are concentrated in one place, and they are consequential: **the three modes measure something narrower than what their output claims.** Specifically, (a) `model_limit` — the exact resource the bug exhausted — is never pinned and silently varies across the sweep; (b) `fit_tree()` returns at most one tree by construction, so the "tie-identity" diagnostics cannot falsify the tie hypothesis; and (c) a fit truncated at the 300 s limit is reported identically to a successful one. For a harness whose output will be cited as evidence that a C++ fix works, these are correctness defects rather than polish items.

**Practical note for downstream use (added post-review, orchestrator):** these issues do NOT invalidate the two things independently verified this session outside this script's own diagnostics — that the package recompiles clean and that the full existing test suite passes unchanged. They DO mean:
- The `repro` mode's headline finding (original L̄=30/n=2000 crash now returns one 30-leaf tree in 29.4s) stands: both the old (crashed) and new (succeeded) runs went through the same auto-heuristic `model_limit`, so the comparison is apples-to-apples for that one case, even though the limit itself was never explicitly pinned.
- The `ties` mode's "DISTINCT fitted functions... 1" line is **not** evidence that the old code's ties were partition-identical — `fit_tree()` forces `single_tree = TRUE` and returns at most one tree on *any* build, so that check is tautological (Critical #2). The partition-identity claim (Oracle's step 1) remains genuinely unverified; doing it for real requires `fit_rashomon(rashomon_ignore_trivial_extensions = FALSE)`, not `fit_tree()`.
- The `grid` mode (unrun this session) should not be trusted as-is: `model_limit` varies by cell (Critical #1), truncated fits would be indistinguishable from converged ones (Critical #3), and a worst-case cell can take hours without a deadline (Medium #9). Fix Critical #1–#3 and High #6–#8 before running it for the actual go/no-go timing number.

---

## Issues

### Issue 1: `model_limit` — the resource the bug exhausted — is never pinned, and silently varies across the sweep

- **File:** `repro_single_tree_tie_blowup.R:102-108`, `159-164`
- **Category:** Domain Correctness
- **Severity:** Critical
- **Current:**
  ```r
  m <- optimaltrees::fit_tree(
    as.data.frame(dat$Xbin[ctrl, , drop = FALSE]), dat$Y[ctrl],
    loss_function = "squared_error", regularization = lam,
    max_depth = truth$depth_mu, time_limit = FIT_TIME_LIMIT,
    worker_limit = 1L, verbose = FALSE
  )
  ```
- **Rationale:** The documented failure mode is that the cross-product enumeration *"exhausts `Configuration::model_limit` and returns nothing"* (`src/optimizer/extraction/models.hpp:9`; the guard is at `models.hpp:233-235`). The script never passes `model_limit`, so it inherits `fit_tree()`'s heuristic (`R/fit_tree.R:127-208`), which estimates the post-discretization feature count **assuming worst case: all features are continuous** (`fit_tree.R:168`) at 32 bins (`compute_bin_count("adaptive", n)` = fixed 32, `discretize.R:232`). For this script's already-binary inputs that estimate is inflated by ~31×, and it lands in different branches per mode:

  | Mode | `p_original` | `p_estimated = p·31` | Branch | Effective `model_limit` |
  |---|---|---|---|---|
  | repro | 10 | 310 | `> 100` | **1,000,000** + `immediate.` warning |
  | ties `d=3` (default) | 3 | 93 | `> 50` | **100,000** |
  | ties `d=4` | 4 | 124 | `> 100` | **1,000,000** |
  | ties `d=5` | 5 | 155 | `> 100` | **1,000,000** |

  So the tie sweep changes `model_limit` by 10× between `d=3` and `d=4` — a confound in precisely the quantity under test — and no run is ever unlimited, despite the low-dimensional branch (`model_limit <- 0`) being the one the package's own comment says single-tree extraction needs (`fit_tree.R:198-203`). Worse, in repro mode a `warning(..., immediate. = TRUE)` about `model_limit` is emitted on every run and is currently unexplained noise that a reader may mistake for the bug resurfacing.
- **Proposed fix:** Pin it explicitly, echo it, and test both regimes — a tie-enumeration fix should hold under a finite cap *and* unlimited:
  ```r
  # model_limit is the resource the old code exhausted (models.hpp:233), so pin it
  # rather than inheriting fit_tree()'s heuristic, which assumes continuous
  # features (fit_tree.R:168) and would silently give 1e5 at d=3 vs 1e6 at d>=4.
  MODEL_LIMIT <- as.integer(Sys.getenv("OT_MODEL_LIMIT", "0"))  # 0 = unlimited
  cli::cli_inform("model_limit = {MODEL_LIMIT} (0 = unlimited)")
  # ... then pass model_limit = MODEL_LIMIT to every fit_tree() call,
  # and forward it through fit_one()'s `...` to bisect_lambda_to_budget().
  ```

---

### Issue 2: Ties mode cannot observe ties — `fit_tree()` returns at most one tree by construction

- **File:** `repro_single_tree_tie_blowup.R:141-192` (esp. `175-184`), and header line 13-14
- **Category:** Domain Correctness
- **Severity:** Critical
- **Current:**
  ```r
  #   mode = "ties"   small tie-prone case; dumps every returned tree for the
  #                   old-vs-new partition-identity comparison
  ...
  m <- optimaltrees::fit_tree(as.data.frame(Xb), y, ...)
  trees <- model_trees(m)
  ...
  cat("leaf counts (unique):", paste(sort(unique(nl)), collapse = ","), "\n")
  cat("model_objective (unique, 8dp):", ...)
  cat("DISTINCT fitted functions over all 2^d cells:", length(unique(preds)), "\n")
  ```
- **Rationale:** Three independent guarantees make `length(trees) <= 1` on the fixed build:
  1. `fit_tree()` forces `single_tree = TRUE` (`fit_tree.R:221`) and *"guarantees that exactly one tree is returned"* (`fit_tree.R:5-6`), warning if `n_trees != 1` (`fit_tree.R:252-254`).
  2. The fix itself returns *one* model by deterministic tie-break — *"Extract exactly ONE optimal tree… Work is proportional to the size of the returned tree, not to the number of tied-optimal trees"* (`models.hpp:1-2`, `25`, `100`).
  3. `rashomon_ignore_trivial_extensions` defaults to `TRUE` (`fit_tree.R:90`), which *"prune[s] trees with identical partitions but different split sequences, keeping one representative"* — i.e. R-level dedup would erase the very multiplicity being counted, even if C++ produced it.

  Consequently `unique(nl)`, `unique(round(objs, 8))`, and `length(unique(preds))` are computed over a set of size ≤ 1, so they are **tautologically** single-valued. The printed line `DISTINCT fitted functions over all 2^d cells: 1` reads as a verified partition-identity result but is guaranteed by construction and cannot fail. And on the *old* build the failure was 0 trees, not many — so the "old-vs-new partition-identity comparison" promised in the header is 0-vs-1, not a comparison of tied trees at all. Additionally, the S7 validator enforces `length(@trees) == @n_trees` (`s7_classes.R:95-96`), so `model_trees()`'s unwrapping branch (`:62`) and `is.null(tr)` guard (`:59`) are hedges against shapes the class forbids.
- **Proposed fix:** Either (a) rename the mode's claims to what it measures, or (b) actually enumerate. For (b), use the Rashomon path with dedup off, which is the only way to observe multiplicity:
  ```r
  # To observe tie MULTIPLICITY you must not use fit_tree(): it forces
  # single_tree = TRUE (fit_tree.R:221) and prunes trivial extensions
  # (fit_tree.R:90), so length(trees) <= 1 regardless of the C++ behaviour.
  m <- optimaltrees::fit_rashomon(
    as.data.frame(Xb), y, loss_function = "squared_error",
    regularization = 1e-6, max_depth = d + 2L,
    rashomon_ignore_trivial_extensions = FALSE,
    model_limit = MODEL_LIMIT, worker_limit = 1L, verbose = FALSE
  )
  ```
  For (a), state plainly: `cat("n_trees (fit_tree guarantees <= 1; 0 == model_limit exhausted) =", length(trees), "\n")` and drop the three "unique"/"DISTINCT" lines, which carry no information in this configuration.

---

### Issue 3: A fit truncated at the 300 s limit is reported identically to a converged one

- **File:** `repro_single_tree_tie_blowup.R:72-84`, `102-119`
- **Category:** Domain Correctness / Error Handling
- **Severity:** Critical
- **Current:**
  ```r
  elapsed <- system.time({ fit <- optimaltrees::bisect_lambda_to_budget(..., fit_time_limit = FIT_TIME_LIMIT, ...) })[["elapsed"]]
  list(n = n, L = L, depth = truth$depth_mu, n_control = sum(ctrl),
       lambda = fit$lambda, n_leaves = fit$n_leaves, sec = elapsed)
  ```
- **Rationale:** `bisect_lambda_to_budget()` returns `any_truncated`, `certified`, `feasible`, `gap`, `depth_sufficient`, and a per-fit `fit_log`, and its own documentation is emphatic that truncation is a distinct failure mode: *"A truncated fit's incumbent is not proven optimal, which breaks the monotone-leaf-count premise `certified` relies on… callers building on this function should treat `any_truncated = TRUE` as its own failure mode, distinct from `certified = FALSE`, **not paper over it**"* (`bisect_lambda_to_budget.R:366-372`). The harness discards all six fields and keeps only `lambda`, `n_leaves`, and wall-clock. A cell that hit the 300 s wall therefore prints e.g. `leaves = 24 (budget 30) | lambda = 1.2e-04 | 300.0 s` — a plausible-looking timing row derived from an unproven incumbent, indistinguishable from success. Since the whole point is to show the fix makes these fits *finish*, silently reporting timeouts as timings inverts the conclusion. Note also that `solver_status = 1` is explicitly **not** a timeout signal (same docstring, `:356-364`), so `fit_log` must be read via `truncated`/`solver_time`, not status.
- **Proposed fix:**
  ```r
  # any_truncated is a distinct failure mode from certified == FALSE and must not
  # be collapsed into the timing number (bisect_lambda_to_budget.R:366-372).
  if (isTRUE(fit$any_truncated)) {
    cli::cli_abort(c(
      "n = {n}, L = {L}: a fit hit its time_limit; incumbent is not proven optimal.",
      i = "Wall-clock {round(elapsed, 1)}s is a lower bound, not a solve time."
    ))
  }
  list(n = n, L = L, depth = truth$depth_mu, n_control = sum(ctrl),
       lambda = fit$lambda, n_leaves = fit$n_leaves, sec = elapsed,
       certified = fit$certified, feasible = fit$feasible,
       depth_sufficient = fit$depth_sufficient, gap = fit$gap,
       n_fits = fit$n_fits, any_truncated = fit$any_truncated)
  ```
  For the repro-mode bare `fit_tree()` call (no `any_truncated` available), compare `treefarms_time_cpp()` against `FIT_TIME_LIMIT` as the docstring describes.

---

### Issue 4: Repro mode crashes with an unhelpful error on the exact failure it reproduces

- **File:** `repro_single_tree_tie_blowup.R:110-114`
- **Category:** Error Handling
- **Severity:** High
- **Rationale:** When the bug is present, `single_model()` returns nothing and `n_trees == 0` (`models.hpp:118`, `125-126`), so `trees1` is `list()` and line 112 dies with `subscript out of bounds`. The script's headline mode thus fails with a generic R indexing error precisely in the case it was written to demonstrate, discarding the timing already measured on line 109. Ties mode handles this correctly (`:168-171`), which makes the omission an inconsistency rather than an oversight of the possibility.
- **Proposed fix:**
  ```r
  trees1 <- model_trees(m)
  if (length(trees1) == 0L) {
    cat(sprintf("   n_trees = 0 -- BUG REPRODUCED (model_limit exhausted) | %.2f s\n", el))
  } else {
    cat(sprintf("   n_trees = %d | leaves = %d | %.2f s\n",
                length(trees1), count_leaves(trees1[[1L]]), el))
  }
  ```

---

### Issue 5: Hardcoded absolute `DGP_DIR`

- **File:** `repro_single_tree_tie_blowup.R:19-21`
- **Category:** Reproducibility
- **Severity:** High
- **Rationale:** Violates `r-code-conventions.md` §1 ("All paths relative to repository root") and the Common Pitfalls table. This matters more than usual here: the script's purpose is to let *someone else* verify a C++ fix, and it cannot run on any machine but the author's. The line is also 121 characters, exceeding §7's 100-char limit with no mathematical justification.
- **Proposed fix:** Derive `DGP_DIR` from the script's own location plus an env override (`OT_DGP_DIR`).

---

### Issue 6: Grid mode overwrites its own output, destroying the old-vs-new comparison

- **File:** `repro_single_tree_tie_blowup.R:139` (vs `:171`, `:190`)
- **Category:** Serialization
- **Severity:** High
- **Rationale:** Ties mode correctly tags artifacts by build (`sprintf("/tmp/ot_ties_d%d_%s.rds", d, tag)`), but grid mode writes one fixed filename (`/tmp/ot_timing_grid.rds`). Running the grid on the old binary and then the new one silently replaces the "before" numbers with the "after" numbers — eliminating the timing comparison that is the mode's entire reason to exist, with no warning and no recovery.
- **Proposed fix:** Tag by `OT_TIE_TAG`, refuse to overwrite an existing file (`cli::cli_abort()` if present).

---

### Issue 7: Saved artifacts record no build provenance

- **File:** `repro_single_tree_tie_blowup.R:139`, `170-171`, `187-190`
- **Category:** Reproducibility
- **Severity:** High
- **Rationale:** The old-vs-new distinction rests entirely on a free-text environment variable (`OT_TIE_TAG`). If it is left at its `"new"` default while the old binary is loaded — the single easiest mistake in this workflow — the artifact is mislabelled with no way to detect it after the fact. Nothing in the file records which `optimaltrees` build actually ran.
- **Proposed fix:** Capture `packageVersion("optimaltrees")`, the package library path, git SHA, `model_limit`, and timestamp in every saved artifact.

---

### Issue 8: Failed grid cells vanish from both the printed table and the saved artifact

- **File:** `repro_single_tree_tie_blowup.R:129-137`
- **Category:** Error Handling
- **Severity:** High
- **Rationale:** `tryCatch(..., error = -> NULL)` is anti-pattern F3 in the `r-interactive-entry` skill. `Filter(Negate(is.null), out)` drops failures from both the printed table and the saved RDS, so a reader of the artifact will conclude the grid completed when some cells actually errored. `bisect_lambda_to_budget()` also raises a dedicated condition class `"optimaltrees_deadline_exceeded"` that deserves distinguishing from a generic solver error.
- **Proposed fix:** Retain every row (with a `status` column: `ok`/`deadline`/`error`) rather than filtering failures out; warn on any non-`ok` count.

---

### Issue 9: No total wall-clock bound — worst-case grid runtime is measured in days, and is undocumented

- **File:** `repro_single_tree_tie_blowup.R:27`, `72-80`, `121-139`
- **Category:** Domain Correctness / Polish
- **Severity:** Medium
- **Rationale:** `fit_time_limit` bounds each internal fit, not the call. Bisection can invoke `fit_tree()` up to `2 * tol_iter + 1` times (default `tol_iter = 40`), so 81 fits × 300s ≈ 6.75h per cell, and the 8-cell grid is ≈ 54h worst case. `bisect_lambda_to_budget()` provides a `deadline` argument for exactly this and the harness doesn't use it.
- **Proposed fix:** Add an `OT_GRID_HOURS`-bounded `deadline` passed through to every cell.

---

### Issue 10: Two different leaf-count implementations are printed side by side as if comparable

- **Severity:** Medium — `count_leaves()` (script-local) vs. `bisect_lambda_to_budget()$n_leaves` (package) are different implementations, printed under the same label. Use the package's counter as the single source of truth.

### Issue 11: Hand-rolled `eval_tree_json()` indexes positionally without asserting column order

- **Severity:** Medium — correct for now (matches package convention), but unasserted; add `stopifnot(identical(colnames(design), colnames(Xb)))` or use the package's tested `predict()` path.

### Issue 12: `set.seed()` inside a function, plus a second undocumented magic seed

- **Severity:** Medium — three call sites (`20260908` twice, `11` once); the per-cell reseeding in `fit_one()` is plausibly deliberate (holds the draw fixed across `L` for comparable timings) but undocumented as such.

### Issue 13: Silent dependence on `getOption("rtvr.*")` set by the sourced `dgp.R`

- **Severity:** Medium — the header's "10 binary features" assumes `rtvr.p=5`/`rtvr.n_thresh=2` defaults, never asserted against a stale option left set from a prior interactive session.

### Issue 14: `%||% NA_real_` can silently turn a broken contract into a printed `NA`

- **Severity:** Medium — `model_objective` is always set by `Model::to_json()`; absence is a bug, not a case to default around, and `sort()` drops `NA` so the failure mode is a silently empty diagnostic line, not a visible one.

### Issue 15: `saveRDS()` to volatile `/tmp` with no directory creation

- **Severity:** Medium — should use `readr::write_rds()` per §5 into a project-relative, `fs::dir_create()`'d directory; `/tmp` is a poor home for evidence meant to survive a rebuild-and-rerun cycle.

### Issue 16: `count_leaves()` and `eval_tree_json()` disagree on what a leaf is

- **Severity:** Medium — two different leaf predicates for one concept in a file whose output is a leaf-count comparison; latent (shouldn't arise from well-formed `Model::to_json()` output) but worth unifying.

### Issue 17: Undocumented magic numbers in the two constructed settings

- **Severity:** Medium — `lam <- log(2000)/2000/20`, `n <- 64L*2^d`, `cell_means <- seq(-2,3.3,...)`, `max_depth = d+2L` all encode design decisions with no stated rationale for the specific constants chosen.

### Issue 18: Repro mode duplicates `fit_one()`'s setup with hardcoded literals

- **Severity:** Low — `2000`/`30` appear as literals four+ times; hoist to named constants or a shared `make_case(n, L)` helper.

### Issue 19: `t` shadows `base::t()` in three closures

- **Severity:** Low — harmless here, rename to `tr` for cleanliness.

### Issue 20: `sprintf("%d", ...)` fed integer-valued doubles

- **Severity:** Low — `partition_depth()` returns a double; works today, fragile if it ever returns non-integral. Coerce with `as.integer()` once.

### Issue 21: Minor output and dispatch nits

- **Severity:** Low — `%s` used for an integer in one spot; a no-op `[1]` after `Reduce()`; the if/else-if mode dispatch could be a `switch()`.

### Issue 22: Console output style — noted, not counted against the script

- **Severity:** Low (informational) — `cat()`/`sprintf()` status output is the correct medium for a CLI harness whose deliverable is human-readable stdout; not a real issue, recorded only so the omission is a judgment call rather than an oversight. `cli::cli_inform()` migration would be a cosmetic win, not urgent.

---

## Checklist Summary

| Category | Pass | Issues |
|----------|------|--------|
| Structure & Header | Yes | 1 (#21c) |
| Console Output | Yes | 1 (#22, informational) |
| Reproducibility | **No** | 4 (#5, #7, #12, #13) |
| Functions | Yes* | 3 (#17, #18, #19) |
| Domain Correctness | **No** | 6 (#1, #2, #3, #10, #11, #16) |
| Figures | n/a | 0 (no figures generated) |
| Serialization | **No** | 2 (#6, #15) |
| Comments | Yes | 1 (#17) |
| Error Handling | **No** | 4 (#4, #8, #9, #14) |
| Polish | Yes | 3 (#20, #21, #5 line length) |

\* Function design is sound for dev tooling — `snake_case`, verb-noun, small, purpose-commented. No roxygen expected at this standard.

---

## Recommended order of work

1. **#1, #2, #3** — until these are addressed the harness's output does not support the claim that the fix works beyond what was independently verified outside the script (compile + test suite + the one repro-mode run already observed). #1 and #2 are cheap (pin `model_limit`; either relabel the ties output or switch to `fit_rashomon(rashomon_ignore_trivial_extensions = FALSE)`); #3 is a few lines of field propagation.
2. **#4, #8** — make failure visible: the repro mode should report `n_trees = 0` as a success condition for *reproduction*, and the grid must not delete failed cells.
3. **#6, #7** — without tagged, provenance-stamped artifacts the old-vs-new comparison is not auditable after the fact.
4. **#5** — required before anyone else can run this.
5. The remainder as convenient.

Two things worth preserving as-is: the `stopifnot()` on `DGP_DIR` (a correctly located failure) and the header's tie-count arithmetic (`T(d) = d·T(d-1)²`), independently verified correct including the `T(1)=1` base case.
