# R Code Review: `fit_twostage.R` / `stage_b.R` -- 2026-09-04 theory-revision reconciliation
**Date:** 2026-09-04
**Reviewer:** r-reviewer agent
**Scope:** staged (uncommitted) changes in `optimaltrees`
**Files reviewed:**
- `R/fit_twostage.R` (full read, 1038 lines)
- `R/stage_b.R` (full read, 1375 lines)
- `tests/testthat/test-fit-twostage.R` (full read, 718 lines)
- `dev-scripts/superseded/select_lambda_plateau.R` (light-touch -- parked reference)
- `dev-scripts/superseded/test-select-lambda-plateau.R` (light-touch -- parked reference)
- Context read: `tests/testthat/test-stage-b.R`, `R/discretize.R`, `DESCRIPTION`, `NEWS.md`, `.Rbuildignore`, `man/*.Rd`, `.claude/rules/r-code-conventions.md`, `.claude/rules/code-paper-package-alignment.md`

---

## Summary

- **Total issues:** 29
- **Critical:** 1 (blocks correctness / an unearned theory claim)
- **High:** 3 (blocks professional quality)
- **Medium:** 11 (improvement recommended)
- **Low:** 14 (style / polish / parked-script notes)

### Verdict

The architectural reconciliation is **substantively well done**. The de-inflation of `.twostage_resolve_budgets()`, the de-pruning of `.twostage_validate_ladder()`, the demotion of `collapse_transitions()` to a documented legacy escape hatch, and the introduction of `stage_b_refine_infeasible` as an ordinary per-rung status are all clean, honest, and correctly threaded end-to-end. The commentary discipline in this codebase is genuinely exceptional -- provenance, empirical confirmation, and refusal-vs-bug distinctions are documented at a level I rarely see. `NEWS.md` was updated and all seven affected `man/*.Rd` files were regenerated in the same staged change, which is the right hygiene and is explicitly noted as a positive below.

**I re-derived the new emptiness check independently and it does prevent the crash it targets.** It is *sound* -- it never lets an empty leaf reach `attach_final_stats()`. It is, however, **placed earlier than necessary**, and therefore *over-fires*: it aborts nodes that a subsequent refinement attempt would have handled correctly (Issue 3). That is a conservatism bug, not an unsoundness bug, but it silently discards legitimate fits and is worth fixing before commit.

The one item I would treat as a genuine blocker is the **scale-dependence of `rho_n = M_n/r_n`** (Issue 1). The new roxygen claims the implementation now "respects Proposition [search-interval validity]'s finite-sample guarantee." As written, that claim only holds when every continuous covariate is on roughly unit scale -- an assumption the package neither documents, asserts, nor tests, and which the default `M_n = sqrt(m)` makes load-bearing.

**Recommended gate:** address Issues 1, 2, 3, 4 before commit. Issues 5-15 before PR.

---

## Issues

### Issue 1: `rho_n = M_n / r_n` is applied in raw covariate units, but `r_n` is a bin *count* -- the claimed finite-sample interval guarantee silently fails off unit scale

- **File:** `R/stage_b.R:968-977`, `R/stage_b.R:1040-1053`; `R/fit_twostage.R:422`, `R/fit_twostage.R:563-571`
- **Category:** Domain / code-paper-package alignment
- **Severity:** **Critical**
- **Current:**
  ```r
  # stage_b.R:976
  rho_n <- M_n / r_n
  ...
  # stage_b.R:1050-1052
  anchor <- if (length(node$grid_cuts) == 1L) node$grid_cuts[[1L]] else node$cut
  br$lo <- max(br$lo, anchor - rho_n)
  br$hi <- min(br$hi, anchor + rho_n)
  ```
  ```r
  # fit_twostage.R:422 -- r_n = m (bins), M_n defaults to sqrt(m)
  M_n_resolved <- if (is.null(M_n)) sqrt(m) else M_n
  refined <- tryCatch(
    refine_tree(res_A$fit, X, y, ..., r_n = m, M_n = M_n_resolved),
  ```
- **The problem, derived:** `anchor - rho_n` / `anchor + rho_n` are arithmetic on the covariate's **raw data scale**. But `r_n = m` is a count of quantile bins (`R/discretize.R:472-503`, `compute_thresholds()` places thresholds at `quantile(x_clean, probs = probs)`), and `M_n = sqrt(m)` is dimensionless. So `rho_n` has no units at all, yet is compared against data-scale values.

  This is coherent **only** under the standard nonparametric normalization `X in [0,1]^p` with an approximately uniform design, where the grid mesh is `~1/m` and `rho_n = M_n/m` reads correctly as "`M_n` mesh widths." Concretely, at `m = 16` with the default `M_n = sqrt(16) = 4`:
  - `X` on `[0, 1]`: `rho_n = 0.25`, mesh `~0.0625` -> radius ~4 mesh widths. **Intended behavior.**
  - `X` on `[0, 1000]`: `rho_n = 0.25`, mesh `~62.5` -> radius is **0.4% of one mesh width**. `bracket_candidates()` (`stage_b.R:795-800`) uses strict inequalities on observed values, so almost certainly **zero** candidates land inside. Every node logs `reason = "no_candidates_in_bracket"` and keeps its grid cut. **Stage B silently does nothing at all** -- the entire off-grid refinement value proposition evaporates, recorded only in a `reason` column nobody is required to read.
  - `X` on `[0, 0.001]`: `rho_n` exceeds the full covariate range -> the intersection is a no-op and behavior silently reverts to the legacy structural-bracket-only path, while the roxygen claims the theory guarantee is in force.

  A secondary wrinkle: because thresholds are **quantile**-based rather than uniform, the mapping from a fixed data-scale `rho_n` to "number of grid atoms" varies across the covariate's own distribution (tight near the mode, wide in the tails). Even on `[0,1]`, a non-uniform design makes `rho_n` a different number of atoms at different nodes.

  Finally, `rho_n` is a **single scalar shared across all coordinates**. A design mixing `x1 in [0,1]` with `x2 in [0,1000]` gets a radius that is simultaneously reasonable for one coordinate and degenerate for the other.

- **Why this is Critical rather than Medium:** the code does not merely behave suboptimally -- the roxygen makes an affirmative theory claim on its behalf:

  > "Passing `r_n`/`M_n` (both non-NULL) intersects that radius into the structural bracket before candidates are drawn, so refinement additionally respects Proposition [search-interval validity]'s finite-sample guarantee" (`stage_b.R:925-933`)

  Per `.claude/rules/code-paper-package-alignment.md` and the research constitution, a claim of theoretical validity has to be earned by the implementation. Here it is earned only under an assumption that is nowhere stated, checked, or tested -- and the failure mode is silent in both directions.

- **Proposed fix** (pick one; (a) is the smallest honest change):

  (a) **Document + assert the normalization the theory assumes.** State in `fit_twostage()`'s and `refine_tree()`/`refine_tree_cuts()`'s roxygen that `rho_n` is in raw covariate units and that `M_n = sqrt(m)` presumes `X in [0,1]`-scale coordinates, then check it:
  ```r
  # in fit_twostage(), alongside the existing up-front X checks
  rng <- vapply(X, function(col) diff(range(col, na.rm = TRUE)), numeric(1))
  if (any(rng > 10) || any(rng < 0.1)) {
    cli::cli_warn(c(
      "fit_twostage: Stage-B's search radius rho_n = M_n/r_n is in RAW covariate units.",
      "i" = "Coordinate range(s) {.val {names(rng)[rng > 10 | rng < 0.1]}} are far from \\
             the unit scale the default {.arg M_n} = sqrt(m) assumes; the radius will be \\
             either negligible or vacuous relative to the grid mesh.",
      "i" = "Rescale X to roughly [0, 1] per coordinate, or pass {.arg M_n} explicitly."
    ))
  }
  ```

  (b) **Make the radius mesh-relative and per-coordinate** (closer to the theory's intent, but a behavior change that must be reconciled against `inst/paper/main.tex`'s own normalization convention before adoption):
  ```r
  # inside the use_rho branch of refine_tree_cuts(), per node
  # rho in units of this coordinate's OWN grid mesh, not raw data units
  gc_all  <- md_thresholds_for(node$coord)          # or diff(range(xj))/r_n
  mesh_j  <- if (length(gc_all) > 1L) median(diff(sort(gc_all))) else diff(range(xj)) / r_n
  rho_n_j <- M_n * mesh_j                            # M_n mesh widths, scale-free
  br$lo <- max(br$lo, anchor - rho_n_j)
  br$hi <- min(br$hi, anchor + rho_n_j)
  ```

  Whichever route is taken, add a test that a rescaled `X` (`X * 1000`) produces the *same* refined partition up to scale -- that single test would have caught this.

- **Rationale:** a scale-dependent tuning constant presented as a theory-derived guarantee is exactly the class of unearned claim the constitution exists to prevent. The failure is silent, the default makes it load-bearing, and no test exercises any non-unit scale.

---

### Issue 2: `refine_tree_cuts()`'s monotone-safety argument still rests on the premise this revision removed

- **File:** `R/stage_b.R:900-909`
- **Category:** Domain / Comments
- **Severity:** **High**
- **Current:**
  ```r
  #' Each node's bracket is recomputed from the CURRENT tree at the moment it
  #' is refined ([node_bracket()]), not stored statically: ... This asymmetry is
  #' monotone-safe and requires no second pass, because after
  #' [collapse_transitions()] any two same-coordinate splits that remain
  #' distinct nodes are separated by the (Sep) margin, not by one grid atom --
  #' do not "fix" this into an alternating minimization.
  ```
- **The problem:** this paragraph's *entire* justification for the single-pass, root-first asymmetry being safe is "**after `collapse_transitions()`** any two same-coordinate splits that remain distinct nodes are separated by the (Sep) margin, not by one grid atom." Under the revised architecture `collapse_transitions()` is no longer called (`refine_tree()`'s `collapse = FALSE`, `stage_b.R:1246`), so that premise is **false by construction on the default path**: two same-coordinate splits exactly one grid atom apart now routinely survive as distinct nodes.

  This is not a pedantic doc nit. The new `optimaltrees_refine_infeasible` check is precisely the empirical acknowledgment that the premise is gone -- the chained-split geometry that `collapse_transitions()` used to intercept is now reachable, which is exactly what the new check's own comment says (`stage_b.R:1009-1012`) and what `test-fit-twostage.R:275` demonstrates. The revision correctly updated `@param tree` ("no longer required by the default pipeline") but left the **safety argument** untouched, so the file now simultaneously (i) asserts single-pass refinement is monotone-safe because collapse ran, and (ii) implements a hard abort for the case where it didn't. Two paragraphs of the same roxygen block contradict each other.

  The forward-looking risk is concrete: the paragraph ends with "do not 'fix' this into an alternating minimization." A future maintainer reading only that instruction, under a premise that no longer holds, has been actively misdirected.

- **Proposed fix:**
  ```r
  #' Each node's bracket is recomputed from the CURRENT tree at the moment it
  #' is refined ([node_bracket()]), not stored statically: refining an
  #' ancestor first means a later same-coordinate descendant's row set
  #' ([coord_tree_rows_at()]) is already filtered by the ancestor's REFINED
  #' cut, while a not-yet-refined descendant node still contributes its GRID
  #' cut as the bounding value seen by its parent.
  #'
  #' \strong{2026-09-04: what makes this asymmetry safe changed.} The
  #' pre-revision argument was that after [collapse_transitions()], any two
  #' same-coordinate splits surviving as distinct nodes are separated by the
  #' (Sep) margin rather than one grid atom, so a single root-first pass
  #' could not strand a descendant. With `collapse = FALSE` the default,
  #' that premise NO LONGER HOLDS -- a same-coordinate pair one grid atom
  #' apart now survives as two nodes, and an ancestor's off-grid move CAN
  #' empty a side of a descendant's own split. This is handled explicitly,
  #' not assumed away: the `optimaltrees_refine_infeasible` check below
  #' detects exactly that case and refuses. The single pass remains correct
  #' in the sense that it never returns a tree with an empty leaf; it is no
  #' longer claimed to always SUCCEED. Do not "fix" this into an
  #' alternating minimization without re-deriving the theory first -- the
  #' refusal, not a second pass, is the sanctioned outcome.
  ```
- **Rationale:** the paper's headline result was retracted because an exclusion argument failed generically. Leaving the *package's* parallel exclusion argument standing, in a docstring, next to the code that refutes it, repeats that failure mode at a smaller scale. This is the single highest-value documentation fix in the change.

---

### Issue 3: The new incumbent-emptiness check over-fires -- it aborts before attempting the refinement that would have succeeded

- **File:** `R/stage_b.R:988-1037`
- **Category:** Domain / Functions
- **Severity:** **High**
- **Current:**
  ```r
  for (p in paths) {
    idx  <- coord_tree_rows_at(tree, X, p)
    node <- coord_tree_node_at(tree, p)

    if (length(idx) >= 1L) {
      xj_incumbent <- X[[node$coord]][idx]
      n_leq_incumbent <- sum(xj_incumbent <= node$cut)
      n_gt_incumbent  <- length(idx) - n_leq_incumbent
    } else {
      n_leq_incumbent <- 0L
      n_gt_incumbent  <- 0L
    }
    if (n_leq_incumbent < 1L || n_gt_incumbent < 1L) {
      cli::cli_abort(c(...), class = "optimaltrees_refine_infeasible")
    }

    br <- node_bracket(tree, p)
    ...                       # rho intersection, gates, candidate scan
  ```

- **My independent re-derivation (does it prevent the crash?):**

  **Soundness -- yes, and the induction is valid.** `paths` is depth-sorted (`stage_b.R:984`, `order()` on integers is stable radix), so every ancestor of `p` is processed before `p`. For each visited split node exactly one of two things happens:
  1. **Refinement occurs.** `feasible_cand` is filtered by `n_leq >= min_leaf_n & n_gt >= min_leaf_n` (`stage_b.R:1095-1097`) with `min_leaf_n >= 1L` enforced (`stage_b.R:964-967`), so both children receive `>= 1` row.
  2. **A gate fires `next`** (`too_few_rows_at_node`, `no_candidates_in_bracket`, `min_leaf_n_infeasible`) and the incumbent cut is retained -- and the new check has already proven the incumbent leaves `>= 1` row on **each** side.

  Either way both children of every visited split get `>= 1` row. By induction from the root, `length(idx) >= 1` at every node, hence no leaf is ever empty, hence `attach_final_stats()`'s "leaf has zero rows after refinement" abort (`stage_b.R:1132-1139`) is unreachable. **The check closes the hole.** (Corollary: the `else { 0L; 0L }` branch at `stage_b.R:1017-1020` is dead for any `nrow(X) >= 1`; harmless defensive code, worth a one-word comment.)

  **Narrowness -- correctly calibrated, and the code comment's reasoning is right.** The check tests `>= 1`, not `>= min_leaf_n`. That is deliberately weaker and it is the right call: the only invariant `attach_final_stats()` enforces is non-emptiness, and `test-stage-b.R:478` ("distinguishes `too_few_rows_at_node` from `min_leaf_n_infeasible`") is a legitimate pre-existing case where an incumbent leaves a side under `min_leaf_n` without that being an error. Testing `>= min_leaf_n` here would have broken that test and converted a documented non-error into a hard abort. **No under-fire, no spurious floor escalation.** This part is exactly right.

  **Over-fire -- yes, and it is reachable under the shipped defaults.** The check runs *before* `node_bracket()`, before the `rho_n` intersection, and before the candidate scan. So it aborts even when a perfectly good refinement was available. Concrete scenario:

  - Node `N` splits on `x1` at incumbent/grid cut `c = 0.35`.
  - An ancestor's off-grid refinement (processed earlier) shrinks `N`'s row set `S` so that every `x1` value in `S` lies in `[0.10, 0.30]`. Now `n_leq_incumbent = |S|`, `n_gt_incumbent = 0` -> **abort**.
  - But: `node_bracket()` would have returned `(-Inf, Inf)` if no descendant splits on `x1`; intersected with `[c - rho_n, c + rho_n]` at the default `m = 16`, `M_n = sqrt(16) = 4`, that is `(0.10, 0.60)` -- a **0.5-wide** window on a unit-scale covariate. Candidates would include most observed `x1` values in `S`, many with rows on both sides, so `scan_cutoff()` would have returned a valid, strictly SSE-reducing cut, and the resulting tree would have had no empty leaf.

  The default `rho_n = 1/sqrt(m)` is `0.25` at `m = 16` and `0.088` at `m = 128` -- large fractions of a unit-scale range. So the bracket is *wide* on the default path precisely at the coarse rungs where the ladder starts, which is exactly where over-firing is most likely. The user-visible consequence is a `stage_b_refine_infeasible` rung that escalates unnecessarily (`fit_twostage.R:760-768`), and in the worst case `fit_twostage()` returns `model = NULL` / `stop_reason = "ladder_exhausted"` when a valid refined tree existed at every rung.

  **On the stated rationale for the placement.** The inline comment (`stage_b.R:992-996`) justifies the early position as being "before any gate below (including the coarse `too_few_rows_at_node` one) has a chance to silently `next` past it." That goal is correct and important -- but it only requires the check to run before a `next` **takes effect**, not before the gates are **evaluated**. Attempting refinement first and aborting only when we are actually about to retain a degenerate incumbent achieves the identical guarantee with no over-fire.

- **Proposed fix** (strictly better: same soundness, no over-fire, same classed condition):
  ```r
  for (p in paths) {
    idx  <- coord_tree_rows_at(tree, X, p)
    node <- coord_tree_node_at(tree, p)

    # Is retaining the incumbent cut SAFE against this node's CURRENT row
    # set? An ancestor's off-grid refinement (processed earlier, root-first)
    # can shrink that set enough to empty a side of this node's own original
    # grid split. Compute the fact here; act on it only at the points where
    # we would actually KEEP the incumbent -- refining past a degenerate
    # incumbent is legitimate and must not be pre-emptively refused.
    xj_all <- X[[node$coord]][idx]
    n_leq_incumbent <- sum(xj_all <= node$cut)
    incumbent_safe  <- n_leq_incumbent >= 1L &&
                       (length(idx) - n_leq_incumbent) >= 1L

    abort_infeasible <- function() {
      cli::cli_abort(c(
        "refine_tree_cuts: node {node$id} (coord {.val {node$coord}})'s \\
         incumbent cut leaves one side completely empty against its current \\
         row set, and no feasible replacement candidate exists.",
        "i" = "An ancestor's off-grid refinement moved a threshold enough to \\
               empty a side of this node's own original grid split -- a \\
               genuine, reachable case once an ancestor and a descendant \\
               split nearby on different coordinates, not a bug in this \\
               function.",
        "i" = "Not handled: repairing this would mean re-deriving a \\
               structurally different tree (pruning this split, or backing \\
               off the ancestor's refinement), behavior this single-pass, \\
               root-first refinement was not derived for. This error is the \\
               honest alternative to silently returning an empty leaf."
      ), class = "optimaltrees_refine_infeasible")
    }

    br <- node_bracket(tree, p)
    if (use_rho) { ... }        # unchanged
    ... base_row() definition unchanged ...

    if (length(idx) < 2L * min_leaf_n) {
      if (!incumbent_safe) abort_infeasible()
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "too_few_rows_at_node")
      next
    }

    Xn <- X[idx, , drop = FALSE]
    yn <- y_centered[idx]
    xj <- xj_all                 # reuse; no second subset (see Issue 17)

    cands_raw <- bracket_candidates(xj, node, br)
    if (length(cands_raw) == 0L) {
      if (!incumbent_safe) abort_infeasible()
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "no_candidates_in_bracket")
      next
    }

    n_leq <- vapply(cands_raw, function(c) sum(xj <= c), integer(1))
    n_gt  <- length(xj) - n_leq
    feasible_cand <- cands_raw[n_leq >= min_leaf_n & n_gt >= min_leaf_n]

    if (length(feasible_cand) == 0L) {
      if (!incumbent_safe) abort_infeasible()
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "min_leaf_n_infeasible")
      next
    }
    ... scan unchanged ...
  ```
  Note this also makes the `length(idx) == 0L` branch genuinely unnecessary (`incumbent_safe` is `FALSE` when `idx` is empty, via `0 >= 1L` being `FALSE`), removing the dead code.

- **Rationale:** the current placement trades away recoverable fits for no additional safety. The reordering is provably equivalent on soundness (every path that could strand an empty leaf still aborts) and strictly better on yield. Given `fit_twostage()`'s whole purpose is to return a certified model, silently converting a certifiable rung into a refusal is a real cost.

---

### Issue 4: No unit test for `optimaltrees_refine_infeasible`, and zero tests anywhere for the `r_n`/`M_n` threading

- **File:** `tests/testthat/test-stage-b.R` (absent), `tests/testthat/test-fit-twostage.R:275-310`
- **Category:** Testing
- **Severity:** **High**
- **Current:** the only coverage of the new refusal path is the single integration test at `test-fit-twostage.R:275`, which drives a real GOSDT fit at `m = 16L, leaf_budget = 8L, lambda_n = 0.01` and asserts `rec$status == "stage_b_refine_infeasible"`. Grepping the whole test directory for `r_n`, `M_n`, `refine_infeasible`, and `collapse =` confirms:
  - **no** test raises `optimaltrees_refine_infeasible` from `refine_tree_cuts()` directly, and **no** test asserts the condition's **class** (only the derived `rec$status` string);
  - **no** test passes `r_n` or `M_n` to `refine_tree_cuts()` or `refine_tree()` at all -- neither the validation branches (`stage_b.R:969-975`) nor the bracket-narrowing behavior (`stage_b.R:1040-1053`) nor the `collapsed`-node anchor fallback (`stage_b.R:1050`) is exercised anywhere;
  - **no** test passes `collapse = TRUE` to `refine_tree()`, so the escape hatch the roxygen promises to keep supported is untested at the `refine_tree()` level (only `collapse_transitions()` in isolation is tested).

  Concretely, the entire `use_rho` block -- roughly 15 new lines including two `cli_abort` guards and the anchor branch -- has **zero** test coverage. And because the only refusal test asserts a *string status* rather than the condition class, the classed-condition contract that `fit_twostage.R:429-431` depends on (`tryCatch(optimaltrees_refine_infeasible = ...)`) is not locked in: renaming the class would break the pipeline while leaving the test green, since `.twostage_run_rung()` would simply let the raw error propagate... which would fail the test, admittedly -- but with an opaque error rather than a pinpointed one. More importantly, `refine_tree()` is **exported**, so external callers depend on that class directly and nothing guards it.

  The integration test is also brittle in a way the file's own conventions elsewhere anticipate: it depends on the C++ solver returning one specific topology at one specific `(seed, n, m, leaf_budget, lambda_n)`. A solver tie-break change, a `discretize_bins` change, or an RNG-stream change would flip it to `status == "ok"` and the test would fail for a reason unrelated to the code under test. `test-stage-b.R:498-509` already establishes the right pattern for this ("Hand-built rather than via a real fit: `fit_tree()`/GOSDT choosing to split at exactly two distinct covariate values on demand isn't reliably controllable"). The same reasoning applies here and was not followed.

- **Proposed fix** -- add to `test-stage-b.R`:
  ```r
  test_that("refine_tree_cuts raises the classed refine_infeasible condition when an ancestor's refinement empties a descendant's split", {
    # Hand-built, not solver-derived: reproducing the ancestor/descendant
    # geometry on demand from a real fit is not reliably controllable (same
    # reasoning as the min_leaf_n_infeasible fixture above). x1's root split
    # is refinable off-grid down to 0.30; the x1 <= cut child then contains
    # only rows with x2 <= 0.20, emptying the {x2 > 0.5} side of its own
    # descendant split.
    fixture <- list(
      kind = "split", id = 1L, coord = "x1", cut = 0.50,
      grid_cuts = 0.50, collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_,
      left = list(
        kind = "split", id = 2L, coord = "x2", cut = 0.50,
        grid_cuts = 0.50, collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_,
        left  = list(kind = "leaf", id = 3L),
        right = list(kind = "leaf", id = 4L)
      ),
      right = list(kind = "leaf", id = 5L)
    )
    # Construct X/y so the root's SSE-optimal off-grid cut strands node 2.
    X <- data.frame(
      x1 = c(rep(0.10, 20), rep(0.40, 20), rep(0.90, 20)),
      x2 = c(rep(0.10, 20), rep(0.80, 20), rep(0.80, 20))
    )
    y <- c(rep(0, 20), rep(5, 20), rep(5, 20))   # boundary at x1 = 0.10, not 0.50

    err <- tryCatch(refine_tree_cuts(fixture, X, y), condition = identity)
    expect_s3_class(err, "optimaltrees_refine_infeasible")
    expect_match(conditionMessage(err), "completely empty")
  })

  test_that("refine_tree_cuts validates r_n/M_n and ignores a half-specified pair", {
    fixture <- list(kind = "split", id = 1L, coord = "x1", cut = 0.5,
                     grid_cuts = 0.5, collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_,
                     left = list(kind = "leaf", id = 2L),
                     right = list(kind = "leaf", id = 3L))
    X <- data.frame(x1 = seq(0, 1, length.out = 40))
    y <- ifelse(X$x1 <= 0.37, 0, 2)

    expect_error(refine_tree_cuts(fixture, X, y, r_n = 0,  M_n = 4), "r_n")
    expect_error(refine_tree_cuts(fixture, X, y, r_n = -1, M_n = 4), "r_n")
    expect_error(refine_tree_cuts(fixture, X, y, r_n = c(8, 16), M_n = 4), "r_n")
    expect_error(refine_tree_cuts(fixture, X, y, r_n = 16, M_n = 0), "M_n")
    expect_error(refine_tree_cuts(fixture, X, y, r_n = 16, M_n = NA_real_), "M_n")
    # Half-specified: currently a SILENT fall-through to structural-only.
    # See Issue 6 -- lock in whichever behavior is decided, do not leave it
    # untested either way.
    ref_half <- refine_tree_cuts(fixture, X, y, r_n = 16)
    ref_none <- refine_tree_cuts(fixture, X, y)
    expect_equal(ref_half$refined$bracket_lo, ref_none$refined$bracket_lo)
  })

  test_that("r_n/M_n narrow the bracket to anchor +/- M_n/r_n, anchored at the grid cut", {
    fixture <- list(kind = "split", id = 1L, coord = "x1", cut = 0.5,
                     grid_cuts = 0.5, collapsed = FALSE, k_lo = 1L, k_hi = NA_integer_,
                     left = list(kind = "leaf", id = 2L),
                     right = list(kind = "leaf", id = 3L))
    X <- data.frame(x1 = seq(0, 1, length.out = 200))
    y <- ifelse(X$x1 <= 0.12, 0, 2)   # true boundary FAR from the grid cut

    wide <- refine_tree_cuts(fixture, X, y)                        # structural only
    tight <- refine_tree_cuts(fixture, X, y, r_n = 100, M_n = 2)   # rho_n = 0.02

    expect_equal(tight$refined$bracket_lo, 0.5 - 0.02)
    expect_equal(tight$refined$bracket_hi, 0.5 + 0.02)
    # The radius must actually BIND: unrestricted refinement reaches the true
    # boundary; the tight radius cannot.
    expect_lt(abs(wide$tree$cut - 0.12), 0.02)
    expect_gt(abs(tight$tree$cut - 0.12), 0.02)
    # And the incumbent grid cut always survives as a candidate, so the
    # never-worse-than-grid guarantee is preserved under narrowing.
    expect_gte(tight$refined$bracket_hi, 0.5)
    expect_lte(tight$refined$bracket_lo, 0.5)
  })

  test_that("refine_tree(collapse = TRUE) still runs the legacy collapse-then-refine path", {
    set.seed(20260901)
    n <- 1500
    X <- data.frame(x1 = runif(n))
    y <- ifelse(X$x1 <= 0.4123456, 0, 3) + rnorm(n, sd = 0.05)
    fit <- fit_tree(X, y, loss_function = "squared_error", regularization = 0.01,
                     discretize_bins = 8L, max_depth = 2L)
    rt_legacy  <- refine_tree(fit, X, y, collapse = TRUE)
    rt_default <- refine_tree(fit, X, y)
    expect_s7_class(rt_legacy, RefinedTreeModel)
    # The escape hatch is claimed supported -- exercise it, do not just
    # document it.
    expect_true(all(is.finite(predict(rt_legacy, X))))
    expect_lte(n_leaves(rt_legacy), n_leaves(rt_default))
  })
  ```
  And strengthen the existing integration test to assert the class contract survives the composition:
  ```r
  # test-fit-twostage.R:302, in addition to the status assertion
  expect_equal(rec$status, "stage_b_refine_infeasible")
  # Lock the CLASS the pipeline catches by, not just the derived string --
  # refine_tree() is exported and external callers depend on it.
  err <- tryCatch(
    refine_tree(fit_a, X, y, min_leaf_n = 1L, r_n = 16, M_n = sqrt(16)),
    condition = identity
  )
  expect_s3_class(err, "optimaltrees_refine_infeasible")
  ```
- **Rationale:** the two things this change is *about* -- a new refusal path and a new theory-specified search radius -- are the two things with the weakest test coverage. The `use_rho` block having zero coverage is the sharpest gap: Issue 1's scale bug would have been caught immediately by the third test above run on a rescaled `X`.

---

### Issue 5: `M_n = NULL` inserted mid-signature ahead of three non-defaulted arguments in `.twostage_run_rung()`

- **File:** `R/fit_twostage.R:357-361`
- **Category:** Functions / API design
- **Severity:** Medium
- **Current:**
  ```r
  .twostage_run_rung <- function(X, y, m, leaf_budget, L_A, d_A, depth_restricted_A,
                                  M_n = NULL, lambda_n, m_n, group = NULL, group_value = 0,
                                  m_n_group = m_n, min_leaf_n,
                                  fit_time_limit = NULL, deadline = NULL,
                                  tol_iter = 12L, ...) {
  ```
- **The problem:** `M_n = NULL` was inserted as the **8th** formal, ahead of `lambda_n`, `m_n`, and `min_leaf_n`, none of which has a default. R permits this, but any positional 8th argument now silently binds to `M_n` instead of `lambda_n`. Every current caller happens to use named arguments -- `fit_twostage.R:730-738` and all five test call sites -- so nothing breaks today. That is luck, not design, and it is exactly the kind of latent trap this change elsewhere goes to great lengths to avoid. A future call like `.twostage_run_rung(X, y, m, lb, L_A, d_A, TRUE, 0.05)` would set `M_n = 0.05` and then fail with `argument "lambda_n" is missing`, or worse succeed with a nonsense radius if `lambda_n` were later given a default.
- **Proposed fix:**
  ```r
  .twostage_run_rung <- function(X, y, m, leaf_budget, L_A, d_A, depth_restricted_A,
                                  lambda_n, m_n, min_leaf_n,
                                  M_n = NULL,
                                  group = NULL, group_value = 0, m_n_group = m_n,
                                  fit_time_limit = NULL, deadline = NULL,
                                  tol_iter = 12L, ...) {
  ```
  (All required arguments first, all defaulted arguments after. No call site changes, since all are named.)
- **Rationale:** the R convention -- required formals before defaulted ones -- exists precisely to make positional calls unambiguous. Restoring it costs nothing here.

---

### Issue 6: A half-specified `r_n`/`M_n` pair silently reverts to legacy behavior

- **File:** `R/stage_b.R:968`; `R/stage_b.R:1244-1250`
- **Category:** Functions / Error handling
- **Severity:** Medium
- **Current:**
  ```r
  use_rho <- !is.null(r_n) && !is.null(M_n)
  ```
- **The problem:** `refine_tree(model, X, y, r_n = 16)` -- `M_n` forgotten -- runs to completion with the theory's search interval **silently not applied**, returning a model that looks identical to one where it was. Same for `M_n` alone. The roxygen documents the rule ("Both must be supplied together to narrow the bracket -- either NULL leaves it structural-only", `stage_b.R:946-948`), but documenting a silent no-op is weaker than refusing it. This is the pattern `.claude/rules/r-code-conventions.md` §9's pipeline checklist targets ("No `%||%` / `is.null` substitution of NA for a value that should exist"), and the pattern this very file's `as_coordinate_tree()` rejects on principle: "silently refining against data that does not actually match what the model was fit on would invalidate every downstream number" (`stage_b.R:285-291`). Half-specifying a theory tuning pair is the same species of error.
- **Proposed fix:**
  ```r
  n_rho_given <- sum(!is.null(r_n), !is.null(M_n))
  if (n_rho_given == 1L) {
    cli::cli_abort(c(
      "refine_tree_cuts: {.arg r_n} and {.arg M_n} must be supplied TOGETHER or both left {.code NULL}.",
      "x" = "Got {.arg {if (is.null(r_n)) 'M_n' else 'r_n'}} only.",
      "i" = "One alone cannot define the theory's search radius rho_n = M_n/r_n; \\
             accepting it would silently fall back to the structural-bracket-only \\
             behavior while looking like the theory-guaranteed path."
    ))
  }
  use_rho <- n_rho_given == 2L
  ```
- **Rationale:** loud refusal over silent degradation, consistent with every other validation in this file.

---

### Issue 7: `M_n` is not validated up front in `fit_twostage()` -- an invalid value surfaces only after a full Stage-A fit

- **File:** `R/fit_twostage.R:648-693`
- **Category:** Error handling
- **Severity:** Medium
- **Current:** `fit_twostage()` validates `loss_function`, `X` type, `nrow(X)`/`length(y)`, `ncol(X)`, per-column numericity, the all-binary pre-flight, and `worker_limit` -- all **before** any fitting, with an explicit comment that the all-binary check exists "before any Stage-A time is spent" (`fit_twostage.R:679-682`). The new `M_n` argument bypasses that discipline entirely: `M_n = "four"` or `M_n = -1` is only caught inside `refine_tree_cuts()` (`stage_b.R:973-975`), i.e. **after** `bisect_lambda_to_budget()` has run to completion on rung 1 -- up to `fit_time_limit` (default 600s) per internal fit, times however many bisection iterations.
- **Proposed fix:** add alongside the other up-front checks:
  ```r
  if (!is.null(M_n) && (!is.numeric(M_n) || length(M_n) != 1L || is.na(M_n) || M_n <= 0)) {
    cli::cli_abort(c(
      "fit_twostage: {.arg M_n} must be {.code NULL} or a single positive number, got {.val {M_n}}.",
      "i" = "Validated here, before any Stage-A time is spent -- {.fn refine_tree_cuts} \\
             would otherwise only catch it after the first rung's full fit."
    ))
  }
  ```
- **Rationale:** the function already establishes "validate everything cheap before spending solver time" as its own contract; the new argument should honor it.

---

### Issue 8: Nothing warns when `lambda_n >> 1/r_n` is violated, though the test suite documents that violating it silently degrades results

- **File:** `R/fit_twostage.R:549-557`, `R/fit_twostage.R:648-693`
- **Category:** Domain / Error handling
- **Severity:** Medium
- **Current:** the roxygen states the requirement and assigns it to the caller:
  > "The revised theory's Stage-A topology-recovery guarantee requires `lambda_n >> 1/r_n` (the REVERSE of a grid-exact analysis' sandwich condition) -- this function does not enforce or select this itself; choosing `lambda_n` relative to `m_ladder` is the caller's responsibility"

  Meanwhile `test-fit-twostage.R:449-459` records, from direct empirical confirmation, exactly what happens when it is violated:
  > "`lambda_n = 0.05` is in the OLD, now-wrong regime (`lambda_n << 1/r_n`) and reproducibly causes exactly the over-splitting failure mode the theory predicts for that regime ... Stage A returns an extra 'transition leaf' ... so the local-optimality certificate correctly fails"

  So the package knows the condition, knows the failure mode, can compute `1/min(m_ladder)` cheaply from arguments it already has, and says nothing. The user gets `certified = FALSE, reason = "local_certificate"` -- a symptom several layers removed from the cause, with no hint that `lambda_n` is the dial.

  I verified the arithmetic in the test comment is right: with `m_ladder = c(8, 16)`, `1/r_n` is largest at the coarsest rung (`1/8 = 0.125`), so clearing `0.125` suffices for every rung. `lambda_n = 0.3` clears it by ~2.4x.

- **Proposed fix:** a purely diagnostic check after `.twostage_validate_ladder()` (no behavior change, no gating):
  ```r
  # Diagnostic only -- the theory's Theorem [Stage-A topology recovery] needs
  # lambda_n >> 1/r_n, and 1/r_n is LARGEST at the coarsest rung, so that
  # rung is the binding one. Not enforced (">>" has no finite-sample
  # threshold, and the paper's Discussion flags lambda selection under this
  # regime as open), but a caller in the OLD regime reproducibly gets
  # Stage-A over-splitting and a failing local certificate -- a symptom
  # several layers from its cause. Confirmed empirically 2026-09-04; see
  # test-fit-twostage.R's lambda_n comment.
  inv_r_coarsest <- 1 / min(ladder_spec$m_ladder)
  if (isTRUE(verbose) && lambda_n <= inv_r_coarsest) {
    cli::cli_warn(c(
      "fit_twostage: lambda_n = {lambda_n} does not exceed 1/r_n = {signif(inv_r_coarsest, 3)} at the coarsest rung (m = {min(ladder_spec$m_ladder)}).",
      "i" = "The revised theory requires lambda_n >> 1/r_n; below it, Stage A \\
             reproducibly over-splits around a grid-misaligned boundary and the \\
             local certificate fails for that reason, not a fit error.",
      "i" = "Raise {.arg lambda_n}, or drop the coarsest {.arg m_ladder} rung."
    ))
  }
  ```
- **Rationale:** "the caller's responsibility" is a defensible division of labor for *selecting* `lambda_n`; it is not a reason to withhold a one-line check that the caller's choice is in the right regime at all. This is the highest-value-per-line addition available in the change.

---

### Issue 9: `stage_b_refine_infeasible` can never become a `stop_reason`, so an all-refusing ladder reports `"ladder_exhausted"`

- **File:** `R/fit_twostage.R:760-768`, `R/fit_twostage.R:636-638`
- **Category:** Domain / Error handling
- **Severity:** Medium
- **Current:** the refine-infeasible branch always `next`s, so if **every** rung refuses this way, `stop_reason` falls through to `"ladder_exhausted"` (`fit_twostage.R:796`) and the user is told nothing about why. Contrast `stage_b_binary_split`, which gets a dedicated `stop_reason` after two strikes, and `stage_a_deadline`/`stage_a_truncated`, which get theirs immediately. The `@return` documentation for `stop_reason` (`fit_twostage.R:636-638`) lists six values and does not mention that a refine-infeasible ladder is invisible in that field.

  The escalate-rather-than-stop policy itself is well justified ("geometry-dependent ... may not recur at a different grid resolution") and I would not change it. The gap is purely in what the user is told at the end.
- **Proposed fix:**
  ```r
  # after the loop, before the is.na(stop_reason) fallback
  if (is.na(stop_reason)) {
    all_refine_infeasible <- length(ladder) > 0L &&
      all(vapply(ladder, function(r)
        identical(r$status, "stage_b_refine_infeasible"), logical(1)))
    if (all_refine_infeasible) stop_reason <- "stage_b_refine_infeasible"
  }
  if (is.na(stop_reason)) stop_reason <- "ladder_exhausted"
  ```
  and add `"stage_b_refine_infeasible"` to the documented `stop_reason` set, noting that a *mixed* ladder still reports `"ladder_exhausted"` and that per-rung detail lives in `$ladder_topologies`.
- **Rationale:** this function's print method is built around the principle that "a reader must never come away thinking the default cap represents a fit FAILURE" (`fit_twostage.R:884-887`). The same standard applies to a ladder that refused for a specific, nameable geometric reason and reported a generic exhaustion instead.

---

### Issue 10: Resetting `binary_split_streak` on a refine-infeasible rung defeats the documented "escalate exactly once" cap

- **File:** `R/fit_twostage.R:766`
- **Category:** Domain
- **Severity:** Medium
- **Current:**
  ```r
  if (identical(rec$status, "stage_b_refine_infeasible")) {
    ...
    binary_split_streak <- 0L
    next
  }
  ```
- **The problem:** the roxygen states the invariant plainly:
  > "A Stage-B binary-passthrough-split refusal escalates only ONCE, then stops -- if the solver keeps splitting on a binary column, a finer grid will not change that, and repeated escalation would be a guaranteed waste." (`fit_twostage.R:527-531`)

  Because the counter is *consecutive*, a ladder producing `binary_split, refine_infeasible, binary_split, refine_infeasible, ...` never reaches `streak >= 2` and walks the **entire** ladder attempting binary-split rungs -- precisely the guaranteed waste the cap exists to prevent. With the standard 4-rung default `m_ladder = c(16, 32, 64, 128)` this is a realistic interleaving, not a contrived one.

  Note the stated *rationale* for the binary-split cap is grid-**independent** ("a finer grid will not change that"), which means consecutiveness was never the right predicate for it -- a total count is. Whether this reset was inherited verbatim from the old `collapse_unsupported` handling does not change that.
- **Proposed fix:**
  ```r
  binary_split_seen <- 0L   # TOTAL, not consecutive
  ...
  if (identical(rec$status, "stage_b_refine_infeasible")) {
    # Geometry-dependent -- may not recur at a different grid resolution, so
    # escalate rather than stop. Deliberately does NOT reset
    # binary_split_seen: the binary-split cap's own rationale ("a finer grid
    # will not change that") is grid-independent, so an interleaved
    # refine-infeasible rung must not buy the ladder another binary-split
    # attempt.
    next
  }
  if (identical(rec$status, "stage_b_binary_split")) {
    binary_split_seen <- binary_split_seen + 1L
    if (binary_split_seen >= 2L) { stop_reason <- "stage_b_binary_split"; break }
    next
  }
  ```
  (Drop the `binary_split_streak <- 0L` at `fit_twostage.R:777` as well, and update the `@description` to say "at most twice in total.") Extend the existing test at `test-fit-twostage.R:511` with an interleaved-ladder case if a fixture producing one is available; otherwise a unit-level note suffices.
- **Rationale:** a cost guard whose bound can be defeated by interleaving is not a bound. Small change, restores the documented contract.

---

### Issue 11: Three other tests retain `lambda_n = 0.05` on the same DGP the new comment declares wrong

- **File:** `tests/testthat/test-fit-twostage.R:484`, `:647`, `:697` (and `:623`, `:680` at `leaf_budget = 2`)
- **Category:** Testing
- **Severity:** Medium
- **Current:** the change deliberately raises `lambda_n` from `0.05` to `0.3` at two sites (`:466`, `:597`) with a thorough, empirically-confirmed explanation that `0.05` is "in the OLD, now-wrong regime ... and reproducibly causes exactly the over-splitting failure mode the theory predicts." But the identical `(DGP, m_ladder = c(8, 16), leaf_budget = 4)` combination with `lambda_n = 0.05` remains at:
  - `:484` -- `verbose = FALSE` suppression test
  - `:647` -- the `leaf_budget > 8` print-framing test
  - `:697` -- the `predict()` delegation test

  These tests pass because none asserts `certified`. But they now knowingly exercise a wrong penalty regime, which has two costs. First, the test suite documents `lambda_n = 0.05` as ordinary usage in five places and wrong in two -- a reader cannot tell which is intended. Second, `:697` (predict delegation) and `:647` (print framing) now run against a Stage-A over-split tree with a failing local certificate; they are testing plumbing on top of a fit the revision considers pathological, which is not what they were written to test and makes them harder to reason about if they ever fail.
- **Proposed fix:** hoist the fixture and the regime-correct `lambda_n` into one shared helper so the regime lives in exactly one place:
  ```r
  # near the top of the fit_twostage() section
  # Shared one-boundary fixture. lambda_n = 0.3 is REGIME-CORRECT for
  # m_ladder = c(8, 16): the revised theory needs lambda_n >> 1/r_n and
  # 1/r_n = 1/8 = 0.125 at the coarsest rung. lambda_n = 0.05 is the OLD
  # regime and reproducibly over-splits on this DGP -- do not reintroduce
  # it here without the same deliberate, commented reason the two
  # regime-specific tests give.
  .ts_fixture <- function(n = 400, seed = 20260901) {
    set.seed(seed)
    X <- data.frame(x1 = runif(n), x2 = runif(n))
    list(X = X, y = ifelse(X$x1 <= 0.4, 0, 3) + rnorm(n, sd = 0.05),
         lambda_n = 0.3, m_ladder = c(8, 16))
  }
  ```
  and use it at `:484`, `:647`, `:697`. Where `leaf_budget = 2` with `depth_budget = 2` is genuinely insensitive to the regime, say so in one line rather than leaving the reader to infer it.
- **Rationale:** the change establishes that this DGP/grid combination has a right and a wrong `lambda_n`. Leaving the wrong one in three tests, uncommented, undermines the documentation value of having established it.

---

### Issue 12: `n_leaves_collapsed == n_leaves_A` is now a documented invariant but the test still asserts only `<=`

- **File:** `tests/testthat/test-fit-twostage.R:222`; contract at `R/fit_twostage.R:346-349`
- **Category:** Testing
- **Severity:** Medium
- **Current:**
  ```r
  expect_true(rec$n_leaves_collapsed <= rec$n_leaves_A)
  ```
  against a `@return` that now states equality:
  > "`n_leaves_collapsed` (the refined tree's leaf count -- Stage B never changes tree structure, only threshold values, so this equals `n_leaves_A`)"
- **The problem:** `<=` was the correct assertion under the old architecture, where `collapse_transitions()` could genuinely reduce the leaf count. With collapse off, `refine_tree_cuts()` only mutates `node$cut` (`stage_b.R:1120-1121`) and never alters structure, so equality holds exactly. The test is now weaker than the documented contract and would not catch a regression that reintroduced structural change during refinement -- which is precisely the invariant the whole "Stage B is threshold-only" architecture rests on.
- **Proposed fix:**
  ```r
  # Equality, not <=: with collapse off by default, Stage B mutates only
  # threshold VALUES, never tree structure, so the refined leaf count must
  # match Stage A's exactly. <= was correct only while collapse_transitions()
  # could genuinely merge leaves.
  expect_equal(rec$n_leaves_collapsed, rec$n_leaves_A)
  expect_equal(rec$n_collapses, 0L)   # documented as always 0 on this path
  ```
- **Rationale:** where a revision *strengthens* an invariant, the test should be strengthened with it, or the strengthening is unverified.

---

### Issue 13: `refine_tree()` is exported but documents neither classed condition it can raise

- **File:** `R/stage_b.R:1181-1246`
- **Category:** Functions / Comments
- **Severity:** Medium
- **Current:** `refine_tree()` is `@export`ed and is the documented public Stage-B entry point ("`doubletree` and other callers should use these, not reach into the coordinate-tree internals directly", `stage_b.R:1155-1163`). It can raise two **classed, catchable, ordinary-refusal** conditions:
  - `optimaltrees_stage_b_binary_split` (via `build_coord_node()`, pre-existing)
  - `optimaltrees_refine_infeasible` (via `refine_tree_cuts()`, **new in this change**)

  Its roxygen mentions neither. An external caller -- `doubletree` is the named consumer -- has no documented way to learn that these are refusals to be handled rather than bugs to be reported, and no documented class string to `tryCatch()` on. The information exists, but only in `.twostage_run_rung()`'s internal roxygen and in the internal functions' own docs, neither of which appears in `refine_tree`'s help page.
- **Proposed fix:** add to `refine_tree()`'s roxygen:
  ```r
  #' @section Classed refusal conditions:
  #' This function raises two ORDINARY-REFUSAL conditions (not bugs) that a
  #' caller orchestrating a search over grid resolutions should catch by
  #' CLASS, not by message text:
  #' \describe{
  #'   \item{`optimaltrees_stage_b_binary_split`}{Stage A split on a
  #'     binary-passthrough column; there is no continuous threshold to
  #'     refine. A different `discretize_bins` will NOT help -- the column
  #'     is binary at every resolution. Do not retry indefinitely.}
  #'   \item{`optimaltrees_refine_infeasible`}{An ancestor's off-grid
  #'     refinement shrank a descendant's row set until the descendant's own
  #'     grid split empties one side. Geometry-dependent, so a DIFFERENT
  #'     `discretize_bins` may well succeed -- retrying at another resolution
  #'     is reasonable. Reachable since the 2026-09-04 revision made
  #'     `collapse = FALSE` the default; see [refine_tree_cuts()].}
  #' }
  #' [fit_twostage()] catches both and reports them as per-rung
  #' `status` values (`"stage_b_binary_split"` /
  #' `"stage_b_refine_infeasible"`) rather than propagating them.
  ```
- **Rationale:** an exported function's error contract is part of its API. Adding a raisable condition class without documenting it on the exported surface leaves consumers to discover it from a stack trace.

---

### Issue 14: `NEWS.md` declares 0.4.1 but `DESCRIPTION` still says 0.4.0

- **File:** `DESCRIPTION:4` vs `NEWS.md:1`
- **Category:** Polish / release hygiene
- **Severity:** Medium
- **Current:**
  ```
  # DESCRIPTION
  Version: 0.4.0
  ```
  ```
  # NEWS.md (staged, new section)
  # optimaltrees 0.4.1 (2026-09-04)
  ```
- **The problem:** the staged change adds a `0.4.1` `NEWS.md` section without bumping `DESCRIPTION`. Any installed build reports `0.4.0` while its own `NEWS` claims `0.4.1`, `utils::news()` will not align sections to versions, and `packageVersion("optimaltrees")` cannot be used to tell whether a given install has the architecture revision -- which matters here, because the revision **changes default behavior** (`collapse` off, no budget inflation) in a way a downstream `doubletree` consumer may need to detect.
- **Proposed fix:** bump `DESCRIPTION` to `Version: 0.4.1` in the same commit (or `0.4.0.9000` if the maintainer prefers dev versions between releases, in which case retitle the `NEWS.md` section to match).
- **Rationale:** version and changelog must agree at every commit, not only at release; this one is a one-line fix caught by inspection.

---

### Issue 15: The print method still reports "transition leaves collapsed" -- a concept the revision removed

- **File:** `R/fit_twostage.R:918-920`
- **Category:** Domain / user-facing disclosure
- **Severity:** Medium
- **Current:**
  ```r
  cat(sprintf("  |- leaves              %s <= %s  (%s transition leaves collapsed; slack %s)\n",
              x$n_leaves_collapsed, x$leaf_budget,
              x$n_transition_leaves_collapsed, x$budget_slack))
  ```
- **The problem:** on the default pipeline this prints "0 transition leaves collapsed" unconditionally. "Transition leaf" is a concept from the retracted theory; the field is retained only for schema stability (correctly and explicitly documented as such at `fit_twostage.R:633-634`). But the **print method is this object's disclosure surface** -- its docstring says it exists so "a reader must never come away thinking the default cap represents a fit FAILURE" (`:884-887`) and it goes on to enumerate exactly what is and is not claimed. Surfacing a retracted mechanism's counter there invites a reader to believe collapse is still part of the pipeline and merely found nothing to do -- the opposite of the revision's message.
- **Proposed fix:**
  ```r
  cat(sprintf("  |- leaves              %s <= %s  (slack %s)\n",
              x$n_leaves_collapsed, x$leaf_budget, x$budget_slack))
  # Only surface the retired collapse counter when it is non-zero, i.e. only
  # for a legacy collapse = TRUE path -- printing "0 transition leaves
  # collapsed" on every default call implies a collapse step still runs.
  if (isTRUE(x$n_transition_leaves_collapsed > 0L)) {
    cat(sprintf("  |  (legacy collapse: %s transition leaf pair(s) merged)\n",
                x$n_transition_leaves_collapsed))
  }
  ```
  The existing print tests (`:590`, `:612`, `:636`) do not assert on this substring, so no test churn.
- **Rationale:** vacuous retained *fields* are a reasonable compatibility cost; vacuous retained *prose in the disclosure output* is not, because disclosure accuracy is the print method's only job.

---

### Issue 16: Stale module-level documentation in `stage_b.R` -- including a section banner that now states the opposite of the default

- **File:** `R/stage_b.R:17-22`, `:53-55`, `:430-435`, `:688-693`
- **Category:** Comments
- **Severity:** Low (but `:688-693` is the one to fix first)
- **Current:**
  ```r
  # :17-22
  #' ([coord_tree_predict()]). Threshold refinement itself
  #' ([refine_tree_cuts()], not yet implemented as of this file) and
  #' transition-leaf collapse (\code{collapse_transitions()}, not yet
  #' implemented) are separate, later pieces...

  # :53-55
  #' came from (length 1 for an ordinary split, length 2 for a collapsed
  #' transition-leaf pair, once \code{collapse_transitions()} exists).

  # :430-435
  # Tree-structure utilities shared by collapse_transitions() and (later)
  # refine_tree_cuts(). ...

  # :688-693
  # Off-grid threshold refinement (refine_tree_cuts()). Run AFTER
  # collapse_transitions() on the same tree -- refinement must never see a
  # still-uncollapsed transition pair, since collapse's detection relies on
  # grid-exact cuts (see collapse_transitions()'s roxygen).
  ```
- **The problem:** the first three are ordinary staleness ("not yet implemented", "once ... exists", "(later)") -- all three functions have existed for some time. `man/stage_b.Rd` is **not** among the seven regenerated `.Rd` files, so the "not yet implemented" text is live in the rendered help page.

  `:688-693` is worse than stale: it is now **actively contradicted by the default**. It instructs that refinement must be run *after* `collapse_transitions()` and "must never see a still-uncollapsed transition pair" -- which is exactly what the default path now does on every call. This is the same defect as Issue 2, in a plain comment rather than roxygen.
- **Proposed fix:**
  ```r
  # ---------------------------------------------------------------------------
  # Off-grid threshold refinement (refine_tree_cuts()).
  #
  # 2026-09-04: this step NO LONGER runs after collapse_transitions() by
  # default -- refine_tree()'s `collapse` defaults to FALSE, so refinement
  # routinely operates on a tree that still contains grid-adjacent
  # same-coordinate split pairs. That is intended, not an ordering bug: the
  # revised architecture has no collapse step (see collapse_transitions()'s
  # roxygen). The consequence -- an ancestor's off-grid move can empty a side
  # of a descendant's own grid split -- is detected and refused explicitly
  # via `optimaltrees_refine_infeasible`, not assumed away. Only the legacy
  # `collapse = TRUE` path still has the collapse-then-refine ordering the
  # original design required.
  # ---------------------------------------------------------------------------
  ```
  Update `:17-22` (drop both "not yet implemented" clauses), `:53-55` (drop "once ... exists"), `:430-435` (drop "(later)"), then re-run `devtools::document()` so `man/stage_b.Rd` regenerates alongside the seven already updated.
- **Rationale:** a section banner that states the inverse of the shipped default is worse than no banner. The rest is cheap cleanup while the file is already open.

---

### Issue 17: The new check duplicates row subsetting and candidate counting already done a few lines later

- **File:** `R/stage_b.R:1013-1020` vs `:1077-1096`
- **Category:** Polish / performance
- **Severity:** Low
- **Current:**
  ```r
  xj_incumbent <- X[[node$coord]][idx]              # :1014
  n_leq_incumbent <- sum(xj_incumbent <= node$cut)  # :1015
  ...
  Xn <- X[idx, , drop = FALSE]                      # :1077  (copies ALL columns)
  yn <- y_centered[idx]
  xj <- Xn[[node$coord]]                            # :1079  == xj_incumbent
  ...
  n_leq <- vapply(cands_raw, function(c) sum(xj <= c), integer(1))   # :1095
  n_gt  <- length(xj) - n_leq                                        # :1096
  ```
- **The problem:** `xj_incumbent` and `xj` are the same vector computed twice, and the `n_leq`/`n_gt` counting idiom appears twice with different names. Minor, but this is a per-split-node hot loop and the second computation routes through `X[idx, , drop = FALSE]`, which materializes every column. More importantly for review purposes, two names for one quantity makes the relationship between the incumbent check and the `min_leaf_n` floor harder to see than it needs to be -- and that relationship is exactly what a reader has to verify to trust the new logic.
- **Proposed fix:** hoist once and reuse (already folded into Issue 3's patch):
  ```r
  xj_all <- X[[node$coord]][idx]      # this node's coordinate values, once
  side_counts <- function(cut) {
    n_leq <- sum(xj_all <= cut)
    c(leq = n_leq, gt = length(xj_all) - n_leq)
  }
  ```
  and use `xj <- xj_all` in place of `Xn[[node$coord]]`. (`Xn` is still needed for `coord_tree_leaf_index()`, so that subset stays.)
- **Rationale:** conventions §2 -- "trade a little efficiency for major gains in readability/maintainability when reasonable"; here both improve at once.

---

### Issue 18: No `NA` guard -- an `NA` in a split coordinate turns the new check into an opaque base-R error

- **File:** `R/stage_b.R:1015`, `:1021`
- **Category:** Error handling
- **Severity:** Low
- **Current:**
  ```r
  n_leq_incumbent <- sum(xj_incumbent <= node$cut)
  ...
  if (n_leq_incumbent < 1L || n_gt_incumbent < 1L) {
  ```
- **The problem:** `sum()` over a comparison containing `NA` returns `NA_integer_`, so `n_leq_incumbent < 1L` is `NA` and the `if` fails with base R's `missing value where TRUE/FALSE needed` -- no class, no context, no indication that the cause is an `NA` covariate. `fit_twostage()` requires `X` to be numeric (`:676-678`) but never checks for `NA`, and nothing in the Stage-B path does either (`coord_tree_assign()` at `:234` would also silently misroute). This is pre-existing, but the new check is now the **first** place an `NA` bites, and it does so with the least informative possible message.
- **Proposed fix:** one assertion at `fit_twostage()`'s existing up-front block, plus one in `refine_tree_cuts()` for direct callers:
  ```r
  # fit_twostage(), with the other pre-flight checks
  na_cols <- names(X)[vapply(X, anyNA, logical(1))]
  if (length(na_cols) > 0L) {
    cli::cli_abort(c(
      "fit_twostage: {.arg X} must not contain missing values.",
      "x" = "Column(s) {.val {na_cols}} contain {.code NA}.",
      "i" = "Stage B routes rows by numeric comparison against each split's cut; \\
             an {.code NA} silently misroutes rather than erroring where it occurs."
    ))
  }
  if (anyNA(y)) cli::cli_abort("fit_twostage: {.arg y} must not contain missing values.")
  ```
- **Rationale:** conventions §9 -- assert inputs where they are read, so failures land at the cause rather than several frames downstream.

---

### Issue 19: `.twostage_validate_ladder()` now validates `n` and `m_n` but uses neither, and the roxygen does not say so

- **File:** `R/fit_twostage.R:196-228`
- **Category:** Functions / Comments
- **Severity:** Low
- **Current:** after de-pruning, `n` and `m_n` are validated (`:218-225`) and then discarded -- nothing downstream reads them. The roxygen carefully explains that `dropped_rungs`/`m_max_supported` are "kept in the return value, always vacuous, for call-site stability" but says nothing equivalent about the two now-vestigial *parameters*, which still read as load-bearing inputs.

  Retaining them for call-site stability is a legitimate choice (`fit_twostage.R:701` still passes both). It just needs saying, or the next reader will hunt for the use.
- **Proposed fix:**
  ```r
  #' @param n Single positive integer/numeric, `nrow(X)`. \strong{Validated
  #'   but no longer used} (2026-09-04): it fed the removed rate-condition
  #'   pruning. Retained in the signature for call-site stability.
  #' @param m_n Single positive integer/numeric, the total per-leaf floor.
  #'   \strong{Validated but no longer used}, same reason as `n`.
  ```
  Also consider moving the `n`/`m_n` validation *above* the ladder checks so a caller passing a bad `n` alongside a bad ladder hears about both in a predictable order -- currently the ladder errors fire first (`:203-216`).
- **Rationale:** a validated-but-unused parameter is a maintenance trap unless labelled; one line of roxygen removes it.

---

### Issue 20: `r_n = m` conflates bin count with threshold count, unstated

- **File:** `R/fit_twostage.R:425`, `R/fit_twostage.R:558-560`
- **Category:** Domain / Comments
- **Severity:** Low
- **Current:** `refine_tree(..., r_n = m, ...)` and the roxygen "`m_ladder` Numeric vector of grid resolutions (`discretize_bins`, the theory's `r_n`)".
- **The problem:** `discretize_bins = m` produces `m - 1` thresholds, not `m` (`R/discretize.R:10`: "Number of bins for quantile discretization (creates `n_bins-1` thresholds)"), and `compute_thresholds()` additionally applies `unique()` (`discretize.R:503`), so the realized cutpoint count can be **fewer** still on a coordinate with ties. Identifying the theory's `r_n` with `m` is asymptotically immaterial (the `(m-1)/m` factor tends to 1), but it is a stated code-paper correspondence and it is off by one, unremarked -- and at the small `m` values the ladder actually starts from (`m = 8` in tests, `m = 16` by default) `1/8` vs `1/7` is a 14% difference in a quantity Issue 8's regime check would compare against.
- **Proposed fix:** one line at the call site:
  ```r
  # r_n := m, the theory's grid resolution. discretize_bins = m yields m - 1
  # thresholds (and fewer after unique() on a tied coordinate), so this is an
  # off-by-one identification -- immaterial asymptotically ((m-1)/m -> 1), but
  # worth naming rather than leaving as an implicit equivalence at the small m
  # the ladder starts from.
  refined <- tryCatch(refine_tree(..., r_n = m, M_n = M_n_resolved), ...)
  ```
- **Rationale:** `.claude/rules/code-paper-package-alignment.md` -- named correspondences between paper symbols and code variables should be exact or explicitly approximate.

---

### Issue 21: `.twostage_run_rung()`'s roxygen omits its own `M_n = NULL -> sqrt(m)` default

- **File:** `R/fit_twostage.R:327-328`
- **Category:** Comments
- **Severity:** Low
- **Current:**
  ```r
  #' @param M_n Numeric, Stage-B's interval-scale tuning constant (see
  #'   [fit_twostage()]'s own `M_n` argument for the package default and its
  #'   rationale).
  ```
- **The problem:** the formal is `M_n = NULL` and the function itself resolves the default (`:422`, `M_n_resolved <- if (is.null(M_n)) sqrt(m) else M_n`). The inline comment at `:418-421` documents this ("`M_n` defaults to `sqrt(m)` when not supplied ... resolved here too so direct callers of this internal function need not pass it"), but the `@param` -- the part that renders into `man/dot-twostage_run_rung.Rd` -- defers entirely to another function. A direct caller reading the help page cannot tell that `NULL` is accepted, let alone what it resolves to.
- **Proposed fix:**
  ```r
  #' @param M_n Numeric, or `NULL` (default), Stage-B's interval-scale tuning
  #'   constant. `NULL` resolves HERE to `sqrt(m)`, mirroring
  #'   [fit_twostage()]'s own default so direct callers of this internal
  #'   function need not pass it; see that function's `M_n` argument for the
  #'   rationale and the theory's `M_n -> Inf`, `M_n/r_n -> 0` requirements.
  ```
- **Rationale:** the `@param` is the rendered contract; an inline comment two dozen lines away is not a substitute.

---

### Issue 22: `fit_twostage()` never discloses when Stage B refined nothing

- **File:** `R/fit_twostage.R:900-996`
- **Category:** Domain / user-facing disclosure
- **Severity:** Low
- **Current:** `print.RefinedTreeModel` reports "splits refined off-grid: N" (`stage_b.R:1356-1359`), but `print.optimaltrees_twostage_fit` -- the object a `fit_twostage()` user actually holds -- reports no such count. A fit where **every** node logged `no_candidates_in_bracket` (the exact silent outcome Issue 1's small-`rho_n` regime produces) prints as fully `certified` with no indication that Stage B was a no-op and the returned thresholds are still grid values.
- **Proposed fix:**
  ```r
  # after the local-certificate line
  n_off_grid <- if (!is.null(x$model@refinement_log)) {
    sum(x$model@refinement_log$refined, na.rm = TRUE)
  } else NA_integer_
  cat(sprintf("  |- stage B             %s of %s split(s) moved off-grid\n",
              n_off_grid, nrow(x$model@refinement_log)))
  if (isTRUE(n_off_grid == 0L)) {
    cat("  |  NOTE: no threshold moved -- every cut is still its Stage-A GRID value.\n")
    cat("  |  Check $model@refinement_log$reason (commonly no_candidates_in_bracket,\n")
    cat("  |  which means the M_n/r_n radius is small relative to your covariate scale).\n")
  }
  ```
- **Rationale:** Stage B's entire value proposition is removing the grid floor. A fit where it did nothing should say so, especially since the mechanism that silently causes it (Issue 1) is scale-dependent and unguarded.

---

### Issue 23: Lines exceeding 100 characters in new and changed code

- **File:** `R/fit_twostage.R:115`, `:164-166`, `:204`, `:214`, `:338-339`, `:663`; `tests/testthat/test-fit-twostage.R:203`, `:253`, `:275`, `:414`, `:441`
- **Category:** Polish
- **Severity:** Low
- **Current:** e.g.
  ```r
  # fit_twostage.R:115 -- ~130 chars
  "fit_twostage: {.arg depth_budget} must be {.code NULL}, {.val full}, or a single positive integer, got {.val {depth_budget}}."
  # fit_twostage.R:164 -- ~230 chars
  "!" = "fit_twostage: depth_budget = NULL defaults to the STAGED cap d_0 = ceiling(log2({res$leaf_budget})) = {res$d_0}, giving Stage-A search depth d_A = {res$d_A} (leaf budget L_A = {res$L_A}).",
  # test-fit-twostage.R:275 -- ~145 chars
  test_that(".twostage_run_rung() reports stage_b_refine_infeasible on a fixture that used to hit collapse_unsupported (collapse is off by default)", {
  ```
- **The problem:** conventions §7 sets a 100-character limit with a **mathematical-formula-only** exception ("Breaking the line would harm readability of the math"). None of these is math; they are `cli` strings and `test_that()` titles. Several are pre-existing, but `:338-339` and the three long test titles are new or touched by this change. `cli` handles wrapping at display time, so the source lines can be broken freely with `\\` continuations, exactly as this file already does elsewhere (e.g. `stage_b.R:1023-1035`).
- **Proposed fix:**
  ```r
  cli::cli_abort(
    "fit_twostage: {.arg depth_budget} must be {.code NULL}, {.val full}, or a \\
     single positive integer, got {.val {depth_budget}}."
  )
  ```
  and shorten the long test titles, moving detail into the body comment where it already lives:
  ```r
  test_that(".twostage_run_rung() reports stage_b_refine_infeasible with collapse off", {
  ```
- **Rationale:** the file already demonstrates the `\\`-continuation style in `stage_b.R`; applying it consistently costs nothing. Per §7 the penalty is minor, hence Low.

---

### Issue 24: Stray double blank line

- **File:** `tests/testthat/test-fit-twostage.R:311-312`
- **Category:** Polish
- **Severity:** Low
- **Current:** two consecutive blank lines between the end of the new `stage_b_refine_infeasible` test and the `stage_b_binary_split` test that follows; every other inter-test gap in the file is a single blank line.
- **Proposed fix:** delete line 312.
- **Rationale:** trivial, but it is the visible residue of the edit and worth removing before commit.

---

## Parked reference scripts (`dev-scripts/superseded/`) -- light-touch review

**Preliminary: the parking itself is correctly executed.** `.Rbuildignore` contains `^dev-scripts$`, so neither file ships in a built package; `testthat` collects only from `tests/testthat/`, so the `test-`-prefixed file cannot be picked up by `devtools::test()`; and `README.md` gives a genuinely good account of *why* the work is parked (the sandwich condition `nu_n << lambda_n << mu_n` was replaced by the one-sided `lambda_n >> 1/r_n`) and *when* to revisit it, including the insight that "under a one-sided condition the story may turn out to be simpler than plateau detection." That is the right disposition -- parked rather than adapted on a guess -- and it is consistent with `.claude/rules/quality-philosophy.md`. Code quality in the parked script is also good: preallocated `n_leaves` (no growing object), `cli` conditions throughout, thorough roxygen with provenance.

### Issue 25: `@export` on a file outside `R/`

- **File:** `dev-scripts/superseded/select_lambda_plateau.R:94`
- **Category:** Polish
- **Severity:** Low
- **Proposed fix:** change `#' @export` to `#' @keywords internal` with a one-line note, or add `# NOTE: parked -- this @export tag is inert here (roxygen only scans R/). It would take effect if this file were ever moved back into R/.` Prevents an accidental export if the file is revived by moving it.

### Issue 26: `split(seq_along(...), run_id)` groups in lexicographic, not grid, order

- **File:** `dev-scripts/superseded/select_lambda_plateau.R:144-153`
- **Category:** Polish / correctness-adjacent
- **Severity:** Low
- **Current:**
  ```r
  run_id <- cumsum(c(TRUE, diff(n_leaves) != 0L))
  plateaus <- do.call(rbind, lapply(split(seq_along(lambda_grid), run_id), function(idx) { ... }))
  ```
- **The problem:** the run-length grouping itself is correct. But `split()` coerces the numeric `run_id` to character and orders groups **lexicographically**, so with 10+ runs the `rbind`ed rows come back in order `1, 10, 11, 2, 3, ...` rather than ascending grid order. Currently benign -- `plateaus` is immediately reordered by `-log_width` (`:154`) and each row's fields are computed from its own `idx`, so no value is wrong -- but it is a latent trap for anyone who later relies on grid ordering (e.g. adding a neighbour-based width correction, which the comment at `:139-143` explicitly anticipates as a known understatement).
- **Proposed fix:**
  ```r
  plateaus <- purrr::map(
    split(seq_along(lambda_grid), factor(run_id, levels = unique(run_id))),
    function(idx) tibble::tibble(...)
  ) |> purrr::list_rbind()
  ```
  or minimally wrap `run_id` in `factor(run_id, levels = unique(run_id))`.

### Issue 27: A `n_leaves_selected == 1` plateau looks maximally robust

- **File:** `dev-scripts/superseded/select_lambda_plateau.R:157-169`
- **Category:** Domain
- **Severity:** Low
- **The problem:** if the whole grid sits above the useful range, every point returns a root-only tree, the single run spans the full grid, and the function reports the **widest possible** plateau with `n_leaves_selected = 1` -- maximal apparent robustness for a degenerate answer. The existing warning covers only the opposite pathology (all-singleton plateaus, `:164-169`).
- **Proposed fix:**
  ```r
  if (isTRUE(winner$n_leaves <= 1L)) {
    cli::cli_warn(c(
      "select_lambda_plateau: the widest plateau is a ROOT-ONLY tree (n_leaves = {winner$n_leaves}).",
      "i" = "A degenerate plateau spanning the whole grid indicates every candidate \\
             lambda is above the useful range, not that lambda selection is robust.",
      "i" = "Lower {.arg lambda_max}, or extend the grid below log(n)/n."
    ))
  }
  ```

### Issue 28: No run-time estimate for a 15-fit unconstrained sweep

- **File:** `dev-scripts/superseded/select_lambda_plateau.R:95-134`
- **Category:** Polish / conventions §2
- **Severity:** Low
- **The problem:** the default sweep runs `n_grid = 15L` full `fit_tree()` calls at `max_depth = 0L` (**unlimited** depth). Conventions §2 ("Long computations") require an approximate run-time note for anything expected to exceed ~1 minute, and this plausibly runs for minutes at moderate `n`/`p`. `verbose` exists but defaults to `FALSE`, so the default experience is a silent multi-minute block with no progress signal.
- **Proposed fix:** add to the roxygen -- `#' @section Run time: 15 unconstrained (max_depth = 0L) fit_tree() calls; expect minutes, not seconds, at moderate n/p. Pass verbose = TRUE for per-grid-point progress, or use cli::cli_progress_along() over the sweep.` -- and consider `cli::cli_progress_along(lambda_grid)` in place of the bare `for`.

### Issue 29: The parked test file's `fit_twostage()` assertions are now expected to fail, and the README does not say so

- **File:** `dev-scripts/superseded/test-select-lambda-plateau.R:59-62`; `dev-scripts/superseded/README.md`
- **Category:** Comments
- **Severity:** Low
- **Current:**
  ```r
  fit <- fit_twostage(X, y, leaf_budget = 4L, m_ladder = c(10, 20, 40),
    lambda_n = res$lambda_selected, min_leaf_n = 1L, verbose = FALSE)
  expect_equal(fit$n_leaves_collapsed, 4L)
  expect_equal(nrow(fit$model@refinement_log), 3L)
  ```
- **The problem:** this asserts current `fit_twostage()` behavior at a `lambda_n` the *old* plateau selector chose -- roughly `3e-3` on that grid, which is far **below** `1/r_n = 1/10 = 0.1` at the coarsest rung. That is exactly the wrong regime the revision identified (Issue 11, and `test-fit-twostage.R:449-459`), so this assertion is now expected to fail. The `README.md` explains why the *selector* is superseded but gives no warning that the accompanying test is not runnable green against the current package -- so a future reader who revives the file, runs it, and sees a failure may reasonably suspect a regression in `fit_twostage()` rather than the intended obsolescence.
- **Proposed fix:** append to `README.md`:
  ```markdown
  **Note on the accompanying test file.** `test-select-lambda-plateau.R` is
  parked in the same state it was written, and is NOT expected to pass against
  the post-2026-09-04 package. In particular its `fit_twostage()` end-to-end
  assertion (lines 59-62) drives the fit at the plateau-selected
  `lambda_n ~ 3e-3`, which is far BELOW `1/r_n = 1/10` at that call's coarsest
  `m_ladder` rung -- i.e. squarely in the old, now-wrong penalty regime (see
  `test-fit-twostage.R`'s own `lambda_n` comment). A failure there is the
  expected consequence of the architecture revision, not a regression in
  `fit_twostage()`.
  ```
- **Rationale:** parked code should record what is expected to break, not only why it was parked.

---

## What was verified and is correct

Recorded so a future reader knows these were checked rather than skipped.

- **The de-inflation is arithmetically consistent end to end.** `L_A = max(min(leaf_budget, A_m), 1L)`, `depth_required_A = L_A - 1`, `depth_reachable_A = ceiling(log2(L_A))`, `d_A = min(d_0, depth_required_A)` then clamped up to `depth_reachable_A`. I recomputed every expectation in the six recomputed budget tests (`test-fit-twostage.R:15-87`) by hand and all six match, including the two load-bearing clamps (`leaf_budget = 1` never yielding `max_depth = 0L`; `leaf_budget = 20, depth_budget = 1` lifting `d_A` to 5).
- **The new emptiness check is sound.** Full derivation in Issue 3. The depth-sorted traversal (`order()` is stable radix on integers; parent depth is strictly less than child depth) makes the induction valid, and no empty leaf can reach `attach_final_stats()`.
- **The check's narrowness is correctly calibrated.** Testing `>= 1` rather than `>= min_leaf_n` is right, and the cited justification (`test-stage-b.R:478`'s legitimate `too_few_rows_at_node` / `min_leaf_n_infeasible` distinction) genuinely supports it. Using the full floor here would have converted a documented non-error into a hard abort.
- **Same-depth nodes cannot interfere.** Two nodes at equal depth lie in disjoint subtrees, so refining one never perturbs the other's row set -- the traversal order is safe beyond just parent-before-child.
- **The `rho_n` intersection preserves the never-worse-than-grid guarantee.** `bracket_candidates()` re-adds `node$grid_cuts` when strictly inside the bracket; since `anchor` is the grid cut and `rho_n > 0`, `anchor` always satisfies `> anchor - rho_n` and `< anchor + rho_n`, so the incumbent survives as a candidate and `refine_tree_cuts()` still cannot score worse than its starting point at an ordinary node.
- **The `rho_n` intersection only ever narrows.** `max(br$lo, ...)` / `min(br$hi, ...)` guarantee it, so the new radius cannot *increase* the frequency of ancestor-induced descendant emptying relative to the legacy structural-only bracket. My initial hypothesis to the contrary was wrong on re-derivation; the risk runs the other way (Issue 22).
- **The `collapsed`-node anchor fallback is the right choice.** `node$grid_cuts` has length 2 on a collapsed node with no single Stage-A threshold to anchor on; falling back to `node$cut` (the midpoint) and documenting that the theory's `I_n(N)` is defined for the uncollapsed case is honest, and the path is only reachable via the opt-in legacy `collapse = TRUE`.
- **The classed-condition plumbing is correct.** `cli_abort(..., class = "optimaltrees_refine_infeasible")` forwards `class` to `rlang::abort()`, matching the two existing precedents in the file (`stage_b.R:200`, `:604`); `.twostage_run_rung()` catches by class rather than message (`fit_twostage.R:429-431`), so an unrelated real bug still propagates raw. This is exactly right, and the `tryCatch` handlers return sentinel-classed lists rather than swallowing, so the distinction survives.
- **`n_collapses` remains truthful.** `sum(refined@refinement_log$collapsed, na.rm = TRUE)` is 0 on the default path because `refine_tree_cuts()`'s `base_row()` writes `collapsed = isTRUE(node$collapsed)` and nothing sets it without `collapse_transitions()`. The documented "always 0" holds; only its appearance in the print method is a problem (Issue 15).
- **Refused rungs still appear in the ladder trace.** `ladder[[...]] <- rec` at `fit_twostage.R:753` precedes every status branch, so `stage_b_refine_infeasible` rungs are recorded in `$ladder_topologies` with their Stage-A fields populated and Stage-B fields `NA` -- and `test-fit-twostage.R:304-309` asserts exactly that (`n_leaves_A`/`n_fits` non-`NA`, `n_leaves_collapsed`/`local_certified` `NA`), which is the right shape of assertion.
- **The `lambda_n` 0.05 -> 0.3 change is justified and the arithmetic checks out.** `1/r_n` is largest at the coarsest rung (`1/8 = 0.125`), so clearing it covers the whole ladder; `0.3` clears it by ~2.4x. The comment's claim of direct empirical confirmation (running both values and inspecting Stage A's leaf structure) is the right standard of evidence, and the `certified = TRUE` expectation at `:470` is consistent (2 leaves <= `leaf_budget = 4`, no bisection needed, so `lambda_case = "ii"`).
- **Release hygiene is largely right.** `NEWS.md` gained a substantive, accurate 0.4.1 section covering every user-facing change, and all seven affected `man/*.Rd` files were regenerated in the same staged change (`collapse_transitions`, `dot-twostage_resolve_budgets`, `dot-twostage_run_rung`, `dot-twostage_validate_ladder`, `fit_twostage`, `refine_tree`, `refine_tree_cuts`). Only `man/stage_b.Rd` is missing, for the pre-existing staleness in Issue 16, and `DESCRIPTION` needs the matching bump (Issue 14).
- **Reproducibility conventions are met in the tests.** Every fitting test calls `set.seed()` once at the top with the `YYYYMMDD` convention (`20260901`), no `require()` anywhere, no absolute paths, no `set.seed()` inside a loop or function.
- **Console-output hygiene is clean.** No `cat()`/`print()`/`sprintf()` used for status or progress in `R/`; the only `cat()` calls are inside `print`/`summary` methods, where they belong. `cli_inform`/`cli_warn`/`cli_abort` used throughout, with `verbose` gating on the one interactive nudge and de-duplicated high-dimensionality warnings at most once per rung (`fit_twostage.R:740-748`).
- **No `sapply()` in the changed code.** `vapply()` with explicit `FUN.VALUE` throughout (`fit_twostage.R:676`, `:683`, `:799`, `:802`, `:822`, `:840`; `stage_b.R:984`, `:1095`), matching conventions §6's type-stability pitfall.
- **No growing-object anti-pattern.** `log_rows[[length(log_rows) + 1L]]` and `ladder[[length(ladder) + 1L]]` are list-append (amortized), not `c()`-vector growth, and both are `rbind`ed once at the end.
- **`on.exit()` resets the process-global solver config unconditionally** (`fit_twostage.R:707-710`), with an accurate comment about why (otherwise every later `fit_tree()` in the session, `doubletree` included, is silently capped). This is the parallel-backend-hygiene analogue and it is handled correctly.
- **The parked files are correctly isolated.** `.Rbuildignore`'s `^dev-scripts$` excludes the directory from `R CMD build`, and the `test-`-prefixed file lives outside `tests/testthat/` so `devtools::test()` cannot collect it.

---

## Checklist Summary

| Category | Pass | Issues |
|----------|------|--------|
| Structure & Header | Yes | 0 |
| Console Output | Yes | 0 |
| Reproducibility | Yes | 0 |
| Functions | No | 5 (5, 6, 7, 17, 21) |
| Domain Correctness | No | 8 (1, 2, 3, 8, 9, 10, 15, 20, 27 -- 1 is Critical) |
| Figures | n/a | 0 (no figure code in scope) |
| Serialization | n/a | 0 (no I/O in scope) |
| Comments | No | 4 (2, 16, 19, 29) |
| Error Handling | No | 4 (6, 7, 18, 9) |
| Polish | No | 6 (14, 23, 24, 25, 26, 28) |
| Testing | No | 3 (4, 11, 12) |

Categories with issues counted where the issue's primary category falls; several issues appear in more than one row because they span categories.

---

## Recommended commit gate

**Before commit (correctness and honesty of claims):**
1. Issue 1 -- document + assert the `rho_n` scale assumption, or make the radius mesh-relative. The roxygen currently claims a finite-sample theory guarantee the code does not deliver off unit scale.
2. Issue 2 -- rewrite the monotone-safety paragraph whose premise the revision removed. Cheapest high-value fix in the change.
3. Issue 3 -- reorder the emptiness check so it guards the keep-incumbent paths rather than pre-empting refinement.
4. Issue 4 -- add unit coverage for `optimaltrees_refine_infeasible` (by class) and for the `r_n`/`M_n` block, which currently has none.

**Before PR:** Issues 5-15 (signature ordering, XOR validation, up-front `M_n` check, the `lambda_n >> 1/r_n` diagnostic, `stop_reason` completeness, the streak-reset cap, test `lambda_n` consistency, the strengthened equality assertion, the exported error contract, the `DESCRIPTION` bump, and the print method's retired-concept text).

**Opportunistic:** Issues 16-29 while the files are open; the parked-script items (25-29) are genuinely optional given the files do not ship.

**Not requested, offered anyway:** the `lambda_n >> 1/r_n` diagnostic (Issue 8) is the single highest-value-per-line addition available here. The revision established, from direct empirical confirmation, that violating this condition silently produces a wrong Stage-A topology and a failing certificate several layers from the cause. The check is three lines and needs no information the function does not already have.

---

## Addendum: fixes applied in response to this review

All four commit-gate items (Issues 1-4) were fixed, plus the highest-value Medium items reachable without further design work. Applied by the author of the staged change, not by this reviewer; recorded here for provenance rather than re-reviewed from scratch.

- **Issue 1 (Critical, scale-dependence):** implemented option (a) as recommended -- `fit_twostage()` now warns (`verbose`-gated) when any covariate's range falls outside `[0.1, 10]`, naming the affected coordinate(s) and explaining that `rho_n` is computed in raw covariate units. Option (b) (mesh-relative radius) was deliberately not adopted -- it is a behavior change needing reconciliation with `inst/paper/main.tex`'s own normalization convention first, exactly as this review flagged.
- **Issue 2 (High, contradictory roxygen):** `refine_tree_cuts()`'s monotone-safety paragraph rewritten essentially verbatim to the reviewer's proposed text -- states plainly that the pre-revision premise (collapse ran first) no longer holds by default, and that the refusal, not a second pass, is the sanctioned outcome.
- **Issue 3 (High, over-firing check):** reordered exactly as proposed -- the incumbent-safety fact is computed once per node, up front, but only ACTED ON (raising `optimaltrees_refine_infeasible`) at the three points where the loop would otherwise keep a now-unsafe incumbent unchanged (`too_few_rows_at_node`, `no_candidates_in_bracket`, `min_leaf_n_infeasible`). Soundness argument unchanged; over-firing on a recoverable node is eliminated. This also folded in Issue 17's deduplication (`xj_all` computed once, reused as `xj`).
- **Issue 4 (High, no test coverage):** added four tests to `test-stage-b.R`: the classed condition asserted by `expect_s3_class` on a real fit's chained-split geometry (through both `refine_tree_cuts()` directly and the exported `refine_tree()`), `r_n`/`M_n` validation including the XOR case, the bracket-narrowing behavior (numerically verified independently before adding -- `wide` recovers a true boundary at 0.12 to within 0.0045; `tight` with `r_n=100, M_n=2` is confined to exactly `[0.48, 0.52]` and cannot reach it), and `refine_tree(collapse = TRUE)`'s legacy path.
- **Issue 5 (Medium, arg ordering):** `.twostage_run_rung()`'s formals reordered to required-before-defaulted; no call-site changes needed (all call sites already use named arguments).
- **Issue 6 (Medium, silent half-specification):** `r_n`/`M_n` now XOR-validated -- supplying exactly one aborts with an explicit "must be supplied TOGETHER" message, per the proposed patch.
- **Issue 7 (Medium, no up-front `M_n` check):** added to `fit_twostage()`'s existing pre-flight block, before any Stage-A time is spent.
- **Issue 10 (Medium, streak-reset bug):** `stage_b_refine_infeasible` no longer resets `binary_split_streak`; only a genuine `"ok"` rung does. Restores the documented "escalate at most twice" contract under interleaving.
- **Issue 12 (Medium, weaker-than-documented test):** strengthened `<=` to `expect_equal` plus an explicit `n_collapses == 0` assertion.
- **Issue 13 (Medium, undocumented exported error contract):** added the proposed `@section Classed refusal conditions:` to `refine_tree()`'s roxygen.
- **Issue 14 (Medium, version/changelog mismatch):** `DESCRIPTION` bumped to `0.4.1` to match `NEWS.md`.
- **Issue 21 (Low, incomplete `@param` for `.twostage_run_rung()`'s own `M_n` default):** fixed in the same pass as Issue 5, since the function was already open.

**Deliberately not fixed this round** (documented here rather than silently dropped): Issues 8, 9, 11, 15-20, 22-29. None is a correctness blocker; several (8, the `lambda_n >> 1/r_n` diagnostic; 9, `stop_reason` completeness; 11, the three remaining `lambda_n = 0.05` call sites; 15, the print method's retired "transition leaves collapsed" text; 16, stale module banners in `stage_b.R`) are genuinely good next-session value and are called out explicitly so they are not lost.

**Full test suite after fixes:** `FAIL 0 | WARN 2 (pre-existing, unrelated -- test-persistence.R future/Rashomon round-trip) | SKIP 24 | PASS 2115` (up from 2095 pre-fix, reflecting the new coverage added for Issue 4).
