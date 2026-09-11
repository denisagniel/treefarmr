# Full Package Audit: optimaltrees — correctness, silent fallbacks, dead code, defaults, efficiency

**Date:** 2026-09-10
**Requested by:** package maintainer (Denis Agniel)
**Scope:** whole-package audit, four parallel workstreams + direct orchestrator review:
1. Code-theory alignment / correctness of the optimal-tree and Rashomon-set machinery (primary goal)
2. Silent-fallback anti-patterns (forbidden per project convention)
3. Dead code / duplicated logic
4. Default-value consistency and provenance (theory-derived vs. empirical vs. arbitrary)
5. Efficiency (direct orchestrator review, no delegation)

**Method:** a `domain-reviewer` agent pass that read `inst/paper/main.tex`'s relevant sections in full and re-derived several of the paper's own claims against the R implementation (17 min); three parallel `explore` sweeps (silent fallbacks, dead code, defaults); direct spot-checks by the orchestrator of the most consequential automated claims, which surfaced two false positives — **automated severity tags in §3 are not independently re-verified and should be triaged individually before acting**, unlike the correctness findings in §1, most of which carry an independent re-derivation.

**Status:** audit only. No files were changed as part of this report.

**IMPORTANT EPISTEMIC CAVEAT (added after initial review, before any fixes were started):** `inst/paper/main.tex` is **not treated as ground truth** by this audit, and should not be treated as such by a reader either. The paper's own headline result was already revised once (2026-09-04) after an exclusion argument was found to fail generically -- so "the code doesn't match what `main.tex` claims" is not the same statement as "the code is wrong," and a finding of the form "code violates Assumption/Proposition X in `main.tex`" is only as strong as `main.tex`'s own claim X. Section 1 below is being re-triaged (see the outside audit noted in the Appendix) to separate:
- **Paper-independent findings** -- internal software bugs/inconsistencies (e.g. two sibling functions silently using different objective values; a package's own docs contradicting its own implementation; an n-independent constant that cannot be `o(n^{-1/2})` under any theory) that stand regardless of whether `main.tex`'s specific theorems are correct.
- **Paper-dependent findings** -- claims that the code fails to satisfy a specific assumption or rate condition stated in `main.tex`. These remain useful as *documentation-accuracy* findings (does the code's own roxygen/NEWS correctly represent what `main.tex` says, even if `main.tex` itself might later be revised again?) but should not be read as proof the underlying estimator is statistically unsound until `main.tex`'s own claims are independently corroborated (e.g. against general M-estimation / quantile-grid / empirical-process results, not just against this one unpublished/evolving paper).

This distinction is being applied by a second, independent review pass; §1's per-finding severities below predate that re-triage and should be read with this caveat in mind until the addendum is appended.

**Context supplied by the maintainer, weighted into prioritization below:**
- The primary use case is **regression** (`squared_error`), not classification.
- A **native leaf budget** (rather than the current lambda-bisection approximation) would better match how the papers are currently written; flagged as a design question for a future dedicated session, not resolved here.

---

## 0. Executive summary

Overall verdict on correctness: **CRITICAL ERRORS** — 33 issues found (5 Critical, 8 High documented in full, 12 Medium, 7 Low). Several are unearned theoretical claims or silent objective/statistical corruption, not polish items.

**The single biggest structural problem:** `inst/paper/main.tex` (the package's own theory paper) contains **no Rashomon theory at all** — zero occurrences of `epsilon_n`/`\varepsilon`, no Rashomon section, two incidental mentions only. Every "theory-justified" / "valid-inference" claim attached to `select_epsilon_n()`, `cross_fitted_rashomon()`, and the auto-tuners cites an absent companion document ("the doubletree manuscript"). Where the claim is independently checkable, it does not hold (C4).

Because the primary use case is regression, the highest-priority findings are the ones in the loss-agnostic Stage-A/Stage-B "two-stage" architecture (`fit_twostage.R`, `stage_b.R`, `bisect_lambda_to_budget.R`, `local_optimality_certificate.R`), which is specifically the estimator built for regression-tree topology recovery, and the Rashomon-set machinery (`rashomon.R`, `cross_fitted_rashomon.R`), which matters equally for regression Rashomon-DML.

---

## 1. Correctness — optimal trees and Rashomon sets

### 1.1 Coverage manifest (what was actually read, not skimmed)

**Theory read in full:** `inst/paper/main.tex` lines 1–942 (abstract; setup/algorithm incl. Algorithm 1, Definition [Stage-B search interval], Example [offgrid-1d]; Assumptions A1–A8, Definition [sufficient class]); 943–1642 (Assumption [jump-regularity], [local-design], [global-density], uniqueness section, Assumption [solver-twostage], Remark [not-assumed]); 2004–2093 (rate conditions (R1)–(R5), Lemma [snapped-oracle]); 2280–2609 (Remark [uep-chain], Theorem [stagea-topology] + proof audit, Remark [identification], Proposition [search-interval] + proof audit, Remark [interval-vs-bracket]); 3320–3609 (Proposition [twostage-superconsistency] + proof audit, Implementation section, Simulations section, Discussion).

**Code read in full:** `R/fit_twostage.R` (1075 lines), `R/bisect_lambda_to_budget.R` (687), `R/local_optimality_certificate.R` (833), `R/rashomon.R` (922), `R/cross_fitted_rashomon.R` (627), `R/auto_tune_rashomon.R` (167), `R/auto_tune_regularization.R` (323), `R/auto_tune_utils.R` (130), `R/rashomon_utils.R` (22), `R/stage_b.R` lines 1–330 and 600–1414.

**Code read targeted:** `R/discretize.R` (`compute_thresholds`, `compute_bin_count`), `R/cv_regularization.R` (`compute_safe_model_limit`, grid construction), `R/cv_regularization_adaptive.R` (grid), `R/tree_refit.R` (empty-leaf contract), `R/structure_selection.R` (headers), `R/treefarms.R` (parameter→config mapping only, lines 617–686, 780–830, 920–992), `src/gosdt.cpp` 388–457, `src/configuration.cpp` 69–72/180–189, `src/configuration.hpp` 40/136–138, `src/dataset.cpp` 282–292.

**Not read (out of scope for this pass, noted so gaps are explicit, not silent):**
- `R/stage_b.R` lines 330–600, `inst/paper/main.tex`'s Supplementary Material (lines 3610–5592, full proofs) — no finding below asserts a Supplementary proof is wrong.
- The cited-but-absent `theory.tex` (doubletree manuscript) — could not verify citations to it. This gap is itself L5 below, not a pass.
- Test suite, C++ branch-and-bound internals beyond parameter ingestion.
- `advanced_tree_viz.R`, `discretize.R` (beyond the two targeted functions), `predict.R`, `s3_methods.R`, `s7_classes.R`, `tree_averaging.R`, `tree_structure.R`, `treefarms.R` (beyond parameter mapping), `treefarms_isolated.R`, `fit_tree.R`, `fit_rashomon.R` — covered only as receiving ends of parameter mappings, per the audit's task split.
- The two already-fixed issues from the 2026-09-04 review (`rho_n` raw-scale dependence; half-specified `r_n`/`M_n`) were confirmed fixed before this audit started and are not re-flagged. **C1 below is a different, residual failure of the same guarantee that the applied fix does not detect.**

### 1.2 Critical findings (unearned theory claims / silent statistical corruption)

#### C4 — `select_epsilon_n()`'s tolerance is realized as a multiplicative bound in C++, not the additive one the docs describe
- **Location:** `R/rashomon.R:1-36`; `src/gosdt.cpp:418,429-433`; `src/configuration.hpp:136-138`; `R/cross_fitted_rashomon.R:33-35,253`; `R/auto_tune_rashomon.R:58,66,73`; `R/auto_tune_regularization.R:70,171`; `NEWS.md:67-87`.
- **Claim in the docs:** "Returns the fixed, deterministic Rashomon tolerance `ε_n = c·log(n)/n` … This rate is `o(n^{-1/2})`, the condition required for valid Rashomon-DML inference"; operationally: "pass a fixed `rashomon_bound_multiplier = select_epsilon_n(nrow(X))`".
- **What the implementation does** (`gosdt.cpp:418,429-433`):
  ```cpp
  float optimal_objective = models.begin()->loss() + models.begin()->complexity();
  if (Configuration::rashomon_bound_multiplier != 0)
      rashomon_bound = optimal_objective * (1 + Configuration::rashomon_bound_multiplier);
  else
      rashomon_bound = optimal_objective + Configuration::rashomon_bound_adder;
  ```
- **The problem:** a Rashomon tolerance in the DML sense is an additive loss slack, `τ ∈ R_n ⟺ R_n(τ) ≤ R_n(τ̂) + ε_n`. Passing `ε_n` as the *multiplier* realizes `slack = ε_n·(R̂_opt + λ·L̂)`, which is (i) random — depends on the realized optimal objective, so it is not the "fixed, deterministic" tolerance promised; (ii) penalty-coupled — the slack grows with λ for no theoretical reason, meaning the Tier-2 λ search in `auto_tune_regularization.R` silently changes the tolerance while claiming to hold `epsilon_n_fixed` constant; (iii) simply not equal to `ε_n`. The additive channel that would implement the theory (`rashomon_bound_adder`) exists, is wired end-to-end (`treefarms.R:937-943` correctly zeroes the multiplier when the adder is set), and is **never used** by any caller — `cross_fitted_rashomon()` hard-passes `rashomon_bound_adder = 0`, both auto-tuners pass `0` explicitly.
- **Rate claim, re-derived:** `ε_n·R̂_opt = O_p(log n / n)` is still `o(n^{-1/2})` as long as `R̂_opt = O_p(1)`, so the *order* survives. What does not survive is "fixed" and "deterministic" — the constant is now random and λ-dependent, and no available document conditions any inference argument on that.
- **Aggravating:** the theory paper has zero Rashomon content to check this against; the sole cited authority (the doubletree manuscript) is not in this repository.
- **Proposed fix:** route `select_epsilon_n()` through `rashomon_bound_adder`, or restate the docs to describe a relative tolerance whose absolute slack is `ε_n·(R̂_opt + λL̂)`.

#### C1 — Quantile cutpoints violate the paper's quasi-uniform-grid hypothesis; the search-interval guarantee can silently fail at low covariate density
- **Location:** `main.tex:446-452` (grid definition), `main.tex:1296-1301` (best achievable η = half the mesh), `main.tex:2534-2536` (search-interval proof audit); `R/discretize.R:494-505`; `R/stage_b.R:1057-1070`; `R/fit_twostage.R:428`.
- **Claim in text:** "let `A_r` be the full product grid generated by `r` cutpoints per coordinate … the ratio of the largest to the smallest cutpoint spacing … is bounded by a fixed quasi-uniformity constant `Q̄ ≥ 1`. Its mesh is `O(r⁻¹)`" and "the best achievable `η` is half the mesh, of order `r⁻¹`".
- **Implementation:** `compute_thresholds(method = "quantiles")` places thresholds at equally-spaced quantile levels.
- **Re-derivation:** equally-spaced quantile levels give a local spacing `≈ 1/(m·f(t))` where `f` is the coordinate density; the spacing ratio `Q̄(m) ≈ sup f / inf f` is **unbounded and grows with `m`** for any coordinate whose density is not bounded away from 0. The Stage-A localisation error is `≈ 1/(2·m·f(t₀))`, not `1/(2m)`. The code's radius is `ρ_n = M_n/r_n = √m/m`; the ratio the theory needs to diverge is `2·f(t₀)·√m`, not `√m`. This does *not* diverge at any practical `m` when `f(t₀)` is small (e.g. `f(t₀) = 10⁻³` gives effective `M_n < 1` for every `m ≤ 2.5×10⁵`). When it fails, Proposition [search-interval] fails, and with it the headline `O_p(n⁻¹)` rate of Proposition [twostage-superconsistency].
- **Why this is distinct from the already-fixed 2026-09-04 issue:** that fix (a range warning at `fit_twostage.R:715-727`) tests global covariate *scale* (`diff(range(col)) > 10 | < 0.1`). A `[0,1]`-scale coordinate with a low-density region has range exactly 1 — the warning is silent — and this failure mode still occurs, surfacing only as `reason = "no_candidates_in_bracket"` in the internal log.
- **Compounding:** the paper's Assumption [global-density] (`f_min ≤ f ≤ f_max` globally) is exactly the condition that would bound `Q̄`; the package never checks it.
- **Proposed fix:** (a) construct equally-spaced cutpoints over `range(x)` for continuous coordinates in the `fit_twostage()` path, or (b) size `ρ_n` from the observed local mesh near the anchor rather than assuming uniformity; and in either case downgrade the current affirmative "respects Proposition [search-interval validity]" language to conditional.

#### C2 — The removal of ladder rate-condition pruning is justified by a claim that misreads the current paper
- **Location:** `R/fit_twostage.R:180-228` (roxygen + body), `NEWS.md:44-49`; `main.tex:2017-2028`, `main.tex:1303-1321`.
- **Claim in code/NEWS:** "the (now-removed) `prop:parsimony-grid`'s rate condition, `m_n = o(n/r_n)`... does not carry over to the revised architecture; the new theory's rate condition ... does not, by itself, imply any particular grid-resolution ceiling as a function of `n`."
- **What the paper says:** `eq:stage-a-rates` carries `m_n = o(n/r_n)` verbatim as clause **(R4)**, and **(R1)** (`r_n → ∞, r_n = o(√(n/log r_n))`) is exactly the grid-resolution ceiling the code claims doesn't exist.
- **Re-derivation at the shipped defaults** (`n=2000`, `m_ladder = c(16,32,64,128)`, `m_n = 1`):

  | rung `r_n` | `√(n/log r_n)` | (R1) satisfied? |
  |---|---|---|
  | 16 | 26.9 | marginal |
  | 32 | 24.0 | **violated** |
  | 64 | 21.9 | **violated** |
  | 128 | 20.3 | **violated** |

  With `m_n = 1` (default): (R4)'s `m_n → ∞` fails, `m_n·λ_n = 0.1` fails to `→ ∞`, (R5)'s `m_n/√n = 0.022` fails to `→ ∞`. **Three of five rate conditions are violated by the default call**, and `.twostage_validate_ladder()` takes `n`/`m_n` as arguments and uses neither.
- **Proposed fix:** restore an advisory (not pruning) `cli_warn` naming which of (R1)/(R4)/(R5) a given `(n, m_ladder, m_n, lambda_n)` tuple violates; correct the roxygen and `NEWS.md`.

#### C3 — Shipped defaults sit inside the region the paper's own feasibility arithmetic identified as infeasible
- **Location:** `R/fit_twostage.R:653-654` (`lambda_n = 0.1`, `m_ladder = c(16,32,64,128)`); `main.tex:2396-2412` (Step 1), `main.tex:2074-2075` (`C_app`), `main.tex:3510-3524` (Spec 1).
- **Claim in text:** Step 1 excludes over-splitting only when `r_n·λ_n > 2·C_app`, `C_app := (L̄−1)·κ_max²·f_max`.
- **Re-derivation** at the paper's own recalibrated DGP (`κ₁=3.0`, `L̄=4`, `f_max=1`): `C_app = 27`, so Step 1 needs `r_n·λ_n > 54`. The defaults give `r_n·λ_n ∈ {1.6, 3.2, 6.4, 12.8}` — short by 4×–34× at every rung. The predicted over-splitting failure is **masked, not absent**: `bisect_lambda_to_budget()` raises λ until the leaf count matches the budget, so the user sees a budget-respecting tree and `certified = TRUE`, never the symptom the theory predicts.
- **Corroboration:** the paper itself (`main.tex:3521-3524`) widened its own grid to `{64,128,256,512,1024,2048}` for exactly this reason. The package default is still the old, infeasible range.
- **Proposed fix:** raise the default `m_ladder`, or warn when `r_n·λ_n` is below a stated multiple of `leaf_budget − 1`.

#### C5 — `perturb_add_deltas()` silently ignores the caller's `lambda`; the local-optimality certificate becomes a mixed-objective quantity
- **Location:** `R/local_optimality_certificate.R:496-497` (signature, no `lambda` parameter), `:502` (recomputes `lambda_sse` from `model@lambda`), `:725` (caller's resolved value, correctly threaded to `perturb_delete_deltas` only); consumer at `R/fit_twostage.R:461-463`.
- **The defect:** `certify_local_optimality(model, X, y, lambda = L)` resolves `lambda_sse` from `L` and threads it to `perturb_delete_deltas()` — but not to `perturb_add_deltas()`, which recomputes from `model@lambda` instead. So `delete` deltas are scored at `L` while `add` deltas are scored at `model@lambda`; the reported `margin` is a minimum over two different objectives.
- **On the live path:** `fit_twostage.R:461` calls exactly this pattern whenever bisection raised λ above `lambda_n` — precisely the case this second certificate exists to check. `model@lambda ≠ lambda_n`, so `local_certified_at_lambda_n`/`local_margin_at_lambda_n` are not the quantities their names describe. Completely silent.
- **Second-order effect:** if `model@lambda == 0` but the caller passes `lambda > 0`, the zero-penalty guard doesn't fire, `add` is scored with no penalty, and the certificate spuriously fails with `reason = "improving_add"`.
- **Proposed fix:** add `lambda_sse` as a `perturb_add_deltas()` parameter, threaded from the same resolved value as `perturb_delete_deltas()`. Add a regression test that changing the caller's `lambda` changes the `add` row's `min_delta`.

### 1.3 High findings (documented in full)

#### H1 — `bisect_lambda_to_budget()` can report `certified = TRUE` for a solver-truncated (not proven-optimal) incumbent
- **Location:** `R/bisect_lambda_to_budget.R:183-206` (`classify_lambda_fit`), `:599` (`truncated`), `:640-686` (search loop).
- Monotone-leaf-count re-derivation (`(λ₂−λ₁)(|τ₂|−|τ₁|) ≤ 0 ⟹ |τ₂| ≤ |τ₁|`) is confirmed correct **for exact global minimizers**. It consumes exact optimality at each λ and nothing else. When a fit hits `time_limit`, the solver's *incumbent* (not a proven optimum) flows into `classify_lambda_fit()`, which has no truncation awareness and will return `certified = TRUE` whenever `n_leaves == effective_budget` regardless. `fit_twostage()` does gate on `any_truncated` separately, but any other caller of `bisect_lambda_to_budget()` gets the unearned claim as the function's own stated contract.
- **Proposed fix:** `classify_lambda_fit()` should take `truncated` and force `certified = FALSE`, `gap = NA_real_`.

#### H2 — Bisecting λ upward can leave the paper's own `λ_n ≪ 1` rate condition (R2/R3) violated, silently, while still reporting `certified`
- **Location:** `R/bisect_lambda_to_budget.R:645-668,672`; `R/fit_twostage.R:405-413`; `main.tex:2021-2023` (R2/R3), `main.tex:2414-2426` (Step 2).
- With `L̄=4` the Step-2 margin is positive only for `λ` below `c_top/3.5`; growth from `hi_init` by `hi_growth=4` over `tol_iter=12` doublings can reach `λ ≈ 1.7×10⁵`. Any λ above the threshold makes Step 2's margin negative, yet `classify_lambda_fit()` still returns `certified = TRUE` at that λ if `n_leaves == leaf_budget`. No field records `λ_returned/λ_n` or flags a large ratio.
- **Proposed fix:** return `lambda_ratio = lambda/lambda_n`, warn above a documented threshold.

#### H3 — `find_tree_intersection()` silently degrades from partition-equivalence to whole-object identity, and its signature space includes infeasible combinations
- **Location:** `R/rashomon.R:237-241` (`> 20` features bail-out), `:683-702` (fallback to whole-tree `digest()`), `:243-314` (signature construction).
- **Defect 1:** when a tree's max feature index exceeds 20 (routine after discretization), `tree_to_partition_signature()` returns `NULL` and `find_tree_intersection()` falls back to hashing the whole tree object, **including leaf values**, which are never bit-identical across folds — the intersection is guaranteed empty, and the print method's advice ("increase regularization") is a wrong diagnosis for the real cause.
- **Defect 2:** even without the fallback, the signature enumerates all `2^n_features` binary vectors, most of which are infeasible given nested threshold indicators from discretization — two trees that agree on every realizable row-combination but differ on infeasible ones get different signatures, producing systematic false negatives (and `2^20` recursive walks per tree in the worst case).
- **Contrast:** `canonical_partition()` (used by the local-optimality certificate) compares induced partitions of the actual training rows — exact, tolerance-free, and available in the same package. The Rashomon path doesn't use it.
- **Proposed fix:** replace the hypercube signature with `canonical_partition`-style row-block hashing on the shared binary design matrix.

#### H4 — Rashomon-set truncation at `model_limit` is invisible to the cross-fold intersection, and breaks the auto-tuner's monotonicity assumption
- **Location:** `R/treefarms.R:661-670,983-989`; `R/cross_fitted_rashomon.R:361-412,443,467-485`; `src/gosdt.cpp:468-469`.
- A `treefarms_model_limit_exceeded_cpp()` flag exists and `treefarms()` warns on it, but nothing downstream records it — `fit_one_fold()` discards it, `CFRashomon` has no truncation field. `find_tree_intersection()` cannot distinguish "not in the Rashomon set" from "never enumerated because extraction stopped at 200,000 models." Re-derivation of the auto-tuner's monotonicity: in exact arithmetic, `R(ε) ⊆ R(ε')` for `ε < ε'` (well-posed for the bisection search), but under truncation this fails — a larger `ε` enumerates more candidates, hits the limit sooner, and there's no guarantee of enumeration in objective order, so a tree present at `ε` can be absent at `ε' > ε`.
- **Proposed fix:** capture the truncation flag inside `fit_one_fold()`, store a per-fold vector on `CFRashomon`, and have `find_tree_intersection()` return a distinguishable "not comparable" state when any fold was truncated.

#### H5 — A hard solver error is classified as "tolerance too small," which drives the search the wrong way
- **Location:** `R/auto_tune_utils.R:43-51`; `R/auto_tune_rashomon.R:65-83`.
- `.probe()` maps any `tryCatch`-caught error (OOM, `model_limit` exhaustion, solver crash) to `"higher"` — but those failures get *more* likely as ε grows (larger tolerance ⇒ larger enumerated set). The search responds to a crash by doubling ε. No error/empty-intersection distinction reaches the caller.
- **Proposed fix:** a distinct `"error"` classification that aborts the search phase rather than continuing it.

#### H6 — The local-optimality certificate searches outside the estimator's own domain and demands a stronger property than the theory claims
- **Location:** `R/local_optimality_certificate.R:387-400,425-426`; `R/stage_b.R:1057-1070`; `main.tex:1529-1533,2543-2560`.
- `perturb_move_deltas()` uses the purely structural `node_bracket()` with **no `ρ_n` intersection** (its own roxygen asserts it does not widen past `node_bracket()`, which was correct before the 2026-09-04 `ρ_n` feature was added but is now stale). It can therefore evaluate candidate cuts outside `I_n(N)` and fail fits that are theory-compliant precisely when `ρ_n` binds. Separately, the paper's Assumption [solver-twostage](b) is explicit that Stage B is "not a joint minimisation," while the certificate's stated purpose is checking a fixed point the theory never claimed the single pass achieves.
- **Proposed fix:** thread `r_n`/`M_n` into `perturb_move_deltas()`; document that the certificate tests a strictly stronger property (a fixed point) than Algorithm 1's single pass.

#### H7 — `cross_fitted_rashomon()` calls an `n`-independent default "theory-consistent"
- **Location:** `R/cross_fitted_rashomon.R:12,69-74,99` (`rashomon_bound_multiplier = 0.05`).
- `0.05` does not depend on `n`, so it is never `o(n^{-1/2})` at any `n` — contradicting `select_epsilon_n()`'s own documented standard (and `NEWS.md`'s explicit rejection of the *stricter* `sqrt(log(n)/n)` for the same reason). Two docstrings in the same package prescribe mutually exclusive tolerances, and the default is the inference-invalid one.
- **Proposed fix:** default `rashomon_bound_multiplier = NULL` resolving to `select_epsilon_n(nrow(X))`, or remove "theory-consistent" from the docs.

#### H8 — Stage B returns the observed value, not Algorithm 1's midpoint convention, and ties are broken toward the grid cut
- **Location:** `R/stage_b.R:795-800,864-885,1140`; `main.tex:504-508`.
- Algorithm 1 Step 4 specifies `ĉ_N ← ½{x_(i) + x_(i+1)}` (the midpoint convention); the implementation returns the observed value `x_(i)` itself, and splits with `x ≤ cut`. Inside `I_n(N)` consecutive spacings are `O_p(1/(n·f))`, so the observed-value estimator has a **systematic, one-sided** `O_p(n^{-1})` bias — the same order as the entire claimed rate. `main.tex:3505-3508` explicitly asks for the tie-breaking convention to be pinned down; the package pinned down a different one than specified. Ties are not exotic (`sse_tol` is an absolute `1e-9`), and the tie-break prefers the candidate closest to the Stage-A grid cut, further degrading the achieved rate on ties.
- **Proposed fix:** return `½(x_(i)+x_(i+1))`; make `sse_tol` relative to node SSE; break ties toward the interval interior rather than the incumbent grid cut.

### 1.4 Verified correct (worth knowing — not just problems)

- The `depth_budget` disclosure discipline (`depth_reachable`/`depth_required` split, refusal to accept `max_depth = 0L` silently, `certified`/`certified_full_class` split, a print method that leads with "depth_sufficient = FALSE BY DESIGN, not a fit failure") is the best-handled default in the package and a model for the rest.
- Stage-B's per-leaf SSE sweep, global-`y` centering, and root-first traversal ordering were independently re-derived and confirmed correct against Algorithm 1 Step 4, including that already-refined sibling subtrees are correctly irrelevant to a node's own argmin.
- `canonical_partition()` is the theoretically correct object for comparing induced partitions (exact, tolerance-free); the Rashomon path (H3) would be materially better off reusing it.
- The monotone-leaf-count lemma underlying bisection is independently re-derived and holds; the defect is truncation voiding its premise (H1), not the lemma itself.
- The previously-fixed `depth_budget = max_depth + 1` off-by-one is correctly re-verified against `dataset.cpp`'s decrement semantics.
- `build_coord_node()` asserts (rather than assumes) branch orientation before mapping; `bin_lookup()` cross-checks its positional reconstruction and aborts on drift — exactly the discipline that would have caught the historical off-by-one class of bug.
- Classed, named refusal conditions (`optimaltrees_refine_infeasible`, `optimaltrees_stage_b_binary_split`, `optimaltrees_deadline_exceeded`), caught by class not message, with differentiated per-condition ladder policy, are the right alternative to silent fallback and the pattern the Rashomon path (H3–H5) most needs to adopt.

### 1.5 Medium findings

| # | Finding | Location |
|---|---|---|
| M1 | `as.integer(2^max_depth)` silently overflows to `NA` at `max_depth ≥ 31` (reachable at `leaf_budget = 32`), producing a cryptic "missing value where TRUE/FALSE needed" error instead of a clear message. | `R/bisect_lambda_to_budget.R:520` |
| M2 | C++ `depth_budget` is an `unsigned char` (wraps mod 256); `max_depth = 255` silently becomes "unlimited," `max_depth = 256` silently becomes a stump. Not reachable today only because M1 fires first (accidentally, not by validation). | `R/treefarms.R:617-618,817-822`; `src/configuration.hpp:40` |
| M3 | `r_n = m` passed to Stage-B is off-by-one against the actual cutpoint count (`discretize_features(n_bins=m)` produces `m-1` thresholds), and `unique()` can shrink the realized grid further on tied quantiles — silently under-sizing `ρ_n`, the dangerous direction. | `R/fit_twostage.R:428`; `R/discretize.R:495-503` |
| M4 | A user-declared `depth_budget` incompatible with `leaf_budget` is silently *raised* rather than reported as a misconfiguration, contrary to the paper treating the pair as a compatibility requirement on the analyst. | `R/fit_twostage.R:121-131,160-161`; `main.tex:909-920` |
| M5 | Stage B's argmin domain (structural bracket ∩ `min_leaf_n` floor) is narrower than Assumption [solver-twostage](b) permits; asymptotically non-binding but undisclosed as a deviation, and `min_leaf_n` defaults to `m_n` so it's always active. | `R/stage_b.R:754-777,1108-1124` |
| M6 | The local-optimality certificate computes `lambda_sse` from `model@n_train` but SSE from the *passed* `X`, guarded only by a length check — not `nrow(X) == model@n_train`. A subset/superset silently rescales every pass/fail threshold. | `R/local_optimality_certificate.R:70-81,707,714,730` |
| M7 | `discretize_bins = "adaptive"` silently resolves to a **different formula** inside `cross_fitted_rashomon()` (log schedule) than everywhere else (`ceiling(n^{1/3})`) — same keyword, different `r_n`, no disclosure. | `R/cross_fitted_rashomon.R:203-221` |
| M8 | `rashomon_ignore_trivial_extensions = FALSE`'s stated rationale (split-order variants breaking structural-identity intersection) is stale now that intersection uses partition signatures, which already collapse split-order variants. The default may still be correct, but for the different, undocumented reason tied to H3.2. | `R/cross_fitted_rashomon.R:14-18` |
| M9 | `lambda_candidates` keeps only the 6 weakest of the documented Tier-3 multipliers `{1,2,5,10,20}`, making the "stronger (simpler trees)" candidates unreachable at defaults; separately, the `lambda_min` rationale is directionally backwards (small λ produces complex trees, not stumps). | `R/auto_tune_regularization.R:59-65,131-150` |
| M10 | A crashed cross-fitting fold is caught, warned, and returned as an empty tree list — but `CFRashomon(..., converged = TRUE)` is still constructed unconditionally, and the print method attributes the resulting empty intersection to modelling choices rather than a fold failure. | `R/cross_fitted_rashomon.R:406-411,483` |
| M11 | `cv_regularization()`'s "theory-driven" λ grid (`(log n/n)·{1,1.5,2,3,5,10}`) tops out below the paper's own rate-condition threshold (R3) for the default ladder at realistic `n` — no function in the package currently selects a λ that satisfies (R3). | `R/cv_regularization.R:208-215` |
| M12 | The Rashomon threshold is carried in C++ `float`; re-derived crossover shows `ε_n = log(n)/n` retains >10× numerical headroom only up to `n ≈ 10⁷` — not live at realistic `n`, but an undocumented ceiling. | `src/gosdt.cpp:402` |

### 1.6 Low findings

- **L1** `classify_lambda_fit()` uses exact `lambda == lambda_n` equality while `fit_twostage()` uses `all.equal()` (tolerance ~1.5e-8) for the same case distinction.
- **L2** A per-fit time-limit computation can start a fit with a 1-second limit when only 0.2s remain on the shared deadline, contradicting the stated "never allowed to overrun it."
- **L3** If bisection's growth phase lands exactly on the target leaf count, the loop can still run further fits up to `tol_iter`, burning the shared deadline unnecessarily.
- **L4** `bidirectional_exp_binary_search` returns the last "lower"-classified probe's result, not necessarily the smallest working ε, contradicting its own "narrow to smallest working epsilon_n" doc.
- **L5** Multiple roxygen blocks cite `theory.tex` labels (`rem:grid-practice`, `prop:greedy`, `ass:global`, "Certificate for the computed selector," "The budget binds asymptotically," "Monotone leaf count") absent from the shipped `inst/paper/main.tex`. Not a correctness defect on its own (the monotone-leaf-count lemma was independently re-derived and holds), but every one of these citations is unverifiable by a reader of this package.
- **L6** `fit_twostage()`'s all-binary pre-flight check can pass a 3-valued coordinate whose quantiles collapse to ≤2 thresholds under `unique()`, leaving Stage B nothing to refine.
- **L7** The C++ solver rounds split thresholds to 6 decimal places while `build_coord_node()` reconstructs the unrounded R-side quantile — benign for 0/1 columns, untested beyond that.

---

## 2. Silent fallbacks

Per-project convention, silent fallbacks are forbidden. An `explore`-agent sweep found ~40 candidate patterns across `tryCatch`, `%||%`, `suppressWarnings`, and `if (verbose)`-gated behavior changes in R/ and src/. **Two of the three highest-severity claims were independently spot-checked and found to be false positives** — the raw sweep output should not be treated as verified without individual triage.

### 2.1 Confirmed genuine — highest priority

**`src/model.cpp:92-106` (CRITICAL, confirmed by direct read).** When a leaf's class-distribution/probability computation throws (invalid probability value, probabilities not summing to ~1, or an empty `state.locals`), the `Model` constructor silently substitutes a **uniform class distribution** (`1.0/dataset_depth` per class) and logs only to `std::cerr` — not an R condition, not visible in a typical R session, and not catchable from R. A buggy or edge-case leaf can silently produce uniform-probability predictions with zero R-visible signal. This is the single clearest match in the whole audit for "silent fallback, forbidden."
- **Scope caveat, not yet resolved:** this constructor path concerns class-distribution/probability computation; it has not been confirmed whether it is exercised only on the classification/log-loss path or also touches regression leaf values. Trace `dataset.get_class_distribution()`'s call sites before fixing, to scope the fix correctly.

### 2.2 Spot-checked and downgraded (false positives from the automated sweep)

- **`R/treefarms.R:1554` (`result$accuracy %||% NA_real_`)**, flagged Critical by the sweep. On inspection this is honest missing-value population into a typed S7 field, not a silent substitution of a wrong numeric result. **Not an issue.**
- **`R/auto_tune.R:239` (`best_result <- result_tier2 %||% result_tier1`)**, flagged Critical by the sweep. On inspection this is immediately followed by an explicit `warning()` reporting non-convergence and `n_trees`, and `best_result$converged <- FALSE` for programmatic detection. **Properly disclosed, not silent.**

### 2.3 Already fixed (no action needed, listed for completeness)

- The half-specified `r_n`/`M_n` fallback in `refine_tree_cuts()` — now an explicit `cli::cli_abort()` (`stage_b.R:980-990`).
- The `rho_n` raw-covariate-scale silent failure from the 2026-09-04 review — now has a range-mismatch warning (`fit_twostage.R:697-725`). **Note:** C1 above (§1.2) is a *different*, still-open residual failure of the same underlying guarantee that this fix does not detect (low-density regions on unit-scale coordinates).

### 2.4 Remaining candidates — not individually re-verified, recommend a dedicated triage pass before acting

The following are the sweep's raw findings, organized by category. Given the false-positive rate found on spot-check (2 of 3 sampled), **treat these as leads, not confirmed findings**, until each is read in context:

**`tryCatch` returns a default/NULL with no caller-visible disclosure (or only verbose-gated disclosure):**
- `R/treefarms.R:1023-1044` (JSON parse error → NULL, warning issued)
- `R/treefarms.R:1712-1720`, `:1749-1761` (print-method display sections silently skip on error)
- `R/treefarms.R:1832-1840,1850-1857,1867-1874` (three lazy-computation paths → NULL, warning issued)
- `R/bisect_lambda_to_budget.R:97` (`assign_leaf_ids` error → NULL, no warning)
- `R/auto_tune.R:285-300` (fit error → NULL, verbose-gated message only)
- `R/auto_tune_regularization.R:164-181,187-193,249-266,269-274` (fit/leaf-count errors → NULL/NA)
- `R/auto_tune_rashomon.R:67-82` (rashomon fit error → NULL)
- `R/cv_regularization.R:255-266` (CV fit error → NULL)
- `R/discretize.R:336-341` (fit error in CV loop → NULL, no warning)
- `R/cross_fitted_rashomon.R:362-411` (fold fit error → empty tree list, warning issued — see also M10 above)
- `R/local_optimality_certificate.R:754-758` (error object captured, then converted to NULL)
- `R/treefarms_isolated.R:229-234` (JSON parse error → NULL, warning issued)
- `src/model_set.cpp:314-337`, `src/gosdt.cpp:229-240,288-300` (C++: trees silently skipped or empty JSON returned on exception, verbose-only logging)

**`if (verbose)` / `withCallingHandlers` gating a behavior change, not just a message:**
- `R/treefarms.R`'s `max_depth = 2L` auto-cap for high-dimensional single-tree regression — already known (2026-09-01 fix), disclosed only when `verbose = TRUE`.
- `R/fit_twostage.R:763-783` — a high-dimensionality warning is muffled via `withCallingHandlers` when `verbose = FALSE`.

**`%||%`/null-coalescing substituting for a value that may represent an upstream failure (needs individual reading, given the two false positives found above):**
- `R/auto_tune.R:158` (`depth_budget <- dots$max_depth %||% dots$depth_budget %||% 0L`)
- `R/auto_tune_utils.R:109`, `R/cv_regularization.R:14`, `R/rashomon.R:770` (`penalized_risk <- tree$penalized_risk %||% tree$model_objective`)

**Type-coercion `suppressWarnings`, likely benign but not individually confirmed:**
- `R/treefarms.R:397`, `R/fit_tree.R:156`

---

## 3. Dead code / duplication

- **HIGH, confirmed, already known and deferred.** The precision-critical CSV-row-building helper is duplicated verbatim between `R/treefarms.R:953-969` and `R/auto_tune.R:139-154`, with *different* upstream preconditions (only `treefarms.R`'s copy has y-normalization) and only one copy under test. Flagged as Issue 2 in the 2026-09-10 CSV-precision review, explicitly deferred, still not fixed.
- **MEDIUM.** `treefarms_fit_cpp` and `treefarms_fit_and_stats_cpp` are registered C++ exports with zero R-level call sites — the active path uses only `treefarms_fit_with_config_cpp`. Dead weight in the compiled library.
- **MEDIUM.** `treefarms_isolated()` is exported in NAMESPACE, fully implemented and documented as a process-isolation wrapper, but has zero call sites anywhere in R/, tests/, or dev-scripts/.
- **LOW.** A handful of C++ `TODO` markers (`model.cpp:449`; `model_set.cpp:241,306`) are architectural refactor notes, not active bugs; `tile.cpp:6`'s "stub, parameters intentionally unused" is by design.
- No large commented-out dead-code blocks found. `main.cpp`/CLI headers are correctly excluded from the R build (verified against both `Makevars` files). All headers under `src/optimizer/` are `#include`d and used.

---

## 4. Defaults — provenance and consistency

Theory-grounded table (from the correctness review, more authoritative than a purely structural pass):

| Default | Value | Status |
|---|---|---|
| `M_n` (Stage-B radius) | `NULL → √r_n` | Honestly documented as not theory-derived. Its *effective* value is `2·f(t₀)·√r_n` (C1), nowhere stated. |
| `m_ladder` | `c(16,32,64,128)` | **Arbitrary and theory-infeasible** at the package's own defaults (C2, C3). The paper's own worked example uses `{64...2048}`. |
| `lambda_n` | `0.1` | Arbitrary; no shipped default satisfies the paper's own rate condition against the default ladder. |
| `m_n` / `min_leaf_n` | `1L` | Violates two live rate conditions (R4, R5); nothing warns. |
| `depth_budget` | `NULL → ⌈log₂ L̄⌉` | **Exemplary** — theory-anchored, honestly disclosed. Best-handled default in the package. Caveat: M4 (silent upward clamp on the user-declared path). |
| `leaf_budget` | required, no default | Correct — no invented default. |
| `rashomon_bound_multiplier` | `0.05` | Arbitrary, `n`-independent, mislabelled "theory-consistent" (H7). |
| `rashomon_bound_adder` | `0` | The channel that would actually implement the theory's additive tolerance — never used (C4). |
| `model_limit` (Rashomon) | `200000L` | Empirically justified and documented as such ("sufficient for max_depth=4 with 10-20 binary features at theory-scale lambda") but unenforced — nothing checks the actual configuration is inside that envelope. |
| `discretize_bins = "adaptive"` | different formulas by caller | `ceiling(n^{1/3})` in most places, a log schedule inside `cross_fitted_rashomon()` — same keyword, different `r_n` (M7). |
| `tol_iter` | `12L` (twostage) / `40` (bisect) | Empirically justified and documented ("keeps a single rung's worst case near 25 fits rather than 81"). |
| `tol_abs` / `tol_rel` | `1e-9` / `1e-9·sse_null` | Correctly reasoned and documented as numerical-noise-only, with an explicit refusal to conflate it with a significance slack. |
| `sse_tol` | `1e-9` absolute | Arbitrary, not scale-free; enables the grid-cut tie-break bias in H8. |
| `lambda_min`/`lambda_saturated` (auto-tune) | `0.5·log n/n` / `0.001·log n/n` | Arbitrary, and the stated rationale is directionally backwards — small λ produces *more* complex trees, not stumps (M9). |
| `c_max` (auto-tune) | `100` (overridden to `10` in practice) | Arbitrary; at `n=1000`, `c=100` gives a 69% relative Rashomon bound while documented as "asymptotically valid." |

**Additional consistency issue from the structural sweep:** `rashomon_ignore_trivial_extensions` defaults to `TRUE` in `fit_tree()` but `FALSE` in `fit_rashomon()`/`cross_fitted_rashomon()`. Per M8 above, the stated rationale for this split predates the switch to partition-signature comparison and may no longer be the operative reason, even if the default itself remains correct.

**Documentation drift:** `README.md` claims adaptive bins use `max(2, ceiling(log(n)/3))`; the current code uses `ceiling(n^{1/3})` (or a log schedule specifically inside cross-fitting, per M7). The README should be corrected regardless of the deeper C1/M7 findings.

---

## 5. Efficiency

- **Confirmed, already fixed 2 days before this audit (commit `140d110`).** Single-tree extraction previously enumerated the full cross-product of tied subtrees (`T(d) = d·T(d-1)²`), exhausting `model_limit` and returning zero trees on product-optimal DGPs. Fixed via `Optimizer::single_model()`, now `O(tree size)` instead of `O(#ties)`. The actual Rashomon-set extraction path intentionally still enumerates every tie (correct, by definition of a Rashomon set) and its truncation is disclosed via `cli::cli_warn()`, not silent — see also H4 above for a gap in how that disclosure propagates through cross-fitting.
- **Found directly (orchestrator review, not delegated).** `predict.cf_rashomon`'s ensemble majority vote (`R/predict.R:442-444`) uses `apply(pred_matrix, 1, function(x) as.numeric(names(sort(table(x), decreasing=TRUE))[1]))` — slow (per-row `table()` + `sort()` over the whole `newdata`) and leaves tie-breaking to `sort()`'s incidental ordering rather than an explicit, documented rule. Since predictions are binary, this should be `rowMeans(pred_matrix) >= 0.5` with a stated tie policy (majority-vote ties are possible whenever `n_intersecting` is even).
- `bisect_lambda_to_budget.R` and `cross_fitted_rashomon.R` were read in full and found well-engineered on efficiency grounds: `bisect_lambda_to_budget()` deliberately avoids a native C++ leaf-count cap (see §6 below) and tries `lambda_n` first so the common case needs one fit; `cross_fitted_rashomon()` parallelizes fold-fitting via `furrr::future_map2` with a documented sequential fallback.
- No other efficiency red flags found in the R layer within this audit's scope. The C++ core is GOSDT/TreeFARMS-derived reference code and was not independently re-profiled beyond the exponential-blowup check above.

---

## 6. Maintainer context folded into prioritization

**Regression as the primary use case:** the Stage-A/B two-stage architecture (§1.2's C1, C2, C3, C5 and §1.3's H1, H2, H6, H8) is loss-agnostic but is specifically the estimator built for regression-tree topology recovery — these should be the top fix priority. The Rashomon-set findings (C4, H3, H4, H5, H7) matter equally for regression Rashomon-DML, since that machinery is also loss-agnostic.

**Native leaf budget:** the package currently approximates a leaf-budget-constrained selector via `bisect_lambda_to_budget()` (repeated `fit_tree()` calls at different `regularization` values) specifically because a native C++ leaf-count cap was evaluated and rejected on "cache-fragmentation/soundness grounds" (referenced in code comments at `bisect_lambda_to_budget.R:214-215` and `fit_twostage.R:300-301`; the design doc that explains the full reasoning, `quality_reports/specs/2026-08-20_leaf-budget-lambda-corollary.md`, no longer exists in this checkout). C2/C3 above show the current workaround already sits in a theoretically fragile spot at the shipped defaults, which is a real point in favor of revisiting the native-cap rejection now that the papers assume one. **This is a genuine architecture decision** — it would require re-deriving branch-and-bound pruning-bound compatibility with an added leaf-count dimension — and should go through a dedicated Oracle/Metis-consulted design session, not be decided inside an audit.

---

## Addendum (2026-09-10, same day): independent re-triage resolving the epistemic caveat above

A second, independent `domain-reviewer` pass was run specifically to resolve the caveat inserted above -- to separate "the code is a bug regardless of what happens to `main.tex`" from "the code doesn't match this specific, once-already-revised paper." It read `main.tex`'s cited sections and the cited R/C++ code directly (not taking the first pass's paraphrase on faith), redid the arithmetic independently for the hardest items, and was explicitly instructed to try to refute each finding.

**Result: it agreed with every one of the first pass's factual code claims (9 AGREE, 4 PARTIALLY AGREE, 0 DISAGREE) and confirmed every `main.tex` citation is accurate** -- the first pass did not misread or hallucinate anything from the paper. What it changed was the *prioritization*, by classifying each finding as paper-independent (survives any future revision of `main.tex`) or paper-dependent (only as strong as this specific, evolving paper's own claim):

- **Paper-independent (8): C4, C5, H1, H3, H4, H5, H6, H7.** These are internal software bugs / self-contradictions / pure arithmetic facts (e.g. "a constant cannot be `o(n^{-1/2})`") that hold regardless of `main.tex`'s fate. **These are the ones to act on.**
- **Hybrid (1): C1.** The core defect -- `R/discretize.R` builds its grid in probability (quantile) space while `R/stage_b.R` sizes the search radius `rho_n` in raw covariate space -- is a genuine unit-consistency bug between two package components, independent of the paper. Only the escalation ("this breaks the headline `O_p(n^{-1})` rate") is paper-dependent.
- **Paper-dependent (4): C2, C3, H2, H8.** **Do not act on these as stated.** Most consequentially: **the reviewer found that `main.tex`'s own conditions are mutually unsatisfiable in the package's realistic operating range** -- Step 1 (behind C3) needs `r_n * lambda_n > 2*C_app = 54` at the paper's own worked-example constants, while the paper's own rate condition (R1) (behind C2) caps `r_n` at roughly 27 when n = 2000, and `lambda_n` is separately required to be `<< 1` -- so **no `(r_n, lambda_n)` pair satisfies both below roughly n ~ 1e5-1e6.** Raising `m_ladder` to satisfy C3 (the first report's own proposed fix) would therefore push the package *further* out of compliance with C2. `C_app` and `c_top` are also functions of unknown true-DGP constants, so neither condition is checkable at the package level even in principle. **The correct action item from this pair is narrower than either original finding: fix the two factually-wrong sentences in `fit_twostage.R:180-193`** (`m_n = o(n/r_n)` was NOT removed from the theory -- main.tex still states it verbatim as (R4) -- and (R1) IS a grid-resolution ceiling, contrary to what the roxygen claims) **and otherwise leave the numeric defaults alone** until `main.tex` reconciles (R1) with its own Step 1.
- **Severity corrections:** H2 (Low/Medium, not High -- `certified` is documented as a leaf-budget-constrained-selector claim, which a large lambda does not actually invalidate via the exact-penalty argument; the real gap is just that `lambda_returned/lambda_n` isn't disclosed) and H8 (Low, not High -- both conventions induce the identical partition and empirical risk; only the reported constant, not the rate, changes. Still worth a one-line fix to match Algorithm 1's literal pseudocode and standard CART practice, just not as a rate-correctness fix).
- **Minor corrections to the first report's own citations,** none changing a verdict: C4's "hard-passes `rashomon_bound_adder=0`" is true of the auto-tuners only, not `cross_fitted_rashomon()` itself (user-overridable there); H2's cited `tol_iter=12`/reachable-lambda numbers were wrong by a wide margin (actual default `tol_iter=40`, reachable lambda ~2.4e23 not 1.7e5 -- understating its own point); H3's "silently degrades" should read "a warning fires, but nothing connects it to the guaranteed-empty-intersection consequence"; H8's tie-frequency reasoning was backwards (an absolute `sse_tol` makes exact ties on continuous SSE values less likely at small scale, not more -- though it can cause spurious ties at large SSE scale via float precision, which is a real but different concern).
- **New finding not in the first report:** the paper's own Discussion-adjacent text (`main.tex:3505-3508`) already pre-registers the exact tie-breaking gap behind H8, and Remark [interval-vs-bracket] (`main.tex:2543-2561`) already volunteers that `n*rho_n -> infinity` requires (R1), not Definition [Stage-B search interval]'s own conditions on `M_n` -- i.e. several of this audit's findings are things the paper's own proof audits already flag as open, which is a point in favor of the paper's honesty and a reason the second reviewer found zero misreadings.

### Corrected fix-priority tiers (supersedes the flat Critical/High tags above)

- **Tier 1 -- fix now, no theory dependency, cheap and self-contained:** C5 (thread `lambda_sse`, not just `lambda`, into `perturb_add_deltas()` -- also fixes an `objective_scale` mismatch the first pass missed), H1 (consume `truncated` in `classify_lambda_fit()`), H5 (give `.probe()` a distinct `"error"` class instead of collapsing every exception to "tolerance too small"), H4 (capture `treefarms_model_limit_exceeded_cpp()` inside `fit_one_fold()`), H7 (resolve the `rashomon_bound_multiplier=0.05` vs. `select_epsilon_n()` contradiction -- fix one docstring or the default), H6 (thread `r_n`/`M_n` into `perturb_move_deltas()`, refresh its now-stale roxygen).
- **Tier 2 -- fix now, larger scope:** H3 (replace the `> 20`-feature hypercube signature fallback with `canonical_partition()`-style row-block hashing -- highest *user-facing impact* on this list, since it makes cross-fold Rashomon intersection structurally unreachable for any moderately-discretized regression fit) and C4 (a policy decision first -- is the intended Rashomon tolerance additive or relative? -- then route `select_epsilon_n()` through whichever channel actually matches, or rewrite the docs to match the multiplicative channel already in use).
- **Tier 3 -- the paper-independent slice of C1:** size `rho_n` from the observed local mesh near the anchor rather than `M_n/r_n` in raw covariate units, or offer equally-spaced (not quantile) cutpoints on the `fit_twostage()` path; soften "respects Proposition [search-interval validity]" to a conditional statement either way.
- **Tier 4 -- do not act on the numerics; fix only the two factually-wrong sentences:** C2/C3 (correct `fit_twostage.R:180-193`'s roxygen, leave `lambda_n`/`m_ladder` defaults alone pending a paper-level reconciliation of (R1) and Step 1) and H8 (a one-line midpoint fix, done opportunistically as spec-fidelity/CART-convention hygiene, not urgency).

---

## Appendix: methodology note on report reliability

- §1 (correctness) findings mostly carry an independent mathematical re-derivation by the reviewing agent, not just a reading of docstrings — this is the highest-confidence section of the report.
- §2 (silent fallbacks), §3 (dead code), §4 (defaults) were produced by parallel automated sweeps and were **not** all individually re-verified. Two of three sampled "Critical" claims in §2 were false positives on inspection. Recommend a dedicated triage pass on §2.4's remaining list before converting any of it into fixed work.
- §5 (efficiency) is orchestrator-direct review, not delegated, and is lower-volume but higher-confidence per item.
