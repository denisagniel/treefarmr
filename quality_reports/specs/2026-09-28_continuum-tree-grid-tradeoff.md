# Simulation Design: Grid-Resolution / Excess-Risk Tradeoff and Jump-vs-Cosmetic Threshold Rates for the Fixed-L Two-Stage Tree Estimator

**Date:** 2026-09-28
**Stem:** 2026-09-28_continuum-tree-grid-tradeoff
**Study dir:** simulations/continuum-tree-grid-tradeoff
**Archetype:** decision
**Status:** APPROVED

## 0. Why this study, and what it is not

This study exists to answer, empirically, the one question the stalled continuum-tree
paper (`inst/paper/story.md`, formally "NOT AN ACTIVE PAPER") never answered before
committing to a theorem: whether the reframed target — a fixed-`L`-leaf tree fit by
ERM on a quantile grid, judged by excess risk against the population *best-in-class*
approximation, not by exact recovery of an assumed-tree-shaped truth — actually behaves
the way the reframe's own decomposition predicts. Two prior orchestrator/Oracle rounds
this session produced that decomposition analytically; nothing in it has been run.

**This is not a resurrection of the growing-complexity theory.** `simulations/dgps.R`,
`sim_rate_estimation.R`, and `adaptive_bins_simple.R`/`adaptive_bins_results_summary.md`
(2026-03-03) are prior work under the *old*, abandoned program (leaf count `s_n` growing
with `n`, bins scheduled as `ceiling(log(n)/3)` to support that growth, regularization
`λ ≍ log(n)/n`). `L` here is **fixed and known**, never growing with `n` — the whole
point of the question this study answers is whether that fixed budget, combined with an
*adaptive* grid, is enough on its own, without any growing-complexity machinery. Do not
adapt the old harness's bin schedule or regularization convention into this study; reuse
only its `dgp_*(n, p, seed) -> list(X, y, truth, ...)` naming/return convention for
implementation consistency, and its one confirmed lesson — squared-error loss needs
`bisect_lambda_to_budget()`'s lambda-bisection (already the plan below), not a fixed
`regularization` value, which that prior study found collapses squared-error fits to a
trivial 2-leaf stump regardless of sample size.

## 1. Objective

As the quantile-grid resolution `m_n` and sample size `n` vary, does a fixed-`L`-leaf
axis-aligned regression tree, fit by exact ERM over that grid (`bisect_lambda_to_budget()`,
Stage A) and then off-grid-refined (`refine_tree_cuts()`, Stage B, fixed this session):

- **(Q1)** achieve excess risk against the population best-`L`-leaf approximation that
  becomes *flat* in `m_n` past some threshold `m_n^†(n)`, consistent with the analytic
  decomposition (grid bias `O(1/m_n)` vs. a grid-independent estimation term
  `O(√(log n/n))` from the fixed VC dimension of the `L`-leaf tree class) — and does
  `m_n^†(n)` grow no faster than `O(√n)`, as that decomposition predicts?
- **(Q2)** show a genuine empirical split between `O(n⁻¹)` convergence of a
  Stage-B-refined threshold at a boundary corresponding to a real jump discontinuity in
  the true regression function, versus `O(n^{-1/2})` at a boundary that is purely an
  artifact of the fixed leaf budget (the true function is smooth there)?

If this study did not exist, there would be no evidence beyond hand-derived orders of
magnitude for either claim, and the paper's next revision — grid-tradeoff methods paper,
pursuit of the harder local-margin superconsistency conjecture, or abandonment — would
be chosen the same way the original theorem was: without ever having been run. The
combined outcome of Q1 and Q2 is the deciding input to that choice (Section 7).

## 2. Estimand and method

- **Method under test:** `bisect_lambda_to_budget(X, y, leaf_budget = L, max_depth = D,
  depth_restricted = TRUE, loss_function = "squared_error", ...)` (Stage A) at a
  specified, EXPLICIT quantile-grid resolution `m_n` (bins per coordinate), followed by
  `refine_tree_cuts()` (Stage B). `m_n` must be supplied directly as the discretization
  resolution for the fit, not derived from the package's own adaptive default
  (`ceiling(log(n)/3)`, which the prior-session analytic derivation already shows is the
  wrong schedule for this fixed-`L` target) — `m_n` is an independent design variable of
  this study, held fixed within each configuration.
- **Estimand (Q1):** population excess risk `R(f̂) − R(f*)`, where `f* :=
  argmin_{τ ∈ 𝒯_L} R(τ)` is the population-best axis-aligned tree with at most `L` leaves
  and depth at most `D` over the FULL continuum threshold class (no grid restriction),
  and `f̂` is the fitted (Stage A + Stage B) tree at a given `(m_n, n)`. Under squared
  error, `R(f)−R(g) = E[(γ₀(X)−f(X))²] − E[(γ₀(X)−g(X))²]` for any two functions `f,g`
  (orthogonality of the conditional-mean residual), so this reduces to a difference of
  two mean-squared-deviation-from-truth terms, both computable once `γ₀` and `f*` are
  known exactly (Section 3).
- **Estimand (Q2):** for a boundary `b` of `f*`'s partition, `|t̂_b − t*_b|`, the absolute
  distance between the Stage-B-refined threshold at the corresponding node of the fitted
  tree and `f*`'s population-optimal threshold at that boundary, as a function of `n`,
  compared separately for boundaries built to be genuine jumps in `γ₀` versus boundaries
  built to be purely budget-forced ("cosmetic").
- **What is NOT estimated here:** no causal parameter, no nuisance-function target, no
  claim about recovering `γ₀` itself or its literal topology. `f*` is the object of
  interest throughout, exactly as the prior-session reframe specifies.

## 3. Data generating process

**Design.** `p = 2` continuous covariates, `X = (X1, X2)`, i.i.d. `Uniform(0,1)`
(independent, compact, full-dimensional support — the DGP does not need the original
paper's global-density-floor or support-geometry assumptions, but a clean, well-behaved
design keeps the grid-mesh arithmetic interpretable and matches the one case the
original theory's own uniqueness result could actually prove things about).
`L = 4` leaves, `D = 2` (a full depth-2 binary tree), fixed for every configuration in
this study.

**True regression function**, constructed so its population-best 4-leaf tree
approximation `f*` has *by design* the topology "split `X1` at `t1`, then split `X2`
within each side" — one genuine jump (the `X1` split) and two cosmetic splits (the `X2`
splits, one per side, deliberately given different curvature so they land at two
different, non-symmetric locations — a matched-but-not-identical replicate pair):

```
γ₀(x1, x2) =
  a_L + b_L · x2²                           if x1 <= t1   (LEFT side, smooth in x2)
  (a_L+Δ1) + b_R · (1-x2)²                  if x1 >  t1   (RIGHT side, smooth in x2,
                                                            MIRRORED quadratic, jump of
                                                            size Δ1+b_R·(1-2x2) at x1=t1)
```

with `t1 = 0.5`, `a_L = 0`, `b_L = b_R = 3`, and `Δ1` the jump-size design parameter
(favorable regime `Δ1 = 5`; weak-separation stress regime shrinks this — see
Section 4). `Y = γ₀(X1,X2) + ε`, `ε ~ N(0, σ²)`, `σ = 0.5` (favorable regime).

**Revision note (2026-09-28, post-pilot-implementation):** the original version of
this DGP used `b_R · x2²` (not mirrored) on the right side with `b_R = -3`. A
same-day implementation pass, gated exactly by this section's own mandatory
topology-verification step, caught that this original form fails on three
compounding, provable grounds before a single replication was run: (1) `b_L+b_R=0`
makes the right piece literally constant in `x2` (`2 + 0·x2²`), so `t2*_R` is not
identified at all; (2) the resulting jump `Δ1+b_R·x2² = 2−3x2²` crosses zero inside
`[0,1]` (at `x2=√(2/3)`), so `γ₀` is not actually discontinuous across `x1=t1`
everywhere — undermining this section's own "exact by construction" claim for
`t1*`; (3) for `g(x2)=a+b·x2²` under `Uniform(0,1)`, the population-optimal single
cutpoint is `t* = (1+√17)/8` **for every** `(a,b)` — verified numerically over 20
`(a,b)` pairs, spread `6.4e-07` — so no re-tuning of `(a_L,b_L,b_R,Δ1)` within that
functional family could ever separate the left and right cosmetic thresholds. An
exhaustive brute-force enumeration of all 8 depth-2 `(x1,x2)` shapes confirmed the
originally-constructed topology was not merely tied but strictly dominated (loses
by 22.6% of its own population risk). The mirrored form above clears the same
gate with margin `+5.72e-02` (constructed-topology risk strictly below every one of
the 7 alternative shapes, verified both in closed form and against a 1e7-draw Monte
Carlo integration) and gives `t2*_L ≠ t2*_R` by construction (see below), with the
jump `Δ1 + b_R·(1−2x2)` bounded away from zero on `[0,1]` **iff `Δ1 > b_R`** — a
condition the favorable regime satisfies with room (`Δ1=5 > b_R=3`) and Regime S1
(Section 4) is revised to cross deliberately, as its most severe stress point.

**Ground truth, and how it is computed to negligible error** (per this design's
constraint that `f*` is not available in closed form for an arbitrary `γ₀`, so the
constraint is met by construction here, not assumed away):

- `t1* = t1 = 0.5` **exactly, by construction, no estimation needed.** For a true
  single discontinuity under a continuous design density with a continuous function on
  each side, the population-risk-minimizing single-threshold split is exactly the true
  break point — standard in the change-point literature the original paper itself cited
  (`chan1993`, `seijosen2011`). This is the JUMP boundary's ground truth.
- `t2*_L, t2*_R` (the COSMETIC boundaries): for a smooth curve `g(x2) = a + b·x2²` on
  `[0,1]` under `Uniform(0,1)`, the population-SSE-optimal single cutpoint `t` for a
  2-piece constant approximation has closed-form leaf means `c1(t) = (1/t)∫₀ᵗ g`,
  `c2(t) = (1/(1−t))∫ₜ¹ g` (both closed-form polynomials in `t` for a quadratic `g`),
  giving an SSE objective `S(t)` that is itself a closed-form rational/polynomial
  function of `t`. **Verified fact used in this design** (numerically confirmed over 20
  `(a,b)` pairs, spread `6.4e-07`): the maximizer of `S(t)`'s explained-variance term is
  `t* = (1+√17)/8 ≈ 0.6403882` for EVERY `(a,b)`, independent of both — which is exactly
  why `t2*_L` and `t2*_R` cannot be made distinct within a single non-mirrored quadratic
  family (see the revision note above), and why the right side is mirrored in `x2`:
  `t2*_L = (1+√17)/8 ≈ 0.6403882` (left side, `g(x2)=a_L+b_L·x2²`); `t2*_R = 1 −
  (1+√17)/8 ≈ 0.3596118` (right side, `g(x2)=(a_L+Δ1)+b_R·(1−x2)²`, same maximizer
  formula applied to the mirrored coordinate `1−x2`). Both obtained by 1-D numerical
  minimization of `S(t)` (`optimize()`); note `optimize()` cannot resolve an interior
  smooth minimum below roughly `1.5e-08` regardless of a smaller requested `tol`, which
  remains four orders of magnitude below any statistical effect size this study
  measures (`O(n^{-1/2})` at the smallest `n = 500` is already `~4×10⁻²`) — negligible-
  error by construction, not by assumption, but the achieved precision is `~1.5e-08`,
  not the literal `tol` value requested.

- `f*`: the 4-leaf tree with topology (`X1` at `t1*`; `X2` at `t2*_L` on the left, `t2*_R`
  on the right) and leaf means equal to the exact population conditional mean of `γ₀`
  over each leaf rectangle (closed-form polynomial integrals, since every piece of `γ₀`
  is a known polynomial).
- **Topology verification (mandatory, one-time per tested `Δ1`, not per replication):**
  before running any replication at a given `Δ1`, numerically verify that the
  constructed `f*` is *actually* the population-best 4-leaf tree by exhaustive
  enumeration of all 8 depth-2 `(X1,X2)` shapes (not a spot-check against one or two
  named alternatives — an earlier pass of this design checked only two alternatives,
  which would have reported a false tie instead of catching that the originally-drafted
  DGP's constructed topology was strictly dominated; see the revision note above), each
  minimized over its own cutpoints, compared in closed form and cross-checked against a
  large (>=1e6) Monte Carlo sample. This is expected to hold with comfortable margin at
  the favorable-regime `Δ1 = 5` (verified margin `+5.72e-02`), and is the exact quantity
  that can plausibly fail as `Δ1` shrinks in the weak-separation stress regime
  (Section 4) — if it fails, `f*`, `t1*`/`t2*`, and this study's ground truth must be
  recomputed for that `Δ1`, not silently reused from the favorable regime.
- **Excess-risk evaluation:** a single fixed Monte Carlo sample of `X`,
  `N_MC = 10⁷` i.i.d. draws (seeded once, reused unchanged across every replication and
  configuration in this study, so MC-integration noise is a constant, documented,
  negligible (`~3×10⁻⁴` relative) offset rather than an added source of across-config
  variance). `γ₀` is evaluated on it once (exact, closed form); for each replication's
  fitted `f̂`, `E[(γ₀−f̂)²]` and `E[(γ₀−f*)²]` are both estimated on this same sample, and
  their difference is the reported excess risk for that replication. **Verification
  tolerance, stated in the right units:** each leaf mean of `f*`'s own closed form
  should agree with an independent `N_MC = 10⁷` Monte Carlo estimate to within its own
  Monte Carlo standard error (observed: `0.79` s.e. units) — an absolute tolerance
  (e.g. `1e-4`) is the wrong check here, since a leaf mean's own MC s.e. at
  `N_MC = 10⁷` is itself `~2–4×10⁻⁴`, i.e. *below* a naive `1e-4` absolute gate before
  any error exists at all. The closed form is separately verified essentially exactly
  (`~4×10⁻¹⁶`) against nested numerical integration (`integrate()`), which is the
  authoritative check; the MC comparison is a sanity check on the MC harness itself, not
  on the closed form.

## 4. Regimes

### Favorable
- **Regime F (baseline):** as specified above — `Δ1 = 5`, `σ = 0.5`, `p = 2`. Both
  boundary types cleanly present; topology verification holds with margin `+5.72e-02`
  (verified; see the revision note in Section 3).

### Stress (at least one, always)

- **Regime S1 — weak separation.** `Δ1` shrunk toward `b_R = 3` (tested values:
  `Δ1 ∈ {4.5, 3.5, 2.5}`, holding `σ = 0.5`, `b_R = 3` fixed, so the jump-to-noise ratio
  AND the jump's uniformity across `x2` are both weakening as `Δ1` falls). The first
  two values (`4.5, 3.5`) keep `Δ1 > b_R`, so the jump `Δ1+b_R(1−2x2)` stays bounded
  away from zero on `[0,1]` (a shrinking-but-still-uniform jump, decreasing margin);
  the third (`2.5 < b_R=3`) crosses that boundary deliberately, so the jump vanishes
  and reverses sign at some `x2 ∈ (0,1)` — the single most severe stress point in this
  regime, and exactly the condition Section 3's mandatory per-`Δ1` topology
  re-verification exists to catch: `f*`'s topology itself, not just its margin, is
  permitted to change at `Δ1 = 2.5`, and must be recomputed, not assumed inherited from
  the milder settings. **Why the method should struggle here:** the entire prior
  theoretical program's global-density-floor machinery existed to guarantee a topology
  margin *uniformly*, however small the true separation; the reframe deliberately drops
  that guarantee and instead only claims consistency toward whatever `f*` the ERM
  actually converges to. This regime tests what actually happens as separation weakens —
  whether topology recovery degrades gracefully (a `P(topology match)` curve that falls
  but excess risk still behaves as Q1 predicts *given* the resulting `f*`) or whether it
  destabilizes the whole measurement (via the topology-verification step: does `f*`
  itself change shape as `Δ1` shrinks?). This is exactly the boundary condition the
  original theory's exclusion of "small" separations was trying to handle by assumption
  rather than by measurement.

- **Regime S2 — higher dimension.** `p ∈ {5, 8}` (vs. the baseline `p = 2`), adding
  `p − 2` pure-noise `Uniform(0,1)` coordinates that `γ₀` does not depend on at all, `L`
  and `D` unchanged. **Why the method should struggle here:** the estimation term's
  constant grows with `p` (VC dimension of `L`-leaf axis-aligned trees over `p`
  coordinates), and the number of candidate binary features after discretization grows
  as `p·(m_n − 1)` (confirmed this session by reading `fit_tree.R`'s own dimensionality
  heuristic) — so the practical `m_n^†(n)` "grid becomes free" threshold from Q1 may
  shift with `p` in a way the baseline regime cannot reveal. This tests whether the
  headline "how coarse can the grid be" answer is dimension-dependent, which matters
  directly for how that answer would be stated in a methods paper.

Every regime is reported in Section 9's output regardless of outcome — a design that
struggles in S1/S2 and is not reported honestly there is a silent fallback in reporting
(Constitution #1), not a clean result.

## 5. Metrics

**Q1 (excess risk vs. grid), per `(regime, m_n, n)` configuration:**
- Mean excess risk across replications, with its Monte Carlo standard error (over
  replications, not to be confused with the fixed-evaluation-sample MC noise in
  Section 3, which is a separate, smaller, documented quantity).
  *Misleading if:* replications where `bisect_lambda_to_budget()` returns
  `certified == FALSE` or `feasible == FALSE` are silently dropped rather than reported
  — report the certified/feasible RATE per configuration as its own column, always,
  even where it is 100%.
- `P(fitted topology == f*'s topology)` per configuration (diagnostic, not gating —
  the reframe explicitly does not require this to hold; seeing it fail is informative,
  not a defect in the study).
- `m_n^†(n)`: smallest tested `m_n` at which mean excess risk is within 10% (relative) of
  mean excess risk at the finest tested `m_n` for that `(regime, n)`, for every
  `m_n' >= m_n` also tested. *Misleading if:* computed from a single point rather than
  checked to hold at every finer grid point tested (a "flat then rising again" pattern,
  e.g. from numerical instability at very fine grids, would otherwise be missed).

**Q2 (threshold rate), per `(regime, n, boundary type ∈ {jump, cosmetic-left,
cosmetic-right})`:**
- Median absolute threshold error across replications where that boundary is present in
  the fitted topology (see exclusion-rate point below).
- **Exclusion rate**: the fraction of replications where the fitted topology does not
  contain the node in question (so "threshold error" is undefined there) — reported
  explicitly per configuration, never silently folded into the median. A rising
  exclusion rate at small `n` is itself a finding (topology-recovery difficulty), not
  noise to discard.
- **Stage-B diagnostic rates**: fraction of replications where `refine_tree_cuts()`'s
  `reason` column at that node is anything other than `"refined"` (i.e.
  `"no_candidates_in_bracket"`, `"min_leaf_n_infeasible"`, `"too_few_rows_at_node"`),
  reported per configuration. *Why this matters here specifically:* this session's own
  Stage-B units-bug fix means a high rate of non-`"refined"` outcomes would point at an
  implementation limitation, not a genuine statistical rate — exactly the class of
  confound this study must rule out before reading a slope as a theory result.
- Fitted slope `b` from OLS of `log(median abs. error)` on `log(n)` across the 6-point
  `n` grid (Section 8), per `(regime, boundary type)`, with its standard error.

## 6. Success criteria

Using this study's own pre-committed tolerances (stated explicitly because they are
method-specific rate targets, not point estimates/SEs/coverage/p-values in the
Constitution #10 sense — no override of #10 is being claimed; #10's tolerances simply do
not apply to a slope-recovery criterion):

- **SC1 (Q1, Regime F):** for every tested `n`, there exists `m_n^†(n)` in the tested
  grid `{4,8,16,32,64,128,256}` satisfying the flatness definition in Section 5.
- **SC2 (Q1, Regime F, rate check):** the log-log slope of `m_n^†(n)` regressed on `n`
  across the 6 tested `n` values has a 95% CI that does not exclude `0.5` (consistent
  with, not required to hit exactly, the `O(√n)` prediction).
- **SC3 (Q2, Regime F):** the fitted slope for the JUMP boundary has a 95% CI that
  includes `−1` and excludes `−0.5`; the fitted slope for EACH cosmetic boundary
  (left and right, checked separately, not pooled, since they are only a matched — not
  independent — replicate pair) has a 95% CI that includes `−0.5` and excludes `−1`.
- **SC4 (diagnostic, not gating):** `P(topology match)` and Stage-B `reason` rates are
  reported at every configuration; no numeric threshold is pre-committed for these,
  consistent with their role as confound-detectors rather than pass/fail targets.

## 7. Decision rule

Written before any configuration has been run.

- **If SC1+SC2+SC3 all hold:** proceed toward the grid-tradeoff methods paper (option
  (a) from the prior session's framing) as the primary, well-supported deliverable, AND
  treat the local-margin superconsistency conjecture (Oracle's option (iv)) as now
  empirically supported enough to justify the ~1–2 day theoretical verification effort
  Oracle scoped for it — pursued as a secondary, non-blocking addition to (a), not a
  precondition for shipping it.
- **If SC1+SC2 hold but SC3 fails:** proceed with (a) ONLY. Explicitly drop the
  local-margin superconsistency angle as empirically unsupported; do not spend further
  theoretical effort trying to prove a conjecture this study's own data contradicts.
- **If SC1/SC2 fail but SC3 holds:** before concluding the estimation-term/VC argument
  itself is wrong, check the mundane causes FIRST, in this order: (i) the certified/
  feasible rate from Q1's metrics — an uncertified fit distorts excess risk for reasons
  unrelated to grid resolution; (ii) the topology-match rate at the SAME configurations —
  a topology mismatch inflates excess risk independent of grid coarseness. If both check
  out clean and the flat-past-some-`m_n` pattern still does not appear, escalate to
  Oracle with this concrete evidence before revising the decomposition — do not revise
  the theory on an unaudited simulation result.
- **If SC1/SC2 hold but `m_n^†(n)` grows visibly faster than `√n`** (SC2's CI excludes
  `0.5`, biased toward faster growth) while still eventually flattening: still usable for
  (a), but the headline "how coarse can the grid be" claim is materially weaker — report
  the actual observed growth rate honestly; this is not an escalation trigger, just a
  less favorable finding to be stated as such.
- **If SC1/SC2 AND SC3 both fail in Regime F:** the strongest signal toward option (c)
  — document both negative findings precisely, get one more Oracle sanity check on the
  evidence specifically (not a re-litigation of the whole program), then default to
  shipping the two-stage algorithm as unguaranteed-but-documented software with an
  explicit negative-results note, per `story.md`'s own third listed option.
- **Regardless of outcome:** report the full excess-risk-vs-`(m_n,n)` surface and the
  full threshold-error-vs-`n` curves (both boundary types) for EVERY regime tested
  (F, S1, S2), including certified/feasible rates, topology-match rates, and Stage-B
  `reason` breakdowns at every configuration. No regime and no configuration is dropped
  from the report because it is inconvenient or because the headline decision did not
  need it.

## 8. Replications

**Grids:**
- `n ∈ {500, 1000, 2000, 4000, 8000, 16000}` (6 points, doubling — matches the scale of
  doubletree's own `remainder_terms_value_recovery` study, so per-fit cost at comparable
  `n` is already roughly known from this session's `fit_tree.R`/Stage-B verification
  work).
- `m_n ∈ {4, 8, 16, 32, 64, 128, 256}` (7 points, doubling; `256` dropped from the two
  stress regimes to control cost, since `√16000 ≈ 126` already brackets the theoretical
  "grid-free" point for the largest `n` at `m_n = 128`).

**Configuration counts and replications per configuration:**

**Resourcing revision (2026-09-28, post-pilot-timing):** the pilot (20-50 reps assumed
above) was run at 10 reps/config across the full grid before committing further, per
the staging plan below, and found per-fit cost 2-3 orders of magnitude over this
section's original assumption — mean 71.5s/call, up to 771s at the single worst cell
(`n=16000, m_n=128`), Stage A's branch-and-bound search being ~97% of it and scaling
superlinearly in both `m_n` and `n`. `model_limit=0` was tested as the one plausible
one-argument fix and ruled out with a bit-identical correctness check (405/405 fields
matched; 1.01-1.04x timing difference, indistinguishable from run-to-run noise) — the
cost is structural to the branch-and-bound search over `p·(m_n-1)` binary features at
fine grids, not a solver configuration issue. Extrapolating the table below at `m_n`
up to 256 would cost on the order of **10,000+ CPU-hours**, not feasible without a
dedicated cluster commitment decided on its own merits.

Decision taken: cap `m_n` at **64** everywhere (dropping 128 and 256 from every grid
below) and run **Regime F only, to full replication (500 reps/config)**, before
deciding whether Regime S1/S2 or a cluster run are needed at all — deferred, not
abandoned. This is itself a real design change (the finest grid points, where the
`m_n^†(n)` flattening claim is most directly tested, are the ones being dropped), so
Q1/Q2-F's results under this resized grid must be read with that in mind: a "flat by
`m_n=64`" finding is informative; a "still declining at `m_n=64`" finding does NOT rule
out flattening beyond 64, it only means this resized run cannot see it, and 128/256
would need to be revisited specifically (most likely on a cluster) before that stronger
claim could be made.

| Sub-study | Regime | Grid | Configs | Reps/config | Fits |
|---|---|---|---|---|---|
| Q1 | F | `n`(6) × `m_n`(5: `{4,8,16,32,64}`) | 30 | 500 | 15,000 |
| Q2 | F | `n`(6) × `m_n`(3: `{16,32,64}`, informed by Q1's own `m_n^†` finding once available) | 18 | 750 | 13,500 |

Deferred pending Regime F's result (grids as originally specified, `m_n` ceiling to be
revisited before running):

| Sub-study | Regime | Grid | Configs | Reps/config | Fits |
|---|---|---|---|---|---|
| Q1 | S1 (`Δ1∈{4.5,3.5,2.5}`) | `n`(6) × `m_n`(6, drop 256) × `Δ1`(3) | 108 | 300 | 32,400 |
| Q1 | S2 (`p∈{5,8}`) | `n`(6) × `m_n`(6, drop 256) × `p`(2) | 72 | 300 | 21,600 |
| Q2 | S1 (one `Δ1`, most stressed: `2.5`) | `n`(6) × `m_n`(1, Q1-informed) | 6 | 300 | 1,800 |

Original (pre-resourcing-revision) total ≈ **90,300** individual two-stage fits;
current committed total (Regime F only, resized grid) = **28,500**. `L=4, D=2` is small
relative to the solver-timing evidence already gathered this session (`L=30` fits at
`n=2000` completed in ~35s post-fix), which is what motivated the original "well under
a second to a few seconds" assumption below — that assumption is now known to be wrong
by measurement, not superseded by a better argument, so it is left visible rather than
silently deleted:

~~per-fit cost should be well under a second to a few seconds even at the largest
`(n, m_n, p)` combinations~~ — **measured false; see the resourcing revision above.**
Cost is explicitly **uneven** across the grid (grows with `n`, `m_n`, and `p`).

**Staging actually followed:**
1. **Pilot**, 10 replications per configuration, full original grid (`m_n` up to 128,
   dropping 256 for time) — completed 2026-09-28. Confirmed the ground-truth/topology-
   verification computation runs correctly and produces sane `f*`/`t*` values;
   produced the timing surface that triggered the resourcing revision above; too
   underpowered (s.e. of the same order as the mean in every Q1 cell at 10 reps) for
   any read on SC1-SC3, as expected at this replication count.
2. **Regime F, full replication, resized grid** — in progress, this is the current
   committed run (see table above).
3. **Regime S1/S2, and any cluster commitment** — deferred until step 2's result is in.

## 9. Reproducibility

- `set.seed()` once at the top of each script, `YYYYMMDD` form (`.claude/rules/
  r-code-conventions.md` §1), never inside a loop.
- Seeds derived per `(configuration, replication)` deterministically (e.g. a fixed hash
  of `(regime, n, m_n or p or Δ1, replication index)`), so re-ordering or re-running a
  subset of configurations cannot change any individual replication's data.
- The `N_MC = 10⁷` excess-risk evaluation sample (Section 3) uses its own single fixed
  seed, drawn once and reused verbatim across every replication and configuration in
  this study — not re-drawn per configuration, so MC-integration noise cannot
  differentially affect one config's comparison over another's.
- DGP implementation should follow `simulations/dgps.R`'s existing
  `dgp_<name>(n, p, seed) -> list(X, y, truth, ...)` convention for consistency with
  other simulations in this repository, extended with the closed-form `f*`/`t*`/leaf-mean
  fields this design requires as part of `truth`.

## 10. Notes

- **Post-approval DGP revision (2026-09-28):** the DGP in Section 3 was revised the
  same day it was approved, during the pilot implementation's own mandatory
  topology-verification gate (Section 3), before any replication was run — see that
  section's revision note for the full mathematical detail. Recorded here as the
  intended behavior of that gate, not a process failure: an approved spec's ground
  truth is still checked against brute-force numerics before being trusted, exactly
  because the prior theoretical program's core failure mode was committing to a claim
  before it was ever run against data (`story.md`'s own diagnosis).
- **Relationship to `doubletree`:** this study is deliberately narrower than what
  `doubletree` explored and shelved (a first-order bias rate for a downstream causal
  parameter). Nothing here targets a causal estimand, a nuisance function, or a bias
  rate relative to a downstream parameter — only the tree-fitting method's own
  prediction risk and threshold-estimation behavior. Do not let a future revision of
  this study import that framing; if a causal question arises from these results, it
  belongs in `doubletree`, not here (per this session's own Oracle finding on that exact
  boundary).
- **Known limitation:** the DGP's smooth pieces are quadratic specifically because a
  quadratic's optimal step-approximation objective has a closed-form-integral SSE
  function amenable to precise 1-D numerical minimization. This buys negligible-error
  ground truth cheaply but does not generalize the finding to arbitrarily-shaped smooth
  functions; a future study could vary the smooth functional form as an additional
  stress regime if S1/S2's results here warrant it.
- **Known limitation:** `p = 2` in the favorable/Q2 regimes and `p ∈ {5,8}` only in S2
  means Q2's threshold-rate question is never tested at high dimension in this design —
  a deliberate scope cut to keep the replication table above tractable; flagged, not
  silently absent.
- **Not tested here:** classification / log-loss. The reframe and this design are
  squared-error-only, matching `bisect_lambda_to_budget()`'s current scope
  (`fit_twostage.R`'s own roxygen already discloses this as regression-only).
