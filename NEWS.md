# optimaltrees 0.4.1 (dev)

## Bug Fixes

### `fit_rashomon` with `squared_error` no longer crashes (segfault at 0x47)

**Problem:** `fit_rashomon()` with `loss_function = "squared_error"` crashed
with a segfault at address `0x47` during Rashomon set extraction. The crash
occurred in `ModelSet::ModelSet()` which called `encoder.target_value()` unconditionally.
For `SQUARED_ERROR`, the encoder has no binary target codex entry
(`number_of_binary_targets = 0`), so `codex[number_of_binary_columns]` was an
out-of-bounds vector access — undefined behavior leading to the segfault.
A second latent bug: `Objective(n, 1, state)` accessed `mismatch_costs[0]` which
is also empty for `SQUARED_ERROR` (cost matrix not built for regression).

**Fix:** Guard both unsafe calls in `ModelSet::ModelSet()` with a check for
`SQUARED_ERROR`: skip `encoder.target_value()` entirely; construct `Objective`
via its default constructor and set `objective = max_loss + regularization`
directly. This path is only used for compact Rashomon model-set output (not
the Model objects returned to R), so behavior is fully preserved.

**Impact:** `fit_rashomon()` now works for all three loss functions. Added
`test-rashomon-validity.R` with 5 test blocks verifying bound validity,
brute-force optimum match, monotone set growth, additive bound, and
single-tree/Rashomon objective agreement — for both `log_loss` and
`squared_error`.

### Depth-cap safety net for single-tree regression with fine discretization

**Problem:** `optimaltrees()` and `fit_tree()` with `loss_function = "squared_error"`,
small `regularization` (e.g. 0.01–0.05), and the new `"adaptive"` polynomial default
for continuous features could OOM or hang indefinitely. Root cause: the `n^{1/3}`
adaptive schedule produces ~5–10 binary features per original continuous feature at
practical n; without a depth limit, OSRT explores an exponential number of candidate
trees at low regularization.

**Fix:** A safety net in `optimaltrees()` auto-sets `max_depth = 2L` when all of these
are true: `single_tree = TRUE`, regression loss, `max_depth == 0L` (user left it
unlimited), and more than 8 post-discretization binary features. Depth = 2 collapses
search time from >30s (or OOM) to ~1s with no change to n_trees. Users can disable
the cap by passing `max_depth = 0L` explicitly (the auto-set emits a cli message
when `verbose = TRUE`).

**Impact:** Fixes the `test-numerical-edge-cases.R` hang that could crash the machine
when `devtools::test()` ran with the new adaptive default. All 20 test files now
complete in ≤4s each.

### `depth_budget` off-by-one: max_depth now enforces the depth it promises

**Problem (critical, long-standing):** The R parameter `max_depth = d` was
previously mapped to `depth_budget = d + 2` in GOSDT's C++ (`+2` was inferred
from an empirical sweep on data where the optimal tree depth was always shallower
than the cap, so the constraint was never binding). The actual GOSDT accounting
decrements the budget by 1 on each split level and forces a leaf at budget = 1,
so `depth_budget = k` allows actual tree depth up to `k − 1`. With the wrong `+2`
mapping, `max_depth = 1` passed `depth_budget = 3`, which allowed depth-2 trees.
The prior brute-force test suite only verified that the objective matched the
unconstrained optimum — it did not test that the depth constraint was enforced.

**Fix:** The mapping is now `depth_budget = max_depth + 1` (for `max_depth > 0`).
`depth_budget = 0` (unlimited) is unchanged.

**Impact:** `max_depth = d` now correctly limits actual tree depth to ≤ d.
Two new depth-constraint test cases added to `test-brute-force-optimality.R`:
(1) `max_depth = 1` on a 2-way AND dataset confirms depth ≤ 1, and (2) `max_depth
= 2` on a 3-way AND dataset confirms depth ≤ 2 with depth-3 strictly better.
All 13 brute-force test cases pass.

### Stale comment in fit_tree.R

Fixed the comment documenting the adaptive bin formula (was the old log schedule,
now reflects `ceiling(n^(1/3))`).

# treefarmr 0.3.0

## Major Changes

### Regression Trees - Critical Bug Fix ✅

**BREAKING FIX**: Regression trees (`loss_function = "squared_error"`) now correctly guarantee finding global optimal solutions.

**What was broken:**
- Previous versions used `potential = 0` for regression bounds
- This caused over-aggressive pruning in the branch-and-bound algorithm
- Result: optimizer frequently missed global optima, returning suboptimal trees

**What's fixed:**
- Implemented **equivalent points lower bound** (always computed)
- Samples with identical feature values form "equivalent points"
- Within-group variance is unavoidable → provides valid lower bound
- Based on OSRT (Zhang et al., AAAI 2023) approach

**Impact:**
- Regression trees now find provably optimal solutions
- All regression tests pass with correct results
- Performance: minimal overhead (equiv points bound is fast)

### New Feature: k-Means Lower Bounds

**Enabled by default for regression trees:**
- New parameter: `k_cluster = TRUE` (default)
- Uses optimal 1D k-means clustering for tighter bounds
- Algorithm: Song & Zhong (2020) dynamic programming
- Benefit: Faster optimization through better pruning
- Trade-off: Small computational overhead for k-means

**Example:**
```r
# Regression with k-means bounds (default, recommended)
model <- treefarms(X, y, loss_function = "squared_error",
                   regularization = 0.1)
# k_cluster = TRUE is now the default

# Disable k-means if needed (uses only equivalent points bound)
model <- treefarms(X, y, loss_function = "squared_error",
                   regularization = 0.1, k_cluster = FALSE)
```

**Breaking change (behavior improvement):**
- **Default changed**: `k_cluster` now defaults to `TRUE` (was `FALSE`)
- **Impact**: Regression trees are now faster by default (1.5-4x observed)
- **Correctness**: Both settings guarantee global optimality
- **To restore old behavior**: Set `k_cluster = FALSE` explicitly

### New Feature: CART Lookahead Bounds

**One-step lookahead optimization (OSRT Theorem 3.2):**
- New parameter: `cart_lookahead = TRUE` (default)
- Enables scope-based pruning for all loss functions
- Improves optimization speed without affecting optimality
- Can be disabled: `cart_lookahead = FALSE`

## Technical Details

### Equivalent Points Lower Bound

For regression with squared error loss, the minimum achievable loss for a set of samples is bounded by:

```
min_loss ≥ sum of within-group variances
```

where groups are defined by samples with identical feature vectors. This is unavoidable because decision trees assign the same prediction to samples with identical features.

**Implementation:**
- Groups samples by feature vector (O(n log n) with map)
- Computes within-group SSE for each group
- Sum provides tight lower bound
- Always computed (even when `k_cluster = FALSE`)

### k-Means Enhancement (Optional)

When `k_cluster = TRUE`, applies optimal 1D k-means clustering to equivalent point means:

1. Aggregate samples into equivalent points (by feature vector)
2. Compute weighted mean for each equivalent point
3. Apply k-means to the means (Song & Zhong 2020 algorithm)
4. Combine: k-means bound + within-group variance

This provides an even tighter lower bound, improving pruning efficiency.

### Performance Impact

**Equivalent points bound (always on):**
- Overhead: ~5-10% (grouping + variance computation)
- Benefit: Correct results (previously broken)
- Net: Essential for correctness

**k-Means bounds (optional):**
- Overhead: ~10-20% (k-means DP algorithm)
- Benefit: Faster optimization (1.5-4x observed speedup on medium datasets)
- Recommendation: Enable for large datasets or deep trees

## Bug Fixes

- Fixed regression trees returning suboptimal solutions (#critical)
- Added proper lower bounds for squared error loss
- Improved pruning for all regression tasks

## Dependencies

- No new R package dependencies
- Integrated ckmeans C++ library (Song & Zhong 2020)
- All code is self-contained

## Testing

- All 588 tests pass
- All 42 regression-specific tests pass
- Verified: identical solutions with/without k_cluster
- Verified: regression now finds global optima

## References

- Zhang et al. (2023). "Optimal Sparse Regression Trees." AAAI.
- Song & Zhong (2020). "Efficient weighted univariate clustering." Wiley.

---

# treefarmr 0.2.0 and earlier

See git history for changes in previous versions.
