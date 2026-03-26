# Test Suite Consolidation Plan

## Current State
- **995 total tests** across 20+ files
- **7,856 lines** of test code
- Many redundant and over-defensive tests
- Test suite takes too long to run

## Target State
- **~250 focused tests** across 8-10 files
- **~2,000 lines** of test code
- Fast, maintainable, covers critical paths
- Each test adds unique value

---

## File-by-File Analysis and Consolidation Strategy

### KEEP & CONSOLIDATE (Core Functionality)

#### 1. **test-core-fitting.R** (NEW - consolidate from 4 files)
**Target: 30-40 tests**

Consolidate from:
- test-fit-functions.R (24 tests → 8 tests)
- test-optimaltrees.R (13 tests → 8 tests)
- test-api.R (15 tests → 10 tests)
- test-discretization.R (21 tests → 10 tests)

**What to keep:**
- Basic fit with each loss function (misclassification, log_loss, squared_error)
- Single tree vs Rashomon set
- Regularization parameter effects
- Discretization of continuous features (basic test only)
- Store/don't store training data
- Worker limits (1 core vs multi-core, one test each)

**What to remove:**
- Repetitive parameter validation (test once, not per function)
- Edge cases better suited for test-edge-cases.R
- Extensive discretization testing (move to integration test)

---

#### 2. **test-prediction.R** (CONSOLIDATE)
**Current: test-predict.R (12 tests)**
**Target: 10 tests**

**What to keep:**
- Predict class/prob for classification
- Predict response for regression
- New data validation (basic)
- Fold-specific predictions for cross-fitted models

**What to remove:**
- Repetitive validation tests
- Edge cases (move to test-edge-cases.R)

---

#### 3. **test-rashomon.R** (NEW - consolidate from 3 files)
**Target: 20 tests**

Consolidate from:
- test-cross-fitted-rashomon.R (19 tests → 10 tests)
- test-rashomon-utils.R (15 tests → 8 tests)
- test-auto-tune-rashomon.R (parts)

**What to keep:**
- Cross-fitted Rashomon basic workflow
- Intersection finding (one good test)
- get_rashomon_trees(), refit_structure_on_data()
- Structure validation utilities

**What to remove:**
- Repetitive K-fold variations
- Extensive auto-tuning (move to test-auto-tuning.R)
- Over-testing of utility functions

---

#### 4. **test-auto-tuning.R** (CONSOLIDATE)
**Current: 11 tests**
**Target: 8 tests**

**What to keep:**
- Basic auto-tuning workflow
- Fixed regularization vs fixed rashomon_bound_multiplier
- Convergence behavior (one test)
- Input validation (one test)

**What to remove:**
- Repetitive parameter range tests
- Verbose output testing (not critical)
- Edge cases (if auto-tuning fails, it fails - that's OK)

---

#### 5. **test-probabilities.R** (CONSOLIDATE HEAVILY)
**Current: test-probabilities.R (16 tests) + test-probability-accuracy.R (10 tests) + test-comprehensive-logloss.R (large)**
**Target: 15 tests total**

**What to keep:**
- Probabilities sum to 1 (one test per loss function)
- Probability bounds [0,1]
- Log-loss probabilities bounded away from 0/1
- Lazy evaluation works
- Basic accuracy verification

**What to remove:**
- test-comprehensive-logloss.R (703 lines) - DELETE ENTIRE FILE
  - This is runtime benchmarking disguised as testing
  - Belongs in `inst/benchmarks/`, not tests
- test-probability-accuracy.R - CONSOLIDATE INTO test-probabilities.R
- Extensive ground-truth probability testing (one good test is enough)

---

#### 6. **test-s3-methods.R** (CONSOLIDATE)
**Current: 14 tests**
**Target: 8 tests**

**What to keep:**
- print.OptimalTreesModel shows key info
- summary.OptimalTreesModel returns list with expected fields
- predict.OptimalTreesModel works (basic)
- print/summary for cf_rashomon

**What to remove:**
- Excessive edge case testing
- Tests that duplicate prediction tests
- Auto-tuning integration tests (belong in test-auto-tuning.R)

---

### CONSOLIDATE HEAVILY

#### 7. **test-edge-cases.R** (SLIM DOWN)
**Current: 25 tests, 559 lines**
**Target: 8 tests, ~150 lines**

**What to keep:**
- Empty dataset → error
- Single observation → error or warning
- Perfect separation → graceful handling
- All features constant → error
- Invalid loss function → error
- Negative regularization → error

**What to remove:**
- Variations on the same theme
- "Extreme but valid" parameters (trust the algorithm)
- Over-defensive edge cases

---

#### 8. **test-parallel.R** (SLIM DOWN)
**Current: 14 tests, 345 lines**
**Target: 5 tests, ~100 lines**

**What to keep:**
- worker_limit=1 vs worker_limit=2 produces same results
- Invalid worker_limit → error
- Parallel consistency (one good stress test)

**What to remove:**
- Extensive worker limit variations
- Performance timing (not a test concern)
- Repetitive consistency checks

---

### DELETE OR MOVE

#### 9. **test-cleanup.R** (DELETE OR MOVE)
**Current: 14 tests, 381 lines**
**Action: DELETE most, keep 2 tests**

**Why delete:**
- Memory cleanup is R's responsibility
- Package unloading breaks test suite
- Over-defensive paranoia

**Keep only:**
- Basic .onUnload hook doesn't crash (skip in full suite)
- Temp files cleaned up (one test)

---

#### 10. **test-comprehensive-logloss.R** (DELETE)
**Current: 703 lines**
**Action: MOVE to inst/benchmarks/**

**Why delete from tests:**
- This is **benchmarking**, not testing
- Runtime performance measurement belongs in benchmarks/
- Tests should be fast; this is slow

**Action:**
```bash
mkdir -p inst/benchmarks
mv tests/testthat/test-comprehensive-logloss.R inst/benchmarks/benchmark-logloss.R
```

---

#### 11. **test-installation.R** (DELETE)
**Action: DELETE - infrastructure test, not package test**

---

#### 12. **test-isolation-wrapper.R** (DELETE)
**Current: 8 tests**
**Action: DELETE if treefarms_isolated() is not user-facing**

If it's an internal utility, don't test it separately.

---

#### 13. **test-numerical-edge-cases.R** (KEEP AS-IS)
**Current: Small, focused**
**Action: Likely good - verify it tests real numerical issues**

---

### NEW TEST FILES (Post-Consolidation)

After consolidation, we should have:

1. **test-core-fitting.R** - Core tree fitting functionality (~40 tests)
2. **test-prediction.R** - Prediction methods (~10 tests)
3. **test-rashomon.R** - Rashomon sets and cross-fitting (~20 tests)
4. **test-auto-tuning.R** - Auto-tuning (~8 tests)
5. **test-probabilities.R** - Probability accuracy (~15 tests)
6. **test-s3-methods.R** - S3 dispatch and printing (~8 tests)
7. **test-edge-cases.R** - Critical edge cases (~8 tests)
8. **test-parallel.R** - Parallel execution (~5 tests)
9. **test-numerical-edge-cases.R** - Numerical stability (keep as-is)
10. **test-integration.R** (NEW) - End-to-end workflows (~10 tests)

**Total: ~130-150 tests across 10 files**

---

## Consolidation Principles

### ✅ Keep tests that:
1. **Test user-facing API** - Functions users call directly
2. **Test critical paths** - Core functionality that must work
3. **Test edge cases that users will hit** - Empty data, bad inputs
4. **Add unique value** - Each test covers something new

### ❌ Remove tests that:
1. **Test implementation details** - Internal functions
2. **Repeat the same validation** - "Test once, trust everywhere"
3. **Are over-defensive** - Paranoid edge cases users won't hit
4. **Benchmark performance** - Move to inst/benchmarks/
5. **Test R language features** - Trust R's type system

### 🔄 Consolidate tests that:
1. **Test variations of same thing** - Use loops or parameterization
2. **Could be combined** - Multiple small tests → one comprehensive test
3. **Test related functionality** - Group by feature, not by parameter

---

## Implementation Steps

### Phase 1: Analysis (30 min)
1. ✅ Count tests per file
2. ✅ Identify redundancy patterns
3. ✅ Create consolidation plan (this document)

### Phase 2: Quick Wins (1 hour)
1. **DELETE** test-comprehensive-logloss.R → move to inst/benchmarks/
2. **DELETE** test-installation.R
3. **DELETE** test-isolation-wrapper.R (if internal-only)
4. **SLIM** test-cleanup.R (keep only 2 tests)
5. **SLIM** test-edge-cases.R (remove paranoid edge cases)

**Expected reduction: ~1,500 lines, ~150 tests**

### Phase 3: Major Consolidation (2-3 hours)
1. Create **test-core-fitting.R** (consolidate test-fit-functions.R + test-optimaltrees.R + test-api.R)
2. Create **test-rashomon.R** (consolidate rashomon-related tests)
3. Consolidate **test-probabilities.R** (merge probability-accuracy tests)
4. Slim down **test-parallel.R** and **test-s3-methods.R**

**Expected reduction: ~3,000 lines, ~200 tests**

### Phase 4: Integration Test (30 min)
Create **test-integration.R** with 8-10 end-to-end workflow tests:
- Fit → predict → validate workflow
- Cross-fitted Rashomon → intersection → predict
- Auto-tune → fit → predict
- Regression workflow
- Classification workflow with probabilities

### Phase 5: Verification (30 min)
1. Run consolidated test suite
2. Verify coverage of critical paths
3. Update documentation

---

## Expected Outcomes

### Before Consolidation
- 995 tests
- 7,856 lines
- ~5-10 minutes to run full suite
- Hard to maintain
- Lots of redundancy

### After Consolidation
- **~150 tests** (85% reduction)
- **~2,000 lines** (75% reduction)
- **~30-60 seconds** to run full suite
- Easy to maintain
- Each test adds value

### Quality Improvements
- ✅ Faster CI/CD
- ✅ Easier to understand test failures
- ✅ Less brittle (fewer tests to break)
- ✅ Better signal-to-noise ratio
- ✅ Maintainable long-term

---

## Decision Points

### 1. test-comprehensive-logloss.R
**Decision needed:** Move to benchmarks/ or delete entirely?
**Recommendation:** Move to `inst/benchmarks/benchmark-logloss.R`

### 2. test-isolation-wrapper.R
**Decision needed:** Is treefarms_isolated() user-facing?
**Recommendation:** If internal-only, delete tests

### 3. test-probability-accuracy.R
**Decision needed:** How much ground-truth probability testing is needed?
**Recommendation:** Keep 2-3 representative tests, delete rest

### 4. Discretization testing
**Decision needed:** How much discretization edge case testing is needed?
**Recommendation:** Basic test only, trust the algorithm

---

## Next Steps

1. **Review this plan** - Does this align with your testing philosophy?
2. **Make decisions** on decision points above
3. **Execute Phase 2** (quick wins) - Delete/slim obvious redundancy
4. **Execute Phase 3** (major consolidation) - Create new consolidated test files
5. **Execute Phase 4** (integration tests) - Add end-to-end tests
6. **Verify and document** - Ensure critical paths covered

**Estimated total time: 4-5 hours to complete full consolidation**

---

## Questions for You

1. **Benchmarking:** Should test-comprehensive-logloss.R → inst/benchmarks/ or just delete?
2. **Coverage target:** Aiming for ~150 tests. Too aggressive? Prefer ~200?
3. **Integration tests:** Want comprehensive end-to-end tests, or trust unit tests?
4. **Execution:** Do Phase 2 (quick wins) now, or full consolidation in one session?
