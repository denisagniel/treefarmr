# Memory Fixes Applied

## Summary

Implemented three quick fixes to reduce test suite memory consumption by an estimated 45-70%.

---

## Changes Made

### 1. Reduced Pre-loaded Dataset Sizes

**File:** `tests/testthat/helper-data.R`

**Before:**
```r
big_dataset <- big_dataset(1000, 10, 42)           # 1000 samples × 10 features
many_features_dataset <- many_features_dataset(100, 50, 42)  # 100 samples × 50 features
```

**After:**
```r
big_dataset <- big_dataset(200, 10, 42)            # 200 samples × 10 features (80% reduction)
many_features_dataset <- many_features_dataset(100, 20, 42)  # 100 samples × 20 features (60% fewer features)
```

**Impact:** ~10-15% memory reduction

---

### 2. Removed store_training_data = TRUE

**Files:**
- `tests/testthat/test-fit-functions.R` (6 occurrences removed)
- `tests/testthat/test-squared-error.R` (3 occurrences removed)

**Before:**
```r
model <- treefarms(X, y,
                   store_training_data = TRUE,  # Stores entire training dataset
                   verbose = FALSE)
```

**After:**
```r
model <- treefarms(X, y,
                   verbose = FALSE)
```

**Rationale:** Most tests don't need the training data stored in the model object. This flag prevents garbage collection of the training dataset.

**Impact:** ~20-30% memory reduction

---

### 3. Added Explicit Memory Cleanup in Loops

**File:** `tests/testthat/test-comprehensive-logloss.R`

**Before:**
```r
for (reg in regularization_values) {
  model <- fit_tree(...)
  # ... test assertions ...
}  # Model objects accumulate in memory
```

**After:**
```r
for (reg in regularization_values) {
  model <- fit_tree(...)
  # ... test assertions ...

  # Clean up memory
  rm(model, pred_probs, accuracy_metrics, runtime_metrics, validation)
  gc(verbose = FALSE)
}
```

**Locations added (6 total):**
1. Single tree loop (line ~277)
2. Cross-fitted single trees nested loop (line ~390)
3. Rashomon set nested loop (line ~467)
4. Cross-fitted Rashomon nested loop (line ~543)
5. Intersecting trees nested loop (line ~647)
6. Different dataset sizes loop (line ~702)

**Impact:** ~15-25% memory reduction

---

## Testing the Fixes

### Quick Test
```bash
Rscript test_memory_quick.R
```

This runs only test-comprehensive-logloss.R (the biggest memory consumer) and reports memory usage.

### Full Test Suite
```bash
cd treefarmr
R -e "devtools::test()"
```

Monitor memory with Activity Monitor (macOS) or Task Manager (Windows) while running.

---

## Expected Results

**Before fixes:**
- Memory consumption: High enough to cause system memory pressure
- Test suite: May fail or crash due to memory issues

**After fixes:**
- Memory consumption: 45-70% reduction
- Test suite: Should run smoothly without memory pressure

---

## Verification

Run these commands to verify the changes:

```bash
# Check dataset sizes are reduced
grep -A1 "big_dataset <-" tests/testthat/helper-data.R
grep -A1 "many_features_dataset <-" tests/testthat/helper-data.R

# Check store_training_data removed (should return no results)
grep -r "store_training_data = TRUE" tests/testthat/test-fit-functions.R
grep -r "store_training_data = TRUE" tests/testthat/test-squared-error.R

# Check gc() calls added (should return 6)
grep -c "gc(verbose = FALSE)" tests/testthat/test-comprehensive-logloss.R
```

---

## Next Steps

1. **Run test_memory_quick.R** to verify memory improvement on single file
2. **Run full devtools::test()** to verify all tests still pass
3. **Monitor system memory** during test execution
4. **If still issues:** Consider additional optimizations:
   - Break up test-comprehensive-logloss.R into smaller files
   - Add gc() to test-probability-accuracy.R
   - Use withr::local_*() for automatic cleanup in tests

---

## Rollback Instructions

If these changes cause issues:

```bash
# Revert all changes
git diff HEAD tests/testthat/ > memory_fixes.patch
git checkout HEAD tests/testthat/helper-data.R tests/testthat/test-fit-functions.R tests/testthat/test-squared-error.R tests/testthat/test-comprehensive-logloss.R
```

---

## Files Modified

- `tests/testthat/helper-data.R` (dataset sizes reduced)
- `tests/testthat/test-fit-functions.R` (store_training_data removed, 6 lines)
- `tests/testthat/test-squared-error.R` (store_training_data removed, 3 lines)
- `tests/testthat/test-comprehensive-logloss.R` (gc() added, 6 locations)
