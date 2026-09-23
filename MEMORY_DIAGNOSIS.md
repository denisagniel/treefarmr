# Memory Issues in testthat Test Suite

## Diagnosis Summary

The test suite has several patterns that contribute to high memory usage:

### 1. Pre-loaded Test Datasets (helper-data.R)

**Problem:** helper-data.R creates 14 datasets at package load time, including:
- `big_dataset`: 1000 samples × 10 features
- `many_features_dataset`: 100 samples × 50 features
- Multiple pattern datasets

**Memory impact:** These stay in memory for the entire test session.

**Line reference:** tests/testthat/helper-data.R:203-218

### 2. store_training_data = TRUE

**Problem:** 9 test cases use `store_training_data = TRUE`, which stores the full training dataset in the model object.

**Files affected:**
- test-fit-functions.R (6 occurrences)
- test-squared-error.R (3 occurrences)

**Memory impact:** Each model retains its training data, preventing garbage collection.

### 3. Nested Loops Creating Many Models

**test-comprehensive-logloss.R:**
- Line 230: Loop over 4 regularization values
- Line 470-471: Nested loops (3 K values × 2 reg values = 6 iterations)
- Line 542-543: Nested loops (3 K values × 2 reg values = 6 iterations)
- Each cross_fitted_rashomon call creates K fold models internally

**Estimated models created:** ~50+ model objects in this file alone

**test-probability-accuracy.R:**
- 633 lines total
- 7 loops with treefarms() calls inside
- 16 direct treefarms() calls

**test-cross-fitted-rashomon.R:**
- Creates cross-fitted models with multiple folds
- Each fold is a separate model object

### 4. Insufficient Garbage Collection

**Problem:** Only 5 gc() calls in the entire test suite

**Files with gc():**
```bash
tests/testthat/test-cleanup.R
tests/testthat/test-state-isolation.R
```

Most tests don't explicitly call gc() after creating large objects or within loops.

### 5. Model Objects Not Removed

**Problem:** Models created in loops are often stored in lists and not explicitly removed.

Example from test-comprehensive-logloss.R:
```r
for (K in K_values) {
  for (reg in regularization_values) {
    result <- cross_fitted_rashomon(...)  # Creates K fold models
    # result is kept in memory until loop ends
  }
}
```

---

## Memory Profile by Test File Size

| File | Lines | Model Calls | Has Loops | Memory Risk |
|------|-------|-------------|-----------|-------------|
| test-comprehensive-logloss.R | 683 | 14 fit_* | Yes (nested) | **HIGH** |
| test-probability-accuracy.R | 633 | 16 treefarms | Yes | **HIGH** |
| test-edge-cases.R | 564 | 49 treefarms | Some | **HIGH** |
| test-discretization.R | 578 | unknown | Yes (2) | MEDIUM |
| test-cross-fitted-rashomon.R | 442 | unknown | Yes | **HIGH** |
| test-fit-functions.R | 522 | 2 treefarms | No | LOW-MED |

---

## Recommendations

### Priority 1: Reduce Pre-loaded Datasets

**Action:** Lazy-load datasets only when needed, or reduce their size.

**Option A - Lazy functions:**
```r
# Instead of creating datasets at load time
big_dataset <- function() {
  # Create only when called
  matrix(sample(0:1, 1000*10, replace=TRUE), ncol=10)
}
```

**Option B - Smaller defaults:**
```r
# Reduce size of big_dataset
big_dataset <- as.data.frame(matrix(sample(0:1, 200*10, replace=TRUE), ncol=10))
```

### Priority 2: Remove store_training_data = TRUE

**Action:** Remove this flag from tests unless specifically testing that feature.

Most tests don't need the training data stored in the model object.

### Priority 3: Add Explicit Garbage Collection in Loops

**Action:** Add gc() calls inside loops that create many models.

```r
for (K in K_values) {
  for (reg in regularization_values) {
    result <- cross_fitted_rashomon(...)
    # ... test assertions ...
    rm(result)  # Explicitly remove
    gc()        # Force garbage collection
  }
}
```

### Priority 4: Break Up Large Test Files

**Action:** Split test-comprehensive-logloss.R and test-probability-accuracy.R into smaller files.

Example:
- `test-logloss-single-tree.R`
- `test-logloss-cross-fitted.R`
- `test-logloss-rashomon.R`

This allows running tests incrementally and gc() between files.

### Priority 5: Use withr for Automatic Cleanup

**Action:** Use withr::defer() or withr::local_* for automatic cleanup.

```r
test_that("some test", {
  local({
    model <- treefarms(...)
    # test assertions
    # Automatic cleanup when local() exits
  })
})
```

---

## Quick Wins (Estimated Impact)

1. **Remove store_training_data from 9 tests:** ~20-30% memory reduction
2. **Add gc() in 3 main loop-heavy tests:** ~15-25% memory reduction
3. **Convert helper-data.R to lazy functions:** ~10-15% memory reduction

**Combined: 45-70% memory reduction**

---

## Test to Verify Fix

Create a simple profiling script:

```r
library(profmem)

# Profile a single test file
p <- profmem({
  testthat::test_file("tests/testthat/test-comprehensive-logloss.R")
})

# Check peak memory
total_bytes <- sum(p$bytes, na.rm = TRUE)
cat("Total allocated:", total_bytes / 1024^2, "MB\n")
```

Run before and after fixes to measure improvement.

---

## Next Steps

1. Implement Priority 1 (lazy datasets) first - easiest and safest
2. Remove store_training_data from tests that don't need it
3. Add gc() in the 3 largest test files
4. Re-run devtools::test() to verify memory improvement
5. If still issues, profile with profmem to identify remaining culprits
