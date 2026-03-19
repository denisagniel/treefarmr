# S7 Migration Guide

**Goal:** Replace informal S3 structure with formal S7 classes for type safety and validation

**Effort:** 2-3 hours

**Benefits:**
- Automatic property validation
- Type-safe `@` access (vs unsafe `$`)
- Better error messages
- Clearer structure

---

## Key Design Decisions

### 1. Single Model Class (Not Separate Log-Loss Class)

**Before (S3):**
- `optimaltrees_model` - for misclassification
- `optimaltrees_logloss_model` - for log_loss
- Two separate classes doing essentially the same thing

**After (S7):**
- `OptimalTreesModel` - handles ALL loss functions
- `loss_function` property distinguishes behavior
- `is_regression` flag for regression vs classification

**Rationale:** The "logloss" variant was an implementation detail of how the C++ backend returns results, not a fundamental class difference. S7 lets us normalize this in the constructor.

### 2. No Backward Compatibility Required

**Means we can:**
- Drop `treefarms_*` names entirely (clean break)
- Simplify nested structure (`model$model$tree_json` → `model@trees`)
- Make breaking changes that improve design
- No need for aliases or migration shims

### 3. Unified Tree Storage

**Before:** Different backends returned different formats:
- `model$model$tree_json` (C++ misclassification)
- `model$trees` (C++ log_loss, list of JSON strings)

**After:** Single `@trees` property (list of parsed tree structures)
- Constructor normalizes all formats
- Internal consistency

---

## Migration Steps

### Step 1: Add S7 Dependency (5 min)

**DESCRIPTION:**
```
Package: optimaltrees
...
Imports:
    S7,
    ...
```

**NAMESPACE:**
```r
import(S7)
```

**Install:**
```r
install.packages("S7")
```

### Step 2: Update Constructor Functions (30 min)

**File:** `R/treefarms.R`

**Before:**
```r
# Bottom of optimaltrees() function
result <- list(
  model = model,
  predictions = predictions,
  probabilities = probabilities,
  accuracy = accuracy,
  loss_function = loss_function,
  regularization = regularization,
  n_trees = n_trees,
  X_train = X_train,
  y_train = y_train,
  ...
)

class(result) <- "optimaltrees_model"
return(result)
```

**After:**
```r
# Bottom of optimaltrees() function
result <- new_optimal_trees_model(
  loss_function = loss_function,
  regularization = regularization,
  n_trees = n_trees,
  trees = trees,  # Normalized in constructor
  accuracy = accuracy,
  predictions = predictions,
  probabilities = probabilities,
  X_train = X_train,
  y_train = y_train,
  discretization_metadata = discretization_metadata,
  is_regression = is_regression
)

return(result)
```

**Do for:**
- `optimaltrees()` in `R/treefarms.R`
- `fit_tree()` in `R/fit_tree.R` (if it constructs directly)
- `fit_rashomon()` in `R/fit_rashomon.R` (if it constructs directly)

### Step 3: Update Cross-Fitted Rashomon (30 min)

**File:** `R/cross_fitted_rashomon.R`

**Before (line ~521):**
```r
result <- list(
  intersecting_trees = intersection_result$intersecting_trees,
  n_intersecting = intersection_result$n_intersecting,
  tree_jsons = intersection_result$tree_jsons,
  intersecting_structures = intersection_result$intersecting_structures,
  tree_risks = intersection_result$tree_risks,
  fold_refits = fold_refits,
  fold_id_per_row = fold_id_per_row,
  fold_models = fold_models,
  rashomon_sets = rashomon_sets,
  rashomon_sizes = rashomon_sizes,
  fold_indices = fold_indices,
  K = K,
  loss_function = loss_function,
  regularization = regularization,
  rashomon_bound_multiplier = rashomon_bound_multiplier,
  rashomon_bound_adder = rashomon_bound_adder,
  max_leaves = max_leaves,
  X_train = X,
  y_train = y,
  call = match.call()
)

class(result) <- "cf_rashomon"
```

**After:**
```r
result <- CFRashomon(
  K = as.integer(K),
  loss_function = loss_function,
  regularization = regularization,
  rashomon_bound_multiplier = rashomon_bound_multiplier,
  rashomon_bound_adder = rashomon_bound_adder,
  max_leaves = if (is.null(max_leaves)) NULL else as.integer(max_leaves),
  rashomon_sizes = as.integer(rashomon_sizes),
  n_intersecting = as.integer(intersection_result$n_intersecting),
  intersecting_trees = intersection_result$intersecting_trees,
  tree_risks = intersection_result$tree_risks,
  fold_refits = fold_refits,
  fold_id_per_row = as.integer(fold_id_per_row),
  fold_indices = fold_indices,
  X_train = X,
  y_train = y,
  converged = TRUE  # or from auto_result$converged
)
```

**Also update:** `try_cross_fitted_rashomon_internal()` (line ~648)

### Step 4: Update Property Access (30 min)

Search and replace throughout codebase:

**In internal functions:**
- `model$loss_function` → `model@loss_function`
- `model$regularization` → `model@regularization`
- `model$n_trees` → `model@n_trees`
- `model$trees` → `model@trees` (NEW: replaces `model$model$tree_json`)
- `result$n_intersecting` → `result@n_intersecting`
- etc.

**Files to update:**
- `R/predict.R` - All property access
- `R/s3_methods.R` - Print/summary methods
- `R/rashomon.R` - `get_rashomon_trees()`, `find_tree_intersection()`
- `R/save_load.R` - Serialization functions
- Any other files accessing model properties

**Tool:**
```bash
# Find all property accesses
grep -r "model\$\|result\$" R/*.R | grep -v "^#"
```

### Step 5: Update get_rashomon_trees() (15 min)

**File:** `R/rashomon.R`

**Before:**
```r
get_rashomon_trees <- function(model, max_leaves = NULL) {
  if (!inherits(model, c("treefarms_model", "treefarms_logloss_model",
                         "optimaltrees_model", "optimaltrees_logloss_model"))) {
    stop("model must be a treefarms_model, treefarms_logloss_model,
          optimaltrees_model, or optimaltrees_logloss_model object")
  }

  if (inherits(model, c("treefarms_logloss_model", "optimaltrees_logloss_model"))) {
    # For log_loss models, trees are stored in model$trees
    if (model$n_trees == 0) {
      warning("No trees in Rashomon set")
      return(list())
    }
    trees <- model$trees
  } else {
    # For regular models, extract from JSON structure
    tree_json <- model$model$tree_json
    # ... complex extraction logic ...
  }
  # ...
}
```

**After:**
```r
get_rashomon_trees <- function(model, max_leaves = NULL) {
  # S7 class check
  if (!S7::S7_inherits(model, OptimalTreesModel)) {
    stop("model must be an OptimalTreesModel object")
  }

  # Simple unified access
  if (model@n_trees == 0) {
    warning("No trees in Rashomon set")
    return(list())
  }

  trees <- model@trees

  # Apply max_leaves filter if specified
  if (!is.null(max_leaves) && length(trees) > 0) {
    nleaves <- vapply(trees, count_leaves_tree, integer(1))
    trees <- trees[nleaves <= max_leaves]
  }

  trees
}
```

### Step 6: Remove Legacy Class Names (10 min)

**Files to clean:**
- Remove all references to `treefarms_model`, `treefarms_logloss_model`
- Remove `optimaltrees_logloss_model` class
- Remove separate print/predict methods for logloss variant

**Search:**
```bash
grep -r "treefarms_model\|logloss_model" R/*.R
```

### Step 7: Update Tests (30 min)

**Update test expectations:**

**Before:**
```r
expect_true(inherits(model, "optimaltrees_model"))
expect_equal(model$n_trees, 1)
expect_gt(model$accuracy, 0.5)
```

**After:**
```r
expect_true(S7::S7_inherits(model, OptimalTreesModel))
expect_equal(model@n_trees, 1L)
expect_gt(model@accuracy, 0.5)
```

**Test validation:**
```r
test_that("OptimalTreesModel validates properties", {
  # Should error on negative regularization
  expect_error(
    new_optimal_trees_model(
      loss_function = "log_loss",
      regularization = -0.1,  # Invalid!
      n_trees = 1L,
      trees = list()
    ),
    "@regularization must be positive"
  )

  # Should error on mismatched tree count
  expect_error(
    new_optimal_trees_model(
      loss_function = "log_loss",
      regularization = 0.1,
      n_trees = 5L,
      trees = list()  # Should have 5 trees!
    ),
    "length\\(@trees\\) must equal @n_trees"
  )
})
```

### Step 8: Update Documentation (15 min)

**Update roxygen @return tags:**

**Before:**
```r
#' @return A list containing:
#'   \item{model}{The trained TreeFARMS model object}
#'   \item{predictions}{Binary predictions (0/1) for training data}
```

**After:**
```r
#' @return An \code{OptimalTreesModel} object with properties:
#'   \item{@loss_function}{Loss function used}
#'   \item{@regularization}{Complexity penalty}
#'   \item{@trees}{List of tree structures}
#'   \item{@predictions}{Predicted values for training data}
```

---

## Property Access Patterns

### Accessing Properties

**S3 (old):**
```r
loss <- model$loss_function      # Unsafe
model$regularization <- 0.2      # No validation
model$n_trees <- -5              # Silent bug!
```

**S7 (new):**
```r
loss <- model@loss_function      # Type-safe
model@regularization <- 0.2      # Validated!
model@regularization <- -0.1     # Error: "must be positive"
```

### Checking Class

**S3 (old):**
```r
if (inherits(model, "optimaltrees_model")) { ... }
```

**S7 (new):**
```r
if (S7::S7_inherits(model, OptimalTreesModel)) { ... }
```

### Setting Multiple Properties

**S7 allows bulk assignment in constructor:**
```r
model <- OptimalTreesModel(
  loss_function = "log_loss",
  regularization = 0.1,
  n_trees = 5L,
  trees = tree_list,
  accuracy = 0.95,
  is_regression = FALSE
)
# All properties validated at once!
```

---

## Testing the Migration

### Manual Verification

```r
# Load package with new S7 classes
devtools::load_all()

# Test basic fitting
X <- data.frame(x1 = rbinom(100, 1, 0.5), x2 = rbinom(100, 1, 0.5))
y <- as.numeric((X$x1 == 1 & X$x2 == 1))

model <- fit_tree(X, y, loss_function = "log_loss", regularization = 0.1)

# Check S7 class
S7::S7_inherits(model, OptimalTreesModel)  # Should be TRUE

# Check properties (type-safe access)
model@loss_function  # "log_loss"
model@regularization  # 0.1
model@n_trees  # 1

# Test validation
try(model@regularization <- -0.1)  # Should error

# Test cross-fitted rashomon
result <- cross_fitted_rashomon(X, y, K = 3, regularization = 0.1)
S7::S7_inherits(result, CFRashomon)  # Should be TRUE
result@n_intersecting  # >= 0
```

### Test Suite

```bash
# Run all tests
devtools::test()

# Should all pass with S7 classes
```

---

## Common Issues & Solutions

### Issue 1: "object not found" errors

**Problem:** Forgot to update `model$prop` to `model@prop`

**Solution:** Search and replace all property accesses

### Issue 2: Type coercion errors

**Problem:** `n_trees = 5` instead of `n_trees = 5L` (integer required)

**Solution:** Use explicit `as.integer()` conversions

### Issue 3: Validation failures

**Problem:** Existing code creating invalid objects now caught by S7

**Solution:** Fix the underlying issue (this is a feature!)

### Issue 4: Method dispatch not working

**Problem:** S3 generics not finding S7 methods

**Solution:** Check that `method(generic, Class)` stubs are defined in `s7_classes.R`

---

## Verification Checklist

- [ ] S7 package added to DESCRIPTION
- [ ] `R/s7_classes.R` loaded and exports classes
- [ ] All constructor functions updated
- [ ] All property access changed from `$` to `@`
- [ ] Legacy class names removed (`treefarms_*`, `*_logloss_model`)
- [ ] Tests updated and passing
- [ ] Documentation updated
- [ ] Manual smoke test successful
- [ ] No backward compatibility shims needed (clean break)

---

## Rollout Plan

### Phase 1: Core Model (1 hour)
1. Add S7 dependency
2. Define `OptimalTreesModel` class
3. Update `optimaltrees()` constructor
4. Update `fit_tree()` and `fit_rashomon()`
5. Test basic fitting

### Phase 2: Cross-Fitted (45 min)
1. Define `CFRashomon` class
2. Update `cross_fitted_rashomon()` constructor
3. Update `try_cross_fitted_rashomon_internal()`
4. Test cross-fitting

### Phase 3: Utilities (30 min)
1. Update `get_rashomon_trees()`
2. Update `predict()` methods
3. Update `print()` and `summary()` methods
4. Update serialization (`save_load.R`)

### Phase 4: Cleanup (15 min)
1. Remove legacy class names
2. Update all documentation
3. Run full test suite
4. Commit with clear message

---

## Example Commit Message

```
Migrate to S7 for type-safe OOP structure

BREAKING CHANGE: Replace informal S3 with formal S7 classes

- Single OptimalTreesModel class (replaces optimaltrees_model + logloss variant)
- Automatic property validation on creation and modification
- Type-safe @ property access (replaces unsafe $)
- Clear structure prevents entire class of bugs (e.g., recent namespace issues)
- CFRashomon class with validation for cross-fitted results

Benefits:
- Can't create invalid objects (e.g., regularization = -1)
- Can't set wrong types (e.g., n_trees = "five")
- Better error messages
- Clearer structure (trees @ trees instead of model$model$tree_json)

Migration: 2-3 hours, no backward compatibility needed (clean break)

Classes:
- OptimalTreesModel (unified, all loss functions)
- CFRashomon (cross-fitted Rashomon results)
- OptimalTreesModelLoaded (serialization wrapper)

Removed:
- treefarms_model, treefarms_logloss_model (legacy names)
- optimaltrees_logloss_model (unnecessary split)
- Complex nested structure (model$model$*)
```
