# Rashomon Set Pruning Fix - 2026-05-29

## Problem

The parameter `rashomon_ignore_trivial_extensions` existed in the C++ backend with a default value of `TRUE`, but was not exposed in the R API. This caused cross-fitted Rashomon workflows to fail because:

1. Each fold's Rashomon set was pruned to 1 representative tree per partition
2. Different folds learned the same partition via different split sequences
3. Cross-fold intersection found no common tree (pruned representatives differed)
4. Result: "Rashomon intersection empty" despite folds learning the same functional partition

## Solution

Exposed `rashomon_ignore_trivial_extensions` parameter explicitly in R API with context-appropriate defaults:

| Function | Default | Rationale |
|----------|---------|-----------|
| `fit_tree()` | `TRUE` | Users want one tree |
| `fit_rashomon()` | `FALSE` | Users need full Rashomon set |
| `cross_fitted_rashomon()` | `FALSE` | **Required** for intersection to work |
| `optimaltrees()` | `NULL` | Inferred from `single_tree` parameter |

## Files Modified

### optimaltrees package (5 files)

1. **R/treefarms.R**
   - Added `rashomon_ignore_trivial_extensions = NULL` parameter
   - Implemented inference: if NULL, set based on `single_tree`
   - Added to config list building
   - Updated roxygen documentation

2. **R/fit_tree.R**
   - Added `rashomon_ignore_trivial_extensions = TRUE` parameter
   - Pass through to `optimaltrees()`
   - Documented rationale for TRUE default

3. **R/fit_rashomon.R**
   - Added `rashomon_ignore_trivial_extensions = FALSE` parameter
   - Pass through to `optimaltrees()`
   - Documented critical importance of FALSE default

4. **R/cross_fitted_rashomon.R**
   - Added `rashomon_ignore_trivial_extensions = FALSE` parameter
   - Pass through at 2 call sites (propensity and outcome models)
   - Strong warning in documentation against overriding

5. **tests/testthat/test-rashomon-trivial-extensions.R** (new)
   - Test `fit_rashomon()` default finds multiple trees when they exist
   - Test TRUE prunes to fewer trees
   - Test cross-fitted intersection enabled with default
   - Test parameter inference from `single_tree`
   - Test explicit TRUE/FALSE values accepted

### doubletree package (1 file)

6. **R/nuisance_trees.R**
   - In `safe_rashomon_fit()`, explicitly pass `rashomon_ignore_trivial_extensions=FALSE`
   - Added comment explaining critical importance for fold intersection

## Implementation Details

### C++ Backend

The C++ parameter already existed:
- Default: `true` (in `configuration.cpp`)
- Config reading: `Configuration::rashomon_ignore_trivial_extensions = config["rashomon_ignore_trivial_extensions"]`
- Used in: `rash_models.hpp` during tree extraction

No C++ changes needed - infrastructure was complete.

### R API Changes

**Parameter flow:**
```
R function call
  → optimaltrees(rashomon_ignore_trivial_extensions = <value>)
  → config list (line ~599)
  → JSON config
  → C++ Configuration class
  → Tree extraction logic
```

**Default inference** (in `optimaltrees()`):
```r
if (is.null(rashomon_ignore_trivial_extensions)) {
  rashomon_ignore_trivial_extensions <- single_tree
}
```

### Breaking Changes

**Potential impact:**
- `fit_rashomon()` default changed from implicit TRUE (C++ default) to explicit FALSE
- Users who relied on `...` passthrough will need to update if they want pruning
- This is intentional - previous behavior was wrong for cross-fitted workflows

**Mitigation:**
- Clear documentation of defaults and rationale
- Explicit parameter allows users to override if needed
- NEWS.md entry when releasing

## Testing

### Unit Tests

**test-rashomon-trivial-extensions.R:**
- ✓ `fit_rashomon()` with default (FALSE)
- ✓ `fit_rashomon()` with TRUE prunes
- ✓ `cross_fitted_rashomon()` with default enables intersection
- ✓ Parameter inference from `single_tree` in `optimaltrees()`
- ✓ `fit_tree()` default (TRUE) works correctly
- ✓ Both TRUE and FALSE values pass through without error

### Manual Verification

**Simple test:**
```r
fit_rashomon(..., rashomon_ignore_trivial_extensions = FALSE)  # Works
fit_rashomon(..., rashomon_ignore_trivial_extensions = TRUE)   # Works
```

**Cross-fitted test:**
```r
cross_fitted_rashomon(..., rashomon_ignore_trivial_extensions = FALSE)  # Works
```

### Simulation Test

**Awaiting completion:** Test with actual failing DGP (complex, n=500, approach 4)
- This will verify the fix resolves the original simulation failure

## Verification Checklist

- [x] `devtools::document()` regenerates NAMESPACE correctly
- [x] `devtools::test()` passes (new + existing tests)
- [x] Parameter added to all affected functions
- [x] Documentation updated with rationale
- [x] Config building includes parameter
- [ ] `devtools::check()` passes (needs full run)
- [ ] Manual test with DGP 3, n=500 succeeds (running)
- [ ] Git commit with clear message
- [ ] Push to GitHub
- [ ] Reinstall on cluster: `R CMD INSTALL optimaltrees doubletree`
- [ ] Test single rep on cluster before full resubmission
- [ ] If successful, resubmit approach 4 jobs

## Risk Assessment

**Low risk:**
- No C++ changes (infrastructure existed)
- Parameter already worked via `...` passthrough
- Just making it explicit with better defaults
- All changes are additive (backward compatible via default NULL)

**Potential issues:**
- Users relying on undocumented `...` passthrough (unlikely)
- Default FALSE in `fit_rashomon()` changes behavior (but previous was wrong)

## Next Steps

1. Complete verification checklist
2. Run `devtools::check()`
3. Verify simulation succeeds with fix
4. Commit changes
5. Push to GitHub
6. Reinstall on cluster
7. Test single simulation rep
8. Resubmit approach 4 jobs if successful

## References

- Plan: `quality_reports/plans/2026-05-29_fix-rashomon-pruning.md`
- C++ config: `optimaltrees/src/configuration.cpp`
- C++ usage: `optimaltrees/src/optimizer/extraction/rash_models.hpp`
- Simulation code: `doubletree/simulations/six_approach_comparison/`
