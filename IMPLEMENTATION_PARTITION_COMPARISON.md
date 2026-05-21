# Implementation: Partition-Based Tree Structure Comparison

**Date:** 2026-05-21
**Status:** COMPLETE
**Package:** optimaltrees v0.4.0

---

## Summary

Implemented partition-based tree structure comparison to correctly identify trees with identical leaf partitions regardless of split order. This fixes false negatives where trees creating the same regions via different split sequences were incorrectly treated as distinct structures.

---

## Problem

**Old behavior (path-based):**
- Tree structure hash computed from depth-first traversal order of splits
- Two trees with same partition but different split order → different hashes
- Modal structure selection failed when folds learned same partition via different paths

**Example of false negative:**
```
Tree A: Root splits on x1, then left child splits on x2
Tree B: Root splits on x2, then left child splits on x1

If both create same 4 leaf regions:
  Region 1: {x1=0, x2=0}
  Region 2: {x1=0, x2=1}
  Region 3: {x1=1, x2=0}
  Region 4: {x1=1, x2=1}

Old: Different hashes (because split order differs)
New: Same hash (because partition is identical)
```

---

## Solution

**New behavior (partition-based):**
- Tree structure defined by the **set of leaf regions**, not split sequence
- Hash computed from canonical representation: sorted set of sorted leaf condition-sets
- Trees with same partition get same hash regardless of split order

---

## Implementation Details

### 1. New Function: `extract_tree_partition()`

**Location:** `R/tree_structure.R` (lines ~230-295)

**Purpose:** Extract all leaf regions from a tree, where each region is defined by the conjunction of split conditions from root to leaf.

**Algorithm:**
1. Traverse tree recursively
2. Accumulate conditions along each path
3. At each leaf, store sorted set of conditions
4. Return list of all leaf condition-sets

**Output:** List of leaf regions, each containing:
- `conditions`: Data frame with columns `feature`, `relation`, `reference`
- `path_str`: Character path for debugging (e.g., "0-1-0")

### 2. New Function: `partition_hash()`

**Location:** `R/tree_structure.R` (lines ~298-328)

**Purpose:** Compute deterministic hash from tree partition.

**Algorithm:**
1. Extract leaf regions using `extract_tree_partition()`
2. Create canonical string for each leaf: `"f0_==_1&f2_==_0"` (sorted conditions)
3. Sort leaf strings lexicographically
4. Hash concatenated sorted list using xxhash64

**Key property:** Two trees with same partition → same hash

### 3. Modified: `TreeStructure` S7 Class

**Location:** `R/tree_structure.R` (lines 38-88)

**Changes:**
- Added property: `partition_hash` (character)
- Updated validator to check `partition_hash` is non-empty string

**Purpose:** Store pre-computed partition hash for fast comparison

### 4. Modified: `extract_tree_structure()`

**Location:** `R/tree_structure.R` (lines 148-176)

**Changes:**
- Compute `partition_hash` from tree using `partition_hash(tree_node)`
- Store in TreeStructure object

**Purpose:** Compute hash once during extraction, not repeatedly during comparison

### 5. Modified: `structure_hash()`

**Location:** `R/tree_structure.R` (lines ~470-500)

**Changes:**
- **Before:** Computed hash from split sequence (depth-first traversal order)
- **After:** Returns pre-computed `structure@partition_hash`

**Breaking change:** Old and new hashes differ (expected and acceptable)

### 6. Modified: `compare_structures()`

**Location:** `R/tree_structure.R` (lines 385-410)

**Changes:**
- **Before:** Compared splits positionally (split-by-split in traversal order)
- **After:** Compares partition hashes: `structure1@partition_hash == structure2@partition_hash`

**Simplification:** O(1) hash comparison instead of O(n) split-by-split comparison

---

## Testing

### Unit Tests

All 39 existing tree structure tests pass without modification:

```r
testthat::test_file('tests/testthat/test-tree-structure.R')
# ✔ | 0 | 0 | 39 | tree-structure
```

### Verification Tests

Created verification scripts demonstrating:

1. **Partition equivalence detection:** Trees with same partition get same hash
2. **Partition difference detection:** Trees with different partitions get different hashes
3. **Integration with doubletree:** Modal structure selection uses partition-based comparison

### Example Output

```r
# 5 trees with different random seeds
Tree 1: 7 leaves, partition_hash = 4e270061803d063e
Tree 2: 7 leaves, partition_hash = 4e270061803d063e
Tree 3: 7 leaves, partition_hash = 4e270061803d063e
Tree 4: 7 leaves, partition_hash = 4e270061803d063e
Tree 5: 7 leaves, partition_hash = 4e270061803d063e

Result: All 5 recognized as same structure (modal frequency 100%)
```

---

## Impact on doubletree

### Automatic Improvement

**No code changes needed in doubletree!**

Because doubletree already uses:
- `structure_hash()` for grouping structures
- `compare_structures()` for equivalence checks

Both now use partition-based comparison automatically.

### When It Helps

**Scenario A: Same partition, different split order**
- Before: 5 folds, all "unique" → modal fails
- After: 5 folds, 3-4 match modal → modal succeeds

**Scenario B: Different partitions**
- Before: 5 folds, all unique → modal fails
- After: 5 folds, all unique → modal still fails (correct!)

**Key insight:** Partition-based comparison helps when folds learn same partition via different sequences. Does NOT help when folds learn fundamentally different partitions (different number of leaves).

---

## Breaking Changes

### 1. Structure Hashes Changed

**Impact:** Old saved hashes incompatible with new hashes

**Mitigation:**
- No saved hashes exist in codebase (structures computed fresh each time)
- If external code saves hashes, they must recompute

### 2. API Signature Change (Minor)

**`compare_structures()`:**
- Parameter `tol` (numerical tolerance) now unused (kept for backward compatibility)
- Comparison no longer uses tolerance (hash is exact match)

**`structure_hash()`:**
- Parameter `precision` now unused (kept for backward compatibility)
- Precision set internally to 8 digits in `partition_hash()`

---

## Performance

**Time complexity:**
- Old: O(n) comparison (iterate through all splits)
- New: O(1) comparison (hash lookup)

**Memory:**
- Adds one `partition_hash` string (~16 bytes) per TreeStructure
- Negligible overhead

**Computation:**
- Partition hash computed once during extraction
- Comparison is faster (hash equality vs split-by-split check)

---

## Documentation

### Updated Files

- `man/TreeStructure.Rd` - Document new `partition_hash` property
- `man/compare_structures.Rd` - Explain partition-based comparison
- `man/structure_hash.Rd` - Document partition-based hash
- `man/extract_tree_partition.Rd` - New function documentation
- `man/partition_hash.Rd` - New function documentation

### Examples Added

All documentation includes examples showing:
- What constitutes "same structure" (partition, not path)
- How partition-based comparison differs from path-based
- When two trees are equivalent vs different

---

## Future Considerations

### Potential Enhancements

1. **Expose partition extraction:** Could make `extract_tree_partition()` exported for debugging/analysis
2. **Add partition visualization:** Function to visualize leaf regions from partition
3. **Partition distance metric:** Measure similarity between different partitions (e.g., Jaccard index)

### Known Limitations

1. **Precision:** Reference values rounded to 8 decimal places (reasonable for double precision)
2. **Hash collisions:** Theoretically possible but astronomically unlikely with xxhash64
3. **Binary features only:** Assumes relations are "==" or "<=" (current optimaltrees design)

---

## Files Modified

```
optimaltrees/
├── R/tree_structure.R                # Main implementation
├── man/TreeStructure.Rd             # Updated docs
├── man/compare_structures.Rd        # Updated docs
├── man/structure_hash.Rd            # Updated docs
├── man/extract_tree_partition.Rd    # New docs
└── man/partition_hash.Rd            # New docs
```

**Lines added:** ~160
**Lines modified:** ~50
**Tests passing:** 39/39

---

## Verification Checklist

- [x] `extract_tree_partition()` correctly extracts leaf conditions
- [x] `partition_hash()` produces canonical hash from leaf conditions
- [x] `TreeStructure` class includes `partition_hash` property
- [x] `extract_tree_structure()` computes and stores partition hash
- [x] `structure_hash()` returns pre-computed partition hash
- [x] `compare_structures()` uses partition hash comparison
- [x] All existing tests pass (39/39)
- [x] Trees with same partition get same hash
- [x] Trees with different partitions get different hashes
- [x] doubletree integration works (modal structure selection)
- [x] Documentation updated and examples added
- [x] MEMORY.md updated with learnings

---

## Success Criteria Met

✅ Two trees with same partition but different split order get same hash
✅ `compare_structures()` returns TRUE for partition-equivalent trees
✅ `extract_tree_partition()` correctly extracts leaf conditions
✅ `partition_hash()` produces canonical hash from leaf conditions
✅ All optimaltrees tests pass
✅ doubletree automatically benefits (no code changes needed)
✅ Clear documentation of what "same structure" means

---

## See Also

- Plan: `quality_reports/plans/2026-05-21_partition-based-comparison.md`
- MEMORY.md: Tree Structure Comparison section
- Tests: `tests/testthat/test-tree-structure.R`
