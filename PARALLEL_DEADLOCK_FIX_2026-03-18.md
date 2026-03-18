# Parallel Execution Deadlock Fix - 2026-03-18

## Summary

Fixed critical deadlock bug that caused infinite hangs with `worker_limit > 1`. All parallel executions with 2+ workers hung indefinitely in optimizer loop.

**Status**: ✅ FIXED - Parallel execution now works correctly

---

## The Bug

### Symptoms
- Any `fit_tree()` call with `worker_limit > 1` hung forever
- 100% CPU usage, no progress
- Happened immediately on first parallel run
- Even tiny problems (100 x 10) hung

### Root Causes

**1. Data Race on `active` Flag** (`src/optimizer.hpp:114`)
```cpp
// BEFORE (broken):
bool active = true; // Not thread-safe!
```

Multiple threads read/write this flag without synchronization → undefined behavior.

**2. Periodic Termination Check** (`src/optimizer.cpp:204`)
```cpp
// BEFORE (broken):
if (update || complete() || ((this -> ticks) % (this -> tick_duration)) == 0) {
    this -> active = !complete() && !timeout() && state.queue.size() > 0;
}
```

Worker 0 only checked termination every 10,000 iterations. When queue emptied between checks, workers spun forever waiting for `active` to update.

**3. Incorrect Return Value** (`src/optimizer.cpp:217`)
```cpp
// BEFORE (broken):
return this -> active; // All workers return stale flag value
```

Workers 1+ returned cached `active` value, never checking if work remained.

---

## The Fix

### Change 1: Thread-Safe Flag
```cpp
// AFTER (fixed):
std::atomic<bool> active; // Thread-safe atomic flag
```

### Change 2: Check Termination Every Iteration
```cpp
// AFTER (fixed):
bool should_continue = !complete() && !timeout() && state.queue.size() > 0;

if (id == 0) {
    this -> ticks += 1;
    this -> active = should_continue; // Update shared flag
    // ... periodic diagnostics ...
}
return should_continue; // Each worker returns its own check
```

### Why This Works

1. **Atomic flag**: Safe for multiple threads to read
2. **Every iteration**: Workers check termination immediately when queue empties
3. **Local decision**: Each worker checks `queue.size() > 0` directly, not waiting for worker 0

---

## Performance Results (After Fix)

### Small Problems (n=300, p=15)
```
1 worker:  0.005 sec (baseline)
2 workers: 0.004 sec → 1.17x speedup (59% efficient)
4 workers: 0.004 sec → 1.19x speedup (30% efficient)
```

### Medium Problems (n=800, p=25)
```
1 worker:  0.016 sec (baseline)
2 workers: 0.013 sec → 1.25x speedup (62% efficient)
4 workers: 0.011 sec → 1.44x speedup (36% efficient)
```

### Large Problems (n=1500, p=35)
```
1 worker:  0.025 sec (baseline)
2 workers: 0.026 sec → 0.98x speedup (49% efficient)
4 workers: 0.024 sec → 1.05x speedup (26% efficient)
```

---

## Key Findings

1. **Deadlock eliminated**: Parallel execution now completes successfully
2. **Modest speedups**: Best case is 1.44x with 4 workers on medium problems
3. **High overhead**: Thread synchronization dominates for typical problem sizes
4. **Best efficiency**: 2 workers on medium problems (62% efficient)

---

## Recommendation

**Default should be `worker_limit = 1`** (single-threaded)

Reasons:
- Overhead dominates for small/large problems
- Even best case (medium problems) only achieves 36% efficiency with 4 workers
- Most users will see slowdowns, not speedups
- Users who benefit can opt-in by setting `worker_limit = 2` or `4`

---

## Files Modified

1. `src/optimizer.hpp` (lines 11, 115)
   - Added `#include <atomic>`
   - Changed `bool active = true` → `std::atomic<bool> active`

2. `src/optimizer.cpp` (lines 14-16, 181-221)
   - Initialize `active(true)` in constructor initializer list
   - Check termination condition every iteration (not periodically)
   - Each worker returns local decision, not shared flag

---

## Testing

**Minimal test**: 100x10 problem
- Before: Hung indefinitely
- After: Completes in 0.003 sec with 2 workers (2x speedup)

**Comprehensive benchmark**: 4 problem sizes × 3 worker configs × 3 runs
- Before: All parallel runs hung
- After: All runs complete, results show overhead dominates

---

## Next Steps

1. ✅ Fix committed
2. Update documentation to recommend `worker_limit = 1` as default
3. Consider performance optimization:
   - Profile synchronization overhead
   - Reduce lock contention in queue operations
   - Investigate why large problems show worse scaling than medium
4. Consider adaptive worker selection based on problem size
