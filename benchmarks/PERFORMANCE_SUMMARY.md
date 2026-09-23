# Parallel Performance Summary - optimaltrees v0.4.0

## TL;DR

**Recommendation: Use `worker_limit = 1` (default) for most use cases.**

Parallelization provides modest speedups (1.2-1.4x with 4 workers at best) due to shared queue contention. Many small problems see slowdowns with multiple workers.

---

## Benchmark Results

### Problem Size: Tiny (n=100, p=10)

| Workers | Time (sec) | Speedup | Efficiency |
|---------|-----------|---------|------------|
| 1       | 0.003     | 1.00x   | 100%       |
| 2       | 0.004     | 0.71x   | 36%        |
| 4       | 0.003     | 0.87x   | 22%        |

**Result**: Overhead dominates. Parallelization causes **slowdowns**.

---

### Problem Size: Small (n=300, p=15)

| Workers | Time (sec) | Speedup | Efficiency |
|---------|-----------|---------|------------|
| 1       | 0.005     | 1.00x   | 100%       |
| 2       | 0.004     | 1.17x   | 59%        |
| 4       | 0.004     | 1.19x   | 30%        |

**Result**: Modest benefit with 2 workers (59% efficient), poor scaling to 4 workers.

---

### Problem Size: Medium (n=800, p=25)

| Workers | Time (sec) | Speedup | Efficiency |
|---------|-----------|---------|------------|
| 1       | 0.016     | 1.00x   | 100%       |
| 2       | 0.013     | 1.25x   | 62%        |
| 4       | 0.011     | 1.44x   | 36%        |

**Result**: **BEST CASE**. Reasonable benefit with 2 workers (62% efficient), moderate with 4 workers.

---

### Problem Size: Large (n=1500, p=35)

| Workers | Time (sec) | Speedup | Efficiency |
|---------|-----------|---------|------------|
| 1       | 0.025     | 1.00x   | 100%       |
| 2       | 0.026     | 0.98x   | 49%        |
| 4       | 0.024     | 1.05x   | 26%        |

**Result**: Overhead + memory contention dominate. Minimal benefit, often slowdowns.

---

## Why Is Parallel Performance Poor?

### Root Cause: Shared Queue Bottleneck

The algorithm uses a work-stealing queue where all workers compete for a single lock:

```cpp
bool Queue::pop(Message & message) {
    std::lock_guard<std::mutex> lock(queue_mutex);  // ALL workers wait here
    if (!this -> queue.empty()) {
        internal_message = this -> queue.top();
        this -> queue.pop();
    }
}
```

**With 4 workers:**
- 1 worker holds lock, pops message (~100 nanoseconds)
- 3 workers idle, waiting for lock
- Result: **75% of worker time is idle**

### Additional Overhead Sources

1. **Atomic flag checks**: `active` flag checked every iteration, causes CPU cache invalidation
2. **Graph synchronization**: `state.graph` is shared, likely locked during writes
3. **Memory contention**: Workers fight for cache lines containing shared data

### Why Medium Problems Scale Best

- **Tiny/Small**: Work items finish in microseconds → queue lock is bottleneck
- **Medium**: Work items take milliseconds → computation dominates lock time (**sweet spot**)
- **Large**: Memory pressure + cache thrashing → additional contention

---

## Comparison to Ideal Parallelism

| Parallelism Type | Typical Efficiency | Example |
|-----------------|-------------------|---------|
| Embarrassingly Parallel | 90-95% | CV folds, bootstrap samples |
| MapReduce | 70-80% | Data partitioning + merge |
| **This Algorithm** | **30-60%** | Shared queue with fine-grained tasks |

This algorithm wasn't designed for parallelism—it's a serial algorithm with locks added.

---

## When to Use Parallelization

### Use `worker_limit = 2` if:
- ✓ Problem size is medium (n=500-1000, p=20-30)
- ✓ You've benchmarked your specific problem and confirmed speedup
- ✓ You're willing to accept ~60% efficiency for modest gains

### Use `worker_limit = 4` if:
- ✓ Problem size is medium-large (n=800-1200)
- ✓ You've confirmed 1.3-1.5x speedup on your data
- ✓ You have spare CPU cores (not running other intensive tasks)

### Always use `worker_limit = 1` if:
- Problem is small (n < 500)
- Problem is very large (n > 1500) unless you've tested
- You need predictable performance
- You value simplicity over marginal speedups

---

## Future Improvements

### Minor (might reach 2x speedup):
- Lock-free queue implementation
- Thread-local work caching
- Reduce atomic flag check frequency

### Major (might reach 3-4x speedup):
- Partition search space across workers (no shared queue)
- Lock-free graph data structure
- Batch operations to reduce synchronization

### Alternative (near-linear speedup):
- Parallelize across CV folds or bootstrap samples instead of tree search
- Would achieve 90%+ efficiency with embarrassingly parallel workload

---

## Running Benchmarks

```r
# Quick 5-run check (30 seconds)
source("benchmarks/quick_performance_check.R")

# Comprehensive 4-size test (2-3 minutes)
source("benchmarks/large_problem_benchmark.R")

# Minimal hang detection
source("benchmarks/minimal_parallel_test.R")
```

---

## References

- **Bug Fix**: `PARALLEL_DEADLOCK_FIX_2026-03-18.md` - Fixed infinite hang with worker_limit > 1
- **Implementation**: `src/optimizer.cpp`, `src/queue.cpp` - Shared queue with mutex locks
- **Commit**: `6dff8ed` - "Fix critical deadlock in parallel execution"

---

**Last Updated**: 2026-03-18
**Package Version**: optimaltrees v0.4.0
