# ThreadSanitizer (TSAN) Testing Instructions

## Purpose

ThreadSanitizer (TSAN) detects data races and other threading issues at runtime. After implementing graph thread-safety fixes, run these tests to verify no data races remain.

## Prerequisites

- Clang compiler (TSAN works best with Clang)
- R built with TSAN support (optional but recommended)

## Compilation with TSAN

### Option 1: Compile Package with TSAN

```bash
cd optimaltrees

# Set compiler flags for TSAN
export CXX="clang++"
export CXXFLAGS="-g -O1 -fsanitize=thread"
export LDFLAGS="-fsanitize=thread"

# Install package
R CMD INSTALL .
```

### Option 2: Configure R with TSAN (Advanced)

For comprehensive testing, rebuild R with TSAN:

```bash
# When building R from source, use:
./configure --with-blas --with-lapack \
  CC="clang -fsanitize=thread" \
  CXX="clang++ -fsanitize=thread" \
  LDFLAGS="-fsanitize=thread"
make
make install
```

## Running Tests with TSAN

### Basic Test

```bash
# Set TSAN options
export TSAN_OPTIONS="halt_on_error=1 second_deadlock_stack=1"

# Run a simple test
Rscript -e "
library(optimaltrees)
set.seed(123)
X <- matrix(rnorm(100*5), 100, 5)
y <- rbinom(100, 1, 0.5)
result <- treefarms(X, y, worker_limit=4, time_limit=10)
print('Test completed without data races')
"
```

### Stress Test

```bash
export TSAN_OPTIONS="halt_on_error=1"
export RUN_STRESS_TESTS="true"

Rscript -e "
library(testthat)
library(optimaltrees)
test_file('tests/testthat/test-threadsafety-stress.R')
"
```

### Full Test Suite

```bash
export TSAN_OPTIONS="halt_on_error=1 history_size=7"

R CMD check --as-cran optimaltrees_*.tar.gz
```

## Expected Behavior

### Success (No Data Races)

```
Test completed without data races
```

TSAN will remain silent if no threading issues are detected.

### Failure (Data Race Detected)

TSAN will print detailed reports like:

```
==================
WARNING: ThreadSanitizer: data race (pid=12345)
  Write of size 8 at 0x7b0400001234 by thread T2:
    #0 Optimizer::store_self() optimizer/dispatch/dispatch.hpp:214
    ...
  Previous write of size 8 at 0x7b0400001234 by thread T1:
    #0 Optimizer::store_self() optimizer/dispatch/dispatch.hpp:214
    ...
==================
```

## Interpreting TSAN Output

- **data race**: Multiple threads accessing same memory without synchronization
- **lock-order-inversion**: Potential deadlock from inconsistent lock ordering
- **thread leak**: Thread not properly joined

## Fixed Issues (Verified)

After implementing all Phase 1-5 fixes, the following should show NO data races:

1. ✅ Graph vertices/bounds/children/edges concurrent access
2. ✅ Configuration static members (documented as read-only after init)
3. ✅ LocalState operations (copy constructor kept, assignment deleted)
4. ✅ Queue operations (already had proper locking)

## Limitations

- TSAN has ~5-15x performance overhead
- May report false positives in some cases
- Requires recompilation of entire package

## Troubleshooting

### "TSAN: failed to intercept"

This usually means TSAN couldn't instrument some library. Safe to ignore if it's not from optimaltrees code.

### Slowness

TSAN adds significant overhead. Use shorter time_limit in tests:

```r
treefarms(..., worker_limit=4, time_limit=5)  # Shorter timeout
```

### False Positives

If TSAN reports races in R internals or system libraries, use suppressions:

```bash
export TSAN_OPTIONS="suppressions=tsan-suppressions.txt"
```

Create `tsan-suppressions.txt`:
```
race:R_*
race:Rf_*
```

## Verification Protocol (from Plan)

```bash
cd optimaltrees

# 1. Compile with TSAN
R CMD INSTALL . --configure-args="CXX='clang++ -fsanitize=thread -g'"

# 2. Run stress tests
TSAN_OPTIONS="halt_on_error=1" Rscript tests/testthat/test-threadsafety-stress.R

# 3. Run full test suite
TSAN_OPTIONS="halt_on_error=1" R CMD check --as-cran .
```

## Success Criteria

- ✅ No TSAN warnings with worker_limit=4
- ✅ All tests pass
- ✅ DML estimates remain accurate and stable across runs
- ✅ No crashes or segfaults

## Additional Resources

- TSAN documentation: https://github.com/google/sanitizers/wiki/ThreadSanitizerCppManual
- R with sanitizers: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Checking-memory-access
