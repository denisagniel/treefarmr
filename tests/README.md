# TreeFARMS Test Suite

This directory contains a comprehensive test suite for the TreeFARMS R/C++ package, with emphasis on probability outputs and log-loss optimization.

## Overview

The test suite is organized into several categories:

1. **Core Functionality Tests**: Basic model fitting and prediction
2. **Probability Tests**: Validation of probability outputs
3. **Log-Loss Tests**: Log-loss specific functionality
4. **Edge Case Tests**: Invalid inputs, extreme values, special cases
5. **Stress Tests**: Large datasets, many features, repeated operations
6. **Memory Tests**: Memory safety with ASan, Valgrind, and R-specific tools
7. **Installation Tests**: Package loading and initialization

## Running Tests

### Standard Test Run

Run all tests using testthat:

```r
library(testthat)
test_dir("tests/testthat")
```

Or from the command line:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

### Running Specific Test Categories

Filter tests by category:

```r
# Probability tests
test_dir("tests/testthat", filter = "probabilities")

# Log-loss tests
test_dir("tests/testthat", filter = "logloss")

# Edge case tests
test_dir("tests/testthat", filter = "edge")

# Stress tests
test_dir("tests/testthat", filter = "stress")

# Memory tests (R-specific)
test_dir("tests/testthat", filter = "memory-r")
```

### Memory Instrumentation Tests

See [MEMORY_TESTING.md](MEMORY_TESTING.md) for detailed instructions on running memory instrumentation tests with ASan, Valgrind, and R-specific tools.

## Test Structure

```
tests/
├── testthat/
│   ├── helper-*.R          # Helper functions and test data
│   ├── test-probabilities.R # Probability validation tests
│   ├── test-logloss.R       # Log-loss specific tests
│   ├── test-edge-cases.R   # Edge case tests
│   ├── test-stress.R       # Stress tests
│   ├── test-memory-*.R     # Memory safety tests
│   └── test-installation.R # Installation/load tests
├── run_with_asan.sh        # ASan test harness
├── run_with_valgrind.sh    # Valgrind test harness
├── run_memory_tests.R      # R memory test runner
└── run_stress_tests.R      # Stress test runner
```

## Test Helpers

The test suite includes several helper functions:

- `setup_test_environment()`: Initialize test environment
- `expect_valid_treefarms_model()`: Validate model structure
- `expect_valid_probabilities()`: Validate probability outputs
- `expect_logloss_bounds()`: Validate log-loss probability bounds
- `run_with_memory_check()`: Run test with memory instrumentation
- `create_probability_test_data()`: Generate test datasets

See helper files in `testthat/` for details.

## Test Data

Test datasets are generated using helper functions in `helper-data.R`:

- `simple_dataset`: Basic binary classification dataset
- `entropy_dataset`: High entropy dataset for log-loss testing
- `pattern_dataset`: Dataset with clear pattern (XOR-like)
- `minimal_dataset`: Minimal valid dataset (4 samples, 2 features)
- `single_row_dataset`: Single row dataset
- `single_class_dataset`: All same class
- `many_features_dataset`: Many features (50+)
- `imbalanced_dataset`: Imbalanced class distribution

## Key Test Scenarios

### Probability Validation

- Probabilities sum to 1.0 per row
- Probabilities in [0, 1] bounds
- For log-loss: probabilities bounded away from 0/1 (e.g., [0.01, 0.99])
- Predictions consistent with probabilities (argmax)
- Probability calibration with class distribution

### Log-Loss Specific

- Loss decreases with training
- Cross-entropy calculation verification
- Probability bounds for log-loss
- Worker limit enforcement (worker_limit=1 for log-loss)

### Edge Cases

- Empty/minimal inputs
- Extreme parameter values
- Invalid inputs (non-binary, NaN, Inf)
- Special values (negative, zero, very large)

### Stress Tests

- Large datasets (1000-10000 samples)
- Many features (50-100 features)
- Deep trees (low regularization)
- Multiple sequential fits
- Long-running probability computations

### Memory Safety

- Memory leak detection
- Buffer overrun detection
- Use-after-free detection
- Uninitialized memory detection

## Test Coverage

See [TEST_COVERAGE.md](TEST_COVERAGE.md) for detailed coverage information.

## Documentation

- [FUNCTION_MAPPING.md](FUNCTION_MAPPING.md): Complete function mapping
- [TEST_SCENARIOS.md](TEST_SCENARIOS.md): Detailed test scenario design
- [MEMORY_TESTING.md](MEMORY_TESTING.md): Memory instrumentation guide

## Reproducibility

All tests use fixed random seeds for reproducibility. Test data generators use `set.seed(42)` by default.

## Continuous Integration

The test suite is designed to run in CI environments. Some stress tests are skipped on CRAN using `skip_on_cran()`.

## Troubleshooting

### Tests Fail with Memory Errors

- Run with memory instrumentation (ASan/Valgrind) to identify issues
- Check for memory leaks in test output
- Verify garbage collection is working

### Tests Timeout

- Some stress tests may take a long time
- Reduce dataset sizes in stress tests if needed
- Skip stress tests on CI if time is limited

### Package Loading Issues

- Verify package is installed correctly
- Check that C++ symbols are resolved
- Run installation tests to diagnose issues

## Contributing

When adding new tests:

1. Follow existing test structure and naming conventions
2. Use helper functions for common validations
3. Add appropriate test data generators
4. Document test scenarios in TEST_SCENARIOS.md
5. Update TEST_COVERAGE.md with new coverage
