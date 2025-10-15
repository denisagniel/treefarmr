# TreeFARMR Test Suite

## Overview

This directory contains a comprehensive automated test suite for the TreeFARMR R package. The test suite provides full coverage of R interfaces, algorithmic correctness, parallel execution, cleanup behavior, and edge cases.

## Test Organization

### Test Files

- **`test-api.R`** - R-level interface tests and basic workflows
- **`test-logloss.R`** - Log-loss specific regression tests
- **`test-parallel.R`** - Parallel/multithreading functionality tests
- **`test-cleanup.R`** - Cleanup and memory safety tests
- **`test-edge-cases.R`** - Edge input handling and boundary conditions
- **`test-treefarms.R`** - Core functionality tests (existing)
- **`test-predict.R`** - Prediction functionality tests (existing)
- **`test-auto-tuning.R`** - Auto-tuning functionality tests (existing)
- **`test-s3-methods.R`** - S3 methods tests (existing)
- **`test-cross-fitted-rashomon.R`** - Cross-fitted Rashomon tests (existing)
- **`test-rashomon-utils.R`** - Rashomon utilities tests (existing)

### Helper Files

- **`helper-data.R`** - Shared test datasets for consistent testing
- **`helper-setup.R`** - Test utilities and helper functions

## Running Tests Locally

### Run All Tests
```r
# Using devtools
devtools::test()

# Using testthat directly
testthat::test_dir("tests/testthat/")
```

### Run Specific Test Files
```r
# Single test file
devtools::test_file("tests/testthat/test-api.R")

# Multiple test files
devtools::test_file(c("tests/testthat/test-api.R", "tests/testthat/test-logloss.R"))
```

### Run Full Package Check
```bash
# From command line
R CMD check .

# From R
devtools::check()
```

## Test Datasets

The test suite includes several pre-defined datasets in `helper-data.R`:

### Simple Datasets
- **`simple_dataset`** - 100 samples, 3 binary features, random labels
- **`pattern_dataset`** - 100 samples with XOR pattern for reliable learning
- **`minimal_dataset`** - 4 samples for edge case testing

### Complex Datasets
- **`imbalanced_dataset`** - 90/10 class split (200 samples)
- **`many_features_dataset`** - 10 binary features (100 samples)
- **`entropy_dataset`** - High entropy labels for log-loss testing

### Edge Case Datasets
- **`single_class_dataset`** - All labels are 0
- **`empty_dataset`** - 0 rows (for error testing)
- **`single_row_dataset`** - 1 row (for edge case testing)
- **`all_zeros_dataset`** - All features are 0
- **`all_ones_dataset`** - All features are 1

## Test Coverage

The test suite covers:

- ✅ R-level interface functionality
- ✅ Input validation and error handling
- ✅ Both loss functions (misclassification and log-loss)
- ✅ Parallel execution with different worker limits
- ✅ Auto-tuning functionality
- ✅ Memory cleanup and safety
- ✅ Edge cases and boundary conditions
- ✅ S3 methods (print, summary, predict)
- ✅ Cross-fitted Rashomon analysis

## Performance

- **Total runtime**: ~2 minutes for full test suite
- **Individual files**: <30 seconds each (CRAN compliant)
- **Memory usage**: Minimal, with proper cleanup
- **Parallel execution**: Tests run independently

## CI/CD Integration

### GitHub Actions
- **R-CMD-check.yml** - Multi-OS testing (Ubuntu, macOS, Windows)
- **test-coverage.yml** - Coverage analysis and reporting

### Coverage Requirements
- Minimum 80% coverage for core functions
- Coverage reports uploaded to Codecov
- PR comments with coverage changes

## Adding New Tests

### Guidelines
1. Use existing helper datasets when possible
2. Follow the test structure pattern:
   ```r
   test_that("descriptive test name", {
     # Setup
     data <- simple_dataset
     
     # Execute
     expect_no_error({
       result <- treefarms(data$X, data$y, verbose = FALSE)
     })
     
     # Verify
     expect_valid_treefarms_model(result)
   })
   ```
3. Keep tests lightweight (<30 seconds)
4. Use `expect_valid_treefarms_model()` for model validation
5. Clean up after tests with `teardown_test_environment()`

### Helper Functions
- **`expect_valid_treefarms_model(model)`** - Validates model structure
- **`expect_valid_predictions(pred, n_samples)`** - Validates predictions
- **`expect_models_consistent(model1, model2)`** - Compares two models
- **`safe_treefarms(...)`** - Wrapper with error handling
- **`setup_test_environment()`** - Test setup
- **`teardown_test_environment()`** - Test cleanup

## Troubleshooting

### Common Issues
1. **Tests fail with "package not found"** - Run `devtools::load_all()` first
2. **Memory issues** - Tests include cleanup, but restart R if needed
3. **Parallel test failures** - Check RcppParallel installation
4. **Coverage not working** - Ensure covr package is installed

### Debug Mode
```r
# Run tests with verbose output
testthat::test_dir("tests/testthat/", reporter = "progress")

# Run single test with debug info
testthat::test_file("tests/testthat/test-api.R", reporter = "progress")
```

## CI Badges

Add these badges to your README:

```markdown
[![R-CMD-check](https://github.com/your-org/treefarmr/workflows/R-CMD-check/badge.svg)](https://github.com/your-org/treefarmr/actions)
[![Coverage](https://codecov.io/gh/your-org/treefarmr/branch/main/graph/badge.svg)](https://codecov.io/gh/your-org/treefarmr)
[![CRAN](https://www.r-pkg.org/badges/version/treefarmr)](https://cran.r-project.org/package=treefarmr)
```

## Contributing

When contributing to the test suite:
1. Add tests for new functionality
2. Update existing tests if behavior changes
3. Ensure all tests pass locally
4. Update this documentation if needed
5. Follow CRAN compliance guidelines
