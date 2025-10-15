# TreeFARMR

[![R-CMD-check](https://github.com/your-org/treefarmr/workflows/R-CMD-check/badge.svg)](https://github.com/your-org/treefarmr/actions)
[![Coverage](https://codecov.io/gh/your-org/treefarmr/branch/main/graph/badge.svg)](https://codecov.io/gh/your-org/treefarmr)
[![CRAN](https://www.r-pkg.org/badges/version/treefarmr)](https://cran.r-project.org/package=treefarmr)

R wrapper for TreeFARMS (Tree-based Fast and Accurate Rule Set Models) with support for log-loss optimization and probability predictions. This package provides direct Rcpp bindings to the C++ implementation, eliminating Python dependencies for easier distribution and better performance.

## Installation

### Prerequisites

1. **System Dependencies**: Install required system libraries:

**macOS:**
```bash
brew install gmp
```

**Ubuntu/Debian:**
```bash
sudo apt-get install libgmp-dev
```

**Windows:**
- Install Rtools (includes GMP)

2. **R Package Dependencies**: Install required R packages:

```r
install.packages(c("Rcpp", "RcppParallel", "jsonlite", "devtools"))
```

### Install TreeFARMR Package

```r
# Install from source
devtools::install("path/to/treefarmr")

# Or if using roxygen2 for documentation
devtools::document()
devtools::install()
```

## Quick Start

```r
library(treefarmr)

# Create sample binary data
set.seed(42)
n <- 200
X <- data.frame(
  feature_1 = sample(0:1, n, replace = TRUE),
  feature_2 = sample(0:1, n, replace = TRUE),
  feature_3 = sample(0:1, n, replace = TRUE)
)

# Create target with pattern
y <- as.numeric((X$feature_1 == 1 & X$feature_2 == 1) | 
                (X$feature_1 == 0 & X$feature_2 == 0))

# Train with misclassification loss
model_misclass <- treefarms(X, y, loss_function = "misclassification", regularization = 0.1)

# Train with log-loss
model_logloss <- treefarms(X, y, loss_function = "log_loss", regularization = 0.1)

# View model summary
print(model_logloss)

# Get probability predictions
probabilities <- model_logloss$probabilities
head(probabilities)

# Make predictions on new data
X_new <- data.frame(
  feature_1 = c(1, 0, 1),
  feature_2 = c(1, 0, 0), 
  feature_3 = c(0, 1, 1)
)

pred_class <- predict(model_logloss, X_new, type = "class")
pred_prob <- predict(model_logloss, X_new, type = "prob")

print("Binary predictions:")
print(pred_class)
print("Probability predictions:")
print(pred_prob)
```

## Key Features

### 1. Loss Function Options
```r
# Traditional misclassification loss
model_misclass <- treefarms(X, y, loss_function = "misclassification")

# Log-loss (cross-entropy) for probabilistic modeling
model_logloss <- treefarms(X, y, loss_function = "log_loss", regularization = 0.1)
```

### 2. Auto-Tuning
```r
# Let TreeFARMR automatically find optimal parameters
model_auto <- treefarms(X, y, regularization = NULL)

# Auto-tune with specific target
model_auto <- treefarms(X, y, regularization = NULL, target_trees = 3, max_trees = 5)
```

### 3. Parallel Execution
```r
# Use multiple threads for faster training
model <- treefarms(X, y, worker_limit = 4, regularization = 0.1)
```

### 4. Cross-Fitted Rashomon Sets
```r
# Find stable trees across cross-fitting folds
result <- cross_fitted_rashomon(X, y, K = 5, regularization = 0.1)

# Check if stable trees were found
print(result)  # Shows number of intersecting trees

# Use stable trees for prediction
if (result$n_intersecting > 0) {
  predictions <- predict(result, X_new)
  cat("Found", result$n_intersecting, "stable tree(s)!\n")
}
```

## Function Reference

### `treefarms(X, y, loss_function, regularization, rashomon_bound_multiplier, target_trees, max_trees, worker_limit, verbose, ...)`

Train a TreeFARMS model.

**Parameters:**
- `X`: Data.frame or matrix of binary features (0/1)
- `y`: Vector of binary class labels (0/1)
- `loss_function`: "misclassification" or "log_loss" (default: "misclassification")
- `regularization`: Model complexity control (default: 0.1, NULL for auto-tuning)
- `rashomon_bound_multiplier`: Rashomon set parameter (default: 0.05, NULL for auto-tuning)
- `target_trees`: Target number of trees for auto-tuning (default: 1)
- `max_trees`: Maximum acceptable trees for auto-tuning (default: 5)
- `worker_limit`: Number of parallel workers (default: 1)
- `verbose`: Print training progress (default: FALSE)

**Returns:** List with model object, predictions, probabilities, accuracy, and metadata.

### `predict(object, newdata, type, ...)`

Make predictions using a trained model.

**Parameters:**
- `object`: Trained TreeFARMS model
- `newdata`: New features to predict on
- `type`: "class" (binary) or "prob" (probabilities)

**Returns:** Binary predictions or probability matrix.

## Data Requirements

- **Features**: Must be binary (0/1) only
- **Target**: Must be binary (0/1) only
- **No missing values**: All data must be complete
- **Consistent feature names**: New data must have same feature names as training data

## Probability Characteristics

TreeFARMR probabilities are:
- **Bounded away from 0 and 1**: Typically range [0.2, 0.8]
- **Well-calibrated**: Correlate highly with true underlying probabilities
- **Consistent**: Same calculation used in training and prediction
- **Meaningful**: Reflect actual uncertainty in the data

## Testing

The package includes a comprehensive test suite:

```r
# Run all tests
devtools::test()

# Run specific test files
devtools::test_file("tests/testthat/test-api.R")
```

See `tests/README.md` for detailed testing documentation.

## Performance Benefits

This Rcpp-based implementation provides:
- **Faster execution**: Direct C++ calls without Python overhead
- **Easier distribution**: No Python environment setup required
- **Better reliability**: Fewer dependency issues
- **Standard R package**: Follows typical Rcpp package patterns
- **Parallel execution**: Multi-threaded training support

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `devtools::test()`
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Citation

If you use TreeFARMR in your research, please cite:

```bibtex
@software{treefarmr2024,
  title = {TreeFARMR: Tree-based Fast and Accurate Rule Set Models for R},
  author = {TreeFARMR Development Team},
  year = {2024},
  url = {https://code.rand.org/personal-packages/treefarmr}
}
```
