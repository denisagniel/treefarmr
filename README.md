# TreeFARMR

[![R-CMD-check](https://github.com/denisagniel/treefarmr/workflows/R-CMD-check/badge.svg)](https://github.com/denisagniel/treefarmr/actions)
[![Coverage](https://codecov.io/gh/denisagniel/treefarmr/branch/main/graph/badge.svg)](https://codecov.io/gh/denisagniel/treefarmr)
[![CRAN](https://www.r-pkg.org/badges/version/treefarmr)](https://cran.r-project.org/package=treefarmr)

R implementation of TreeFARMS (Tree-based Fast and Accurate Rule Set Models) with support for log-loss optimization and probability predictions. This package provides direct Rcpp bindings to the integrated C++ implementation, eliminating external dependencies for easier distribution and better performance.

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
install.packages(c("Rcpp", "jsonlite", "devtools"))
```

### Install TreeFARMR Package

```r
# Install from GitHub
devtools::install_github("denisagniel/treefarmr")
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

# Fit a single tree (most common use case)
model_single <- fit_tree(X, y, loss_function = "misclassification", regularization = 0.1)

# Fit with rashomon set (exploratory analysis)
model_rashomon <- fit_rashomon(X, y, loss_function = "misclassification", regularization = 0.1)

# Or use treefarms() directly (defaults to single tree)
model_misclass <- treefarms(X, y, loss_function = "misclassification", regularization = 0.1)

# Train with log-loss
model_logloss <- treefarms(X, y, loss_function = "log_loss", regularization = 0.1)

# Regression (squared-error loss): continuous outcome, prediction = fitted values
y_cont <- 2 + 3*X$feature_1 - X$feature_2 + rnorm(n, 0, 0.5)
model_reg <- treefarms(X, y_cont, loss_function = "squared_error", regularization = 0.1, single_tree = TRUE)
fitted <- predict(model_reg$model, X)  # vector of fitted values

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

## Working with Continuous Features

TreeFARMS automatically discretizes continuous features using threshold-based splits. No preprocessing required:

```r
library(treefarmr)

# Continuous features - no preprocessing needed
set.seed(42)
X <- data.frame(
  age = runif(200, 18, 80),
  income = rnorm(200, 50000, 15000),
  score = rbeta(200, 2, 5)
)

y <- as.numeric(X$age > 40 & X$income > 45000)

# Fit tree (automatic discretization at median)
model <- treefarms(X, y, loss_function = "log_loss")

# Predictions work on continuous data
X_new <- data.frame(
  age = c(25, 55, 70),
  income = c(35000, 60000, 80000),
  score = c(0.3, 0.6, 0.8)
)

predictions <- predict(model, X_new, type = "prob")
print(predictions)

# Inspect discretization thresholds
print(model$discretization$features$age$thresholds)
print(model$discretization$features$income$thresholds)
```

### Custom Thresholds

You can specify custom thresholds for specific features:

```r
# Use custom thresholds for age and income
model <- treefarms(
  X, y,
  discretize_thresholds = list(
    age = c(30, 50, 65),      # 3 thresholds for age
    income = 50000            # 1 threshold for income
  )
)

# score will use default median discretization
```

### Discretization Methods

Two discretization methods are available:

- **Median (default)**: Creates one threshold at the median value
  ```r
  model <- treefarms(X, y, discretize_method = "median")
  # Each continuous feature gets 1 binary indicator
  ```

- **Quantiles**: Creates multiple thresholds using quantiles
  ```r
  model <- treefarms(X, y,
                    discretize_method = "quantiles",
                    discretize_bins = 4)
  # n_bins=4 creates 4 bins with 3 thresholds (quartiles)
  # Each continuous feature gets 3 binary indicators
  ```

- **Adaptive bins** (recommended for theory): Bins grow with sample size
  ```r
  model <- treefarms(X, y,
                    discretize_method = "quantiles",
                    discretize_bins = "adaptive")
  # Bins = max(2, ceiling(log(n)/3))
  # Required for theoretical convergence rate guarantees
  # Allows tree complexity to grow with n
  ```

### Mixed Binary and Continuous Features

TreeFARMS handles mixed feature types automatically:

```r
X_mixed <- data.frame(
  is_member = sample(0:1, 200, replace = TRUE),  # Binary
  age = runif(200, 18, 80),                       # Continuous
  purchased = sample(0:1, 200, replace = TRUE),  # Binary
  income = rnorm(200, 50000, 15000)              # Continuous
)

# Binary features pass through unchanged
# Continuous features are automatically discretized
model <- treefarms(X_mixed, y, loss_function = "log_loss")
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

### 3. Single Tree vs Rashomon Set
```r
# Fit exactly one tree (guaranteed n_trees = 1)
model_single <- fit_tree(X, y, regularization = 0.1)
print(model_single$n_trees)  # Always 1

# Fit with rashomon set (n_trees >= 1)
model_rashomon <- fit_rashomon(X, y, regularization = 0.1, rashomon_bound_multiplier = 0.05)
print(model_rashomon$n_trees)  # >= 1

# Extract all trees from rashomon set
trees <- get_rashomon_trees(model_rashomon)
cat("Found", length(trees), "trees in rashomon set\n")
```

### 4. Cross-Fitted Rashomon Sets
```r
# Cross-fitting with single tree per fold
result_single <- cross_fitted_rashomon(X, y, K = 5, regularization = 0.1, single_tree = TRUE)

# Cross-fitting with rashomon sets per fold (default)
result_rashomon <- cross_fitted_rashomon(X, y, K = 5, regularization = 0.1, single_tree = FALSE)

# Check if stable trees were found
print(result_rashomon)  # Shows number of intersecting trees

# Use stable trees for prediction
if (result_rashomon$n_intersecting > 0) {
  predictions <- predict(result_rashomon, X_new)
  cat("Found", result_rashomon$n_intersecting, "stable tree(s)!\n")
}
```

## Function Reference

### `fit_tree(X, y, loss_function, regularization, worker_limit, verbose, ...)`

Fit exactly one optimal tree. This is the most common use case.

**Parameters:**
- `X`: Data.frame or matrix of binary features (0/1)
- `y`: Vector of binary class labels (0/1)
- `loss_function`: "misclassification" or "log_loss" (default: "misclassification")
- `regularization`: Model complexity control (default: 0.1, NULL for auto-tuning)
- `worker_limit`: Number of workers (default: 1)
- `verbose`: Print training progress (default: FALSE)

**Returns:** List with model object, predictions, probabilities, accuracy, and metadata. Guaranteed `n_trees = 1`.

### `fit_rashomon(X, y, loss_function, regularization, rashomon_bound_multiplier, worker_limit, verbose, ...)`

Fit a TreeFARMS model with a full rashomon set (multiple near-optimal trees).

**Parameters:**
- `X`: Data.frame or matrix of binary features (0/1)
- `y`: Vector of binary class labels (0/1)
- `loss_function`: "misclassification" or "log_loss" (default: "misclassification")
- `regularization`: Model complexity control (default: 0.1, NULL for auto-tuning)
- `rashomon_bound_multiplier`: Rashomon set size control (default: 0.05, NULL for auto-tuning)
- `worker_limit`: Number of workers (default: 1)
- `verbose`: Print training progress (default: FALSE)

**Returns:** List with model object, predictions, probabilities, accuracy, and metadata. `n_trees >= 1` (number of trees in rashomon set).

### `treefarms(X, y, loss_function, regularization, rashomon_bound_multiplier, target_trees, max_trees, worker_limit, verbose, single_tree, ...)`

Train a TreeFARMS model. Convenience wrapper that defaults to `single_tree = TRUE`.

**Parameters:**
- `X`: Data.frame or matrix of binary features (0/1)
- `y`: Vector of binary class labels (0/1)
- `loss_function`: "misclassification" or "log_loss" (default: "misclassification")
- `regularization`: Model complexity control (default: 0.1, NULL for auto-tuning)
- `rashomon_bound_multiplier`: Rashomon set parameter (default: 0.05, NULL for auto-tuning)
- `target_trees`: Target number of trees for auto-tuning (default: 1)
- `max_trees`: Maximum acceptable trees for auto-tuning (default: 5)
- `worker_limit`: Number of workers (default: 1)
- `verbose`: Print training progress (default: FALSE)
- `single_tree`: If TRUE, fit exactly one tree (default: TRUE)

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
- **Faster execution**: Direct C++ calls without external process overhead
- **Easier distribution**: No external dependencies or environment setup required
- **Better reliability**: Fewer dependency issues and no subprocess management
- **Standard R package**: Follows typical Rcpp package patterns
- **Single-threaded execution**: Stable, reliable operation without external threading dependencies

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
  author = {Denis Agniel},
  year = {2025},
  url = {https://github.com/denisagniel/treefarmr}
}
```
