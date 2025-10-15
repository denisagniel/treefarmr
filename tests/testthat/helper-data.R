# Helper datasets for TreeFARMS testing
# These datasets are designed to be lightweight and cover various scenarios

# Simple synthetic datasets (quick generation)
simple_dataset <- local({
  set.seed(42)
  list(
    X = data.frame(
      feature_1 = sample(0:1, 100, replace = TRUE),
      feature_2 = sample(0:1, 100, replace = TRUE),
      feature_3 = sample(0:1, 100, replace = TRUE)
    ),
    y = sample(0:1, 100, replace = TRUE)
  )
})

# Pattern dataset with clear XOR relationship for reliable learning
pattern_dataset <- local({
  list(
    X = data.frame(
      x1 = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
      x2 = c(rep(0, 25), rep(0, 25), rep(1, 25), rep(1, 25))
    ),
    y = c(rep(0, 50), rep(1, 50))  # Clear pattern: x1 XOR x2
  )
})

# Minimal dataset for edge case testing
minimal_dataset <- local({
  list(
    X = data.frame(f1 = c(0, 1, 0, 1)),
    y = c(0, 1, 1, 0)
  )
})

# Complex/realistic datasets
imbalanced_dataset <- local({
  set.seed(123)
  n_0 <- 180
  n_1 <- 20
  list(
    X = data.frame(
      feature_1 = sample(0:1, n_0 + n_1, replace = TRUE),
      feature_2 = sample(0:1, n_0 + n_1, replace = TRUE),
      feature_3 = sample(0:1, n_0 + n_1, replace = TRUE)
    ),
    y = c(rep(0, n_0), rep(1, n_1))  # 90/10 class split
  )
})

# Many features dataset
many_features_dataset <- local({
  set.seed(456)
  list(
    X = data.frame(
      f1 = sample(0:1, 100, replace = TRUE),
      f2 = sample(0:1, 100, replace = TRUE),
      f3 = sample(0:1, 100, replace = TRUE),
      f4 = sample(0:1, 100, replace = TRUE),
      f5 = sample(0:1, 100, replace = TRUE),
      f6 = sample(0:1, 100, replace = TRUE),
      f7 = sample(0:1, 100, replace = TRUE),
      f8 = sample(0:1, 100, replace = TRUE),
      f9 = sample(0:1, 100, replace = TRUE),
      f10 = sample(0:1, 100, replace = TRUE)
    ),
    y = sample(0:1, 100, replace = TRUE)
  )
})

# Single class dataset (edge case)
single_class_dataset <- local({
  list(
    X = data.frame(
      feature_1 = sample(0:1, 50, replace = TRUE),
      feature_2 = sample(0:1, 50, replace = TRUE)
    ),
    y = rep(0, 50)  # All labels are 0
  )
})

# High entropy dataset for log-loss testing
entropy_dataset <- local({
  set.seed(789)
  # Create data where log-loss should show different behavior than misclassification
  X <- data.frame(
    x1 = sample(0:1, 200, replace = TRUE),
    x2 = sample(0:1, 200, replace = TRUE),
    x3 = sample(0:1, 200, replace = TRUE)
  )
  # Create probabilistic relationship
  prob <- 0.3 + 0.4 * (X$x1 == 1) + 0.2 * (X$x2 == 1) - 0.1 * (X$x3 == 1)
  prob <- pmax(0.1, pmin(0.9, prob))  # Bound between 0.1 and 0.9
  y <- rbinom(200, 1, prob)
  list(X = X, y = y)
})

# Empty dataset (edge case)
empty_dataset <- local({
  list(
    X = data.frame(feature_1 = integer(0), feature_2 = integer(0)),
    y = integer(0)
  )
})

# Single row dataset (edge case)
single_row_dataset <- local({
  list(
    X = data.frame(feature_1 = 1, feature_2 = 0),
    y = 1
  )
})

# All zeros dataset (edge case)
all_zeros_dataset <- local({
  list(
    X = data.frame(
      feature_1 = rep(0, 20),
      feature_2 = rep(0, 20)
    ),
    y = rep(0, 20)
  )
})

# All ones dataset (edge case)
all_ones_dataset <- local({
  list(
    X = data.frame(
      feature_1 = rep(1, 20),
      feature_2 = rep(1, 20)
    ),
    y = rep(1, 20)
  )
})

# Dataset with one feature (minimal features)
one_feature_dataset <- local({
  list(
    X = data.frame(feature_1 = sample(0:1, 50, replace = TRUE)),
    y = sample(0:1, 50, replace = TRUE)
  )
})
