#' Discretize Features for TreeFARMS
#'
#' @description
#' Converts continuous features to binary indicators using threshold-based
#' discretization. Binary features are passed through unchanged. This is the
#' preprocessing layer that allows TreeFARMS to work with continuous input.
#'
#' @param X A data.frame of features (continuous or binary)
#' @param method Discretization method: "median" or "quantiles"
#' @param n_bins Number of bins for quantile discretization (creates n_bins-1 thresholds).
#'   Can be numeric >= 2, or "adaptive" for data-dependent bins that grow with sample size.
#'   Adaptive uses: max(2, ceiling(log(n) / 3)).
#' @param thresholds Optional named list of user-provided thresholds for specific features
#'
#' @return A list with two elements:
#'   \item{X_binary}{Data.frame with binary features}
#'   \item{metadata}{List containing discretization metadata}
#'
#' @keywords internal
discretize_features <- function(X, method = "median", n_bins = 2, thresholds = NULL) {

  # Validate inputs
  if (!is.data.frame(X)) {
    stop("X must be a data.frame")
  }

  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("X must have at least one row and one column")
  }

  if (!method %in% c("median", "quantiles")) {
    stop("method must be 'median' or 'quantiles'")
  }

  # Handle adaptive n_bins
  if (is.character(n_bins)) {
    if (n_bins == "adaptive") {
      n <- nrow(X)
      # Theory: need bins ~ log(n) to allow tree complexity to grow with n
      # Practical choice: max(2, ceiling(log(n) / 3))
      n_bins <- max(2, ceiling(log(n) / 3))
      if (n_bins < 2) n_bins <- 2  # Safety
    } else {
      stop("n_bins must be numeric >= 2 or 'adaptive'")
    }
  }

  if (!is.numeric(n_bins) || n_bins < 2) {
    stop("n_bins must be a numeric value >= 2 or 'adaptive'")
  }

  # Fast path: Check if all features are already binary
  all_binary <- all(vapply(X, is_binary, logical(1)))

  if (all_binary) {
    # Minimal metadata for binary-only data
    features_meta <- lapply(names(X), function(name) {
      list(
        type = "binary",
        new_names = name
      )
    })
    names(features_meta) <- names(X)

    metadata <- list(
      all_binary = TRUE,
      method = method,
      n_bins = n_bins,
      features = features_meta,
      binary_names = names(X)
    )

    return(list(
      X_binary = X,
      metadata = metadata
    ))
  }

  # Process each feature
  binary_cols_list <- list()
  features_meta <- list()

  for (col_name in names(X)) {
    x <- X[[col_name]]

    # Get user-provided threshold for this feature (if any)
    user_threshold <- if (!is.null(thresholds)) thresholds[[col_name]] else NULL

    # Discretize this feature
    result <- discretize_feature_column(
      x = x,
      name = col_name,
      method = method,
      n_bins = n_bins,
      user_threshold = user_threshold
    )

    # Store binary columns
    binary_cols_list[[col_name]] <- result$binary_cols

    # Store metadata
    features_meta[[col_name]] <- result$meta
  }

  # Combine all binary columns into a single data.frame
  X_binary <- do.call(cbind, binary_cols_list)

  # Check for feature name collisions
  all_names <- unlist(lapply(features_meta, function(m) m$new_names))
  if (any(duplicated(all_names))) {
    dups <- unique(all_names[duplicated(all_names)])
    stop("Feature name collision detected after discretization: ",
         paste(dups, collapse = ", "),
         ". Please rename your original features to avoid conflicts with ",
         "generated names (e.g., 'feature_leq_1').")
  }

  # Build metadata
  metadata <- list(
    all_binary = FALSE,
    method = method,
    n_bins = n_bins,
    features = features_meta,
    binary_names = colnames(X_binary)
  )

  return(list(
    X_binary = X_binary,
    metadata = metadata
  ))
}


#' Discretize a Single Feature Column
#'
#' @param x Numeric vector (feature values)
#' @param name Feature name
#' @param method Discretization method
#' @param n_bins Number of bins for quantiles
#' @param user_threshold Optional user-provided thresholds
#'
#' @return A list with binary_cols (data.frame) and meta (list)
#'
#' @keywords internal
discretize_feature_column <- function(x, name, method, n_bins, user_threshold = NULL) {

  # Check if already binary
  if (is_binary(x)) {
    binary_cols <- data.frame(x)
    names(binary_cols) <- name

    meta <- list(
      type = "binary",
      new_names = name
    )

    return(list(
      binary_cols = binary_cols,
      meta = meta
    ))
  }

  # Handle edge case: 2 unique values not in {0,1}
  unique_vals <- unique(x[!is.na(x)])
  if (length(unique_vals) == 2) {
    # Map to {0, 1}
    x_binary <- as.numeric(x == max(unique_vals))
    binary_cols <- data.frame(x_binary)
    names(binary_cols) <- name

    meta <- list(
      type = "binary_converted",
      original_values = sort(unique_vals),
      new_names = name
    )

    return(list(
      binary_cols = binary_cols,
      meta = meta
    ))
  }

  # Handle edge case: All identical values
  if (length(unique_vals) == 1) {
    # Create a single binary column = 0 (tree can't split but needs column)
    x_binary <- rep(0, length(x))
    binary_cols <- data.frame(x_binary)
    names(binary_cols) <- paste0(name, "_leq_1")

    meta <- list(
      type = "constant",
      value = unique_vals[1],
      thresholds = numeric(0),
      new_names = paste0(name, "_leq_1")
    )

    return(list(
      binary_cols = binary_cols,
      meta = meta
    ))
  }

  # Compute thresholds for continuous feature
  thresholds <- compute_thresholds(x, method, n_bins, user_threshold)

  # Create binary indicators
  binary_cols <- data.frame(
    lapply(seq_along(thresholds), function(i) {
      as.numeric(x <= thresholds[i])
    })
  )

  # Name binary columns: feature_leq_1, feature_leq_2, ...
  new_names <- paste0(name, "_leq_", seq_along(thresholds))
  names(binary_cols) <- new_names

  meta <- list(
    type = "continuous",
    thresholds = thresholds,
    new_names = new_names
  )

  return(list(
    binary_cols = binary_cols,
    meta = meta
  ))
}


#' Compute Discretization Thresholds
#'
#' @param x Numeric vector
#' @param method Discretization method
#' @param n_bins Number of bins for quantiles
#' @param user_threshold Optional user-provided thresholds
#'
#' @return Numeric vector of thresholds
#'
#' @keywords internal
compute_thresholds <- function(x, method, n_bins, user_threshold = NULL) {

  # Use user-provided thresholds if available
  if (!is.null(user_threshold)) {
    if (!is.numeric(user_threshold)) {
      stop("User-provided thresholds must be numeric")
    }
    return(sort(user_threshold))
  }

  # Remove NA values
  x_clean <- x[!is.na(x)]

  if (length(x_clean) == 0) {
    stop("Cannot compute thresholds: all values are NA")
  }

  # Compute thresholds based on method
  if (method == "median") {
    threshold <- median(x_clean)
    return(threshold)

  } else if (method == "quantiles") {
    # n_bins bins → n_bins-1 thresholds
    # Example: n_bins=4 → probs=[0.25, 0.5, 0.75]
    probs <- seq(0, 1, length.out = n_bins + 1)
    probs <- probs[-c(1, length(probs))]  # Remove 0 and 1

    thresholds <- quantile(x_clean, probs = probs, names = FALSE)

    # Remove duplicates (can happen if many repeated values)
    thresholds <- unique(thresholds)

    return(thresholds)
  }

  stop("Unknown discretization method: ", method)
}


#' Apply Stored Discretization to New Data
#'
#' @description
#' Applies the discretization metadata from training to new prediction data.
#' Ensures feature names match and applies the same thresholds.
#'
#' @param X A data.frame of new features (must match training features)
#' @param metadata Discretization metadata from training
#'
#' @return A data.frame with binary features (same structure as training)
#'
#' @keywords internal
apply_discretization <- function(X, metadata) {

  if (!is.data.frame(X)) {
    stop("X must be a data.frame")
  }

  # Fast path: If all features were binary during training, just validate
  if (!is.null(metadata$all_binary) && metadata$all_binary) {
    # Check that all training features are present
    missing <- setdiff(names(metadata$features), names(X))
    if (length(missing) > 0) {
      stop("Features missing in newdata: ", paste(missing, collapse = ", "),
           ". Training features: ", paste(names(metadata$features), collapse = ", "))
    }

    # Return X with columns in training order
    return(X[, names(metadata$features), drop = FALSE])
  }

  # Check that all training features are present
  missing <- setdiff(names(metadata$features), names(X))
  if (length(missing) > 0) {
    stop("Features missing in newdata: ", paste(missing, collapse = ", "),
         ". Training features: ", paste(names(metadata$features), collapse = ", "))
  }

  # Process each feature
  binary_cols_list <- list()

  for (col_name in names(metadata$features)) {
    x <- X[[col_name]]
    feature_meta <- metadata$features[[col_name]]

    # Handle different feature types
    if (feature_meta$type == "binary") {
      # Already binary - pass through
      binary_cols_list[[col_name]] <- data.frame(x)
      names(binary_cols_list[[col_name]]) <- col_name

    } else if (feature_meta$type == "binary_converted") {
      # Was converted from 2 unique values to {0,1}
      x_binary <- as.numeric(x == max(feature_meta$original_values))
      binary_cols_list[[col_name]] <- data.frame(x_binary)
      names(binary_cols_list[[col_name]]) <- col_name

    } else if (feature_meta$type == "constant") {
      # All identical values during training
      x_binary <- rep(0, length(x))
      binary_cols_list[[col_name]] <- data.frame(x_binary)
      names(binary_cols_list[[col_name]]) <- feature_meta$new_names

    } else if (feature_meta$type == "continuous") {
      # Apply stored thresholds
      thresholds <- feature_meta$thresholds
      binary_cols <- data.frame(
        lapply(seq_along(thresholds), function(i) {
          as.numeric(x <= thresholds[i])
        })
      )
      names(binary_cols) <- feature_meta$new_names
      binary_cols_list[[col_name]] <- binary_cols

    } else {
      stop("Unknown feature type in metadata: ", feature_meta$type)
    }
  }

  # Combine all binary columns
  X_binary <- do.call(cbind, binary_cols_list)

  # Ensure columns are in same order as training
  X_binary <- X_binary[, metadata$binary_names, drop = FALSE]

  return(X_binary)
}


#' Check if a Vector is Binary
#'
#' @param x A vector
#' @return Logical: TRUE if all non-NA values are in {0, 1}
#'
#' @keywords internal
is_binary <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(FALSE)
  all(x_clean %in% c(0, 1))
}
