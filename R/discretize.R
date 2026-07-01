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
#'   Can be numeric >= 2, or a character schedule: "adaptive" (default meaning) uses a
#'   theory-motivated polynomial rate that refines with n, \code{ceiling(n^(1/3))} capped
#'   for feasibility (see \code{\link{compute_bin_count}}); "log" reproduces the legacy
#'   \code{ceiling(log(n)/3)} schedule. Note "cv" (data-driven selection via
#'   \code{\link{select_bins_cv}}) must be resolved by the caller, which has the outcome y.
#'   For binary covariates no bins are created regardless. See \code{\link{compute_bin_count}}
#'   for the theory link (grid must refine polynomially to attain the continuous rate).
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

  # Resolve a character schedule ("adaptive"/"log") to an integer bin count.
  # "cv" cannot be resolved here (it needs the outcome y); callers that have y
  # must resolve it to a numeric before calling (see select_bins_cv()).
  if (is.character(n_bins)) {
    if (identical(n_bins, "cv")) {
      stop("n_bins = 'cv' must be resolved by the caller (it needs the outcome y); ",
           "use select_bins_cv() upstream, then pass the selected integer.",
           call. = FALSE)
    }
    n_bins <- compute_bin_count(n_bins, nrow(X))
  }

  if (!is.numeric(n_bins) || n_bins < 2) {
    stop("n_bins must be a numeric value >= 2, 'adaptive', 'log', or 'cv'")
  }

  # Fast path: Check if all features are already binary
  all_binary <- all(vapply(X, is_binary, logical(1)))

  if (all_binary) {
    # Minimal metadata for binary-only data
    features_meta <- purrr::map(names(X), ~ {
      list(
        type = "binary",
        new_names = .x
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
  if (length(binary_cols_list) == 0) {
    stop("No features to discretize. X has no columns or all were filtered.", call. = FALSE)
  }
  X_binary <- do.call(cbind, binary_cols_list)

  # Check for feature name collisions
  all_names <- purrr::map(features_meta, "new_names") |> unlist()
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


#' Resolve a bin-count schedule to an integer number of bins
#'
#' @description
#' Maps a character schedule to a concrete bin count as a function of sample
#' size \code{n}. This is the per-coordinate discretization resolution used to
#' turn continuous covariates into binary features.
#'
#' \strong{Theory link.} The convergence theory for tree nuisance estimators
#' (piecewise sparse anisotropic Besov class) requires the per-coordinate grid
#' to \emph{refine with n} at a polynomial rate,
#' \eqn{m_{n,i} \gtrsim n^{(\alpha_{\min}/\alpha_i)/(2\bar\alpha + s)}}, where
#' \eqn{\bar\alpha} is the harmonic-mean anisotropic smoothness and \eqn{s} the
#' sparsity. A logarithmically growing grid (the previous default) is too coarse
#' to attain the continuous rate: it yields convergence only to a fixed-grid
#' approximation of the truth. Since \eqn{\bar\alpha} and \eqn{s} are unknown in
#' practice, the \code{"adaptive"} default uses a conservative fixed-exponent
#' polynomial surrogate \eqn{m_n = \lceil c\, n^{\rho}\rceil} with \eqn{\rho =
#' 1/3}, which dominates the log schedule and meets the required rate for a broad
#' range of \eqn{(\bar\alpha, s)}. For binary covariates no bins are needed at all
#' (a tree fits the active subcube exactly). Data-driven selection is available
#' via \code{"cv"} (see \code{\link{select_bins_cv}}).
#'
#' @param schedule Character: "adaptive" (polynomial, default), "log" (legacy
#'   \eqn{\lceil \log(n)/3 \rceil}), or a numeric value (returned as-is after
#'   validation).
#' @param n Sample size.
#' @param rho Polynomial exponent for "adaptive" (default 1/3, must be in (0, 1/2)).
#' @param const Multiplicative constant \eqn{c} for "adaptive" (default 1).
#' @param cap Upper bound on the returned bin count (default: a bounded
#'   \eqn{\lceil 4 \log_2(n) \rceil^2}-style cap; see Details). Keeps the number of
#'   binary features---hence tree/Rashomon-set size---bounded at feasible \eqn{n}.
#'
#' @details
#' The cap is deliberately polynomial-but-bounded to avoid an explosion in the
#' number of binary features (which enlarges candidate trees and Rashomon sets).
#' The floor is 2 (at least one threshold). Returns an integer \eqn{\ge 2}.
#'
#' @return Integer number of bins (\eqn{\ge 2}).
#' @keywords internal
compute_bin_count <- function(schedule, n, rho = 1/3, const = 1, cap = NULL) {
  if (is.numeric(schedule)) {
    if (length(schedule) != 1 || schedule < 2) {
      stop("numeric n_bins must be a single value >= 2, got: ", schedule, call. = FALSE)
    }
    return(as.integer(round(schedule)))
  }
  if (!is.character(schedule) || length(schedule) != 1) {
    stop("bin schedule must be a single string or numeric, got: ",
         paste(schedule, collapse = ", "), call. = FALSE)
  }
  if (is.null(cap)) {
    # Bounded cap: grows slowly so bin count stays feasible even for large n.
    cap <- max(2L, ceiling((log2(max(n, 2)))^2))
  }
  m <- switch(schedule,
    "adaptive" = {
      if (rho <= 0 || rho >= 0.5) {
        stop("rho must be in (0, 1/2), got: ", rho, call. = FALSE)
      }
      ceiling(const * n^rho)
    },
    # Legacy logarithmic schedule; retained for reproducibility of older runs.
    "log" = ceiling(log(n) / 3),
    stop("unknown bin schedule '", schedule,
         "'; use 'adaptive', 'log', 'cv', or a numeric value >= 2", call. = FALSE)
  )
  max(2L, min(as.integer(cap), as.integer(m)))
}


#' Select the number of discretization bins by cross-validation
#'
#' @description
#' Data-driven alternative to the fixed \code{"adaptive"} polynomial schedule
#' (\code{\link{compute_bin_count}}). Chooses the per-coordinate bin count from a
#' candidate grid by minimizing \eqn{K}-fold held-out risk under the given loss.
#' Because the theory-optimal grid resolution depends on the unknown smoothness
#' and sparsity of the target, cross-validation is the practical way to adapt the
#' grid to the data; the polynomial default is the theory-motivated fallback.
#'
#' @param X Data frame of features (continuous and/or binary).
#' @param y Outcome vector.
#' @param loss_function "misclassification", "log_loss", or "squared_error".
#' @param candidate_bins Integer vector of candidate bin counts to try. If NULL,
#'   a default geometric-ish grid spanning the log and polynomial schedules is used.
#' @param K Number of CV folds (default 5).
#' @param regularization Regularization passed to \code{fit_tree} during CV
#'   (default \code{(log n)/n}, the theory rate on the mean-loss scale).
#' @param method Discretization method ("quantiles" recommended for CV).
#'
#' @return A list with \code{best_bins} (integer), \code{cv_loss} (numeric vector
#'   aligned with \code{candidate_bins}), and \code{candidate_bins}.
#' @keywords internal
select_bins_cv <- function(X, y, loss_function = "log_loss",
                           candidate_bins = NULL, K = 5L,
                           regularization = NULL, method = "quantiles") {
  n <- nrow(X)
  if (is.null(candidate_bins)) {
    # Span from the legacy log schedule up to the polynomial default and a bit beyond.
    lo <- compute_bin_count("log", n)
    hi <- compute_bin_count("adaptive", n)
    candidate_bins <- sort(unique(pmax(2L, as.integer(round(
      seq(lo, max(hi, lo + 2L), length.out = 4L)
    )))))
  }
  if (is.null(regularization)) regularization <- log(n) / n

  is_regression <- loss_function == "squared_error"
  folds <- create_folds(if (is_regression) rep(0, n) else y, K = K)

  cv_loss <- vapply(candidate_bins, function(nb) {
    fold_losses <- vapply(seq_len(K), function(k) {
      test_idx <- folds[[k]]
      train_idx <- setdiff(seq_len(n), test_idx)
      if (length(train_idx) < 2L || length(test_idx) < 1L) return(NA_real_)
      fit <- tryCatch(
        fit_tree(X[train_idx, , drop = FALSE], y[train_idx],
                 loss_function = loss_function, regularization = regularization,
                 discretize_method = method, discretize_bins = nb, verbose = FALSE),
        error = function(e) NULL
      )
      if (is.null(fit)) return(NA_real_)
      Xtest <- X[test_idx, , drop = FALSE]
      ytest <- y[test_idx]
      if (is_regression) {
        pred <- predict(fit, Xtest)
        mean((ytest - pred)^2)
      } else {
        prob <- predict(fit, Xtest, type = "prob")[, 2L]
        prob <- pmin(pmax(prob, 1e-12), 1 - 1e-12)
        -mean(ytest * log(prob) + (1 - ytest) * log(1 - prob))
      }
    }, numeric(1))
    mean(fold_losses, na.rm = TRUE)
  }, numeric(1))

  best_bins <- candidate_bins[which.min(cv_loss)]
  list(best_bins = as.integer(best_bins), cv_loss = cv_loss,
       candidate_bins = candidate_bins)
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
    if (length(unique_vals) == 0) {
      stop("Feature '", name, "' has no non-NA values. Cannot discretize.", call. = FALSE)
    }
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
    purrr::map(seq_along(thresholds), ~ {
      as.numeric(x <= thresholds[.x])
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
      if (length(feature_meta$original_values) == 0) {
        stop("Feature '", col_name, "' has empty original_values in metadata. ",
             "This indicates corrupted discretization metadata.", call. = FALSE)
      }
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
      if (length(thresholds) == 0) {
        stop("Feature '", col_name, "' has empty thresholds in metadata. ",
             "This indicates corrupted discretization metadata.", call. = FALSE)
      }
      binary_cols <- data.frame(
        purrr::map(seq_along(thresholds), ~ {
          as.numeric(x <= thresholds[.x])
        })
      )
      names(binary_cols) <- feature_meta$new_names
      binary_cols_list[[col_name]] <- binary_cols

    } else {
      stop("Unknown feature type in metadata: ", feature_meta$type)
    }
  }

  # Combine all binary columns
  if (length(binary_cols_list) == 0) {
    stop("No features to apply discretization to. This indicates empty metadata or feature mismatch.",
         call. = FALSE)
  }
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
