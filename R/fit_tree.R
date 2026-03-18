#' Fit a Single Tree Model
#'
#' @description
#' Fit exactly one optimal decision tree. This is the most common use case
#' for TreeFARMS models. The function guarantees that exactly one tree is
#' returned (no rashomon set computation).
#'
#' @param X A data.frame or matrix of features. Must contain only binary (0/1) features.
#' @param y A vector of binary class labels (0/1).
#' @param loss_function Character string specifying the loss function to use.
#'   Options: "misclassification" (default) or "log_loss".
#' @param regularization Numeric value controlling model complexity. Higher values
#'   lead to simpler models. Default: 0.1. If NULL, will be auto-tuned.
#' @param worker_limit Integer: number of parallel workers to use (default: 1).
#' @param verbose Logical. Whether to print training progress. Default: FALSE.
#' @param store_training_data Logical. Whether to store training data in the model object.
#'   Default: FALSE. Set to TRUE only if you need to access training data later.
#' @param compute_probabilities Logical. Whether to compute probabilities for all training samples immediately.
#'   Default: FALSE. Probabilities will be computed on-demand when accessed.
#' @param ... Additional parameters passed to TreeFARMS configuration.
#'
#' @return A list containing:
#'   \item{model}{The trained TreeFARMS model object}
#'   \item{predictions}{Binary predictions (0/1) for training data}
#'   \item{probabilities}{Probability predictions [P(class=0), P(class=1)] for training data}
#'   \item{accuracy}{Training accuracy}
#'   \item{loss_function}{The loss function used}
#'   \item{regularization}{The regularization parameter used}
#'   \item{n_trees}{Number of trees in the Rashomon set (always 1 for fit_tree)}
#'   \item{X_train}{Training features (only if store_training_data=TRUE)}
#'   \item{y_train}{Training labels (only if store_training_data=TRUE)}
#'
#' @details
#' This function is a convenience wrapper around `treefarms()` with `single_tree = TRUE`.
#' It guarantees that exactly one tree is returned, making it ideal for:
#' - Standard prediction tasks
#' - When you need a single, interpretable model
#' - When computational efficiency is important
#'
#' The function disables rashomon set computation, so only the optimal tree is returned.
#' This is faster and uses less memory than computing a full rashomon set.
#'
#' @examples
#' \dontrun{
#' # Create sample binary data
#' set.seed(42)
#' n <- 200
#' X <- data.frame(
#'   feature_1 = sample(0:1, n, replace = TRUE),
#'   feature_2 = sample(0:1, n, replace = TRUE)
#' )
#' y <- as.numeric((X$feature_1 == 1 & X$feature_2 == 1))
#'
#' # Fit a single tree
#' model <- fit_tree(X, y, regularization = 0.1)
#'
#' # Verify exactly one tree
#' print(model$n_trees)  # Should be 1
#'
#' # Make predictions
#' X_new <- data.frame(feature_1 = c(1, 0), feature_2 = c(1, 0))
#' predictions <- predict(model, X_new, type = "class")
#' }
#'
#' @seealso
#' \code{\link{fit_rashomon}} for fitting with rashomon sets
#' \code{\link{treefarms}} for the underlying fitting function
#' \code{\link{cross_fitted_rashomon}} for cross-fitting workflows
#'
#' @export
fit_tree <- function(X, y, loss_function = "misclassification", regularization = 0.1,
                    worker_limit = 1L, verbose = FALSE, store_training_data = NULL,
                    compute_probabilities = FALSE, ...) {
  # Issue #11: Validate X and y inputs
  if (is.null(X) || (!is.data.frame(X) && !is.matrix(X))) {
    stop("X must be a data.frame or matrix", call. = FALSE)
  }
  if (is.null(y)) {
    stop("y must be provided", call. = FALSE)
  }
  if (!is.vector(y) && !is.factor(y)) {
    stop("y must be a vector or factor", call. = FALSE)
  }
  if (nrow(X) != length(y)) {
    stop("X and y must have the same number of observations. ",
         "nrow(X) = ", nrow(X), ", length(y) = ", length(y), call. = FALSE)
  }

  # Issue #12: Validate worker_limit
  if (!is.numeric(worker_limit) || length(worker_limit) != 1 || worker_limit < 1) {
    stop("worker_limit must be a single positive integer, got: ", worker_limit, call. = FALSE)
  }
  if (worker_limit != as.integer(worker_limit)) {
    stop("worker_limit must be an integer, got: ", worker_limit, call. = FALSE)
  }

  # Issue #13: Validate logical parameters
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be a single logical value (TRUE or FALSE)", call. = FALSE)
  }
  if (!is.logical(compute_probabilities) || length(compute_probabilities) != 1) {
    stop("compute_probabilities must be a single logical value (TRUE or FALSE)", call. = FALSE)
  }
  if (!is.null(store_training_data) && (!is.logical(store_training_data) || length(store_training_data) != 1)) {
    stop("store_training_data must be NULL or a single logical value (TRUE or FALSE)", call. = FALSE)
  }

  # High-dimensional safeguard: estimate post-discretization feature count
  # and set appropriate model_limit to prevent memory issues
  dots <- list(...)
  user_model_limit <- dots$model_limit

  if (is.null(user_model_limit)) {
    # Estimate binary feature count after discretization
    # (Issue #14: X already validated above, safe to use nrow/ncol)
    n <- nrow(X)
    p_original <- ncol(X)

    # Estimate number of bins (adaptive formula or user-specified)
    # Check both dots and use optimaltrees default if not specified
    discretize_bins_param <- dots$discretize_bins
    if (is.null(discretize_bins_param)) {
      # Use optimaltrees default (which is 2, but check the function signature)
      discretize_bins_param <- 2
    }

    if (is.character(discretize_bins_param) && discretize_bins_param == "adaptive") {
      n_bins <- max(2, ceiling(log(n) / 3))
    } else {
      n_bins <- suppressWarnings(as.numeric(discretize_bins_param))
      if (is.na(n_bins)) {
        stop("discretize_bins must be numeric or 'adaptive', got: ", discretize_bins_param, call. = FALSE)
      }
      # Issue #15: Validate n_bins is at least 2
      if (n_bins < 2) {
        stop("discretize_bins must be >= 2, got: ", n_bins, call. = FALSE)
      }
    }

    # Estimate binary features: continuous features → (n_bins - 1) binary features each
    # Threshold encoding: k bins → k-1 indicators
    # Assume worst case: all features are continuous
    p_estimated <- p_original * (n_bins - 1)

    # Validate estimated feature count
    if (!is.finite(p_estimated) || p_estimated < 0) {
      stop("Invalid estimated feature count: ", p_estimated,
           ". Check discretization parameters (n_bins=", n_bins, ", p=", p_original, ").",
           call. = FALSE)
    }

    # Set model_limit based on estimated dimensionality
    if (p_estimated > 100) {
      # High-dimensional case: use memory-safe limit
      model_limit <- 1000000  # 1M models should be safe for most systems
      warning(
        "High-dimensional data detected (estimated ", p_estimated, " binary features after discretization). ",
        "Setting model_limit=", format(model_limit, scientific=FALSE), " to prevent memory issues. ",
        "Override by passing model_limit=0 (unlimited) or model_limit=<value> explicitly.",
        call. = FALSE, immediate. = TRUE
      )
    } else if (p_estimated > 50) {
      # Moderate dimensions: conservative limit
      model_limit <- 100000
      if (verbose) {
        message(
          "Moderate dimensionality (estimated ", p_estimated, " binary features). ",
          "Setting model_limit=", format(model_limit, scientific=FALSE), " as safety measure."
        )
      }
    } else {
      # Low-dimensional case: unlimited
      # CRITICAL: Disable model_limit for single-tree fits with low dimensions
      # Rationale: Single-tree extraction can generate >10,000 candidate models
      # during optimization with even moderate feature counts (e.g., 12 features).
      # model_limit was designed for Rashomon sets, not single-tree extraction.
      model_limit <- 0  # Unlimited
    }
  } else {
    # User explicitly set model_limit - respect it
    model_limit <- user_model_limit
  }

  # Call optimaltrees with single_tree = TRUE to guarantee exactly one tree
  result <- optimaltrees(
    X = X,
    y = y,
    loss_function = loss_function,
    regularization = regularization,
    worker_limit = worker_limit,
    verbose = verbose,
    store_training_data = store_training_data,
    compute_probabilities = compute_probabilities,
    single_tree = TRUE,  # Force single tree
    model_limit = model_limit,
    ...
  )
  
  # If auto-tuning was used, result is a list with 'model' field and other auto-tuning metadata
  if (is.list(result) && ("iterations" %in% names(result) || "converged" %in% names(result))) {
    if (!"model" %in% names(result)) {
      stop("Auto-tuning result missing 'model' field")
    }
    model_obj <- result$model
    # Verify single tree (should always be 1 when single_tree=TRUE)
    if (model_obj$n_trees != 1) {
      warning(sprintf("Expected exactly one tree, but got n_trees = %d. This should not happen.", model_obj$n_trees))
    }
    return(model_obj)
  }

  # Verify single tree (should always be 1 when single_tree=TRUE)
  if (result$n_trees != 1) {
    warning(sprintf("Expected exactly one tree, but got n_trees = %d. This should not happen.", result$n_trees))
  }
  return(result)
}


