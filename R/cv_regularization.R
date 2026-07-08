#' Count post-discretization binary feature count
#'
#' Analytically estimates the number of binary columns that will exist after
#' discretization, without running the full discretization. Each continuous
#' feature expands to (n_bins - 1) binary indicators; binary features stay as 1.
#'
#' @param X data.frame of features (pre-discretization)
#' @param dots Additional arguments captured from \code{...} (may include
#'   \code{discretize_bins} and \code{discretize_thresholds})
#' @param n Number of rows in X (used for adaptive bin calculation)
#' @return Integer: estimated number of binary features after discretization
#' @keywords internal
count_post_discretization_features <- function(X, dots, n) {
  n_bins_arg <- dots$discretize_bins %||% 2L
  thresholds <- dots$discretize_thresholds

  n_bins <- if (identical(n_bins_arg, "adaptive")) {
    max(2L, ceiling(log(n) / 3))
  } else {
    as.integer(n_bins_arg)
  }

  total <- 0L
  for (col_name in names(X)) {
    x <- X[[col_name]]
    if (all(x %in% c(0, 1, NA_real_))) {
      total <- total + 1L
    } else if (!is.null(thresholds[[col_name]])) {
      total <- total + length(thresholds[[col_name]])
    } else {
      total <- total + (n_bins - 1L)
    }
  }
  total
}

#' Compute safe model_limit for given lambda and n
#'
#' Theory prescribes lambda ~ log(n)/n, but small lambda requires
#' larger model_limit for solver to complete successfully.
#'
#' @param lambda Regularization value
#' @param n Sample size
#' @param p_features Number of binary features (after discretization)
#' @return Safe model_limit value
#' @keywords internal
compute_safe_model_limit <- function(lambda, n, p_features) {
  lambda_theory <- log(n) / n

  # Ratio: how much smaller is lambda than theory value?
  ratio <- lambda / lambda_theory

  if (ratio >= 10) {
    # Strong regularization: default is fine
    return(10000)
  } else if (ratio >= 5) {
    # Moderate: 5× default
    return(50000)
  } else if (ratio >= 2) {
    # Weak: 10× default
    return(100000)
  } else if (ratio >= 1) {
    # At theory value: 20× default
    return(200000)
  } else {
    # Below theory: unlimited (risky but necessary)
    # Dimension-based cutoff from fit_tree() logic
    if (p_features > 100) {
      return(1000000)
    } else if (p_features > 50) {
      return(500000)
    } else {
      return(0)  # Unlimited for low-dimensional
    }
  }
}

#' Cross-validation for regularization parameter (lambda)
#'
#' @description
#' Select the regularization parameter lambda by K-fold cross-validation:
#' for each candidate lambda in a grid, fit a single tree on training folds,
#' compute held-out loss on the validation fold using the same loss as for
#' fitting, and choose the lambda that minimizes average held-out loss.
#' Automatically adjusts solver parameters for small lambda values to ensure
#' computational feasibility.
#'
#' @param X A data.frame or matrix of features.
#' @param y A vector of outcomes. Binary (0/1) for classification; numeric for regression
#'   (when \code{loss_function = "squared_error"}).
#' @param loss_function Character string: \code{"misclassification"}, \code{"log_loss"}, or
#'   \code{"squared_error"} (same as \code{\link{fit_tree}}).
#' @param lambda_grid Optional numeric vector of lambda values. If \code{NULL}, uses a theory-driven default grid around (log n)/n (see Details).
#' @param K Number of folds (e.g. 5 or 10).
#' @param refit If \code{TRUE}, refit a single tree on the full data with the chosen lambda and return it so the user can call \code{predict()} on it.
#' @param verbose Logical. Whether to print progress. Default: \code{FALSE}.
#' @param worker_limit Integer: number of parallel workers for \code{fit_tree}. Default: 1.
#' @param parallel Logical. Whether to parallelize across CV folds using \code{furrr}. Default: \code{TRUE}. If \code{TRUE}, uses the current \code{future} plan (set with \code{future::plan()}). If no plan is set, falls back to sequential execution.
#' @param seed Optional integer. If provided, \code{set.seed(seed)} is called before creating folds for reproducibility.
#' @param fold_indices Optional list of pre-computed fold index vectors (length K). If provided, skips fold creation and \code{seed} is used only for per-task reproducibility. Default: \code{NULL}.
#' @param ... Additional arguments passed to \code{\link{fit_tree}}.
#'
#' @return A list containing:
#'   \item{best_lambda}{The chosen lambda (minimizes average held-out loss).}
#'   \item{cv_loss}{Numeric vector of average held-out loss per lambda (same order as \code{lambda_grid}).}
#'   \item{lambda_grid}{The grid of lambda values used.}
#'   \item{fold_losses}{Matrix of size length(lambda_grid) x K (rows = lambda, columns = fold) for diagnostics.}
#'   \item{model}{If \code{refit = TRUE}, the fitted model from \code{fit_tree(X, y, ..., regularization = best_lambda)} for use with \code{predict()}.}
#'
#' @details
#' The DML/tree paper specifies that the regularization parameter lambda (penalty per leaf) should be chosen by cross-validation: for each nuisance, minimize held-out loss (the same loss L used for fitting) over a grid of lambda values. A theory-driven default is lambda proportional to (log n)/n (e.g. lambda = (log n)/n); the proportionality constant can be tuned by cross-validation.
#'
#' Held-out loss is computed as follows:
#' \itemize{
#'   \item For \code{loss_function = "misclassification"}: mean(1 - correct) on the validation fold (misclassification rate).
#'   \item For \code{loss_function = "log_loss"}: mean cross-entropy on the validation fold; predicted class probabilities are clamped to avoid log(0).
#'   \item For \code{loss_function = "squared_error"}: mean squared error on the validation fold.
#' }
#'
#' \strong{Computational Considerations:} Small lambda values allow complex trees
#' and may require larger \code{model_limit} for the solver to complete. This function
#' automatically scales \code{model_limit} based on lambda magnitude relative to the
#' theory-driven value \eqn{(\log n)/n}. Additionally, if a lambda value fails on 3 or
#' more folds, remaining folds for that lambda are skipped to avoid wasted computation.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 200
#' X <- data.frame(
#'   x1 = sample(0:1, n, replace = TRUE),
#'   x2 = sample(0:1, n, replace = TRUE)
#' )
#' y <- as.numeric((X$x1 == 1 & X$x2 == 1))
#' cv <- cv_regularization(X, y, loss_function = "misclassification",
#'                        K = 5, lambda_grid = c(0.05, 0.1, 0.2), refit = TRUE)
#' print(cv$best_lambda)
#' pred <- predict(cv$model, X, type = "class")
#' }
#'
#' @seealso
#' \code{\link{fit_tree}} for fitting a single tree with fixed regularization,
#' \code{\link{auto_tune_optimaltrees}} for tuning to a target number of trees (different tuning path).
#'
#' @export
cv_regularization <- function(X, y, loss_function = "misclassification",
                             lambda_grid = NULL, K = 5L, refit = TRUE,
                             verbose = FALSE, worker_limit = 1L, parallel = TRUE,
                             seed = NULL, fold_indices = NULL, ...) {
  # Capture additional arguments, filtering out model_limit (we set it adaptively)
  dots <- list(...)
  dots$model_limit <- NULL

  # Input validation (aligned with fit_tree)
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("{.arg X} must be a data.frame or matrix.")
  }
  if (!is.numeric(y) && !is.logical(y)) {
    cli::cli_abort("{.arg y} must be numeric or logical.")
  }
  if (nrow(X) == 0) {
    cli::cli_abort("{.arg X} must have at least one row.")
  }
  if (length(y) != nrow(X)) {
    cli::cli_abort("Length of {.arg y} must match number of rows in {.arg X}.")
  }
  if (!loss_function %in% c("misclassification", "log_loss", "squared_error")) {
    cli::cli_abort("{.arg loss_function} must be 'misclassification', 'log_loss', or 'squared_error'.")
  }
  is_regression <- loss_function == "squared_error"
  if (!is_regression && !all(y %in% c(0, 1))) {
    cli::cli_abort("{.arg y} must contain only binary values (0 and 1) for classification.")
  }
  if (is_regression && !is.numeric(y)) {
    cli::cli_abort("{.arg y} must be numeric for squared_error (regression).")
  }
  if (length(K) != 1 || !is.numeric(K) || K < 2) {
    cli::cli_abort("{.arg K} must be an integer >= 2.")
  }
  K <- as.integer(K)
  n <- nrow(X)
  p_binary <- count_post_discretization_features(X, dots, n)
  if (is.logical(y)) {
    y <- as.numeric(y)
  }

  if (is.null(fold_indices)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    # For regression, use simple random folds; for classification, stratify by class
    if (is_regression) {
      fold_assignment <- sample(rep(seq_len(K), length.out = n))
      fold_indices <- purrr::map(seq_len(K), ~ which(fold_assignment == .x))
    } else {
      fold_indices <- create_folds(y, K = K)
    }
  }

  if (is.null(lambda_grid)) {
    # Changed from c(0.05, 0.1, 0.25, 0.5, 1, 2) to c(1, 1.5, 2, 3, 5, 10)
    # Previous grid started at (log n)/n * 0.05 which was too small and caused overfitting
    # New grid starts at theory-based (log n)/n and searches larger values
    # This prevents overfitting and eliminates persistent bias in cross-fitted estimation
    lambda_grid <- (log(n) / n) * c(1, 1.5, 2, 3, 5, 10)
  }
  lambda_grid <- sort(unique(lambda_grid))

  n_lambda <- length(lambda_grid)
  fold_losses <- matrix(NA_real_, nrow = n_lambda, ncol = K)

  # Determine if we can use parallel processing
  use_parallel <- parallel && .has_furrr &&
                  .has_future

  # Create a grid of all (lambda, fold) combinations
  grid <- expand.grid(lambda_idx = seq_len(n_lambda), fold_idx = seq_len(K),
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  # Sort grid to distribute expensive fits across workers
  # Smaller lambda = higher computational cost, so sort by 1/lambda (descending)
  grid$cost_est <- 1 / lambda_grid[grid$lambda_idx]
  grid <- grid[order(grid$cost_est, decreasing = TRUE), ]
  grid$cost_est <- NULL  # Remove temporary column

  # Progress bar setup
  if (verbose && .has_cli) {
    cli::cli_progress_bar(
      "Cross-validating",
      total = nrow(grid),
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  # Generate deterministic per-task seeds (if user provided a seed)
  task_seeds <- if (!is.null(seed)) {
    seed + seq_len(nrow(grid))  # Deterministic per-task seeds
  } else {
    rep(NA_integer_, nrow(grid))  # No seeding if user didn't set seed
  }

  # Track failures per lambda for early stopping
  lambda_failure_count <- rep(0, n_lambda)

  # Function to fit one (lambda, fold) combination with seed support and error handling
  fit_one_combo <- function(i, task_seed) {
    tryCatch(
      {
        if (!is.na(task_seed)) set.seed(task_seed)

        lambda_idx <- grid$lambda_idx[i]
        fold_idx <- grid$fold_idx[i]
        lam <- lambda_grid[lambda_idx]

        # Early stopping: Skip if this lambda has failed multiple times already
        if (lambda_failure_count[lambda_idx] >= 3) {
          return(list(lambda_idx = lambda_idx, fold_idx = fold_idx,
                      loss = NA_real_, skipped = TRUE))
        }

        val_idx <- fold_indices[[fold_idx]]
        train_idx <- setdiff(seq_len(n), val_idx)
        X_train <- X[train_idx, , drop = FALSE]
        y_train <- y[train_idx]
        X_val <- X[val_idx, , drop = FALSE]
        y_val <- y[val_idx]

        # Compute safe model_limit based on lambda magnitude
        safe_limit <- compute_safe_model_limit(lam, n, p_binary)

        # Always use worker_limit=1 inside parallel execution to avoid nested parallelism
        # Call optimaltrees directly to avoid parameter passing issues with fit_tree
        fit_args <- list(X = X_train, y = y_train, loss_function = loss_function,
                         regularization = lam, worker_limit = 1L, verbose = FALSE,
                         single_tree = TRUE, model_limit = safe_limit)

        # Merge with dots (for discretization params, etc.)
        # Use modifyList so fit_args overrides dots
        fit_args <- modifyList(dots, fit_args)

        fit <- do.call(optimaltrees, fit_args)

        if (loss_function == "misclassification") {
          pred <- predict(fit, X_val, type = "class")
          loss <- mean(y_val != pred)
        } else if (loss_function == "squared_error") {
          pred <- predict(fit, X_val)
          loss <- mean((y_val - pred)^2)
        } else {
          probs <- predict(fit, X_val, type = "prob")
          p <- probs[, 2L]
          p <- pmax(pmin(p, 1 - 1e-15), 1e-15)
          loss <- -mean(y_val * log(p) + (1 - y_val) * log(1 - p))
        }

        list(lambda_idx = lambda_idx, fold_idx = fold_idx, loss = loss)
      },
      error = function(e) {
        lambda_idx <- grid$lambda_idx[i]
        fold_idx <- grid$fold_idx[i]

        # NOTE: <<- only effective in sequential mode; in parallel, each worker
        # has its own copy so early-stopping won't trigger. This is acceptable —
        # correctness is unaffected, only performance of failing lambdas.
        lambda_failure_count[lambda_idx] <<- lambda_failure_count[lambda_idx] + 1

        warning("CV task ", i, " (lambda index ", lambda_idx,
               ", fold ", fold_idx, ") failed: ", e$message,
               call. = FALSE)
        list(lambda_idx = lambda_idx, fold_idx = fold_idx, loss = NA_real_)
      }
    )
  }

  # Execute: parallel or sequential
  if (use_parallel) {
    results <- furrr::future_map2(seq_len(nrow(grid)), task_seeds, fit_one_combo,
                                  .options = furrr::furrr_options(seed = TRUE))
  } else {
    results <- Map(fit_one_combo, seq_len(nrow(grid)), task_seeds)
  }

  # Update progress bar in main thread after collecting results
  if (verbose && .has_cli) {
    for (i in seq_along(results)) {
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }

  # Fill in the fold_losses matrix
  for (res in results) {
    fold_losses[res$lambda_idx, res$fold_idx] <- res$loss
  }

  cv_loss <- rowMeans(fold_losses, na.rm = TRUE)
  best_idx <- which.min(cv_loss)

  # Check if CV completely failed (all lambdas failed)
  if (length(best_idx) == 0 || is.na(cv_loss[best_idx])) {
    warning("Cross-validation failed for all lambda values. Returning NA results.",
            call. = FALSE)
    result <- list(
      best_lambda = NA_real_,
      cv_loss = cv_loss,
      lambda_grid = lambda_grid,
      fold_losses = fold_losses
    )
    # Only add model if refit=TRUE (even though it's NULL)
    if (refit) {
      result$model <- NULL
    }
    return(result)
  }

  best_lambda <- lambda_grid[best_idx]

  out <- list(
    best_lambda = best_lambda,
    cv_loss = cv_loss,
    lambda_grid = lambda_grid,
    fold_losses = fold_losses
  )

  if (refit) {
    if (verbose) {
      message(sprintf("Refitting on full data with lambda = %.6f\n", best_lambda))
    }
    # Use safe model_limit for refit as well
    safe_limit_refit <- compute_safe_model_limit(best_lambda, n, p_binary)

    # Build refit_args - call optimaltrees directly to avoid parameter issues
    refit_args <- list(X = X, y = y, loss_function = loss_function,
                       regularization = best_lambda,
                       worker_limit = worker_limit, verbose = verbose,
                       single_tree = TRUE, model_limit = safe_limit_refit)

    # Merge with dots (for discretization params, etc.)
    refit_args <- modifyList(dots, refit_args)

    out$model <- do.call(optimaltrees, refit_args)
  }

  out
}
