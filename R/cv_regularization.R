#' Cross-validation for regularization parameter (lambda)
#'
#' @description
#' Select the regularization parameter lambda by K-fold cross-validation:
#' for each candidate lambda in a grid, fit a single tree on training folds,
#' compute held-out loss on the validation fold using the same loss as for
#' fitting, and choose the lambda that minimizes average held-out loss.
#'
#' @param X A data.frame or matrix of features. Must contain only binary (0/1) features.
#' @param y A vector of binary class labels (0/1).
#' @param loss_function Character string: \code{"misclassification"} or \code{"log_loss"} (same as \code{\link{fit_tree}}).
#' @param lambda_grid Optional numeric vector of lambda values. If \code{NULL}, uses a theory-driven default grid around (log n)/n (see Details).
#' @param K Number of folds (e.g. 5 or 10).
#' @param refit If \code{TRUE}, refit a single tree on the full data with the chosen lambda and return it so the user can call \code{predict()} on it.
#' @param verbose Logical. Whether to print progress. Default: \code{FALSE}.
#' @param worker_limit Integer: number of parallel workers for \code{fit_tree}. Default: 1.
#' @param parallel Logical. Whether to parallelize across CV folds using \code{furrr}. Default: \code{TRUE}. If \code{TRUE}, uses the current \code{future} plan (set with \code{future::plan()}). If no plan is set, falls back to sequential execution.
#' @param seed Optional integer. If provided, \code{set.seed(seed)} is called before creating folds for reproducibility.
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
#' }
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
#' \code{\link{auto_tune_treefarms}} for tuning to a target number of trees (different tuning path).
#'
#' @export
cv_regularization <- function(X, y, loss_function = "misclassification",
                             lambda_grid = NULL, K = 5L, refit = TRUE,
                             verbose = FALSE, worker_limit = 1L, parallel = TRUE,
                             seed = NULL, ...) {
  # Input validation (aligned with fit_tree)
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X must be a data.frame or matrix")
  }
  if (!is.numeric(y) && !is.logical(y)) {
    stop("y must be numeric or logical")
  }
  if (nrow(X) == 0) {
    stop("X must have at least one row")
  }
  if (length(y) != nrow(X)) {
    stop("Length of y must match number of rows in X")
  }
  if (!all(y %in% c(0, 1))) {
    stop("y must contain only binary values (0 and 1)")
  }
  if (!loss_function %in% c("misclassification", "log_loss")) {
    stop("loss_function must be either 'misclassification' or 'log_loss'")
  }
  if (length(K) != 1 || !is.numeric(K) || K < 2) {
    stop("K must be an integer >= 2")
  }
  K <- as.integer(K)
  n <- nrow(X)
  if (is.logical(y)) {
    y <- as.numeric(y)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  fold_indices <- create_folds(y, K = K)

  if (is.null(lambda_grid)) {
    lambda_grid <- (log(n) / n) * c(0.25, 0.5, 1, 2, 4)
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

  # Function to fit one (lambda, fold) combination with seed support and error handling
  fit_one_combo <- function(i, task_seed) {
    tryCatch(
      {
        if (!is.na(task_seed)) set.seed(task_seed)

        lambda_idx <- grid$lambda_idx[i]
        fold_idx <- grid$fold_idx[i]
        lam <- lambda_grid[lambda_idx]

        val_idx <- fold_indices[[fold_idx]]
        train_idx <- setdiff(seq_len(n), val_idx)
        X_train <- X[train_idx, , drop = FALSE]
        y_train <- y[train_idx]
        X_val <- X[val_idx, , drop = FALSE]
        y_val <- y[val_idx]

        # Always use worker_limit=1 inside parallel execution to avoid nested parallelism
        fit <- fit_tree(X_train, y_train, loss_function = loss_function,
                        regularization = lam, worker_limit = 1L,
                        verbose = FALSE, ...)

        if (loss_function == "misclassification") {
          pred <- predict(fit, X_val, type = "class")
          loss <- mean(y_val != pred)
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
                                  .options = furrr::furrr_options(seed = FALSE))
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

  cv_loss <- rowMeans(fold_losses)
  best_idx <- which.min(cv_loss)
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
    out$model <- fit_tree(X, y, loss_function = loss_function,
                          regularization = best_lambda,
                          worker_limit = worker_limit, verbose = verbose, ...)
  }

  out
}
