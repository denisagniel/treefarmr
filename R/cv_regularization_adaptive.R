#' Adaptive cross-validation for regularization parameter
#'
#' @description
#' Like \code{\link{cv_regularization}}, but automatically extends the lambda grid
#' if CV selects a boundary point (weakest or strongest lambda). This prevents
#' suboptimal regularization when the initial grid doesn't span the right range.
#'
#' The algorithm iteratively:
#' \enumerate{
#'   \item Runs cross-validation on current grid
#'   \item If selected lambda is at grid boundary (first or last), extends grid in that direction
#'   \item Repeats until interior point selected or max iterations reached
#' }
#'
#' @param X A data.frame or matrix of features.
#' @param y A vector of outcomes. Binary (0/1) for classification; numeric for regression
#'   (when \code{loss_function = "squared_error"}).
#' @param loss_function Character string: \code{"misclassification"}, \code{"log_loss"}, or
#'   \code{"squared_error"} (same as \code{\link{fit_tree}}).
#' @param lambda_grid_init Optional numeric vector of initial lambda values. If \code{NULL}, uses default grid.
#' @param K Number of CV folds (e.g. 5 or 10).
#' @param max_iterations Maximum number of grid extensions. Default: 10.
#' @param extension_factor When extending grid, multiply/divide by this factor. Default: 2.
#' @param n_extend Number of new lambda values to add when extending. Default: 2.
#' @param refit If \code{TRUE}, refit on full data with chosen lambda. Default: \code{TRUE}.
#' @param verbose Logical. Print progress. Default: \code{FALSE}.
#' @param worker_limit Integer: parallel workers for \code{fit_tree}. Default: 1.
#' @param parallel Logical. Parallelize CV folds. Default: \code{TRUE}.
#' @param seed Optional integer for reproducibility.
#' @param ... Additional arguments passed to \code{\link{fit_tree}}.
#'
#' @return A list containing:
#'   \item{best_lambda}{The chosen lambda (interior point or best after max_iterations).}
#'   \item{cv_loss}{Numeric vector of average held-out loss per lambda.}
#'   \item{lambda_grid}{Final lambda grid used.}
#'   \item{fold_losses}{Matrix of fold losses (rows = lambda, columns = fold).}
#'   \item{model}{If \code{refit = TRUE}, fitted model on full data.}
#'   \item{iterations}{Number of iterations performed.}
#'   \item{converged}{Logical. TRUE if interior point selected, FALSE if hit max_iterations.}
#'   \item{history}{List of CV results from each iteration (for diagnostics).}
#'
#' @details
#' This function addresses the common problem where the default CV grid is either too strong
#' (all lambdas prune too much) or too weak (all lambdas allow overfitting). By adaptively
#' extending the grid, it finds the right regularization scale automatically.
#'
#' \strong{When to use:}
#' \itemize{
#'   \item Complex DGPs where optimal lambda is unknown
#'   \item Varying sample sizes (small n needs stronger lambda, large n needs weaker)
#'   \item When you want robust regularization without manual tuning
#' }
#'
#' \strong{Computational cost:}
#' \itemize{
#'   \item Best case: 1 iteration (interior point selected immediately)
#'   \item Typical case: 2-3 iterations (one or two extensions)
#'   \item Worst case: max_iterations (usually 10)
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 500
#' X <- data.frame(x1 = rbinom(n, 1, 0.5), x2 = rbinom(n, 1, 0.5))
#' y <- as.numeric((X$x1 == 1 & X$x2 == 1))
#'
#' # Adaptive CV - automatically finds right lambda scale
#' cv_result <- cv_regularization_adaptive(
#'   X, y,
#'   loss_function = "log_loss",
#'   K = 5,
#'   verbose = TRUE
#' )
#'
#' print(cv_result$best_lambda)
#' print(cv_result$iterations)  # How many extensions needed
#' print(cv_result$converged)   # Did we find interior point?
#' }
#'
#' @export
cv_regularization_adaptive <- function(X, y,
                                       loss_function = "log_loss",
                                       lambda_grid_init = NULL,
                                       K = 5,
                                       max_iterations = 10,
                                       extension_factor = 2,
                                       n_extend = 2,
                                       refit = TRUE,
                                       verbose = FALSE,
                                       worker_limit = 1,
                                       parallel = TRUE,
                                       seed = NULL,
                                       max_lambda = Inf,
                                       ...) {

  # Input validation
  if (max_iterations < 1) {
    cli::cli_abort("{.arg max_iterations} must be at least 1.")
  }
  if (extension_factor <= 1) {
    cli::cli_abort("{.arg extension_factor} must be greater than 1.")
  }
  if (n_extend < 1) {
    cli::cli_abort("{.arg n_extend} must be at least 1.")
  }

  n <- nrow(X)
  K <- as.integer(K)

  # Initialize grid if not provided
  if (is.null(lambda_grid_init)) {
    # Use same default as cv_regularization (updated 2026-05-29)
    # Start at theory-based (log n)/n to prevent overfitting
    lambda_grid <- (log(n) / n) * c(1, 1.5, 2, 3, 5, 10)
  } else {
    lambda_grid <- sort(unique(lambda_grid_init))
  }

  if (length(lambda_grid) < 3) {
    cli::cli_abort("Initial {.arg lambda_grid} must have at least 3 values for adaptive extension.")
  }

  # Pre-compute fold indices once — all iterations use the same folds.
  # This ensures lambdas evaluated in different iterations are comparable.
  is_regression <- loss_function == "squared_error"
  if (!is.null(seed)) set.seed(seed)
  fold_indices_fixed <- if (is_regression) {
    fold_assignment <- sample(rep(seq_len(K), length.out = n))
    purrr::map(seq_len(K), ~ which(fold_assignment == .x))
  } else {
    create_folds(y, K = K)
  }

  # Lambda cache: R environment as hash map.
  # Key: sprintf("%.15g", lambda); Value: numeric vector of length K (fold losses).
  lambda_cache <- new.env(hash = TRUE, parent = emptyenv())
  cache_key <- function(lam) sprintf("%.15g", lam)

  # Storage for iteration history
  history <- list()
  converged <- FALSE

  if (verbose) {
    cli::cli_h1("Adaptive CV Regularization")
    cli::cli_alert_info("Initial grid: {.val {round(lambda_grid, 5)}}")
    cli::cli_alert_info("Max iterations: {.val {max_iterations}}")
  }

  # Iterative grid refinement
  for (iter in 1:max_iterations) {

    if (verbose) {
      cli::cli_h2("Iteration {iter}/{max_iterations}")
      cli::cli_alert("Testing {length(lambda_grid)} lambda values...")
    }

    # Evaluate only lambdas not yet in the cache
    new_lambdas <- lambda_grid[
      !vapply(lambda_grid,
              function(l) exists(cache_key(l), envir = lambda_cache, inherits = FALSE),
              logical(1))
    ]

    if (length(new_lambdas) > 0) {
      cv_new <- cv_regularization(
        X = X, y = y, loss_function = loss_function,
        lambda_grid = new_lambdas, K = K, refit = FALSE,
        verbose = FALSE, worker_limit = worker_limit,
        parallel = parallel,
        fold_indices = fold_indices_fixed,
        seed = NULL,  # fold assignment already done
        ...
      )
      # Store each new lambda's fold losses in cache
      for (i in seq_along(new_lambdas)) {
        assign(cache_key(new_lambdas[i]), cv_new$fold_losses[i, ], envir = lambda_cache)
      }
    }

    # Assemble full fold_losses matrix from cache
    fold_losses_full <- matrix(NA_real_, nrow = length(lambda_grid), ncol = K)
    for (i in seq_along(lambda_grid)) {
      key <- cache_key(lambda_grid[i])
      if (exists(key, envir = lambda_cache, inherits = FALSE)) {
        fold_losses_full[i, ] <- get(key, envir = lambda_cache, inherits = FALSE)
      }
    }
    cv_loss <- rowMeans(fold_losses_full, na.rm = TRUE)
    best_lambda <- lambda_grid[which.min(cv_loss)]

    # Reconstruct cv_result structure
    cv_result <- list(
      lambda_grid = lambda_grid,
      cv_loss = cv_loss,
      best_lambda = best_lambda,
      fold_losses = fold_losses_full
    )

    # Store iteration results
    history[[iter]] <- list(
      lambda_grid = cv_result$lambda_grid,
      cv_loss = cv_result$cv_loss,
      best_lambda = cv_result$best_lambda,
      fold_losses = cv_result$fold_losses
    )

    # Find which lambda was selected
    best_idx <- which.min(cv_result$cv_loss)
    n_lambda <- length(cv_result$lambda_grid)

    if (verbose) {
      cli::cli_alert_success("Best lambda: {.val {round(cv_result$best_lambda, 6)}} (index {best_idx}/{n_lambda})")
      cli::cli_alert("CV losses: {.val {round(cv_result$cv_loss, 4)}}")
    }

    # Check convergence
    if (best_idx > 1 && best_idx < n_lambda) {
      # Interior point selected - converged!
      converged <- TRUE

      if (verbose) {
        cli::cli_alert_success("Converged! Selected interior point.")
      }

      # Final result uses current grid
      final_lambda <- cv_result$best_lambda
      final_cv_loss <- cv_result$cv_loss
      final_lambda_grid <- cv_result$lambda_grid
      final_fold_losses <- cv_result$fold_losses

      break

    } else if (best_idx == 1) {
      # Hit lower boundary - extend to weaker regularization

      if (verbose) {
        cli::cli_alert_warning("Hit lower boundary - extending to weaker lambda...")
      }

      # Add weaker lambda values (smaller values)
      weakest <- lambda_grid[1]
      new_lambdas <- weakest / (extension_factor ^ (n_extend:1))

      # Ensure new values are positive and not too small
      new_lambdas <- new_lambdas[new_lambdas > 1e-8]

      if (length(new_lambdas) == 0) {
        # Can't go weaker - stop here
        if (verbose) {
          cli::cli_alert_warning("Cannot extend weaker (lambda too small). Stopping.")
        }

        final_lambda <- cv_result$best_lambda
        final_cv_loss <- cv_result$cv_loss
        final_lambda_grid <- cv_result$lambda_grid
        final_fold_losses <- cv_result$fold_losses
        break
      }

      lambda_grid <- sort(unique(c(new_lambdas, lambda_grid)))

      if (verbose) {
        cli::cli_alert_info("Extended grid: {.val {round(lambda_grid, 5)}}")
      }

    } else if (best_idx == n_lambda) {
      # Hit upper boundary - extend to stronger regularization

      if (verbose) {
        cli::cli_alert_warning("Hit upper boundary - extending to stronger lambda...")
      }

      # Add stronger lambda values (larger values)
      strongest <- lambda_grid[n_lambda]
      new_lambdas <- strongest * (extension_factor ^ (1:n_extend))

      # Ensure new values are not too large.
      # Hard cap at 10 (absolute); callers can set a tighter max_lambda
      # (e.g. 15 * (log n)/n) to prevent stump-producing lambda values.
      new_lambdas <- new_lambdas[new_lambdas < min(10, max_lambda)]

      if (length(new_lambdas) == 0) {
        # Can't go stronger - stop here
        if (verbose) {
          cli::cli_alert_warning("Cannot extend stronger (lambda >= 10). Stopping.")
        }

        final_lambda <- cv_result$best_lambda
        final_cv_loss <- cv_result$cv_loss
        final_lambda_grid <- cv_result$lambda_grid
        final_fold_losses <- cv_result$fold_losses
        break
      }

      lambda_grid <- sort(unique(c(lambda_grid, new_lambdas)))

      if (verbose) {
        cli::cli_alert_info("Extended grid: {.val {round(lambda_grid, 5)}}")
      }
    }

    # Store for potential final result if we hit max_iterations
    final_lambda <- cv_result$best_lambda
    final_cv_loss <- cv_result$cv_loss
    final_lambda_grid <- cv_result$lambda_grid
    final_fold_losses <- cv_result$fold_losses
  }

  # If we didn't converge, warn user
  if (!converged && verbose) {
    cli::cli_alert_warning("Did not converge after {max_iterations} iterations. Using best lambda from final iteration.")
  }

  # Refit on full data if requested
  final_model <- NULL
  if (refit) {
    if (verbose) {
      cli::cli_alert("Refitting on full data with lambda = {.val {round(final_lambda, 6)}}...")
    }

    final_model <- fit_tree(
      X = X,
      y = y,
      loss_function = loss_function,
      regularization = final_lambda,
      worker_limit = worker_limit,
      verbose = FALSE,
      ...
    )

    if (verbose) {
      n_leaves <- count_leaves_node(final_model@trees[[1]])
      cli::cli_alert_success("Final model: {n_leaves} leaves")
    }
  }

  # Return
  result <- list(
    best_lambda = final_lambda,
    cv_loss = final_cv_loss,
    lambda_grid = final_lambda_grid,
    fold_losses = final_fold_losses,
    model = final_model,
    iterations = length(history),
    converged = converged,
    history = history
  )

  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_success("Adaptive CV complete!")
    cli::cli_ul(c(
      "Iterations: {result$iterations}",
      "Converged: {result$converged}",
      "Best lambda: {round(result$best_lambda, 6)}",
      "Relative to theory: {round(result$best_lambda / (log(n)/n), 2)}x"
    ))
  }

  return(result)
}
