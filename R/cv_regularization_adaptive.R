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

    # Run CV on current grid
    cv_result <- cv_regularization(
      X = X,
      y = y,
      loss_function = loss_function,
      lambda_grid = lambda_grid,
      K = K,
      refit = FALSE,  # Don't refit yet - we may extend grid
      verbose = FALSE,  # We handle verbosity here
      worker_limit = worker_limit,
      parallel = parallel,
      seed = if (!is.null(seed)) seed + iter else NULL,  # Different seed per iteration
      ...
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

      # Ensure new values are not too large
      # For DML/causal inference, we may need lambda > 1 (strong regularization)
      # to prevent overfitting. Cap at 10 instead of 1.
      # Lambda = 10 means ~800× (log n)/n for n=500, which is extremely strong
      # but may be needed for very overfit-prone problems
      new_lambdas <- new_lambdas[new_lambdas < 10]

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
      n_leaves <- count_leaves_tree(final_model@trees[[1]])
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
