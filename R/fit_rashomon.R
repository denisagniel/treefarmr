#' Fit a Tree Model with Rashomon Set
#'
#' @description
#' Fit a TreeFARMS model with a full rashomon set (multiple near-optimal trees).
#' This function is useful for exploratory analysis when you want to explore
#' multiple models that are nearly as good as the optimal tree.
#'
#' @param X A data.frame or matrix of features. Must contain only binary (0/1) features.
#' @param y A vector of binary class labels (0/1).
#' @param loss_function Character string specifying the loss function to use.
#'   Options: "misclassification" (default) or "log_loss".
#' @param regularization Numeric value controlling model complexity. Higher values
#'   lead to simpler models. Default: 0.1. If NULL, will be auto-tuned.
#' @param rashomon_bound_multiplier Numeric value controlling Rashomon set size (multiplicative). Default: 0.05.
#' @param rashomon_bound_adder Numeric value for additive Rashomon bound. Default: 0. If non-zero, bound = optimum + adder.
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
#'   \item{n_trees}{Number of trees in the Rashomon set (>= 1)}
#'   \item{X_train}{Training features (only if store_training_data=TRUE)}
#'   \item{y_train}{Training labels (only if store_training_data=TRUE)}
#'
#' @details
#' This function is a convenience wrapper around `treefarms()` with `single_tree = FALSE`.
#' It computes a full rashomon set, which includes:
#' - The optimal tree
#' - All trees within the rashomon bound (nearly optimal trees)
#'
#' The rashomon set is useful for:
#' - Exploring model uncertainty
#' - Finding stable patterns across multiple models
#' - Ensemble predictions
#' - Understanding the solution space
#'
#' The size of the rashomon set is controlled by `rashomon_bound_multiplier`:
#' - Smaller values (e.g., 0.01) → more trees in the set
#' - Larger values (e.g., 0.1) → fewer trees in the set
#'
#' \strong{Theory-consistent defaults (DML):} Use small \eqn{\varepsilon}
#' (e.g. \code{rashomon_bound_multiplier = 0.05}), \eqn{\lambda \propto (\log n)/n}
#' (see \code{\link{cv_regularization}}). See
#' \file{docs/Implementation-requirements-Rashomon-DML.md}.
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
#' # Fit with rashomon set
#' model <- fit_rashomon(X, y, regularization = 0.1, rashomon_bound_multiplier = 0.05)
#'
#' # Check number of trees in rashomon set
#' print(model$n_trees)  # Should be >= 1
#'
#' # Extract all trees from rashomon set
#' trees <- get_rashomon_trees(model)
#' cat("Number of trees in rashomon set:", length(trees), "\n")
#'
#' # Compare trees
#' if (length(trees) >= 2) {
#'   are_same <- compare_trees(trees[[1]], trees[[2]])
#'   cat("Trees 1 and 2 are", ifelse(are_same, "identical", "different"), "\n")
#' }
#' }
#'
#' @seealso
#' \code{\link{fit_tree}} for fitting a single tree
#' \code{\link{treefarms}} for the underlying fitting function
#' \code{\link{get_rashomon_trees}} to extract trees from the rashomon set
#' \code{\link{cross_fitted_rashomon}} for cross-fitting workflows
#'
#' @export
fit_rashomon <- function(X, y, loss_function = "misclassification", regularization = 0.1,
                        rashomon_bound_multiplier = 0.05, rashomon_bound_adder = 0, worker_limit = 1L, verbose = FALSE,
                        store_training_data = NULL, compute_probabilities = FALSE, ...) {
  
  # Call treefarms with single_tree = FALSE to compute rashomon set
  result <- treefarms(
    X = X,
    y = y,
    loss_function = loss_function,
    regularization = regularization,
    rashomon_bound_multiplier = rashomon_bound_multiplier,
    rashomon_bound_adder = rashomon_bound_adder,
    worker_limit = worker_limit,
    verbose = verbose,
    store_training_data = store_training_data,
    compute_probabilities = compute_probabilities,
    single_tree = FALSE,  # Compute rashomon set
    ...
  )
  
  # Verify that we got at least one tree
  if (result$n_trees < 1) {
    warning("No trees found in rashomon set. Consider adjusting regularization or rashomon_bound_multiplier.")
  }
  
  return(result)
}


