#' Cross-Fitted Rashomon Set Analysis
#'
#' @description
#' Perform K-fold cross-fitting with TreeFARMS and find trees that appear
#' in the Rashomon set of ALL folds. This identifies stable, robust tree models.
#'
#' @param X Data.frame or matrix of binary features (0/1)
#' @param y Vector of binary class labels (0/1)
#' @param K Number of folds for cross-fitting. Default: 5
#' @param loss_function Loss function: "misclassification" or "log_loss". Default: "misclassification"
#' @param regularization Model complexity penalty. Default: 0.1
#' @param rashomon_bound_multiplier Rashomon set size control. Default: 0.05
#' @param seed Random seed for reproducibility. Default: NULL
#' @param verbose Print progress information. Default: TRUE
#' @param ... Additional parameters passed to TreeFARMS
#'
#' @return Object of class `cf_rashomon` containing:
#'   \item{intersecting_trees}{Tree(s) appearing in ALL K Rashomon sets}
#'   \item{n_intersecting}{Number of intersecting trees}
#'   \item{fold_models}{List of K fitted TreeFARMS models}
#'   \item{rashomon_sets}{List of K Rashomon sets}
#'   \item{rashomon_sizes}{Vector of Rashomon set sizes per fold}
#'   \item{fold_indices}{List indicating which observations are in each fold}
#'   \item{K}{Number of folds}
#'   \item{call}{The function call}
#'
#' @details
#' Cross-fitting algorithm:
#' 1. Split data into K folds
#' 2. For each fold k:
#'    - Train TreeFARMS on all observations NOT in fold k (K-1 folds combined)
#'    - Extract full Rashomon set
#' 3. Find trees appearing in ALL K Rashomon sets (intersection)
#'
#' A tree appearing in all K Rashomon sets is nearly-optimal regardless of
#' which training subset was used, indicating strong stability and generalizability.
#'
#' @examples
#' \dontrun{
#' # Create binary data
#' X <- data.frame(
#'   feature_1 = sample(0:1, 100, replace = TRUE),
#'   feature_2 = sample(0:1, 100, replace = TRUE)
#' )
#' y <- as.numeric((X$feature_1 == 1 & X$feature_2 == 1))
#'
#' # Find stable trees across folds
#' result <- cross_fitted_rashomon(X, y, K = 5, regularization = 0.1)
#' print(result)
#'
#' # If stable trees found, use for prediction
#' if (result$n_intersecting > 0) {
#'   predictions <- predict(result, X_new)
#' }
#' }
#'
#' @export
cross_fitted_rashomon <- function(X, y, K = 5,
                                  loss_function = "misclassification",
                                  regularization = 0.1,
                                  rashomon_bound_multiplier = 0.05,
                                  seed = NULL,
                                  verbose = TRUE,
                                  ...) {
  
  # Input validation
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X must be a data.frame or matrix")
  }
  
  if (!is.numeric(y) && !is.logical(y)) {
    stop("y must be numeric or logical")
  }
  
  if (length(y) != nrow(X)) {
    stop("Length of y must match number of rows in X")
  }
  
  if (!all(y %in% c(0, 1))) {
    stop("y must contain only binary values (0 and 1)")
  }
  
  if (K < 2) {
    stop("K must be at least 2")
  }
  
  if (K > nrow(X)) {
    stop("K cannot be larger than the number of observations")
  }
  
  n <- nrow(X)
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (verbose) {
    cat("Cross-Fitted Rashomon Set Analysis\n")
    cat("==================================\n")
    cat(sprintf("Data: %d observations, %d features\n", n, ncol(X)))
    cat(sprintf("K-fold cross-fitting: K = %d\n", K))
    cat(sprintf("Loss function: %s\n", loss_function))
    cat(sprintf("Regularization: %.3f\n\n", regularization))
  }
  
  # Create stratified folds
  # Stratify by outcome to maintain class balance
  fold_indices <- create_folds(y, K = K)
  
  if (verbose) {
    fold_sizes <- sapply(fold_indices, length)
    cat(sprintf("Fold sizes: %s\n\n", paste(fold_sizes, collapse = ", ")))
  }
  
  # Initialize storage
  fold_models <- vector("list", K)
  rashomon_sets <- vector("list", K)
  rashomon_sizes <- integer(K)
  
  # Fit model for each fold
  for (k in 1:K) {
    if (verbose) {
      cat(sprintf("Fold %d/%d: ", k, K))
    }
    
    # Get training data (all folds except k)
    test_idx <- fold_indices[[k]]
    train_idx <- setdiff(1:n, test_idx)
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    
    if (verbose) {
      cat(sprintf("Training on %d obs, testing on %d obs\n", 
                  length(train_idx), length(test_idx)))
    }
    
    # Train TreeFARMS model
    model <- treefarms(
      X = X_train,
      y = y_train,
      loss_function = loss_function,
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier,
      verbose = FALSE,
      ...
    )
    
    # Extract Rashomon set
    trees <- get_rashomon_trees(model)
    
    # Store results
    fold_models[[k]] <- model
    rashomon_sets[[k]] <- trees
    rashomon_sizes[k] <- length(trees)
    
    if (verbose) {
      cat(sprintf("  -> Rashomon set size: %d trees\n", length(trees)))
    }
    
    # Warning if no trees generated
    if (length(trees) == 0) {
      warning(sprintf("Fold %d: No trees in Rashomon set. Consider adjusting regularization.", k))
    }
  }
  
  if (verbose) {
    cat("\n")
  }
  
  # Find intersection of trees across all folds
  if (verbose) {
    cat("Finding trees appearing in all Rashomon sets...\n")
  }
  
  intersection_result <- find_tree_intersection(rashomon_sets, verbose = verbose)
  
  # Create result object
  result <- list(
    intersecting_trees = intersection_result$intersecting_trees,
    n_intersecting = intersection_result$n_intersecting,
    tree_jsons = intersection_result$tree_jsons,
    fold_models = fold_models,
    rashomon_sets = rashomon_sets,
    rashomon_sizes = rashomon_sizes,
    fold_indices = fold_indices,
    K = K,
    loss_function = loss_function,
    regularization = regularization,
    X_train = X,
    y_train = y,
    call = match.call()
  )
  
  class(result) <- "cf_rashomon"
  
  return(result)
}

#' Create Stratified Folds
#'
#' @description
#' Internal function to create K stratified folds based on outcome variable.
#'
#' @param y Binary outcome vector
#' @param K Number of folds
#'
#' @return List of length K, each element containing indices for that fold
#'
#' @keywords internal
create_folds <- function(y, K) {
  n <- length(y)
  
  # Stratify by class
  class_0_idx <- which(y == 0)
  class_1_idx <- which(y == 1)
  
  # Randomly assign class 0 observations to folds
  fold_0 <- sample(rep(1:K, length.out = length(class_0_idx)))
  
  # Randomly assign class 1 observations to folds
  fold_1 <- sample(rep(1:K, length.out = length(class_1_idx)))
  
  # Create fold index vector
  fold_assignment <- integer(n)
  fold_assignment[class_0_idx] <- fold_0
  fold_assignment[class_1_idx] <- fold_1
  
  # Convert to list of indices
  fold_indices <- lapply(1:K, function(k) {
    which(fold_assignment == k)
  })
  
  return(fold_indices)
}

#' Print Method for cf_rashomon Objects
#'
#' @param x A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
print.cf_rashomon <- function(x, ...) {
  cat("Cross-Fitted Rashomon Set Analysis\n")
  cat("==================================\n")
  cat(sprintf("Number of folds: %d\n", x$K))
  cat(sprintf("Loss function: %s\n", x$loss_function))
  cat(sprintf("Regularization: %.3f\n\n", x$regularization))
  
  cat("Rashomon set sizes per fold:\n")
  for (k in 1:x$K) {
    cat(sprintf("  Fold %d: %d trees\n", k, x$rashomon_sizes[k]))
  }
  
  cat(sprintf("\nIntersecting trees: %d\n", x$n_intersecting))
  
  if (x$n_intersecting > 0) {
    cat("\n✓ Found stable tree(s) appearing in all folds!\n")
    cat("  Use predict() to make predictions with the stable model.\n")
  } else {
    cat("\n✗ No trees appear in all folds.\n")
    cat("  Consider:\n")
    cat("  - Increasing regularization\n")
    cat("  - Adjusting rashomon_bound_multiplier\n")
    cat("  - Using fewer folds (smaller K)\n")
  }
}

#' Summary Method for cf_rashomon Objects
#'
#' @param object A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
summary.cf_rashomon <- function(object, ...) {
  print.cf_rashomon(object)
  
  cat("\nDetailed Summary:\n")
  cat("================\n")
  
  cat(sprintf("Training data: %d observations, %d features\n", 
              nrow(object$X_train), ncol(object$X_train)))
  
  cat(sprintf("Class distribution: %s\n", 
              paste(table(object$y_train), collapse = " / ")))
  
  if (object$n_intersecting > 0) {
    cat("\nStable Tree Rules:\n")
    cat("==================\n")
    for (i in 1:min(object$n_intersecting, 3)) {
      cat(sprintf("\nTree %d:\n", i))
      rules <- get_tree_rules(object$intersecting_trees[[i]], 
                             colnames(object$X_train))
      cat(rules)
    }
    
    if (object$n_intersecting > 3) {
      cat(sprintf("\n... and %d more tree(s)\n", object$n_intersecting - 3))
    }
  }
}


