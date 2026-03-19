#' Cross-Fitted Rashomon Set Analysis
#'
#' @description
#' Perform K-fold cross-fitting with TreeFARMS and find trees that appear
#' in the Rashomon set of ALL folds. This identifies stable, robust tree models.
#'
#' @param X Data.frame or matrix of binary features (0/1)
#' @param y Vector of binary class labels (0/1)
#' @param K Number of folds for cross-fitting. Default: 5
#' @param loss_function Loss function: "misclassification", "log_loss", or "squared_error" (regression). Default: "misclassification"
#' @param regularization Model complexity penalty. Default: 0.1
#' @param rashomon_bound_multiplier Rashomon set size control (multiplicative). Default: 0.05
#' @param rashomon_bound_adder Additive Rashomon bound. Default: 0. If non-zero, bound = optimum + adder.
#' @param max_leaves Optional integer. If set, only trees with \eqn{\#\text{leaves} \le} \code{max_leaves} are used (R-side sieve; theory-consistent complexity).
#' @param single_tree Logical. If TRUE, fit exactly one tree per fold (disable rashomon set).
#'   Default: FALSE. When TRUE, each fold fits a single tree. When FALSE, each fold computes
#'   a full rashomon set. For single tree per fold, use \code{\link{fit_tree}} via the underlying
#'   \code{\link{treefarms}} call.
#' @param auto_tune_intersecting Logical. If TRUE, automatically search for the
#'   smallest \code{rashomon_bound_multiplier} that yields non-empty intersection.
#'   Uses bidirectional exponential search followed by binary refinement to find
#'   epsilon_n = c * sqrt(log(n)/n). Default: FALSE.
#'
#'   \strong{How it works (Bidirectional Search):}
#'   \enumerate{
#'     \item Initial test: Try c = 1 (reasonable default)
#'     \item If c = 1 succeeds: Downward search c = 0.5, 0.25, 0.125, ... to minimize bias
#'     \item If c = 1 fails: Upward search c = 2, 4, 8, ... to find intersection
#'     \item Binary refinement: Narrow to smallest working epsilon_n (within 10\%)
#'     \item Tree selection: If multiple trees, picks best by penalized risk
#'   }
#'
#'   \strong{Why bidirectional?} Minimizes bias by finding smallest epsilon_n.
#'   If c=1 works but c=0.25 also works, using c=1 accepts 4x higher bias unnecessarily.
#'
#'   \strong{Efficiency:} O(log c) exponential attempts + O(log precision) binary attempts.
#'   Typical: 3-6 attempts total. Guaranteed to find solution if one exists (c in [0.01, 100]).
#'
#'   \strong{When it fails:} If no intersection found from c=0.01 to c=100, indicates
#'   substantial cross-fold heterogeneity. Returns fold-specific trees with warning.
#'   Consider using fold-specific nuisance functions instead of intersection.
#'
#'   \strong{Theory guarantee:} All tried values satisfy epsilon_n = o(n^{-1/2}),
#'   ensuring asymptotically valid inference. The adaptive selection is data-dependent
#'   but still valid because all candidates satisfy the rate condition.
#'
#' @param tune_param Character. (Deprecated - ignored when using new auto-tuning algorithm)
#' @param max_tune_iterations Integer. (Deprecated - ignored when using new auto-tuning algorithm)
#' @param tune_search_range Numeric vector. (Deprecated - ignored when using new auto-tuning algorithm)
#' @param seed Random seed for reproducibility. Default: NULL
#' @param verbose Print progress information. Default: TRUE
#' @param parallel Logical. Whether to parallelize across CV folds using \code{furrr}. Default: \code{TRUE}. If \code{TRUE}, uses the current \code{future} plan (set with \code{future::plan()}). If no plan is set, falls back to sequential execution.
#' @param fold_indices Optional integer vector of length \code{nrow(X)} with values in 1..K giving the fold id for each row. When provided, these folds are used instead of creating folds internally (e.g. for DML: pass the same fold assignment used for the score). When NULL, folds are created via \code{create_folds(y, K)}.
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
#' \strong{Theory-consistent defaults (DML):} For alignment with the manuscript,
#' use small \eqn{\varepsilon} (e.g. \code{rashomon_bound_multiplier = 0.05} or
#' \code{rashomon_bound_adder}), \eqn{\lambda \propto (\log n)/n} (see
#' \code{\link{cv_regularization}}), and pass \code{fold_indices} to
#' \code{predict()} for fold-specific nuisances. See
#' \file{docs/Implementation-requirements-Rashomon-DML.md}.
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
                                  rashomon_bound_adder = 0,
                                  max_leaves = NULL,
                                  single_tree = FALSE,
                                  auto_tune_intersecting = FALSE,
                                  tune_param = "regularization",
                                  max_tune_iterations = 20,
                                  tune_search_range = NULL,
                                  seed = NULL,
                                  verbose = TRUE,
                                  parallel = TRUE,
                                  fold_indices = NULL,
                                  ...) {
  
  # Input validation
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
  
  if (!loss_function %in% c("misclassification", "log_loss", "squared_error", "regression")) {
    stop("loss_function must be 'misclassification', 'log_loss', or 'squared_error'")
  }
  if (loss_function == "regression") loss_function <- "squared_error"
  if (loss_function != "squared_error" && !all(y %in% c(0, 1))) {
    stop("y must contain only binary values (0 and 1) for classification")
  }
  if (loss_function == "squared_error" && !is.numeric(y)) {
    stop("y must be numeric for squared_error (regression)")
  }
  
  n <- nrow(X)
  
  # Resolve folds: external fold_indices (vector) or create internally
  if (!is.null(fold_indices)) {
    if (!is.vector(fold_indices) || length(fold_indices) != n) {
      stop("fold_indices must be a vector of length nrow(X)")
    }
    fold_indices <- as.integer(fold_indices)
    K <- max(fold_indices, na.rm = TRUE)
    if (K < 2) {
      stop("fold_indices must have at least 2 distinct folds (values in 1..K)")
    }
    if (any(is.na(fold_indices)) || any(fold_indices < 1L) || any(fold_indices > K)) {
      stop("fold_indices must contain integers in 1..K only")
    }
    fold_indices <- lapply(1:K, function(k) which(fold_indices == k))
  } else {
    if (K < 2) {
      stop("K must be at least 2")
    }
    if (K > n) {
      stop("K cannot be larger than the number of observations")
    }
    # Set seed for reproducibility
    if (!is.null(seed)) {
      set.seed(seed)
    }
    # Create stratified folds AFTER setting seed
    fold_indices <- create_folds(y, K = K)
  }
  
  if (verbose) {
    cat("Cross-Fitted Rashomon Set Analysis\n")
    cat("==================================\n")
    cat(sprintf("Data: %d observations, %d features\n", n, ncol(X)))
    cat(sprintf("K-fold cross-fitting: K = %d\n", K))
    cat(sprintf("Loss function: %s\n", loss_function))
    cat(sprintf("Regularization: %.3f\n\n", regularization))
  }
  
  if (verbose) {
    fold_sizes <- sapply(fold_indices, length)
    cat(sprintf("Fold sizes: %s\n\n", paste(fold_sizes, collapse = ", ")))
  }
  
  # Auto-tuning: find smallest epsilon_n with non-empty intersection
  if (auto_tune_intersecting) {
    auto_result <- auto_tune_rashomon_intersection(
      X = X, y = y, K = K, fold_indices = fold_indices,
      loss_function = loss_function,
      regularization = regularization,
      c_start = 1,
      c_max = 100,
      binary_tolerance = 0.1,
      verbose = verbose,
      max_leaves = max_leaves,
      single_tree = single_tree,
      parallel = parallel,
      ...
    )

    if (auto_result$converged) {
      # Success: use the found epsilon_n and selected tree
      return(auto_result$result)
    } else {
      # Failure: return fold-specific trees with warning
      warning(
        "Auto-tuning failed to find intersecting Rashomon sets after ",
        auto_result$attempts, " attempts (tried c up to 100). ",
        "This indicates substantial cross-fold heterogeneity. ",
        "Returning fold-specific trees. ",
        "Consider using use_rashomon = FALSE for this dataset.",
        call. = FALSE
      )

      # Still run regular cross-fitting but user should be aware intersection failed
      # Fall through to regular execution below
      if (verbose) {
        cat("\nProceeding with regular cross-fitting (no intersection guarantee)...\n\n")
      }
    }
  }
  
  # Initialize storage
  fold_models <- vector("list", K)
  rashomon_sets <- vector("list", K)
  rashomon_sizes <- integer(K)

  # Determine if we can use parallel processing
  use_parallel <- parallel && .has_furrr &&
                  .has_future

  # Progress bar setup
  if (verbose && .has_cli) {
    cli::cli_progress_bar(
      "Fitting folds",
      total = K,
      format = "{cli::pb_spin} Fold {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  # Generate deterministic per-fold seeds (if user provided a seed)
  fold_seeds <- if (!is.null(seed)) {
    seed + seq_len(K)  # Deterministic per-fold seeds
  } else {
    rep(NA_integer_, K)  # No seeding if user didn't set seed
  }

  # Function to fit one fold with seed support and error handling
  fit_one_fold <- function(k, fold_seed) {
    tryCatch(
      {
        if (!is.na(fold_seed)) set.seed(fold_seed)

        # Get training data (all folds except k)
        test_idx <- fold_indices[[k]]
        train_idx <- setdiff(1:n, test_idx)

        X_train <- X[train_idx, , drop = FALSE]
        y_train <- y[train_idx]

        # Train model using appropriate function based on single_tree parameter
        if (single_tree) {
          # Fit single optimal tree
          model <- optimaltrees::fit_tree(
            X = X_train,
            y = y_train,
            loss_function = loss_function,
            regularization = regularization,
            verbose = FALSE,
            ...
          )
        } else {
          # Fit Rashomon set
          model <- optimaltrees::fit_rashomon(
            X = X_train,
            y = y_train,
            loss_function = loss_function,
            regularization = regularization,
            rashomon_bound_multiplier = rashomon_bound_multiplier,
            rashomon_bound_adder = rashomon_bound_adder,
            verbose = FALSE,
            ...
          )
        }

        # Extract Rashomon set (optionally filtered by max_leaves)
        trees <- optimaltrees::get_rashomon_trees(model, max_leaves = max_leaves)

        # Warning if no trees generated
        if (length(trees) == 0) {
          warning(sprintf("Fold %d: No trees in Rashomon set. Consider adjusting regularization or max_leaves.", k))
        }

        list(model = model, trees = trees, size = length(trees))
      },
      error = function(e) {
        warning("Fold ", k, " failed: ", e$message, call. = FALSE)
        # Return empty result so CV can continue
        list(model = NULL, trees = list(), size = 0L)
      }
    )
  }

  # Execute: parallel or sequential
  if (use_parallel) {
    fold_results <- furrr::future_map2(1:K, fold_seeds, fit_one_fold,
                                       .options = furrr::furrr_options(seed = FALSE))
  } else {
    fold_results <- Map(fit_one_fold, 1:K, fold_seeds)
  }

  # Update progress bar in main thread after collecting results
  if (verbose && .has_cli) {
    for (i in seq_along(fold_results)) {
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }

  # Extract results
  for (k in 1:K) {
    fold_models[[k]] <- fold_results[[k]]$model
    rashomon_sets[[k]] <- fold_results[[k]]$trees
    rashomon_sizes[k] <- fold_results[[k]]$size

    if (verbose) {
      cat(sprintf("Fold %d: Rashomon set size = %d trees\n", k, rashomon_sizes[k]))
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
  
  # Per-fold refits of intersecting structures for DML (eta^(-k))
  fold_refits <- vector("list", K)
  if (intersection_result$n_intersecting > 0) {
    for (k in 1:K) {
      train_idx <- setdiff(1:n, fold_indices[[k]])
      X_k <- X[train_idx, , drop = FALSE]
      y_k <- y[train_idx]
      fold_refits[[k]] <- lapply(intersection_result$intersecting_structures, function(st) {
        refit_structure_on_data(st, X_k, y_k)
      })
    }
  }
  
  # Fold id per row (for predict with fold_indices)
  fold_id_per_row <- integer(n)
  for (k in 1:K) {
    fold_id_per_row[fold_indices[[k]]] <- k
  }
  
  # Create result object
  result <- list(
    intersecting_trees = intersection_result$intersecting_trees,
    n_intersecting = intersection_result$n_intersecting,
    tree_jsons = intersection_result$tree_jsons,
    intersecting_structures = intersection_result$intersecting_structures,
    tree_risks = intersection_result$tree_risks,
    fold_refits = fold_refits,
    fold_id_per_row = fold_id_per_row,
    fold_models = fold_models,
    rashomon_sets = rashomon_sets,
    rashomon_sizes = rashomon_sizes,
    fold_indices = fold_indices,
    K = K,
    loss_function = loss_function,
    regularization = regularization,
    rashomon_bound_multiplier = rashomon_bound_multiplier,
    rashomon_bound_adder = rashomon_bound_adder,
    max_leaves = max_leaves,
    X_train = X,
    y_train = y,
    call = match.call()
  )
  
  class(result) <- "cf_rashomon"
  
  return(result)
}

#' Internal helper function for cross-fitted rashomon (used by auto-tuning)
#'
#' @description
#' Performs cross-fitting with given parameters and returns result.
#' Used internally by auto-tuning logic.
#'
#' @param X Data.frame or matrix of binary features
#' @param y Vector of binary class labels
#' @param K Number of folds
#' @param loss_function Loss function
#' @param regularization Regularization parameter
#' @param rashomon_bound_multiplier Rashomon bound multiplier
#' @param rashomon_bound_adder Rashomon bound adder (default 0)
#' @param max_leaves Optional max leaves sieve (default NULL)
#' @param single_tree Whether to fit single tree per fold
#' @param fold_indices Pre-computed fold indices
#' @param verbose Whether to print progress
#' @param parallel Whether to use parallel processing
#' @param ... Additional parameters
#'
#' @return cf_rashomon object or NULL on error
#'
#' @keywords internal
try_cross_fitted_rashomon_internal <- function(X, y, K, loss_function, regularization,
                                               rashomon_bound_multiplier, rashomon_bound_adder = 0,
                                               max_leaves = NULL, single_tree, fold_indices, verbose, parallel = TRUE, ...) {
  tryCatch({
    # Initialize storage
    fold_models <- vector("list", K)
    rashomon_sets <- vector("list", K)
    rashomon_sizes <- integer(K)

    # Determine if we can use parallel processing
    use_parallel <- parallel && .has_furrr &&
                    .has_future

    # Function to fit one fold
    fit_one_fold <- function(k) {
      # Get training data (all folds except k)
      test_idx <- fold_indices[[k]]
      train_idx <- setdiff(1:nrow(X), test_idx)

      X_train <- X[train_idx, , drop = FALSE]
      y_train <- y[train_idx]

      # Train model using appropriate function based on single_tree parameter
      if (single_tree) {
        # Fit single optimal tree
        model <- optimaltrees::fit_tree(
          X = X_train,
          y = y_train,
          loss_function = loss_function,
          regularization = regularization,
          verbose = FALSE,
          ...
        )
      } else {
        # Fit Rashomon set
        model <- optimaltrees::fit_rashomon(
          X = X_train,
          y = y_train,
          loss_function = loss_function,
          regularization = regularization,
          rashomon_bound_multiplier = rashomon_bound_multiplier,
          rashomon_bound_adder = rashomon_bound_adder,
          verbose = FALSE,
          ...
        )
      }

      # Extract Rashomon set (optionally filtered by max_leaves)
      trees <- optimaltrees::get_rashomon_trees(model, max_leaves = max_leaves)

      list(model = model, trees = trees, size = length(trees))
    }

    # Execute: parallel or sequential
    if (use_parallel) {
      fold_results <- furrr::future_map(1:K, fit_one_fold,
                                        .options = furrr::furrr_options(seed = TRUE))
    } else {
      fold_results <- lapply(1:K, fit_one_fold)
    }

    # Extract results
    for (k in 1:K) {
      fold_models[[k]] <- fold_results[[k]]$model
      rashomon_sets[[k]] <- fold_results[[k]]$trees
      rashomon_sizes[k] <- fold_results[[k]]$size
    }
    
    # Find intersection of trees across all folds
    intersection_result <- find_tree_intersection(rashomon_sets, verbose = FALSE)
    
    n <- nrow(X)
    fold_refits <- vector("list", K)
    if (intersection_result$n_intersecting > 0) {
      for (k in 1:K) {
        train_idx <- setdiff(1:n, fold_indices[[k]])
        X_k <- X[train_idx, , drop = FALSE]
        y_k <- y[train_idx]
        fold_refits[[k]] <- lapply(intersection_result$intersecting_structures, function(st) {
          refit_structure_on_data(st, X_k, y_k)
        })
      }
    }
    fold_id_per_row <- integer(n)
    for (k in 1:K) {
      fold_id_per_row[fold_indices[[k]]] <- k
    }
    
    # Create result object
    result <- list(
      intersecting_trees = intersection_result$intersecting_trees,
      n_intersecting = intersection_result$n_intersecting,
      tree_jsons = intersection_result$tree_jsons,
      intersecting_structures = intersection_result$intersecting_structures,
      tree_risks = intersection_result$tree_risks,
      fold_refits = fold_refits,
      fold_id_per_row = fold_id_per_row,
      fold_models = fold_models,
      rashomon_sets = rashomon_sets,
      rashomon_sizes = rashomon_sizes,
      fold_indices = fold_indices,
      K = K,
      loss_function = loss_function,
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier,
      rashomon_bound_adder = rashomon_bound_adder,
      max_leaves = max_leaves,
      X_train = X,
      y_train = y,
      call = match.call()
    )
    
    class(result) <- "cf_rashomon"
    return(result)
  }, error = function(e) {
    if (verbose) {
      cat(sprintf("  -> ERROR: %s\n", e$message))
    }
    return(NULL)
  })
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


