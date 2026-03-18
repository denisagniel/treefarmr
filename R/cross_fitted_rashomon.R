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
#' @param auto_tune_intersecting Logical. If TRUE, auto-tune parameters until at least one
#'   intersecting tree is found. Default: FALSE.
#' @param tune_param Character. Which parameter to tune when auto_tune_intersecting=TRUE.
#'   Options: "regularization", "rashomon_bound_multiplier", or "both". Default: "regularization".
#' @param max_tune_iterations Integer. Maximum number of auto-tuning iterations. Default: 20.
#' @param tune_search_range Numeric vector of length 2. Search range for the parameter being tuned.
#'   If NULL, uses default ranges based on loss_function. Default: NULL.
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
  
  # Auto-tuning for intersecting trees
  if (auto_tune_intersecting) {
    if (verbose) {
      cat("Auto-tuning parameters to find intersecting trees...\n")
      cat(sprintf("Tuning parameter: %s\n", tune_param))
      cat(sprintf("Max iterations: %d\n\n", max_tune_iterations))
    }
    
    # Validate tune_param
    if (!tune_param %in% c("regularization", "rashomon_bound_multiplier", "both")) {
      stop("tune_param must be 'regularization', 'rashomon_bound_multiplier', or 'both'")
    }
    
    # Set default search ranges
    if (is.null(tune_search_range)) {
      if (tune_param == "regularization") {
        if (loss_function == "log_loss" || loss_function == "squared_error") {
          tune_search_range <- c(0.1, 1.0)
        } else {
          tune_search_range <- c(0.01, 0.5)
        }
      } else if (tune_param == "rashomon_bound_multiplier") {
        tune_search_range <- c(0.01, 0.5)
      } else {
        if (loss_function == "log_loss" || loss_function == "squared_error") {
          tune_search_range <- c(0.1, 1.0)
        } else {
          tune_search_range <- c(0.01, 0.5)
        }
      }
    }
    
    # Store original parameters
    orig_regularization <- regularization
    orig_rashomon_multiplier <- rashomon_bound_multiplier
    
    # Binary search for parameters that yield intersecting trees
    best_result <- NULL
    best_n_intersecting <- 0
    iterations <- 0
    
    # For "both", we'll tune regularization first, then rashomon_bound_multiplier
    if (tune_param == "both") {
      # First tune regularization
      low_reg <- tune_search_range[1]
      high_reg <- tune_search_range[2]
      
      while (iterations < max_tune_iterations && (high_reg - low_reg) > 0.001) {
        iterations <- iterations + 1
        mid_reg <- (low_reg + high_reg) / 2
        regularization <- mid_reg
        
        if (verbose) {
          cat(sprintf("Iteration %d: regularization=%.3f, rashomon_bound_multiplier=%.3f\n",
                     iterations, regularization, rashomon_bound_multiplier))
        }

        # Try with current parameters
        result <- try_cross_fitted_rashomon_internal(
          X, y, K, loss_function, regularization, rashomon_bound_multiplier,
          rashomon_bound_adder, max_leaves, single_tree, fold_indices, verbose, parallel, ...
        )

        if (is.null(result)) {
          # Error occurred, try more restrictive parameters
          high_reg <- mid_reg
          next
        }
        
        if (result$n_intersecting > 0) {
          if (verbose) {
            cat(sprintf("  -> SUCCESS: Found %d intersecting tree(s)!\n", result$n_intersecting))
          }
          return(result)
        }
        
        # Adjust search range - if no intersecting trees, try higher regularization
        # (higher regularization = simpler trees = more likely to intersect)
        if (result$n_intersecting == 0) {
          high_reg <- mid_reg  # Increase regularization (move toward higher values)
        } else {
          low_reg <- mid_reg  # Found intersecting trees, can try lower regularization
        }
        
        if (result$n_intersecting > best_n_intersecting) {
          best_result <- result
          best_n_intersecting <- result$n_intersecting
        }
      }
      
      # If still no intersecting trees, try tuning rashomon_bound_multiplier
      if (best_n_intersecting == 0) {
        regularization <- orig_regularization
        low_mult <- 0.01
        high_mult <- 0.5
        
        while (iterations < max_tune_iterations && (high_mult - low_mult) > 0.001) {
          iterations <- iterations + 1
          mid_mult <- (low_mult + high_mult) / 2
          rashomon_bound_multiplier <- mid_mult
          
          if (verbose) {
            cat(sprintf("Iteration %d: regularization=%.3f, rashomon_bound_multiplier=%.3f\n",
                       iterations, regularization, rashomon_bound_multiplier))
          }
          
          result <- try_cross_fitted_rashomon_internal(
            X, y, K, loss_function, regularization, rashomon_bound_multiplier,
            rashomon_bound_adder, max_leaves, single_tree, fold_indices, verbose, parallel, ...
          )
          
          if (is.null(result)) {
            # Error occurred, try more permissive parameters
            low_mult <- mid_mult
            next
          }
          
          if (result$n_intersecting > 0) {
            if (verbose) {
              cat(sprintf("  -> SUCCESS: Found %d intersecting tree(s)!\n", result$n_intersecting))
            }
            return(result)
          }
          
          # Lower multiplier = more trees = more likely to intersect
          if (result$n_intersecting == 0) {
            low_mult <- mid_mult  # Decrease multiplier (move toward lower values)
          } else {
            high_mult <- mid_mult  # Found intersecting trees, can try higher multiplier
          }
          
          if (result$n_intersecting > best_n_intersecting) {
            best_result <- result
            best_n_intersecting <- result$n_intersecting
          }
        }
      }
    } else {
      # Tune single parameter
      low <- tune_search_range[1]
      high <- tune_search_range[2]
      
      while (iterations < max_tune_iterations && (high - low) > 0.001) {
        iterations <- iterations + 1
        mid <- (low + high) / 2
        
        if (tune_param == "regularization") {
          regularization <- mid
        } else {
          rashomon_bound_multiplier <- mid
        }
        
        if (verbose) {
          cat(sprintf("Iteration %d: regularization=%.3f, rashomon_bound_multiplier=%.3f\n",
                     iterations, regularization, rashomon_bound_multiplier))
        }

        # Try with current parameters
        result <- try_cross_fitted_rashomon_internal(
          X, y, K, loss_function, regularization, rashomon_bound_multiplier,
          rashomon_bound_adder, max_leaves, single_tree, fold_indices, verbose, parallel, ...
        )

        if (is.null(result)) {
          # Error occurred, adjust search range based on parameter
          if (tune_param == "regularization") {
            high <- mid  # Try more restrictive
          } else {
            low <- mid  # Try more permissive
          }
          next
        }
        
        if (result$n_intersecting > 0) {
          if (verbose) {
            cat(sprintf("  -> SUCCESS: Found %d intersecting tree(s)!\n", result$n_intersecting))
          }
          return(result)
        }
        
        # Adjust search range
        if (tune_param == "regularization") {
          # Higher regularization = simpler trees = more likely to intersect
          if (result$n_intersecting == 0) {
            high <- mid  # Increase regularization (move toward higher values)
          } else {
            low <- mid  # Found intersecting trees, can try lower regularization
          }
        } else {
          # Lower multiplier = more trees = more likely to intersect
          if (result$n_intersecting == 0) {
            low <- mid  # Decrease multiplier (move toward lower values)
          } else {
            high <- mid  # Found intersecting trees, can try higher multiplier
          }
        }
        
        if (result$n_intersecting > best_n_intersecting) {
          best_result <- result
          best_n_intersecting <- result$n_intersecting
        }
      }
    }
    
    # If we found a result (even if not intersecting), return it
    if (!is.null(best_result)) {
      if (verbose) {
        cat(sprintf("\nAuto-tuning completed: Found %d intersecting tree(s) after %d iterations\n",
                   best_n_intersecting, iterations))
        if (best_n_intersecting == 0) {
          cat("Warning: No intersecting trees found. Consider adjusting search range or parameters.\n")
        }
      }
      return(best_result)
    }
    
    # If no result found, fall through to regular execution with original parameters
    if (verbose) {
      cat("Auto-tuning failed, using original parameters\n\n")
    }
    regularization <- orig_regularization
    rashomon_bound_multiplier <- orig_rashomon_multiplier
  }
  
  # Initialize storage
  fold_models <- vector("list", K)
  rashomon_sets <- vector("list", K)
  rashomon_sizes <- integer(K)

  # Determine if we can use parallel processing
  use_parallel <- parallel && requireNamespace("furrr", quietly = TRUE) &&
                  requireNamespace("future", quietly = TRUE)

  # Progress bar setup
  if (verbose && requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_bar(
      "Fitting folds",
      total = K,
      format = "{cli::pb_spin} Fold {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  # Function to fit one fold
  fit_one_fold <- function(k) {
    # Get training data (all folds except k)
    test_idx <- fold_indices[[k]]
    train_idx <- setdiff(1:n, test_idx)

    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]

    # Train TreeFARMS model
    model <- treefarms(
      X = X_train,
      y = y_train,
      loss_function = loss_function,
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier,
      rashomon_bound_adder = rashomon_bound_adder,
      single_tree = single_tree,
      verbose = FALSE,
      ...
    )

    # Extract Rashomon set (optionally filtered by max_leaves)
    trees <- get_rashomon_trees(model, max_leaves = max_leaves)

    if (verbose && requireNamespace("cli", quietly = TRUE)) {
      cli::cli_progress_update()
    }

    # Warning if no trees generated
    if (length(trees) == 0) {
      warning(sprintf("Fold %d: No trees in Rashomon set. Consider adjusting regularization or max_leaves.", k))
    }

    list(model = model, trees = trees, size = length(trees))
  }

  # Execute: parallel or sequential
  if (use_parallel) {
    fold_results <- furrr::future_map(1:K, fit_one_fold,
                                      .options = furrr::furrr_options(seed = TRUE))
  } else {
    fold_results <- lapply(1:K, fit_one_fold)
  }

  # Complete progress bar
  if (verbose && requireNamespace("cli", quietly = TRUE)) {
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
    use_parallel <- parallel && requireNamespace("furrr", quietly = TRUE) &&
                    requireNamespace("future", quietly = TRUE)

    # Function to fit one fold
    fit_one_fold <- function(k) {
      # Get training data (all folds except k)
      test_idx <- fold_indices[[k]]
      train_idx <- setdiff(1:nrow(X), test_idx)

      X_train <- X[train_idx, , drop = FALSE]
      y_train <- y[train_idx]

      # Train TreeFARMS model
      model <- treefarms(
        X = X_train,
        y = y_train,
        loss_function = loss_function,
        regularization = regularization,
        rashomon_bound_multiplier = rashomon_bound_multiplier,
        rashomon_bound_adder = rashomon_bound_adder,
        single_tree = single_tree,
        verbose = FALSE,
        ...
      )

      # Extract Rashomon set (optionally filtered by max_leaves)
      trees <- get_rashomon_trees(model, max_leaves = max_leaves)

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


