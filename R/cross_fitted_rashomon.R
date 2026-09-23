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
#' @param rashomon_ignore_trivial_extensions Logical. If FALSE (default), keep all trees including trivial
#'   extensions (same partition, different split sequences). This is REQUIRED for cross-fitted intersection
#'   to work correctly. When folds learn the same partition via different split orders, setting TRUE would
#'   prune each fold's Rashomon set to 1 representative, causing intersection to fail even when folds agree
#'   on the optimal partition. \strong{Do not override this default unless you understand the implications.}
#' @param max_leaves Optional integer. If set, only trees with \eqn{\#\text{leaves} \le} \code{max_leaves} are used (R-side sieve; theory-consistent complexity).
#' @param single_tree Logical. If TRUE, fit exactly one tree per fold (disable rashomon set).
#'   Default: FALSE. When TRUE, each fold fits a single tree. When FALSE, each fold computes
#'   a full rashomon set. For single tree per fold, use \code{\link{fit_tree}} via the underlying
#'   \code{\link{treefarms}} call.
#' @param auto_tune_intersecting Logical. If TRUE, search (over the multiplier
#'   \code{c}, base rate \code{select_epsilon_n(n)} = log(n)/n) for the smallest
#'   tolerance that yields a non-empty cross-fold intersection. Default: FALSE.
#'
#'   \strong{Not valid for inference.} Data-adaptive selection of the Rashomon
#'   tolerance is a \emph{post-selection} device: the tolerance becomes a function
#'   of the data, so it is not covered by the fixed-\eqn{\varepsilon_n} validity
#'   theory, and on hard instances the search can inflate \eqn{\varepsilon_n} well
#'   beyond \eqn{o(n^{-1/2})}, voiding the CLT. Use it only for exploration. For
#'   inference, pass a fixed \code{rashomon_bound_multiplier =
#'   select_epsilon_n(n)} and, if the intersection is empty at that tolerance,
#'   fall back to fold-specific trees rather than enlarging \eqn{\varepsilon_n}.
#'   A \code{warning} is emitted at runtime when this is TRUE.
#'
#'   \strong{Mechanics (exploratory):} bidirectional exponential search from
#'   \code{c = 1} (down if it succeeds, up if it fails) plus binary refinement,
#'   \code{c} in [0.01, 100]; if none intersects, returns fold-specific trees.
#'
#' @param seed Random seed for reproducibility. Default: NULL
#' @param verbose Print progress information. Default: TRUE
#' @param parallel Logical. Whether to parallelize across CV folds using \code{furrr}. Default: \code{TRUE}. If \code{TRUE}, uses the current \code{future} plan (set with \code{future::plan()}). If no plan is set, falls back to sequential execution.
#' @param fold_indices Optional integer vector of length \code{nrow(X)} with values in 1..K giving the fold id for each row. When provided, these folds are used instead of creating folds internally (e.g. for DML: pass the same fold assignment used for the score). When NULL, folds are created via \code{create_stratified_folds_from_y(y, K)}.
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
                                  rashomon_ignore_trivial_extensions = FALSE,
                                  max_leaves = NULL,
                                  single_tree = FALSE,
                                  auto_tune_intersecting = FALSE,
                                  seed = NULL,
                                  verbose = TRUE,
                                  parallel = TRUE,
                                  fold_indices = NULL,
                                  discretize_method = "quantiles",
                                  discretize_bins = "adaptive",
                                  ...) {
  
  # Input validation
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
  
  if (!loss_function %in% c("misclassification", "log_loss", "squared_error", "regression")) {
    cli::cli_abort("{.arg loss_function} must be 'misclassification', 'log_loss', or 'squared_error'.")
  }
  if (loss_function == "regression") loss_function <- "squared_error"
  if (loss_function != "squared_error" && !all(y %in% c(0, 1))) {
    cli::cli_abort("{.arg y} must contain only binary values (0 and 1) for classification.")
  }
  if (loss_function == "squared_error" && !is.numeric(y)) {
    cli::cli_abort("{.arg y} must be numeric for squared_error (regression).")
  }
  
  n <- nrow(X)
  
  # Resolve folds: external fold_indices (vector) or create internally
  if (!is.null(fold_indices)) {
    if (!is.vector(fold_indices) || length(fold_indices) != n) {
      cli::cli_abort("{.arg fold_indices} must be a vector of length {.code nrow(X)}.")
    }
    fold_indices <- as.integer(fold_indices)
    K <- max(fold_indices, na.rm = TRUE)
    if (K < 2) {
      cli::cli_abort("{.arg fold_indices} must have at least 2 distinct folds (values in 1..K).")
    }
    if (any(is.na(fold_indices)) || any(fold_indices < 1L) || any(fold_indices > K)) {
      cli::cli_abort("{.arg fold_indices} must contain integers in 1..K only.")
    }
    fold_indices <- purrr::map(seq_len(K), ~ which(fold_indices == .x))
  } else {
    if (K < 2) {
      cli::cli_abort("{.arg K} must be at least 2.")
    }
    if (K > n) {
      cli::cli_abort("{.arg K} cannot be larger than the number of observations.")
    }
    # Set seed for reproducibility (restore RNG state after)
    if (!is.null(seed)) {
      old_seed <- if (exists(".Random.seed", envir = globalenv())) {
        get(".Random.seed", envir = globalenv())
      }
      on.exit({
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      }, add = TRUE)
      set.seed(seed)
    }
    # Create stratified folds AFTER setting seed
    fold_indices <- create_stratified_folds_from_y(y, K = K)
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
    fold_sizes <- purrr::map_int(fold_indices, length)
    cat(sprintf("Fold sizes: %s\n\n", paste(fold_sizes, collapse = ", ")))
  }
  
  # Pre-compute global discretization from ALL data (needed by both auto-tune and main paths).
  # This ensures every fold uses the same binary feature space (same thresholds),
  # which is required for Rashomon set intersection to work with continuous
  # covariates. Without this, each fold computes fold-specific quantile thresholds,
  # making trees structurally incomparable across folds (only the stump would ever
  # intersect, causing constant nuisance predictions and naive-estimator bias).
  X_df <- if (is.matrix(X)) as.data.frame(X) else X
  # GATED (2026-07-01): the Rashomon path is sensitive to the number of binary
  # features (finer grid -> larger candidate trees -> larger Rashomon sets ->
  # potential model_limit overflow / OOM on complex DGPs, cf. the eps_n work).
  # Until the Rashomon radius (eps_n) is retuned, this path uses the conservative
  # LEGACY log schedule for the "adaptive" keyword rather than the new polynomial
  # default used elsewhere. "cv" is resolved to an integer via select_bins_cv.
  n_bins_global <- if (is.character(discretize_bins)) {
    if (identical(discretize_bins, "cv")) {
      # Resolve CV against the pooled data (outcome available here as `A`/`Y`?).
      # cross_fitted_rashomon fits a single nuisance target `y`; use it.
      select_bins_cv(X_df, y, loss_function = loss_function,
                     method = discretize_method)$best_bins
    } else if (discretize_bins %in% c("adaptive", "log")) {
      # Force the bounded log schedule on the Rashomon path (gating).
      compute_bin_count("log", n)
    } else {
      stop("discretize_bins must be numeric, 'adaptive', 'log', or 'cv', got: ",
           discretize_bins, call. = FALSE)
    }
  } else {
    as.integer(discretize_bins)
  }
  global_disc <- discretize_features(
    X_df,
    method = discretize_method,
    n_bins = n_bins_global
  )
  X_binary <- global_disc$X_binary  # used for fold_refits (need binary X for refit_structure_on_data)
  disc_metadata <- global_disc$metadata

  # Extract per-feature thresholds to pass to treefarms so each fold uses them
  # instead of computing from fold-specific training data.
  global_thresh_list <- Filter(
    Negate(is.null),
    lapply(disc_metadata$features, function(f) {
      if (!is.null(f$thresholds) && length(f$thresholds) > 0) f$thresholds else NULL
    })
  )
  if (length(global_thresh_list) == 0) global_thresh_list <- NULL

  # Auto-tuning: multi-tier search for non-empty intersection
  # Tier 1: tune epsilon at current lambda
  # Tier 2: search over lambda multipliers with fixed epsilon
  # Tier 3: saturated trees (lambda -> 0)
  # Tier 4: give up, fall through to regular cross-fitting
  if (auto_tune_intersecting) {
    warning(
      "auto_tune_intersecting = TRUE selects the Rashomon tolerance from the ",
      "data (post-selection), which voids the o(n^{-1/2}) valid-inference ",
      "guarantee; use it for exploration only. For inference, set ",
      "rashomon_bound_multiplier = select_epsilon_n(nrow(X)) and fall back to ",
      "fold-specific trees if the intersection is empty.",
      call. = FALSE
    )
    auto_result <- auto_tune_regularization_for_intersection(
      X_binary = X_binary, X = X, y = y, K = K, fold_indices = fold_indices,
      loss_function = loss_function,
      regularization_start = regularization,
      verbose = verbose,
      max_leaves = max_leaves,
      discretize_thresholds = global_thresh_list,
      ...
    )

    if (auto_result$converged) {
      result_obj <- auto_result$result
      result_obj@disc_metadata <- disc_metadata
      return(result_obj)
    } else {
      # All tiers failed: substantial cross-fold heterogeneity
      warning(
        "Auto-tuning failed to find intersecting Rashomon sets (tried epsilon tuning, ",
        "lambda search, and saturated trees). ",
        "This indicates substantial cross-fold heterogeneity. ",
        "Returning fold-specific trees. ",
        "Consider using use_rashomon = FALSE for this dataset.",
        call. = FALSE
      )
      if (verbose) {
        cat("\nProceeding with regular cross-fitting (no intersection guarantee)...\n\n")
      }
    }
  }
  
  # Generate deterministic per-fold seeds (if user provided a seed)
  fold_seeds <- if (!is.null(seed)) {
    seed + seq_len(K)
  } else {
    rep(NA_integer_, K)
  }

  fit_rashomon_folds(
    X_binary = X_binary, y = y, K = K,
    fold_indices = fold_indices, fold_seeds = fold_seeds,
    regularization = regularization,
    rashomon_bound_multiplier = rashomon_bound_multiplier,
    rashomon_bound_adder = rashomon_bound_adder,
    rashomon_ignore_trivial_extensions = rashomon_ignore_trivial_extensions,
    loss_function = loss_function,
    max_leaves = max_leaves, single_tree = single_tree, n = n,
    verbose = verbose, parallel = parallel,
    disc_metadata = disc_metadata,
    ...
  )
}

#' Shared Fold-Fitting Helper for Cross-Fitted Rashomon
#'
#' @description
#' Fits K folds, finds the Rashomon intersection, computes per-fold refits
#' using the pre-discretized binary X, and returns a CFRashomon object.
#' Used by both the main cross-fitting path and the auto-tuning path.
#'
#' @param X_binary Pre-discretized binary feature matrix (rows = observations,
#'   cols = binary features). Used for both fold fitting and per-fold refits,
#'   ensuring tree feature indices align with the binary column space.
#' @param y Outcome vector
#' @param K Number of folds
#' @param fold_indices List of length K with test indices per fold
#' @param fold_seeds Integer vector of length K with per-fold seeds (NA = no seeding)
#' @param regularization Regularization parameter
#' @param rashomon_bound_multiplier Rashomon bound multiplier
#' @param rashomon_bound_adder Rashomon bound adder
#' @param rashomon_ignore_trivial_extensions Logical
#' @param loss_function Loss function
#' @param max_leaves Optional max leaves sieve (default NULL)
#' @param single_tree Whether to fit single tree per fold (default FALSE)
#' @param n Number of observations
#' @param verbose Print progress (default FALSE)
#' @param parallel Use parallel processing (default TRUE)
#' @param disc_metadata Discretization metadata to store on result (default NULL)
#' @param ... Additional parameters passed to tree fitting
#'
#' @return CFRashomon object
#' @keywords internal
fit_rashomon_folds <- function(X_binary, y, K, fold_indices, fold_seeds,
                               regularization, rashomon_bound_multiplier,
                               rashomon_bound_adder, rashomon_ignore_trivial_extensions,
                               loss_function, max_leaves = NULL, single_tree = FALSE, n,
                               verbose = FALSE, parallel = TRUE, disc_metadata = NULL, ...) {
  # Initialize storage
  fold_models <- vector("list", K)
  rashomon_sets <- vector("list", K)
  rashomon_sizes <- integer(K)

  # Determine if we can use parallel processing
  use_parallel <- parallel && .has_furrr && .has_future

  # Progress bar setup
  if (verbose && .has_cli) {
    cli::cli_progress_bar(
      "Fitting folds",
      total = K,
      format = "{cli::pb_spin} Fold {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }

  # Function to fit one fold with seed support and error handling
  fit_one_fold <- function(k, fold_seed) {
    tryCatch(
      {
        if (!is.na(fold_seed)) set.seed(fold_seed)

        # Get training data (all folds except k)
        test_idx <- fold_indices[[k]]
        train_idx <- setdiff(seq_len(n), test_idx)

        # X_binary is already globally discretized — use it directly so all folds
        # share the same binary feature space (required for Rashomon intersection).
        X_train <- X_binary[train_idx, , drop = FALSE]
        y_train <- y[train_idx]

        if (single_tree) {
          model <- optimaltrees::fit_tree(
            X = X_train,
            y = y_train,
            loss_function = loss_function,
            regularization = regularization,
            verbose = FALSE,
            ...
          )
        } else {
          model <- optimaltrees::fit_rashomon(
            X = X_train,
            y = y_train,
            loss_function = loss_function,
            regularization = regularization,
            rashomon_bound_multiplier = rashomon_bound_multiplier,
            rashomon_bound_adder = rashomon_bound_adder,
            rashomon_ignore_trivial_extensions = rashomon_ignore_trivial_extensions,
            verbose = FALSE,
            ...
          )
        }

        trees <- optimaltrees::get_rashomon_trees(model, max_leaves = max_leaves)
        if (length(trees) == 0) {
          warning(sprintf(
            "Fold %d: No trees in Rashomon set. Consider adjusting regularization or max_leaves.", k
          ))
        }

        list(model = model, trees = trees, size = length(trees))
      },
      error = function(e) {
        warning("Fold ", k, " failed: ", e$message, call. = FALSE)
        list(model = NULL, trees = list(), size = 0L)
      }
    )
  }

  # Execute: parallel or sequential
  if (use_parallel) {
    fold_results <- furrr::future_map2(seq_len(K), fold_seeds, fit_one_fold,
                                       .options = furrr::furrr_options(seed = FALSE))
  } else {
    fold_results <- Map(fit_one_fold, seq_len(K), fold_seeds)
  }

  # Update progress bar after collecting results
  if (verbose && .has_cli) {
    for (i in seq_along(fold_results)) {
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }

  # Extract results
  for (k in seq_len(K)) {
    fold_models[[k]] <- fold_results[[k]]$model
    rashomon_sets[[k]] <- fold_results[[k]]$trees
    rashomon_sizes[k] <- fold_results[[k]]$size
    if (verbose) {
      cat(sprintf("Fold %d: Rashomon set size = %d trees\n", k, rashomon_sizes[k]))
    }
  }

  if (verbose) cat("\n")

  if (verbose) cat("Finding trees appearing in all Rashomon sets...\n")
  intersection_result <- find_tree_intersection(rashomon_sets, verbose = verbose)

  # Per-fold refits of intersecting structures for DML (eta^(-k)).
  # Use X_binary so feature indices in the tree structure align with binary columns.
  fold_refits <- vector("list", K)
  if (intersection_result$n_intersecting > 0) {
    for (k in seq_len(K)) {
      train_idx <- setdiff(seq_len(n), fold_indices[[k]])
      X_k <- X_binary[train_idx, , drop = FALSE]
      y_k <- y[train_idx]
      fold_refits[[k]] <- purrr::map(intersection_result$intersecting_structures, ~ {
        # Cross-fitting folds may not cover every leaf of the shared structure;
        # tolerate empty leaves (filled with the fold's overall mean, with a warning).
        refit_structure_on_data(.x, X_k, y_k, allow_partial_leaves = TRUE)
      })
    }
  }

  # Fold id per row (for predict with fold_indices)
  fold_id_per_row <- integer(n)
  for (k in seq_len(K)) {
    fold_id_per_row[fold_indices[[k]]] <- k
  }

  CFRashomon(
    K = as.integer(K),
    loss_function = loss_function,
    regularization = regularization,
    rashomon_bound_multiplier = rashomon_bound_multiplier,
    rashomon_bound_adder = rashomon_bound_adder,
    max_leaves = if (is.null(max_leaves)) NULL else as.integer(max_leaves),
    rashomon_sizes = as.integer(rashomon_sizes),
    n_intersecting = as.integer(intersection_result$n_intersecting),
    intersecting_trees = intersection_result$intersecting_trees,
    tree_risks = intersection_result$tree_risks,
    fold_refits = fold_refits,
    fold_id_per_row = as.integer(fold_id_per_row),
    fold_indices = fold_indices,
    X_train = X_binary,
    y_train = y,
    converged = TRUE,
    disc_metadata = disc_metadata
  )
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
create_stratified_folds_from_y <- function(y, K) {
  n <- length(y)
  is_binary <- all(y %in% c(0, 1))

  if (is_binary) {
    # Stratify by class for binary outcomes
    class_0_idx <- which(y == 0)
    class_1_idx <- which(y == 1)

    fold_0 <- sample(rep(seq_len(K), length.out = length(class_0_idx)))
    fold_1 <- sample(rep(seq_len(K), length.out = length(class_1_idx)))

    fold_assignment <- integer(n)
    fold_assignment[class_0_idx] <- fold_0
    fold_assignment[class_1_idx] <- fold_1
  } else {
    # Simple random fold assignment for continuous outcomes
    fold_assignment <- sample(rep(seq_len(K), length.out = n))
  }

  # Convert to list of indices
  fold_indices <- purrr::map(seq_len(K), ~ {
    which(fold_assignment == .x)
  })

  return(fold_indices)
}

#' Print Method for cf_rashomon Objects
#'
#' @param x A cf_rashomon object (S7 CFRashomon or S3 list)
#' @param ... Additional arguments (unused)
#'
#' @export
print.cf_rashomon <- function(x, ...) {
  is_s7 <- S7::S7_inherits(x, CFRashomon)
  K <- if (is_s7) x@K else x$K
  loss_function <- if (is_s7) x@loss_function else x$loss_function
  regularization <- if (is_s7) x@regularization else x$regularization
  rashomon_sizes <- if (is_s7) x@rashomon_sizes else x$rashomon_sizes
  n_intersecting <- if (is_s7) x@n_intersecting else x$n_intersecting

  cat("Cross-Fitted Rashomon Set Analysis\n")
  cat("==================================\n")
  cat(sprintf("Number of folds: %d\n", K))
  cat(sprintf("Loss function: %s\n", loss_function))
  cat(sprintf("Regularization: %.3f\n\n", regularization))

  cat("Rashomon set sizes per fold:\n")
  for (k in seq_along(rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", k, rashomon_sizes[k]))
  }

  cat(sprintf("\nIntersecting trees: %d\n", n_intersecting))

  if (n_intersecting > 0) {
    cat("\n\u2713 Found stable tree(s) appearing in all folds!\n")
    cat("  Use predict() to make predictions with the stable model.\n")
  } else {
    cat("\n\u2717 No trees appear in all folds.\n")
    cat("  Consider:\n")
    cat("  - Increasing regularization\n")
    cat("  - Adjusting rashomon_bound_multiplier\n")
    cat("  - Using fewer folds (smaller K)\n")
  }
  invisible(x)
}

#' Summary Method for cf_rashomon Objects
#'
#' @param object A cf_rashomon object (S7 CFRashomon or S3 list)
#' @param ... Additional arguments (unused)
#'
#' @export
summary.cf_rashomon <- function(object, ...) {
  is_s7 <- S7::S7_inherits(object, CFRashomon)
  K <- if (is_s7) object@K else object$K
  loss_function <- if (is_s7) object@loss_function else object$loss_function
  regularization <- if (is_s7) object@regularization else object$regularization
  X_train <- if (is_s7) object@X_train else object$X_train
  rashomon_sizes <- if (is_s7) object@rashomon_sizes else object$rashomon_sizes
  n_intersecting <- if (is_s7) object@n_intersecting else object$n_intersecting

  cat("Cross-Fitted Rashomon Set Summary\n")
  cat("=================================\n\n")

  cat("Configuration:\n")
  cat("  K-fold cross-fitting: K =", K, "\n")
  cat("  Loss function:", loss_function, "\n")
  cat("  Regularization:", regularization, "\n\n")

  cat("Data:\n")
  cat("  Samples:", nrow(X_train), "\n")
  cat("  Features:", ncol(X_train), "\n\n")

  cat("Fold Results:\n")
  for (i in seq_along(rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", i, rashomon_sizes[i]))
  }
  cat("\n")

  cat("Intersection Results:\n")
  cat("  Trees in all folds:", n_intersecting, "\n")

  if (n_intersecting > 0) {
    cat("  \u2713 Stable trees found!\n")
    cat("  Use predict() to make predictions with stable model.\n")
  } else {
    cat("  \u2717 No stable trees found.\n")
    cat("  Consider:\n")
    cat("    - Increasing regularization\n")
    cat("    - Adjusting rashomon_bound_multiplier\n")
    cat("    - Using different K value\n")
  }

  result <- list(
    model_type = "CFRashomon",
    K = K,
    loss_function = loss_function,
    regularization = regularization,
    n_intersecting = n_intersecting,
    rashomon_sizes = rashomon_sizes,
    n_samples = nrow(X_train),
    n_features = ncol(X_train)
  )
  invisible(result)
}


