#' Auto-tune TreeFARMS Parameters
#'
#' @description
#' Automatically find optimal parameters (regularization or rashomon_bound_multiplier)
#' to achieve a target number of trees in the Rashomon set.
#'
#' @param X A data.frame or matrix of features
#' @param y A vector of binary class labels
#' @param loss_function Character string: "misclassification" or "log_loss"
#' @param target_trees Integer: target number of trees (default: 1)
#' @param max_trees Integer: maximum acceptable number of trees (default: 5)
#' @param fixed_param Character: which parameter is fixed ("regularization" or "rashomon_bound_multiplier")
#' @param fixed_value Numeric: the fixed parameter value
#' @param search_range Numeric vector: range to search for the variable parameter
#' @param max_iterations Integer: maximum number of search iterations (default: 20)
#' @param verbose Logical: whether to print search progress
#' @param ... Additional parameters passed to TreeFARMS
#'
#' @return A list containing:
#'   \item{model}{The trained TreeFARMS model}
#'   \item{regularization}{Final regularization value}
#'   \item{rashomon_bound_multiplier}{Final rashomon_bound_multiplier value}
#'   \item{n_trees}{Number of trees found}
#'   \item{iterations}{Number of search iterations used}
#'   \item{converged}{Whether the search converged to target}
#'
#' @examples
#' # Generate binary classification data
#' set.seed(42)
#' n <- 100
#' X <- data.frame(X1 = rbinom(n, 1, 0.5), X2 = rbinom(n, 1, 0.5))
#' y <- rbinom(n, 1, plogis(0.5 * X$X1 - 0.3 * X$X2))
#'
#' # Auto-tune to find exactly 1 tree (single optimal model)
#' result <- auto_tune_optimaltrees(
#'   X, y,
#'   loss_function = "log_loss",
#'   target_trees = 1,
#'   max_trees = 1,
#'   fixed_param = "regularization",
#'   fixed_value = 0.1
#' )
#'
#' print(result$n_trees)  # Should be 1
#' print(result$converged)  # Should be TRUE
#'
#' @export
auto_tune_optimaltrees <- function(X, y, loss_function = "misclassification",
                                target_trees = 1, max_trees = 5,
                                fixed_param = "regularization", fixed_value = 0.1,
                                search_range = NULL, max_iterations = 20,
                                verbose = FALSE,
                                discretize_method = "quantiles",
                                discretize_bins = "adaptive",
                                discretize_thresholds = NULL, ...) {

  # Input validation
  if (!fixed_param %in% c("regularization", "rashomon_bound_multiplier")) {
    stop("fixed_param must be 'regularization' or 'rashomon_bound_multiplier'")
  }

  if (target_trees < 1) {
    stop("target_trees must be at least 1")
  }

  if (max_trees < target_trees) {
    stop("max_trees must be at least target_trees")
  }

  # Validate discretization parameters
  if (!discretize_method %in% c("median", "quantiles")) {
    stop("discretize_method must be 'median' or 'quantiles', got: ",
         discretize_method, call. = FALSE)
  }

  valid_bins <- (is.character(discretize_bins) &&
                   discretize_bins %in% c("adaptive", "log", "cv")) ||
    (is.numeric(discretize_bins) && length(discretize_bins) == 1 && discretize_bins >= 2)
  if (!valid_bins) {
    stop("discretize_bins must be 'adaptive', 'log', 'cv', or a single numeric value >= 2, got: ",
         discretize_bins, call. = FALSE)
  }

  if (!is.null(discretize_thresholds) && !is.numeric(discretize_thresholds)) {
    stop("discretize_thresholds must be numeric if provided", call. = FALSE)
  }

  # Set default search ranges based on fixed parameter
  if (is.null(search_range)) {
    if (fixed_param == "regularization") {
      # Search rashomon_bound_multiplier
      search_range <- c(0.01, 0.5)
    } else {
      # Search regularization
      if (loss_function == "log_loss") {
        search_range <- c(0.1, 1.0)  # More reasonable range for log_loss
      } else {
        search_range <- c(0.01, 0.5)  # Lower range for misclassification
      }
    }
  }

  if (verbose) {
    message("=== AUTO-TUNING OPTIMALTREES ===")
    message(sprintf("Target trees: %d, Max trees: %d", target_trees, max_trees))
    message(sprintf("Fixed %s: %.3f", fixed_param, fixed_value))
    message(sprintf("Searching %s in range [%.3f, %.3f]",
                ifelse(fixed_param == "regularization", "rashomon_bound_multiplier", "regularization"),
                search_range[1], search_range[2]))
    message(sprintf("Loss function: %s\n", loss_function))
  }

  # Convert to data.frame if matrix
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }

  # Store original X for metadata
  X_original <- X

  # Resolve a "cv" bin schedule to a concrete count (needs the outcome y).
  bins_arg <- discretize_bins
  if (identical(bins_arg, "cv")) {
    bins_arg <- select_bins_cv(X, y, loss_function = loss_function,
                               method = discretize_method)$best_bins
  }

  # Discretize continuous features
  discretization_result <- discretize_features(
    X = X,
    method = discretize_method,
    n_bins = bins_arg,
    thresholds = discretize_thresholds
  )

  X <- discretization_result$X_binary
  discretization_metadata <- discretization_result$metadata

  # Build CSV string once for reuse in the loop
  data_df <- as.data.frame(X)
  data_df$class <- y
  header <- paste(names(data_df), collapse = ",")
  body <- apply(data_df, 1L, function(r) paste(as.character(r), collapse = ","))
  csv_string <- paste(c(header, body), collapse = "\n")
  # Extract max_depth/depth_budget/model_limit from ... if provided
  dots <- list(...)
  depth_budget <- dots$max_depth %||% dots$depth_budget %||% 0L
  model_limit_arg <- dots$model_limit

  base_config <- list(
    loss_function = loss_function,
    verbose = FALSE,
    worker_limit = 1L,
    rashomon = TRUE,
    depth_budget = as.integer(depth_budget)
  )
  if (!is.null(model_limit_arg)) {
    base_config$model_limit <- as.integer(model_limit_arg)
  }

  fit_with_csv <- get(".treefarms_fit_with_csv", envir = asNamespace("optimaltrees"))

  # TIER 1: Exponential + Binary with default range
  if (verbose) {
    message("--- Tier 1: Standard Search ---")
  }

  result_tier1 <- .auto_tune_exponential_binary(
    csv_string = csv_string,
    base_config = base_config,
    fit_with_csv = fit_with_csv,
    X = X,
    y = y,
    X_original = X_original,
    discretization_metadata = discretization_metadata,
    target_trees = target_trees,
    max_trees = max_trees,
    fixed_param = fixed_param,
    fixed_value = fixed_value,
    search_range = search_range,
    max_iterations = max_iterations,
    verbose = verbose
  )

  if (!is.null(result_tier1) && result_tier1$converged) {
    if (verbose) {
      message(sprintf("\n=== SUCCESS: Found %d trees in Tier 1 ===\n", result_tier1$n_trees))
    }
    return(result_tier1)
  }

  # TIER 2: Wider search range
  if (verbose) {
    message("\n--- Tier 2: Wider Search Range ---")
  }

  wider_range <- if (fixed_param == "regularization") {
    c(0.001, 2.0)  # Much wider rashomon_bound_multiplier range
  } else {
    c(0.001, 2.0)  # Much wider regularization range
  }

  result_tier2 <- .auto_tune_exponential_binary(
    csv_string = csv_string,
    base_config = base_config,
    fit_with_csv = fit_with_csv,
    X = X,
    y = y,
    X_original = X_original,
    discretization_metadata = discretization_metadata,
    target_trees = target_trees,
    max_trees = max_trees,
    fixed_param = fixed_param,
    fixed_value = fixed_value,
    search_range = wider_range,
    max_iterations = max_iterations,
    verbose = verbose
  )

  if (!is.null(result_tier2) && result_tier2$converged) {
    if (verbose) {
      message(sprintf("\n=== SUCCESS: Found %d trees in Tier 2 ===\n", result_tier2$n_trees))
    }
    return(result_tier2)
  }

  # TIER 3: Best effort (return closest result with warning)
  best_result <- result_tier2 %||% result_tier1

  if (!is.null(best_result)) {
    warning(sprintf(
      "Auto-tuning did not converge to target %d-%d trees. Returning best result with %d trees.",
      target_trees, max_trees, best_result$n_trees
    ), call. = FALSE)
    best_result$converged <- FALSE
    return(best_result)
  }

  # Only error if no fits succeeded at all
  stop(sprintf("Auto-tuning failed: no successful model fits after %d iterations",
               max_iterations), call. = FALSE)
}

#' Exponential Search + Binary Refinement for Auto-Tuning
#'
#' @description
#' Wraps \code{bidirectional_exp_binary_search()} for single-model tree-count fitting.
#' Searches for a parameter value (regularization or rashomon_bound_multiplier) that
#' yields a tree count in [target_trees, max_trees].
#'
#' @keywords internal
.auto_tune_exponential_binary <- function(csv_string, base_config, fit_with_csv,
                                         X, y, X_original, discretization_metadata,
                                         target_trees, max_trees,
                                         fixed_param, fixed_value,
                                         search_range, max_iterations,
                                         verbose = FALSE) {

  # Build model for a specific parameter value
  try_fit <- function(param_value) {
    if (fixed_param == "regularization") {
      regularization <- fixed_value
      rashomon_bound_multiplier <- param_value
    } else {
      regularization <- param_value
      rashomon_bound_multiplier <- fixed_value
    }

    config <- c(base_config, list(
      regularization = regularization,
      rashomon_bound_multiplier = rashomon_bound_multiplier
    ))

    tryCatch({
      model <- fit_with_csv(csv_string, config, X, y,
                           single_tree = FALSE, store_training_data = FALSE,
                           compute_probabilities = FALSE,
                           discretization_metadata, X_original)
      list(
        model = model,
        regularization = regularization,
        rashomon_bound_multiplier = rashomon_bound_multiplier,
        n_trees = model@n_trees,
        converged = FALSE
      )
    }, error = function(e) {
      if (verbose) message(sprintf("  Error fitting: %s", e$message))
      NULL
    })
  }

  # Track the best result (closest to target) via closure for best-effort fallback
  best_result   <- NULL
  best_distance <- Inf

  probe_fn <- function(val) {
    result <- try_fit(val)
    if (!is.null(result)) {
      n_trees <- result$n_trees
      dist <- if (n_trees >= target_trees && n_trees <= max_trees) 0L else
        min(abs(n_trees - target_trees), abs(n_trees - max_trees))
      if (dist < best_distance) {
        best_result   <<- result
        best_distance <<- dist
      }
    }
    result
  }

  # "lower" = too few trees → search downward (smaller param = more permissive = more trees)
  # "higher" = too many trees → search upward (larger param = more restrictive = fewer trees)
  classify_fn <- function(result) {
    n_trees <- result$n_trees
    if (n_trees >= target_trees && n_trees <= max_trees) "success"
    else if (n_trees < target_trees) "lower"
    else "higher"
  }

  param_start <- mean(search_range)

  if (verbose) {
    message(sprintf("\n--- Exponential + Binary Search (starting at %.4f) ---", param_start))
  }

  # Starting point NULL-check: the generic utility returns NULL result + converged=FALSE
  # if the first probe itself returns NULL. Handle this before calling.
  result_start_check <- try_fit(param_start)
  if (is.null(result_start_check)) {
    return(NULL)
  }
  # Update best tracking with start result
  n_trees_start <- result_start_check$n_trees
  dist_start <- if (n_trees_start >= target_trees && n_trees_start <= max_trees) 0L else
    min(abs(n_trees_start - target_trees), abs(n_trees_start - max_trees))
  if (dist_start < best_distance) {
    best_result   <- result_start_check
    best_distance <- dist_start
  }

  # Re-wrap probe_fn so it passes the already-computed start result on first call
  first_call_done <- FALSE
  probe_fn_wrapped <- function(val) {
    if (!first_call_done && isTRUE(all.equal(val, param_start))) {
      first_call_done <<- TRUE
      return(result_start_check)
    }
    probe_fn(val)
  }

  search <- bidirectional_exp_binary_search(
    probe_fn    = probe_fn_wrapped,
    classify_fn = classify_fn,
    start       = param_start,
    val_min     = search_range[1],
    val_max     = search_range[2],
    binary_tolerance = 0.001,
    max_attempts = max_iterations,
    verbose     = verbose
  )

  # Determine convergence from the ACTUAL tree count of the closest result found,
  # not from search$converged. The generic bidirectional_exp_binary_search sets
  # converged = TRUE whenever a "lower"-or-"success" result was ever seen (and its
  # terminal binary-refinement return is unconditionally TRUE). For tree-count tuning
  # a "lower" result means too FEW trees -> NOT success. So an impossible target
  # (e.g. more trees than the data can produce) would falsely report convergence.
  # `best_result`/`best_distance` track the closest-to-target result across all probes
  # and are authoritative: best_distance == 0 iff some probe landed in [target, max].
  final <- best_result %||% search$result
  if (is.null(final)) return(NULL)
  final$iterations <- search$attempts
  final$converged  <- (final$n_trees >= target_trees && final$n_trees <= max_trees)
  if (verbose && !final$converged) {
    message(sprintf("\nNo exact match found. Best result: %d trees", final$n_trees))
  }
  final
}

