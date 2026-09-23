#' Generic bidirectional exponential + binary refinement search
#'
#' Implements the shared algorithmic skeleton used by both
#' \code{auto_tune_rashomon_intersection()} and \code{.auto_tune_exponential_binary()}:
#' bidirectional exponential probing to bracket a target zone, followed by
#' binary refinement to narrow within the bracket.
#'
#' @param probe_fn function(value) -> result | NULL. Evaluates the objective
#'   at a given parameter value. Return NULL signals a hard failure (treated as
#'   "higher", i.e., the value was too small/permissive).
#' @param classify_fn function(result) -> "lower" | "higher" | "success".
#'   "lower" = probe says try smaller values (current value is too large).
#'   "higher" = probe says try larger values (current value is too small).
#'   "success" = probe result is exactly on target; return immediately.
#' @param start Numeric starting value.
#' @param val_min Numeric floor (search will not go below this). Default: 0.
#' @param val_max Numeric ceiling. Default: Inf (capped at 1e6 internally).
#' @param binary_tolerance Stop binary refinement when interval < this. Default: 0.01.
#' @param max_attempts Maximum total probe calls. Default: 50.
#' @param verbose Logical. Print progress. Default: FALSE.
#'
#' @return List with:
#'   \item{result}{Best result found (last "lower"-classified or "success" result), or NULL.}
#'   \item{converged}{TRUE if a "lower" or "success" result was ever found.}
#'   \item{low}{Lower bracket bound (last "higher" value), or NULL.}
#'   \item{high}{Upper bracket bound (last "lower" value), or NULL.}
#'   \item{attempts}{Total probes made.}
#'
#' @keywords internal
bidirectional_exp_binary_search <- function(
  probe_fn, classify_fn,
  start, val_min = 0, val_max = Inf,
  binary_tolerance = 0.01,
  max_attempts = 50,
  verbose = FALSE
) {
  attempts <- 0L
  best_result <- NULL    # best "lower" or "success" result seen
  c_last_lower <- NULL   # last value classified "lower"
  c_last_higher <- NULL  # last value classified "higher"
  val_max_eff <- min(val_max, 1e6)

  .probe <- function(val) {
    attempts <<- attempts + 1L
    if (verbose) cat(sprintf("  Attempt %d: value = %.6g\n", attempts, val))
    r <- probe_fn(val)
    if (is.null(r)) {
      if (verbose) cat("    -> NULL (treated as 'higher')\n")
      c_last_higher <<- val
      return("higher")
    }
    cls <- classify_fn(r)
    if (verbose) cat(sprintf("    -> %s\n", cls))
    if (cls %in% c("lower", "success")) {
      best_result <<- r
      c_last_lower <<- val
    } else {
      c_last_higher <<- val
    }
    cls
  }

  # Phase 1: probe start
  if (verbose) cat(sprintf("--- Phase 1: Initial probe at %.6g ---\n", start))
  cls_start <- .probe(start)

  if (cls_start == "success") {
    return(list(result = best_result, converged = TRUE,
                low = c_last_higher, high = c_last_lower, attempts = attempts))
  }

  # Phase 2: directional exponential search
  if (cls_start == "lower") {
    # Start was "lower" → search downward for smaller working value
    if (verbose) cat("--- Phase 2: Downward exponential search ---\n")
    current <- start / 2
    while (current >= val_min && attempts < max_attempts) {
      cls <- .probe(current)
      if (cls == "success") {
        return(list(result = best_result, converged = TRUE,
                    low = c_last_higher, high = c_last_lower, attempts = attempts))
      }
      if (cls == "higher") break  # bracketed: [current, last_lower]
      current <- current / 2
    }
  } else {
    # Start was "higher" → search upward for any working value
    if (verbose) cat("--- Phase 2: Upward exponential search ---\n")
    current <- start * 2
    while (current <= val_max_eff && attempts < max_attempts) {
      cls <- .probe(current)
      if (cls == "success") {
        return(list(result = best_result, converged = TRUE,
                    low = c_last_higher, high = c_last_lower, attempts = attempts))
      }
      if (cls == "lower") break  # bracketed: [last_higher, current]
      current <- current * 2
    }
  }

  # Check if we have a bracket for binary refinement
  if (is.null(c_last_lower)) {
    # Never found a "lower" result
    return(list(result = NULL, converged = FALSE,
                low = c_last_higher, high = NULL, attempts = attempts))
  }

  # Phase 3: binary refinement
  c_low  <- c_last_higher %||% val_min
  c_high <- c_last_lower
  if (verbose) cat(sprintf("--- Phase 3: Binary refinement [%.6g, %.6g] ---\n", c_low, c_high))

  while ((c_high - c_low) > binary_tolerance && attempts < max_attempts) {
    c_mid <- (c_low + c_high) / 2
    cls <- .probe(c_mid)
    if (cls == "success") {
      return(list(result = best_result, converged = TRUE,
                  low = c_low, high = c_high, attempts = attempts))
    }
    if (cls == "lower") {
      c_high <- c_mid
    } else {
      c_low <- c_mid
    }
  }

  if (verbose) cat(sprintf("Binary refinement converged: value ~ %.6g\n", c_high))
  list(result = best_result, converged = TRUE,
       low = c_low, high = c_high, attempts = attempts)
}
