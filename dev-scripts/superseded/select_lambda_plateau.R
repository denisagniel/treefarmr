#' Select a Stage-A regularization penalty by locating the widest
#' leaf-count plateau along a log-spaced lambda grid
#'
#' @description
#' `bisect_lambda_to_budget()` (and, through it, [fit_twostage()]) treats the
#' analyst's own `lambda_n` as a starting point and only searches *upward*
#' from it, as a finite-sample fallback for the case where that first try
#' overshoots the leaf budget (see its own roxygen). It does not search
#' *downward*, and it provides no guidance at all on what `lambda_n` to
#' start from -- an analyst who picks a `lambda_n` that undershoots (returns
#' fewer leaves than the truth needs) gets that undershot fit back silently,
#' with no error, no warning, and no correction (confirmed empirically
#' 2026-09-01: `lambda_case == "ii"`, `lambda_binding == FALSE`,
#' `gap == 0` -- the fit is *certified* to solve the theoretical selector
#' exactly at the analyst's own too-large `lambda_n`; nothing about that
#' outcome looks wrong from inside `bisect_lambda_to_budget()`'s own
#' bookkeeping).
#'
#' This function exists to fill that gap on the SELECTION side, before any
#' call to [bisect_lambda_to_budget()]/[fit_twostage()]: it sweeps a
#' log-spaced grid of candidate regularization values, fits an *unconstrained*
#' (no leaf-budget ceiling) tree at each one via [fit_tree()] directly, and
#' reports the leaf count achieved. Under the sandwich condition
#' \eqn{\nu_n \ll \lambda_n \ll \mu_n} that `prop:parsimony-grid`/
#' `prop:boundary-recovery` (`optimaltrees/inst/paper/main.tex`) require for
#' topology recovery, the TRUE leaf count occupies a genuine range of
#' \eqn{\lambda} values (a "plateau") of positive width in \eqn{\log\lambda}
#' -- picking any \eqn{\lambda_n} strictly inside that plateau, rather than
#' at an arbitrary fixed value like the package-wide default `0.1`, is the
#' finite-sample analogue of satisfying the sandwich. Returns the midpoint
#' (in \eqn{\log\lambda}) of the WIDEST such plateau across the whole grid --
#' not necessarily the plateau matching any particular target leaf count,
#' since the point of this function is to let the *data* reveal which leaf
#' count is stable, not to assume the analyst already knows it.
#'
#' @details
#' Per the 2026-09-01 Oracle-consulted diagnosis
#' (`quality_reports/plans/2026-09-01_two-paper-split.md`), the package's
#' existing [cv_regularization()]/[cv_regularization_adaptive()] select
#' \eqn{\lambda} to minimise held-out PREDICTION risk, which is a different
#' object from the topology-recovery-optimal \eqn{\lambda} this function
#' targets -- prediction-optimal \eqn{\lambda} sits near the classical
#' \eqn{\sigma^2\log n/n} scale, roughly an order of magnitude below the
#' noise floor a topology-recovery selector needs to clear. Do not use
#' [cv_regularization()] as a substitute for this function for a two-stage
#' fit; its objective is genuinely different, not merely an older version
#' of the same thing.
#'
#' The default grid anchors its lower end at \eqn{\log(n)/n} (this quantity
#' IS still the right *scale* for the lower end, per the same consult --
#' only the selection *criterion* built on top of it needed to change, not
#' the anchor itself) and its upper end at `lambda_max` (default `1`,
#' comfortably above the package-wide `fit_tree()`/`fit_twostage()` default
#' of `0.1`).
#'
#' @param X A data.frame or matrix of covariates (continuous or binary;
#'   passed straight to [fit_tree()], which handles discretization
#'   internally).
#' @param y Numeric response vector (squared-error loss only, matching
#'   [fit_twostage()]'s own v1 scope).
#' @param discretize_bins,max_depth Forwarded to every [fit_tree()] call in
#'   the sweep; should match whatever Stage-A configuration the eventual
#'   [fit_twostage()]/[bisect_lambda_to_budget()] call will use, or the
#'   selected \eqn{\lambda} will not correspond to the grid it is meant for.
#'   `max_depth` defaults to `0L` (unlimited), matching [fit_tree()]'s own
#'   convention -- `NULL` is rejected by the underlying solver with an
#'   unhelpful low-level error, so this function never forwards `NULL`.
#' @param lambda_grid Optional numeric vector of candidate \eqn{\lambda}
#'   values, ascending. If `NULL` (default), a log-spaced grid of
#'   `n_grid` points from `log(n)/n` to `lambda_max` is used.
#' @param n_grid Integer, number of grid points when `lambda_grid` is not
#'   supplied. Default `15L`.
#' @param lambda_max Numeric, upper end of the default grid. Default `1`.
#' @param verbose Logical, print progress per grid point. Default `FALSE`.
#' @param ... Additional arguments forwarded to [fit_tree()] (e.g.
#'   `worker_limit`, which must remain `1` per this project's
#'   memory-safety rules for sequential replication runs).
#'
#' @return A list with:
#'   \item{lambda_selected}{The selected \eqn{\lambda} (log-midpoint of the
#'     widest plateau).}
#'   \item{n_leaves_selected}{The leaf count of the winning plateau.}
#'   \item{plateau_log_width}{The winning plateau's width in
#'     \eqn{\log\lambda} -- report this alongside `lambda_selected`; a
#'     narrow winning plateau (of the ones available, this was still the
#'     widest) is itself informative that no \eqn{\lambda} choice is very
#'     robust at this `(n, p, discretize_bins)` configuration, which is
#'     worth surfacing rather than silently accepting.}
#'   \item{grid}{A data.frame with one row per grid point:
#'     `lambda`, `n_leaves`.}
#'   \item{plateaus}{A data.frame with one row per detected plateau:
#'     `n_leaves`, `lambda_lo`, `lambda_hi`, `log_width`, `n_points`,
#'     ordered by `log_width` descending (row 1 is the selected plateau).}
#' @export
select_lambda_plateau <- function(X, y, discretize_bins, max_depth = 0L,
                                   lambda_grid = NULL, n_grid = 15L,
                                   lambda_max = 1, verbose = FALSE, ...) {
  if (is.null(max_depth)) max_depth <- 0L
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("select_lambda_plateau: {.arg X} must be a data.frame or matrix.")
  }
  if (nrow(X) != length(y)) {
    cli::cli_abort("select_lambda_plateau: nrow(X) must equal length(y).")
  }
  n <- nrow(X)

  if (is.null(lambda_grid)) {
    lambda_lo <- log(n) / n
    if (!(lambda_lo < lambda_max)) {
      cli::cli_abort(c(
        "select_lambda_plateau: default lower grid end log(n)/n = {signif(lambda_lo, 3)} is not below {.arg lambda_max} = {lambda_max}.",
        "i" = "Supply an explicit {.arg lambda_grid}, or raise {.arg lambda_max}."
      ))
    }
    lambda_grid <- exp(seq(log(lambda_lo), log(lambda_max), length.out = n_grid))
  } else {
    lambda_grid <- sort(unique(lambda_grid))
    if (length(lambda_grid) < 2L) {
      cli::cli_abort("select_lambda_plateau: {.arg lambda_grid} must have at least 2 distinct values.")
    }
  }

  n_leaves <- integer(length(lambda_grid))
  for (i in seq_along(lambda_grid)) {
    fit_i <- fit_tree(
      X, y, loss_function = "squared_error", regularization = lambda_grid[i],
      discretize_bins = discretize_bins, max_depth = max_depth,
      worker_limit = 1L, verbose = FALSE, ...
    )
    n_leaves[i] <- count_tree_leaves(fit_i)
    if (isTRUE(verbose)) {
      cli::cli_inform("select_lambda_plateau: lambda = {signif(lambda_grid[i], 4)} -> {n_leaves[i]} leaves")
    }
  }

  grid <- data.frame(lambda = lambda_grid, n_leaves = n_leaves)

  # Identify maximal runs of equal n_leaves along the (ascending) grid --
  # each run is one plateau. log-width is measured between the OUTERMOST
  # grid points of the run, which understates the true plateau (the real
  # boundary lies somewhere between this run's edge and its neighbour's),
  # but is the only thing directly observable from a finite grid, and is
  # conservative (never overstates robustness).
  run_id <- cumsum(c(TRUE, diff(n_leaves) != 0L))
  plateaus <- do.call(rbind, lapply(split(seq_along(lambda_grid), run_id), function(idx) {
    data.frame(
      n_leaves = n_leaves[idx[1]],
      lambda_lo = lambda_grid[idx[1]],
      lambda_hi = lambda_grid[idx[length(idx)]],
      log_width = log(lambda_grid[idx[length(idx)]]) - log(lambda_grid[idx[1]]),
      n_points = length(idx)
    )
  }))
  plateaus <- plateaus[order(-plateaus$log_width), , drop = FALSE]
  rownames(plateaus) <- NULL

  winner <- plateaus[1L, ]
  lambda_selected <- exp((log(winner$lambda_lo) + log(winner$lambda_hi)) / 2)
  # A single-point "plateau" (log_width == 0, an isolated leaf count that no
  # neighbouring grid point shares) has no interior to take a midpoint of --
  # the midpoint formula above degenerates correctly to that one point, but
  # flag it explicitly rather than let a width-0 "plateau" look like a real
  # answer.
  if (isTRUE(winner$log_width == 0) && nrow(plateaus) > 1L) {
    cli::cli_warn(c(
      "select_lambda_plateau: every leaf count on this grid occurs at only one point.",
      "i" = "No lambda choice here is robust to a small perturbation; refine the grid (larger {.arg n_grid}, or a narrower {.arg lambda_grid} range) before trusting {.field lambda_selected}."
    ))
  }

  list(
    lambda_selected = lambda_selected,
    n_leaves_selected = winner$n_leaves,
    plateau_log_width = winner$log_width,
    grid = grid,
    plateaus = plateaus
  )
}
