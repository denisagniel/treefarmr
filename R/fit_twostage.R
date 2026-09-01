#' fit_twostage() -- Milestone E, sub-step E1 (pure budget resolution)
#'
#' @description
#' `fit_twostage()` itself (the public orchestrating function -- the
#' `m`-ladder loop, Stage A/B/certificate composition, `certified`/
#' `certified_full_class`) is not yet built. This file currently holds only
#' the PURE, no-fitting-required pieces of its design (Oracle-consulted; see
#' `quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md`
#' Milestone E addendum in the `global-scholars` project for the full
#' architecture and the empirical findings behind it), per that consult's
#' recommended implementation order (E0/E0b done; this is E1; E2-E4 remain).
#'
#' @keywords internal
#' @name fit_twostage
NULL

#' Resolve `fit_twostage()`'s per-call Stage-A budgets, before any fitting
#'
#' @description
#' Applies `prop:transition-leaves`' inflated working-budget formula
#' (\eqn{L_A = \min(2\bar L - 1, |A_m|)}, plan file §4.3) together with the
#' depth-cap product decision (Oracle-consulted, Milestone E addendum): the
#' cap applies to `d_0` (the analyst's declared belief about the TRUE tree's
#' depth), never to the already-inflated `d_A` -- so the inflation formula
#' (\eqn{d_A = \min(2 d_0, \mathrm{depth\_required}_A)}) keeps its exact
#' theory-derived role regardless of which `d_0` was used, and the default
#' cap lands on the one quantity that is genuinely an assumption about an
#' unobservable population tree, not on the derived working budget.
#'
#' `depth_budget`'s three-valued contract mirrors this package's existing
#' `discretize_bins` string-sentinel convention (numeric, OR
#' `"adaptive"`/`"log"`/`"cv"`; see [fit_tree()]):
#' \itemize{
#'   \item `NULL` (default): the STAGED cap,
#'     \eqn{d_0 = \lceil \log_2(\bar L) \rceil}. This reproduces
#'     Milestone D's two measured, validated benchmark points EXACTLY
#'     (`leaf_budget = 4` -> `d_A = 4`; `leaf_budget = 8` -> `d_A = 6`) and
#'     invents no new number -- it is not full compliance, and
#'     `depth_sufficient` will be `FALSE` on this path for every
#'     `leaf_budget >= 4`. See [.twostage_emit_depth_message()] for the
#'     disclosure text a caller should show on this path.
#'   \item `"full"`: full compliance over the inflated class,
#'     `d_A = depth_required_A` (the worst-case chain-shaped topology).
#'     Cost grows superlinearly in `p` at this setting -- see Milestone D's
#'     benchmark table in the plan file.
#'   \item A single positive integer: the analyst's own declared belief
#'     about the true tree's depth, `d_0`.
#' }
#'
#' @param leaf_budget Single positive integer/numeric: the analyst's
#'   declared leaf budget (\eqn{\bar L}).
#' @param depth_budget `NULL`, `"full"`, or a single positive integer. See
#'   above.
#' @param n Single positive integer/numeric, `nrow(X)`. Used only as a safe,
#'   cheap upper bound on \eqn{|A_m|} (the number of distinct cells any
#'   binned design could produce) so `L_A` stays honest at tiny `n`; for
#'   `p >= 2`, `m >= 16` this bound never actually binds in practice.
#' @return A list: `leaf_budget` (echoed, coerced to integer), `L_A`,
#'   `depth_required_A`, `depth_reachable_A`, `d_0` (`NA_integer_` when
#'   `d_0_source = "full"`, since full compliance has no separate `d_0`),
#'   `d_0_source` (one of `"default_balanced"`, `"full"`, `"user"`), `d_A`,
#'   `depth_restricted_A` (`TRUE` iff `d_A < depth_required_A` -- what
#'   `fit_twostage()` will pass to `bisect_lambda_to_budget()`'s own
#'   `depth_restricted` argument).
#' @keywords internal
.twostage_resolve_budgets <- function(leaf_budget, depth_budget, n) {
  if (!is.numeric(leaf_budget) || length(leaf_budget) != 1L ||
      is.na(leaf_budget) || leaf_budget < 1) {
    cli::cli_abort("fit_twostage: {.arg leaf_budget} must be a single positive integer.")
  }
  leaf_budget <- as.integer(leaf_budget)

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
    cli::cli_abort("fit_twostage: {.arg n} (nrow(X)) must be a single positive integer.")
  }
  n <- as.integer(n)

  # A_m: max leaves any grid tree could have (distinct cells of the binned
  # design). nrow(X) is a safe, cheap, always-available upper bound; never
  # binds for p >= 2, m >= 16 (see roxygen), computed exactly only where a
  # tighter number would matter, which is not this function's job.
  A_m <- n

  L_A <- max(min(2L * leaf_budget - 1L, A_m), 1L)
  depth_required_A <- max(L_A - 1L, 1L)
  depth_reachable_A <- max(as.integer(ceiling(log2(L_A))), 1L)

  d_0 <- NA_integer_
  if (is.null(depth_budget)) {
    d_0 <- max(as.integer(ceiling(log2(leaf_budget))), 1L)
    d_0_source <- "default_balanced"
  } else if (identical(depth_budget, "full")) {
    d_0_source <- "full"
  } else if (is.numeric(depth_budget) && length(depth_budget) == 1L &&
               !is.na(depth_budget) && depth_budget >= 1) {
    d_0 <- max(as.integer(depth_budget), 1L)
    d_0_source <- "user"
  } else {
    cli::cli_abort(
      "fit_twostage: {.arg depth_budget} must be {.code NULL}, {.val full}, or a single positive integer, got {.val {depth_budget}}."
    )
  }

  d_A <- if (identical(d_0_source, "full")) {
    depth_required_A
  } else {
    min(2L * d_0, depth_required_A)
  }
  # Load-bearing clamps (Oracle consult, not in the original plan draft):
  # bisect_lambda_to_budget() errors unconditionally if max_depth is below
  # ceiling(log2(leaf_budget)) (mechanically unreachable, regardless of
  # depth_restricted), and rejects max_depth = 0L outright. Both clamps
  # apply here against L_A/depth_reachable_A -- Stage A's leaf_budget IS
  # L_A, not the analyst's declared leaf_budget.
  d_A <- max(as.integer(d_A), depth_reachable_A, 1L)

  depth_restricted_A <- d_A < depth_required_A

  list(
    leaf_budget = leaf_budget, L_A = L_A,
    depth_required_A = depth_required_A, depth_reachable_A = depth_reachable_A,
    d_0 = d_0, d_0_source = d_0_source,
    d_A = d_A, depth_restricted_A = depth_restricted_A
  )
}

#' Emit the disclosure message when `fit_twostage()`'s depth cap defaulted
#'
#' @description
#' A caller should show this whenever `depth_budget = NULL` resolved to the
#' STAGED cap (`res$d_0_source == "default_balanced"`) -- i.e. every default
#' call, for which `depth_sufficient` (and therefore `certified_full_class`)
#' will be `FALSE` by design, not because of a fit failure. Silent on the
#' `"full"`/`"user"` paths, since those are explicit analyst choices that
#' need no disclosure. Separated from [.twostage_resolve_budgets()] so that
#' function stays a pure computation with no side effects -- easier to test,
#' and lets a caller decide independently whether/when to show this.
#'
#' @param res The list returned by [.twostage_resolve_budgets()].
#' @param verbose Logical, default `TRUE`. `FALSE` suppresses the message
#'   entirely (the disclosure still lives in the returned object's fields
#'   and its print method -- this only controls the interactive nudge).
#' @return `invisible(NULL)`.
#' @keywords internal
.twostage_emit_depth_message <- function(res, verbose = TRUE) {
  if (!isTRUE(verbose) || !identical(res$d_0_source, "default_balanced")) {
    return(invisible(NULL))
  }
  cli::cli_inform(c(
    "!" = "fit_twostage: depth_budget = NULL defaults to the STAGED cap d_0 = ceiling(log2({res$leaf_budget})) = {res$d_0}, giving Stage-A search depth d_A = {res$d_A} (working leaf budget L_A = {res$L_A}).",
    "i" = "This EXCLUDES deeper, chain-shaped topologies: full compliance over the inflated class needs depth {res$depth_required_A}. `depth_sufficient` will be FALSE and `certified_full_class` cannot be TRUE.",
    "i" = "Pass an explicit integer `depth_budget` if you have a view on the true tree's depth, or `depth_budget = \"full\"` for the full-compliance search (measured cost grows superlinearly in p -- see quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md Milestone D in the global-scholars project)."
  ))
  invisible(NULL)
}

#' Validate and prune an `m`-ladder against what the sample can support
#'
#' @description
#' Pure: enforces the structural requirements `fit_twostage()`'s ladder loop
#' depends on (at least 2 distinct rungs -- `topology_stable` requires
#' comparing two completed rungs, so a 1-rung ladder can never be certified;
#' every rung `>= 2`, since [fit_tree()]'s own `discretize_bins` validation
#' requires that), then prunes any rung the sample cannot support under
#' `prop:parsimony-grid`'s rate condition (\eqn{m_n = o(n/r_n)}, the plan
#' file's theory.tex-scan finding) using the SAME total per-leaf floor
#' `fit_twostage()` will pass through to Stage A/B (`m_n` -- not the group
#' floor, which is orthogonal to grid resolution).
#'
#' @param m_ladder Numeric vector of candidate bin counts.
#' @param n Single positive integer/numeric, `nrow(X)`.
#' @param m_n Single positive integer/numeric, the total per-leaf floor.
#' @return A list: `m_ladder` (sorted, deduplicated, pruned to what `n`/`m_n`
#'   support), `dropped_rungs` (the rungs removed for exceeding
#'   `m_max_supported`, possibly `integer(0)`), `m_max_supported`.
#' @keywords internal
.twostage_validate_ladder <- function(m_ladder, n, m_n) {
  if (!is.numeric(m_ladder) || length(m_ladder) == 0L || anyNA(m_ladder)) {
    cli::cli_abort("fit_twostage: {.arg m_ladder} must be a non-empty numeric vector with no missing values.")
  }
  m_ladder <- sort(unique(as.integer(m_ladder)))

  if (any(m_ladder < 2L)) {
    cli::cli_abort("fit_twostage: every {.arg m_ladder} rung must be >= 2 (discretize_bins requires it).")
  }
  if (length(m_ladder) < 2L) {
    cli::cli_abort(c(
      "fit_twostage: {.arg m_ladder} needs at least 2 distinct rungs.",
      "i" = "`topology_stable` requires comparing two completed rungs, so a 1-rung ladder can never be certified."
    ))
  }

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
    cli::cli_abort("fit_twostage: {.arg n} (nrow(X)) must be a single positive integer.")
  }
  n <- as.integer(n)
  if (!is.numeric(m_n) || length(m_n) != 1L || is.na(m_n) || m_n < 1) {
    cli::cli_abort("fit_twostage: {.arg m_n} must be a single positive integer.")
  }
  m_n <- as.integer(m_n)

  m_max_supported <- max(2L, as.integer(floor(n / max(2L * m_n, 2L))))
  dropped_rungs <- m_ladder[m_ladder > m_max_supported]
  kept <- m_ladder[m_ladder <= m_max_supported]

  if (length(kept) < 2L) {
    cli::cli_abort(c(
      "fit_twostage: after pruning rungs the sample cannot support (m_max_supported = {m_max_supported}, from n = {n} and m_n = {m_n}), fewer than 2 rungs remain.",
      "i" = "Lower `m_n`, supply a coarser `m_ladder`, or use more data."
    ))
  }

  list(m_ladder = kept, dropped_rungs = dropped_rungs, m_max_supported = m_max_supported)
}
