#' The two-stage tree estimator (Milestone E)
#'
#' @description
#' This file implements `fit_twostage()`, the public orchestrating function
#' for the "two-stage" tree estimator: Stage A (discrete grid search) then
#' Stage B (off-grid threshold refinement), escalated across an `m`-ladder
#' of grid resolutions with a local-optimality certificate and a real
#' wall-clock cost guard. See [fit_twostage()] itself for the full
#' argument/return contract, and
#' `quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md`
#' §4 in the `global-scholars` project (Oracle-consulted; every design
#' decision below was verified empirically before being accepted) for the
#' complete theory and design history.
#'
#' Built across five sub-steps (E0/E0b: wall-clock guard + classed
#' conditions on `bisect_lambda_to_budget()`/`stage_b.R`; E1: pure budget
#' resolution/ladder validation; E2: the single-rung runner; E3: the ladder
#' loop and result assembly, [fit_twostage()] itself -- E4, print/summary
#' methods, is the only piece not yet built).
#'
#' @keywords internal
#' @name fit_twostage_internals
NULL

#' Resolve `fit_twostage()`'s per-call Stage-A budgets, before any fitting
#'
#' @description
#' \strong{2026-09-04 architecture revision.} Earlier versions of this
#' function applied a "transition-leaf-inflated" working-budget formula
#' (`L_A = min(2*leaf_budget-1, |A_m|)`, `d_A = min(2*d_0, depth_required_A)`)
#' derived from the paper's then-current Stage-A/B design, in which Stage A
#' searched an inflated class and a `collapse_transitions()` post-processing
#' step merged the resulting "transition leaf" artifacts back down. That
#' design's headline topology-recovery result was found to be FALSE (the
#' collapse-map's exclusion argument fails generically, not at an edge
#' case) and was replaced in the theory (`inst/paper/main.tex`, Algorithm 1
#' / `alg:twostage`): Stage A now fits DIRECTLY under the analyst's TRUE
#' leaf/depth budget, with NO inflation and no collapse step ("no collapse,
#' no post-processing" -- main.tex's own Step 3 comment), under a REVERSED
#' penalty regime (`lambda_n >> 1/r_n`, enforced by the caller via
#' `lambda_n`/`m_ladder`, not by this function) that accepts `O(1/r_n)`
#' threshold-localization error at each node instead of trying to exactly
#' represent a grid-misaligned boundary. This function now simply validates
#' and passes the declared budgets through, with the same depth-cap
#' three-valued contract as before -- it is kept as the single validation
#' point for budgets, not because inflation logic remains.
#'
#' `depth_budget`'s three-valued contract mirrors this package's existing
#' `discretize_bins` string-sentinel convention (numeric, OR
#' `"adaptive"`/`"log"`/`"cv"`; see [fit_tree()]):
#' \itemize{
#'   \item `NULL` (default): the STAGED cap,
#'     \eqn{d_0 = \lceil \log_2(\bar L) \rceil}.
#'   \item `"full"`: full compliance, `d_A = depth_required_A` (the
#'     worst-case chain-shaped topology at the declared leaf budget).
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
#'   `p >= 2, m >= 16` this bound never actually binds in practice.
#' @return A list: `leaf_budget` (echoed, coerced to integer), `L_A`
#'   (now simply `min(leaf_budget, A_m)` -- the declared budget, clamped
#'   only by the trivial `A_m` safety bound), `depth_required_A`,
#'   `depth_reachable_A`, `d_0` (`NA_integer_` when `d_0_source = "full"`,
#'   since full compliance has no separate `d_0`), `d_0_source` (one of
#'   `"default_balanced"`, `"full"`, `"user"`), `d_A` (now simply the
#'   resolved `d_0`, clamped by `depth_required_A`/`depth_reachable_A`),
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

  # No inflation (2026-09-04 revision): Stage A now fits at the declared
  # true leaf budget directly.
  L_A <- max(min(leaf_budget, A_m), 1L)
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

  # No inflation (2026-09-04 revision): d_A is simply d_0, clamped by the
  # structural bounds below -- not 2*d_0.
  d_A <- if (identical(d_0_source, "full")) {
    depth_required_A
  } else {
    min(d_0, depth_required_A)
  }
  # Load-bearing clamps: bisect_lambda_to_budget() errors unconditionally if
  # max_depth is below ceiling(log2(leaf_budget)) (mechanically unreachable,
  # regardless of depth_restricted), and rejects max_depth = 0L outright.
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
    "!" = "fit_twostage: depth_budget = NULL defaults to the STAGED cap d_0 = ceiling(log2({res$leaf_budget})) = {res$d_0}, giving Stage-A search depth d_A = {res$d_A} (leaf budget L_A = {res$L_A}).",
    "i" = "This EXCLUDES deeper, chain-shaped topologies: full compliance at this leaf budget needs depth {res$depth_required_A}. `depth_sufficient` will be FALSE and `certified_full_class` cannot be TRUE.",
    "i" = "Pass an explicit integer `depth_budget` if you have a view on the true tree's depth, or `depth_budget = \"full\"` for the full-compliance search."
  ))
  invisible(NULL)
}

#' Validate an `m`-ladder's basic structural requirements
#'
#' @description
#' Pure: enforces the structural requirements `fit_twostage()`'s ladder loop
#' depends on (at least 2 distinct rungs -- `topology_stable` requires
#' comparing two completed rungs, so a 1-rung ladder can never be certified;
#' every rung `>= 2`, since [fit_tree()]'s own `discretize_bins` validation
#' requires that).
#'
#' \strong{2026-09-04 architecture revision.} This function previously also
#' pruned any rung the sample could not support under the (now-removed)
#' `prop:parsimony-grid`'s rate condition, `m_n = o(n/r_n)`. That result was
#' specific to the old inflated-budget/collapse-map design and does not
#' carry over to the revised architecture; the new theory's rate condition
#' on the penalty (`lambda_n >> 1/r_n`, Theorem [Stage-A topology recovery])
#' does not, by itself, imply any particular grid-resolution ceiling as a
#' function of `n`. Rather than guess a replacement rate condition the
#' package's own theory does not supply, this function no longer prunes the
#' ladder at all -- callers are responsible for choosing a sample-
#' appropriate `m_ladder`. (This is an explicitly open question, tracked in
#' the paper's own Discussion outline as "the m-ladder / depth-budget
#' question raised on the package side.") `dropped_rungs`/`m_max_supported`
#' are kept in the return value, always vacuous, for call-site stability.
#'
#' @param m_ladder Numeric vector of candidate bin counts.
#' @param n Single positive integer/numeric, `nrow(X)`.
#' @param m_n Single positive integer/numeric, the total per-leaf floor.
#' @return A list: `m_ladder` (sorted, deduplicated), `dropped_rungs`
#'   (always `integer(0)`, kept for call-site stability), `m_max_supported`
#'   (always `NA_integer_`, kept for call-site stability).
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

  list(m_ladder = m_ladder, dropped_rungs = integer(0), m_max_supported = NA_integer_)
}

# ---------------------------------------------------------------------------
# Sub-step E2: single-rung runner + group-aware refined-feasibility check.
# Everything below computes facts about ONE m-ladder rung in isolation --
# Stage A, Stage B, the refined-leaf floor re-check, and the local
# certificate. Cross-rung comparison (compare_topology() against the
# PREVIOUS rung), the ladder's stop conditions, and final result assembly
# (which rung's model wins, certified/certified_full_class) are E3's job,
# not this one's -- they need state spanning multiple rungs, which this
# function deliberately does not carry.
# ---------------------------------------------------------------------------

#' Refined-tree leaf-size feasibility, group-aware (Oracle consult, Q5.3)
#'
#' @description
#' `bisect_lambda_to_budget()`'s own `feasible` field describes the STAGE-A
#' tree against the discretized design -- it says nothing about the
#' RETURNED (refined) tree, because Stage B moves cuts and therefore changes
#' leaf membership and leaf sizes. Reusing Stage A's `feasible` for a field
#' describing the object `fit_twostage()` actually returns would be a lie.
#'
#' This closes a second, independent gap Oracle's consult flagged as
#' previously unflagged anywhere in the plan: [refine_tree_cuts()]'s
#' `min_leaf_n` floor has NO `group`/`m_n_group` awareness at all -- only
#' the total floor is enforced during refinement. For `doubletree`'s
#' propensity-tree use case (the entire reason the group floor exists --
#' see [check_leaf_feasibility()]), refinement can therefore genuinely
#' break the group floor even when the total floor holds. This function
#' re-derives leaf assignments from the REFINED tree and checks both floors
#' directly, rather than trusting any upstream field.
#'
#' @param refined A [RefinedTreeModel].
#' @param X Data.frame or matrix with the coordinates `refined` splits on.
#' @param group,group_value,m_n,m_n_group Same semantics as
#'   [check_leaf_feasibility()]/[bisect_lambda_to_budget()].
#' @return A single logical.
#' @keywords internal
.refined_leaf_floor_ok <- function(refined, X, group, group_value, m_n, m_n_group) {
  ids <- leaf_assignments(refined, X)
  n_tab <- table(ids)
  ok <- all(n_tab >= m_n)
  if (!is.null(group)) {
    g <- table(factor(ids[group == group_value], levels = names(n_tab)))
    ok <- ok && all(as.integer(g) >= m_n_group)
  }
  ok
}

#' Run one `m`-ladder rung: Stage A -> Stage B -> feasibility -> certificate
#'
#' @description
#' Computes everything `fit_twostage()`'s ladder needs about a SINGLE grid
#' resolution `m`, in isolation from every other rung. Never throws for the
#' ORDINARY refusal outcome this composition can hit (a Stage-A deadline
#' exhaustion) -- reported via `status`/`reason`, since a caller escalating
#' the ladder needs to distinguish it from a genuine bug.
#'
#' \strong{2026-09-04 architecture revision.} [refine_tree()] is called with
#' its (now default-off) `collapse` argument left at `FALSE`: the revised
#' theory's Algorithm 1, Step 3 needs no collapse step at all ("no collapse,
#' no post-processing" -- because Stage A now fits directly at the true
#' budget, there are no transition-leaf artifacts to remove). The
#' `optimaltrees_collapse_unsupported` refusal this function previously
#' caught and turned into a `status = "collapse_unsupported"` rung outcome
#' can therefore no longer occur on this call path, and that handling has
#' been removed. `r_n = m` (this rung's own grid resolution) and `M_n` are
#' threaded to [refine_tree()] so Stage B's search interval is sized to the
#' theory's `rho_n = M_n/r_n` radius (`prop:search-interval`) rather than
#' being unbounded within the identifying bracket.
#'
#' `max_leaves` is deliberately `NULL` on the [refine_tree()] call (never
#' `L_A`) -- Stage A has no native leaf-count cap (`bisect_lambda_to_budget()`
#' exists precisely because one was rejected on cache-fragmentation grounds),
#' so `L_A` is a target hit by tuning lambda, not a class boundary. Capping
#' `max_leaves` here would silently let [certify_local_optimality()]'s
#' `add` perturbation ignore a genuinely objective-improving split that WAS
#' in Stage A's search space and lost on the penalized objective -- double-
#' counting what lambda already prices in.
#'
#' The local certificate is evaluated TWICE: once at the fit's own
#' (possibly bisected) `lambda` -- this is what gates `certified` -- and
#' once at the analyst's declared `lambda_n`, reported only. `refine_tree()`
#' records `lambda = res_A$fit@regularization`, which in case (i) (bisection
#' raised lambda above `lambda_n`) is LARGER than `lambda_n`, making
#' `certified` at that lambda a WEAKER claim than at `lambda_n` (harder for
#' a perturbation to look improving against a bigger per-leaf penalty) --
#' an honest artifact of certifying the objective Stage A actually
#' minimized, invisible unless both are reported.
#'
#' @param X,y Training data.
#' @param m Grid resolution (`discretize_bins`) for this rung; also used as
#'   the theory's `r_n` for Stage-B's search-interval radius.
#' @param leaf_budget The ANALYST's declared leaf budget (for `budget_slack`
#'   only -- Stage A itself is run against `L_A`, not this value).
#' @param L_A,d_A,depth_restricted_A The already-resolved (once, outside the
#'   ladder loop -- they do not depend on `m`) working budgets from
#'   [.twostage_resolve_budgets()].
#' @param M_n Numeric, or `NULL` (default). `NULL` resolves HERE to
#'   `sqrt(m)`, mirroring [fit_twostage()]'s own default so direct callers
#'   of this internal function need not pass it; see that function's `M_n`
#'   argument for the rationale and the theory's `M_n -> Inf`,
#'   `M_n/r_n -> 0` requirements.
#' @param lambda_n,m_n,group,group_value,m_n_group,min_leaf_n Forwarded to
#'   [bisect_lambda_to_budget()]/[refine_tree()]; see their own docs.
#' @param fit_time_limit,deadline,tol_iter Forwarded to
#'   [bisect_lambda_to_budget()]; see its own docs for `fit_time_limit`/
#'   `deadline`.
#' @param ... Additional arguments forwarded to [bisect_lambda_to_budget()]
#'   (and, through it, to [fit_tree()]).
#' @return A list (one record for `fit_twostage()`'s `ladder_topologies`
#'   table): `m`, `status` (`"ok"`, `"stage_a_deadline"`,
#'   `"stage_b_binary_split"`, or `"stage_b_refine_infeasible"` -- only
#'   `"ok"` populates the remaining fields
#'   beyond this and `n_leaves_A`/`n_fits`/`stage_a_truncated`, which are set
#'   whenever Stage A itself completed), `n_leaves_A`, `lambda`,
#'   `lambda_case` (one of `"i"`, `"ii"`, `"iii"`, `"infeasible"`), `gap`,
#'   `feasible_A`, `n_fits`, `stage_a_truncated`, `lambda_binding`
#'   (`n_leaves_A >= L_A`, i.e. whether the leaf budget actually bound),
#'   `n_collapses` (always `0` -- collapse is off by default under the
#'   revised architecture; kept for schema stability), `n_leaves_collapsed`
#'   (the refined tree's leaf count -- Stage B never changes tree
#'   structure, only threshold values, so this equals `n_leaves_A`),
#'   `budget_slack` (`leaf_budget - n_leaves_collapsed`), `feasible_refined`
#'   (group-aware, on the REFINED tree -- see [.refined_leaf_floor_ok()]),
#'   `local_certified`, `local_margin`, `local_certified_at_lambda_n`,
#'   `local_margin_at_lambda_n`, `depth_sufficient` (echoed from Stage A),
#'   `topology_key` (from [canonical_partition()], for E3's cross-rung
#'   comparison), `model` (the [RefinedTreeModel], or `NULL` unless
#'   `status == "ok"`).
#' @keywords internal
.twostage_run_rung <- function(X, y, m, leaf_budget, L_A, d_A, depth_restricted_A,
                                lambda_n, m_n, min_leaf_n,
                                M_n = NULL, group = NULL, group_value = 0,
                                m_n_group = m_n,
                                fit_time_limit = NULL, deadline = NULL,
                                tol_iter = 12L, ...) {
  rec <- list(
    m = as.integer(m), status = NA_character_,
    n_leaves_A = NA_integer_, lambda = NA_real_, lambda_case = NA_character_,
    gap = NA_real_, feasible_A = NA, n_fits = NA_integer_,
    stage_a_truncated = NA, lambda_binding = NA,
    n_collapses = NA_integer_, n_leaves_collapsed = NA_integer_,
    budget_slack = NA_integer_, feasible_refined = NA,
    local_certified = NA, local_margin = NA_real_,
    local_certified_at_lambda_n = NA, local_margin_at_lambda_n = NA_real_,
    depth_sufficient = NA, topology_key = NA_character_, model = NULL
  )

  # -- 1. STAGE A. A deadline abort is an ORDINARY outcome here (the ladder
  #    decides what to do about it -- E3), not a bug to propagate raw. --------
  res_A <- tryCatch(
    bisect_lambda_to_budget(
      X, y, leaf_budget = L_A, lambda_n = lambda_n,
      max_depth = d_A, depth_restricted = depth_restricted_A,
      loss_function = "squared_error",
      m_n = m_n, group = group, group_value = group_value, m_n_group = m_n_group,
      tol_iter = tol_iter, fit_time_limit = fit_time_limit, deadline = deadline,
      discretize_bins = m, ...
    ),
    optimaltrees_deadline_exceeded = function(c) {
      structure(list(msg = conditionMessage(c)), class = "twostage_stage_a_deadline")
    }
  )
  if (inherits(res_A, "twostage_stage_a_deadline")) {
    rec$status <- "stage_a_deadline"
    return(rec)
  }

  rec$n_leaves_A <- res_A$n_leaves
  rec$lambda <- res_A$lambda
  rec$gap <- res_A$gap
  rec$feasible_A <- res_A$feasible
  rec$n_fits <- res_A$n_fits
  rec$stage_a_truncated <- res_A$any_truncated
  rec$lambda_binding <- res_A$n_leaves >= L_A
  rec$depth_sufficient <- res_A$depth_sufficient
  rec$lambda_case <- if (!isTRUE(res_A$feasible) || res_A$n_leaves > L_A) {
    "infeasible"
  } else if (isTRUE(res_A$certified)) {
    if (isTRUE(all.equal(res_A$lambda, lambda_n))) "ii" else "i"
  } else if (is.finite(res_A$gap)) {
    "iii"
  } else {
    "infeasible"
  }

  # -- 2. STAGE B. A binary-split refusal, or a refine-infeasible refusal
  #    (an ancestor's off-grid refinement shrank a descendant's row set
  #    below what its own split needs -- see refine_tree_cuts()'s roxygen;
  #    reachable now that collapse is off by default), is the ORDINARY case
  #    -- caught by CLASS (E0b), not by string-matching, so an unrelated
  #    real bug still propagates raw. `collapse` left at its default
  #    `FALSE`: see this function's roxygen. `M_n` defaults to `sqrt(m)`
  #    when not supplied (mirrors fit_twostage()'s own default; resolved
  #    here too so direct callers of this internal function need not pass
  #    it). ---------------------------------------------------------------
  M_n_resolved <- if (is.null(M_n)) sqrt(m) else M_n
  refined <- tryCatch(
    refine_tree(res_A$fit, X, y, min_leaf_n = min_leaf_n, tree_index = 1L,
                max_depth = d_A, max_leaves = NULL, r_n = m, M_n = M_n_resolved),
    optimaltrees_stage_b_binary_split = function(c) {
      structure(list(msg = conditionMessage(c)), class = "twostage_binary_split")
    },
    optimaltrees_refine_infeasible = function(c) {
      structure(list(msg = conditionMessage(c)), class = "twostage_refine_infeasible")
    }
  )
  if (inherits(refined, "twostage_binary_split")) {
    rec$status <- "stage_b_binary_split"
    return(rec)
  }
  if (inherits(refined, "twostage_refine_infeasible")) {
    rec$status <- "stage_b_refine_infeasible"
    return(rec)
  }

  rec$n_leaves_collapsed <- n_leaves(refined)
  rec$n_collapses <- if (is.null(refined@refinement_log)) {
    0L
  } else {
    sum(refined@refinement_log$collapsed, na.rm = TRUE)
  }
  rec$budget_slack <- as.integer(leaf_budget) - rec$n_leaves_collapsed

  # -- 3. RE-CHECK feasibility on the REFINED tree, group-aware (Q5.3). -------
  rec$feasible_refined <- .refined_leaf_floor_ok(refined, X, group, group_value,
                                                   m_n, m_n_group)

  # -- 4. LOCAL CERTIFICATE, twice (Q5.2). ------------------------------------
  cert <- certify_local_optimality(refined, X, y)
  rec$local_certified <- cert$certified
  rec$local_margin <- cert$margin
  cert_ln <- certify_local_optimality(refined, X, y, lambda = lambda_n)
  rec$local_certified_at_lambda_n <- cert_ln$certified
  rec$local_margin_at_lambda_n <- cert_ln$margin

  rec$topology_key <- canonical_partition(refined@tree, X)$key
  rec$status <- "ok"
  rec$model <- refined
  rec
}

# ---------------------------------------------------------------------------
# Sub-step E3: the m-ladder loop, stop conditions, cross-rung topology
# comparison, result assembly, and the certified / certified_full_class
# split. This is the public entry point.
# ---------------------------------------------------------------------------

#' Fit a two-stage tree: discrete grid search, then off-grid refinement
#'
#' @description
#' Orchestrates the full two-stage estimator: an escalating `m`-ladder of
#' grid resolutions, each rung running Stage A ([bisect_lambda_to_budget()]
#' at the analyst's DECLARED leaf/depth budget -- no inflation, per the
#' 2026-09-04 architecture revision below), Stage B ([refine_tree()],
#' off-grid threshold refinement within a `rho_n = M_n/r_n`-radius search
#' interval), and the local-optimality certificate
#' ([certify_local_optimality()]); stopping at the first rung whose
#' topology agrees with the previous completed rung's ([compare_topology()]).
#'
#' \strong{2026-09-04 architecture revision.} Earlier versions of this
#' function ran Stage A at an inflated working budget and required a
#' `collapse_transitions()` post-processing step to merge the resulting
#' "transition leaf" artifacts, escalating to the next `m` rung whenever
#' that step refused. The theory's headline topology-recovery result for
#' that design was found to be FALSE and was replaced
#' (`inst/paper/main.tex`, Algorithm 1): Stage A now fits directly at the
#' declared budget under a REVERSED penalty regime (`lambda_n >> 1/r_n`;
#' the analyst controls this via `lambda_n` and `m_ladder`, this function
#' does not enforce it), and Step 3 needs no collapse step at all ("no
#' collapse, no post-processing" -- main.tex's own comment). Accordingly:
#' `collapse_transitions()` is no longer called by this pipeline (it
#' remains available, off by default, directly on [refine_tree()] for
#' other callers); the `"collapse_unsupported"` rung status and its
#' associated ladder escalation have been removed; and Stage B's search
#' interval is now sized to the theory's `rho_n = M_n/r_n` radius (see the
#' `M_n` argument) rather than left unbounded within the identifying
#' bracket.
#'
#' \strong{Scope, disclosed}: regression (`loss_function = "squared_error"`)
#' only. \strong{`certified` is LOCAL}: it certifies exactness over the
#' DECLARED problem `(leaf_budget, depth_budget, m_used)`, per the returned
#' fit's own local certificate -- it does NOT assert global compliance over
#' the full continuum class. `certified_full_class` additionally requires
#' `depth_sufficient` -- see the `depth_budget` argument below for why this
#' is `FALSE` on every default call, by design, not a fit failure.
#'
#' \strong{Cost guard, three layers} (see [bisect_lambda_to_budget()]'s
#' `fit_time_limit`/`deadline` for layers 1-2): a per-fit `fit_time_limit`
#' inside the C++ solver; a `deadline` threaded into each rung's Stage-A
#' search (so bisection cannot silently overrun the remaining budget); and
#' `time_budget`, a SHRINKING budget across the whole ladder, checked
#' before every rung starts. A rung that hits ITS OWN deadline, or whose
#' Stage-A search truncated ANY internal fit (see `any_truncated` in
#' [bisect_lambda_to_budget()]'s return value), STOPS the ladder
#' immediately rather than escalating to a finer `m` -- Stage-A cost is
#' monotone increasing in `m` (more discretized features per continuous
#' coordinate), so a rung that could not finish guarantees every later rung
#' cannot either; retrying would be a guaranteed-waste, exactly the
#' "silently commit to hours" failure mode this guard exists to prevent.
#'
#' A Stage-B binary-passthrough-split refusal
#' (`"optimaltrees_stage_b_binary_split"`) escalates only ONCE, then stops
#' -- if the solver keeps splitting on a binary column, a finer grid will
#' not change that, and repeated escalation would be a guaranteed waste.
#'
#' @param X,y Training data. `X` must be numeric throughout (Stage B's
#'   perturbation enumerators loop over every column and compare values
#'   numerically) and have at least one continuous covariate (Stage B has
#'   nothing to refine on an all-binary design -- checked up front, before
#'   any Stage-A time is spent, though the AUTHORITATIVE check happens
#'   per-rung against each fit's own discretization metadata).
#' @param leaf_budget Integer, the analyst's declared leaf budget
#'   (\eqn{\bar L}). Stage A is fit directly at this budget -- no inflation
#'   (see [.twostage_resolve_budgets()]).
#' @param depth_budget `NULL` (default), `"full"`, or a single positive
#'   integer -- see [.twostage_resolve_budgets()] for the exact formula.
#'   `NULL` resolves to the STAGED cap (\eqn{d_0 = \lceil\log_2(\bar
#'   L)\rceil}); on this path `depth_sufficient` is `FALSE` and
#'   `certified_full_class` can never be `TRUE`, by design -- pass an
#'   explicit `depth_budget` (your own belief about the true tree's depth)
#'   or `depth_budget = "full"` (full compliance at the declared leaf
#'   budget) to change this.
#' @param lambda_n Numeric, the analyst's declared regularization -- the
#'   estimand `certified` is stated relative to. Tried first at every rung;
#'   see [bisect_lambda_to_budget()]. The revised theory's Stage-A
#'   topology-recovery guarantee requires `lambda_n >> 1/r_n` (the REVERSE
#'   of a grid-exact analysis' sandwich condition) -- this function does
#'   not enforce or select this itself; choosing `lambda_n` relative to
#'   `m_ladder` is the caller's responsibility (an explicitly open
#'   question on the package side, tracked in the paper's own Discussion
#'   outline; see this file's own `NEWS.md` entry for provenance).
#' @param m_ladder Numeric vector of grid resolutions (`discretize_bins`,
#'   the theory's `r_n`) to escalate through, ascending. Must retain at
#'   least 2 rungs (`topology_stable` needs two completed rungs to
#'   compare); see [.twostage_validate_ladder()] (no longer prunes rungs
#'   against a rate condition -- see that function's docs for why).
#' @param M_n Numeric, or `NULL` (default). Stage-B's interval-scale tuning
#'   constant: the search interval at each node is `[t_hat +/- M_n/r_n]`
#'   intersected with the node's identifying bracket (`r_n` is that rung's
#'   own `m`; Definition [Stage-B search interval], `inst/paper/main.tex`).
#'   The theory requires `M_n -> Inf` with `M_n/r_n -> 0` but does not pin
#'   down a specific sequence. `NULL` defaults to `sqrt(m)` per rung, which
#'   satisfies both limits -- an explicit package default, not a
#'   theory-derived one; pass a numeric value (or a function of `r_n`, via
#'   your own wrapper) to override.
#' @param m_n,group,group_value,m_n_group Leaf-size floor(s); see
#'   [check_leaf_feasibility()]/[bisect_lambda_to_budget()]. `group` is the
#'   propensity-tree control-count use case (`doubletree::estimate_att()`'s
#'   own requirement).
#' @param min_leaf_n Integer or `NULL` (default). Stage-B's own floor,
#'   passed to [refine_tree()]. `NULL` reuses `m_n` (the SAME total floor
#'   Stage A enforces) -- passing a different value is possible but means
#'   Stage A's and Stage B's floors diverge, which the `feasible` field's
#'   re-derivation on the REFINED tree ([.refined_leaf_floor_ok()]) will
#'   correctly reflect either way.
#' @param loss_function Must be `"squared_error"` -- regression-only v1,
#'   disclosed, not silently unsupported. Any other value errors immediately.
#' @param time_budget Numeric, seconds, default `3600`. SHRINKING budget
#'   across the WHOLE ladder call (not per rung) -- see the cost-guard
#'   description above.
#' @param fit_time_limit Numeric, seconds, default `600`. Per-fit cap inside
#'   the C++ solver, forwarded to every [bisect_lambda_to_budget()] call.
#' @param tol_iter Integer, default `12L`. Forwarded to
#'   [bisect_lambda_to_budget()]'s own bisection-iteration cap (its own
#'   default is `40`; `12` keeps a single rung's worst case near 25 fits
#'   rather than 81).
#' @param verbose Logical, default `TRUE`. Controls the depth-cap disclosure
#'   message ([.twostage_emit_depth_message()]) and de-duplicated
#'   high-dimensionality warnings from [fit_tree()] (at most one per rung,
#'   not one per internal fit).
#' @param ... Additional arguments forwarded to every rung's
#'   [bisect_lambda_to_budget()] call (and, through it, to [fit_tree()]).
#'   `worker_limit` other than `1` is rejected outright -- no parallel
#'   execution (this project's own memory-safety rule).
#'
#' @return A list, class `"optimaltrees_twostage_fit"`:
#'   \item{model}{The winning [RefinedTreeModel], or `NULL` if no rung
#'     completed (`predict()` on this object must abort, not silently
#'     return garbage -- see the S3 methods).}
#'   \item{m_used}{The grid resolution `model` came from.}
#'   \item{certified}{`TRUE` iff EVERY one of: a rung completed;
#'     `lambda_case %in% c("i","ii")`; `n_leaves_collapsed <= leaf_budget`;
#'     `feasible` (re-derived on the refined tree, group-aware);
#'     Stage A did not truncate any internal fit; the local certificate
#'     passed (at the fit's own, possibly-bisected lambda); AND topology
#'     agreed across >= 2 completed rungs. Certifies exactness over the
#'     DECLARED problem -- see `certified_full_class` for the additional
#'     depth requirement.}
#'   \item{certified_full_class}{`certified && depth_sufficient`. `FALSE`
#'     on every default `depth_budget = NULL` call, by design -- see that
#'     argument's docs.}
#'   \item{reason}{The single first-failing check name when `!certified`
#'     (one of `any_rung_completed`, `lambda_case`, `budget_respected`,
#'     `feasible`, `stage_a_converged`, `local_certificate`,
#'     `topology_stable`), else `NA`.}
#'   \item{lambda_case,gap,n_leaves_collapsed,max_depth,depth_required,depth_sufficient,feasible,stage_b_local_margin}{
#'     The "guaranteed" field group from the winning rung (or `NA`/`FALSE`
#'     defaults if none completed); see [.twostage_run_rung()].
#'     `n_leaves_collapsed` is now simply the refined tree's leaf count
#'     (collapse is off by default; kept for schema stability).}
#'   \item{budget_slack,lambda_binding}{`leaf_budget - n_leaves_collapsed`
#'     and whether Stage A's leaf count actually bound `leaf_budget` --
#'     diagnostics for "the budget did not meaningfully constrain the fit."}
#'   \item{topology_stable,n_rungs_completed}{Whether topology agreed
#'     across any two adjacent completed rungs, and how many rungs reached
#'     `status == "ok"` at all.}
#'   \item{n_transition_leaves_collapsed}{From the winning rung; always `0`
#'     under the default pipeline (kept for schema stability).}
#'   \item{stop_reason}{Why the ladder stopped: `"topology_stable"`
#'     (success), `"stage_a_truncated"`, `"stage_a_deadline"`,
#'     `"stage_b_binary_split"`, `"budget_exhausted"`, or
#'     `"ladder_exhausted"`.}
#'   \item{ladder_topologies}{Data.frame, one row per rung ATTEMPTED
#'     (including refused/timed-out ones), for the disclosure print method
#'     and manual inspection.}
#'   \item{leaf_budget,L_A,d_A,depth_restricted_A,d_0,d_0_source,dropped_rungs}{
#'     Echoed budget-resolution/ladder-validation inputs, for full
#'     disclosure. `L_A`/`d_A` are now simply the declared/resolved budgets
#'     (no inflation); `dropped_rungs` is always empty.}
#'   \item{elapsed_secs}{Total wall-clock time across the whole call.}
#' @export
fit_twostage <- function(X, y, leaf_budget,
                          depth_budget = NULL,
                          lambda_n = 0.1,
                          m_ladder = c(16, 32, 64, 128),
                          M_n = NULL,
                          m_n = 1L, group = NULL, group_value = 0, m_n_group = m_n,
                          min_leaf_n = NULL,
                          loss_function = "squared_error",
                          time_budget = 3600, fit_time_limit = 600,
                          tol_iter = 12L,
                          verbose = TRUE,
                          ...) {
  if (!identical(loss_function, "squared_error")) {
    cli::cli_abort(c(
      "fit_twostage: only {.val squared_error} is supported.",
      "i" = "Stage B and the local-optimality certificate are regression-only v1 (Milestones A/B), disclosed as such -- classification is a separate, later milestone."
    ))
  }
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("fit_twostage: {.arg X} must be a data.frame or matrix.")
  }
  if (is.matrix(X)) X <- as.data.frame(X)
  if (nrow(X) != length(y)) {
    cli::cli_abort("fit_twostage: nrow(X) ({nrow(X)}) must equal length(y) ({length(y)}).")
  }
  if (ncol(X) < 1L) {
    cli::cli_abort("fit_twostage: {.arg X} must have at least one covariate.")
  }
  if (!all(vapply(X, is.numeric, logical(1)))) {
    cli::cli_abort("fit_twostage: every column of {.arg X} must be numeric.")
  }
  # Fast, informative pre-flight only -- NOT authoritative. Stage B's real
  # check (bin_lookup()'s "all features are binary" abort) runs per-rung
  # against each fit's own discretization metadata; this just avoids
  # spending any Stage-A time on a design that can never reach Stage B.
  n_unique <- vapply(X, function(col) length(unique(col)), integer(1))
  if (all(n_unique <= 2L)) {
    cli::cli_abort(c(
      "fit_twostage: every column of {.arg X} has <= 2 distinct values (all-binary design).",
      "i" = "Stage B has nothing to refine on an all-binary design -- at least one continuous covariate is required."
    ))
  }
  worker_limit <- list(...)$worker_limit
  if (!is.null(worker_limit) && !identical(as.integer(worker_limit), 1L)) {
    cli::cli_abort("fit_twostage: {.arg worker_limit} must be 1 -- no parallel execution.")
  }
  if (!is.null(M_n) && (!is.numeric(M_n) || length(M_n) != 1L || is.na(M_n) || M_n <= 0)) {
    cli::cli_abort(c(
      "fit_twostage: {.arg M_n} must be {.code NULL} or a single positive number, got {.val {M_n}}.",
      "i" = "Validated here, before any Stage-A time is spent -- refine_tree_cuts() \\
             would otherwise only catch it after the first rung's full fit."
    ))
  }
  # rho_n = M_n/r_n (Stage-B's search-interval radius) is computed in RAW
  # covariate units, and M_n's default (sqrt(r_n)) presumes those units are
  # roughly unit-scale -- the standard nonparametric normalization the
  # theory's own exposition assumes, nowhere enforced by this package. Off
  # that scale the radius silently does the wrong thing in either
  # direction: negligible (every candidate excluded, Stage B becomes a
  # no-op) on a covariate ranging in the hundreds, or vacuous (no
  # restriction at all, silently reverting to the legacy structural-only
  # bracket) on one ranging in the thousandths. Warn rather than guess a
  # rescaling -- rescaling is the caller's job and depends on choices
  # (which quantile, which reference range) this function has no basis for.
  rng <- vapply(X, function(col) diff(range(col, na.rm = TRUE)), numeric(1))
  off_scale <- names(rng)[rng > 10 | rng < 0.1]
  if (isTRUE(verbose) && length(off_scale) > 0L) {
    cli::cli_warn(c(
      "fit_twostage: Stage-B's search radius rho_n = M_n/r_n is computed in RAW covariate units.",
      "i" = "Coordinate(s) {.val {off_scale}} have range far from the unit scale the \\
             default {.arg M_n} = sqrt(r_n) assumes; the radius will be either \\
             negligible or effectively unrestricted for them, not the theory's \\
             intended few-mesh-widths window.",
      "i" = "Rescale {.arg X} to roughly [0, 1] per coordinate, or pass an explicit \\
             {.arg M_n} sized to your own covariate scale."
    ))
  }

  n <- nrow(X)
  budgets <- .twostage_resolve_budgets(leaf_budget, depth_budget, n)
  .twostage_emit_depth_message(budgets, verbose = verbose)

  min_leaf_n <- if (is.null(min_leaf_n)) as.integer(m_n) else as.integer(min_leaf_n)

  ladder_spec <- .twostage_validate_ladder(m_ladder, n, m_n)

  # Configuration is PROCESS-GLOBAL (see bisect_lambda_to_budget()'s
  # fit_time_limit roxygen) -- reset it unconditionally on exit, or this
  # call silently caps every later, unrelated fit_tree() call in this R
  # session (doubletree included).
  on.exit(
    treefarms_configure_cpp(jsonlite::toJSON(list(time_limit = 0L), auto_unbox = TRUE)),
    add = TRUE
  )

  ladder <- list()
  prev_model <- NULL
  prev_m <- NA_integer_
  binary_split_streak <- 0L
  t_start <- Sys.time()
  stop_reason <- NA_character_

  for (m in ladder_spec$m_ladder) {
    elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    remaining <- time_budget - elapsed_total
    if (remaining < fit_time_limit) {
      stop_reason <- "budget_exhausted"
      break
    }
    rung_deadline <- Sys.time() + remaining

    warned_this_rung <- FALSE
    rec <- withCallingHandlers(
      .twostage_run_rung(
        X, y, m = m, leaf_budget = budgets$leaf_budget,
        L_A = budgets$L_A, d_A = budgets$d_A,
        depth_restricted_A = budgets$depth_restricted_A,
        M_n = M_n,
        lambda_n = lambda_n, m_n = m_n, group = group, group_value = group_value,
        m_n_group = m_n_group, min_leaf_n = min_leaf_n,
        fit_time_limit = fit_time_limit, deadline = rung_deadline,
        tol_iter = tol_iter, ...
      ),
      warning = function(w) {
        if (grepl("High-dimensional data detected", conditionMessage(w), fixed = TRUE)) {
          if (!warned_this_rung && isTRUE(verbose)) {
            cli::cli_inform("fit_twostage: rung m = {m} -- {conditionMessage(w)}")
            warned_this_rung <<- TRUE
          }
          invokeRestart("muffleWarning")
        }
      }
    )
    rec$compared_to_m <- NA_integer_
    rec$topology_stable_vs_prev <- NA
    rec$refinement <- NA_character_
    ladder[[length(ladder) + 1L]] <- rec
    idx_this <- length(ladder)

    if (identical(rec$status, "stage_a_deadline")) {
      stop_reason <- "stage_a_deadline"
      break
    }
    if (identical(rec$status, "stage_b_refine_infeasible")) {
      # Geometry-dependent (an ancestor's off-grid refinement shrank a
      # descendant's row set below its own split's floor) -- may not recur
      # at a different grid resolution, so escalate to the next rung rather
      # than stopping outright, the same treatment the OLD architecture
      # gave collapse_transitions()'s refusal. Deliberately does NOT reset
      # binary_split_streak: the binary-split cap's own rationale ("a finer
      # grid will not change that") is grid-independent, so an interleaved
      # refine-infeasible rung must not buy the ladder another
      # binary-split attempt.
      next
    }
    if (identical(rec$status, "stage_b_binary_split")) {
      binary_split_streak <- binary_split_streak + 1L
      if (binary_split_streak >= 2L) {
        stop_reason <- "stage_b_binary_split"
        break
      }
      next
    }
    binary_split_streak <- 0L
    # status == "ok" from here on.
    if (!is.null(prev_model)) {
      cmp <- compare_topology(prev_model, rec$model, X)
      ladder[[idx_this]]$compared_to_m <- prev_m
      ladder[[idx_this]]$topology_stable_vs_prev <- isTRUE(cmp$topology_stable)
      ladder[[idx_this]]$refinement <- cmp$refinement
    }
    if (isTRUE(rec$stage_a_truncated)) {
      stop_reason <- "stage_a_truncated"
      break
    }
    if (isTRUE(ladder[[idx_this]]$topology_stable_vs_prev)) {
      stop_reason <- "topology_stable"
      break
    }
    prev_model <- rec$model
    prev_m <- m
  }
  if (is.na(stop_reason)) stop_reason <- "ladder_exhausted"

  lad_df <- do.call(rbind, lapply(ladder, function(r) {
    as.data.frame(r[setdiff(names(r), "model")], stringsAsFactors = FALSE)
  }))

  ok_idx <- which(vapply(ladder, function(r) identical(r$status, "ok"), logical(1)))
  n_rungs_completed <- length(ok_idx)
  # The LAST successfully completed rung wins -- when topology_stable
  # triggered the stop, the two candidates are observationally equivalent
  # (identical row partitions => identical leaf means => identical
  # predictions/training risk); the finer m is the resolution the
  # stability claim is anchored at. When the ladder ended for another
  # reason, it is simply the best result obtained.
  if (n_rungs_completed == 0L) {
    r <- NULL
    model <- NULL
    m_used <- NA_integer_
  } else {
    i_ret <- max(ok_idx)
    r <- ladder[[i_ret]]
    model <- r$model
    m_used <- r$m
  }

  topology_stable <- n_rungs_completed >= 2L &&
    any(vapply(ladder[ok_idx], function(x) isTRUE(x$topology_stable_vs_prev), logical(1)))

  lambda_case          <- if (is.null(r)) NA_character_ else r$lambda_case
  gap                  <- if (is.null(r)) NA_real_ else r$gap
  n_leaves_collapsed   <- if (is.null(r)) NA_integer_ else r$n_leaves_collapsed
  depth_sufficient     <- if (is.null(r)) FALSE else isTRUE(r$depth_sufficient)
  feasible             <- if (is.null(r)) FALSE else (isTRUE(r$feasible_A) && isTRUE(r$feasible_refined))
  stage_b_local_margin <- if (is.null(r)) NA_real_ else r$local_margin
  local_certified      <- if (is.null(r)) NA else isTRUE(r$local_certified)
  budget_slack         <- if (is.null(r)) NA_integer_ else r$budget_slack
  lambda_binding       <- if (is.null(r)) NA else isTRUE(r$lambda_binding)
  n_transition_leaves_collapsed <- if (is.null(r)) NA_integer_ else r$n_collapses

  checks <- c(
    any_rung_completed = n_rungs_completed > 0L,
    lambda_case        = isTRUE(lambda_case %in% c("i", "ii")),
    budget_respected   = isTRUE(!is.na(n_leaves_collapsed) && n_leaves_collapsed <= budgets$leaf_budget),
    feasible           = isTRUE(feasible),
    stage_a_converged  = if (is.null(r)) FALSE else !isTRUE(r$stage_a_truncated),
    local_certificate  = isTRUE(local_certified),
    topology_stable    = isTRUE(topology_stable) && n_rungs_completed >= 2L
  )
  certified <- all(checks)
  certified_full_class <- certified && depth_sufficient
  reason <- if (certified) NA_character_ else names(checks)[which(!checks)[[1]]]

  structure(
    list(
      model = model, m_used = m_used,
      certified = certified, certified_full_class = certified_full_class, reason = reason,
      lambda_case = lambda_case, gap = gap, n_leaves_collapsed = n_leaves_collapsed,
      max_depth = budgets$d_A, depth_required = budgets$depth_required_A,
      depth_sufficient = depth_sufficient, feasible = feasible,
      stage_b_local_margin = stage_b_local_margin, local_certified = local_certified,
      budget_slack = budget_slack, lambda_binding = lambda_binding,
      topology_stable = topology_stable, n_rungs_completed = n_rungs_completed,
      n_transition_leaves_collapsed = n_transition_leaves_collapsed,
      stop_reason = stop_reason, ladder_topologies = lad_df,
      leaf_budget = budgets$leaf_budget, L_A = budgets$L_A, d_A = budgets$d_A,
      depth_restricted_A = budgets$depth_restricted_A,
      d_0 = budgets$d_0, d_0_source = budgets$d_0_source,
      dropped_rungs = ladder_spec$dropped_rungs,
      elapsed_secs = as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    ),
    class = "optimaltrees_twostage_fit"
  )
}

# ---------------------------------------------------------------------------
# Sub-step E4: print/summary/predict S3 methods. This completes Milestone E.
# ---------------------------------------------------------------------------

#' @keywords internal
.twostage_fmt <- function(x, digits = 3) {
  if (length(x) == 0L || all(is.na(x))) return("NA")
  if (is.logical(x)) return(as.character(x))
  format(x, digits = digits)
}

#' Print a `fit_twostage()` result
#'
#' @description
#' Leads with WHY `certified_full_class` is `FALSE` when it is (per the
#' default depth cap) -- the point Oracle's consult made explicitly: a
#' reader must never come away thinking the default cap represents a fit
#' FAILURE. States every guaranteed check individually (not just the
#' aggregate `certified`), whether `leaf_budget` sits inside the
#' empirically-validated `{4, 8}` staged range (decision #5 -- disclose,
#' never silently gate), and the disclaimer categories `theory.tex`
#' explicitly does NOT let this mechanism assert (`ass:global`, the `(Grid)`
#' condition, `L^cont_j <= leaf_budget`; the local certificate is LOCAL
#' ONLY -- `prop:greedy` is a proven counterexample to it implying global
#' optimality).
#'
#' @param x An `optimaltrees_twostage_fit`, from [fit_twostage()].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.optimaltrees_twostage_fit <- function(x, ...) {
  cat(sprintf("<fit_twostage> leaf_budget = %s, stop_reason = %s\n",
              x$leaf_budget, x$stop_reason))

  if (is.null(x$model)) {
    cat(sprintf("  No rung completed -- model is NULL; predict() on this object will abort.\n"))
    cat(sprintf("  certified = FALSE, reason: %s\n", x$reason))
    cat(sprintf("  Ladder attempted %d rung(s); see $ladder_topologies for what each one hit.\n",
                nrow(x$ladder_topologies)))
    return(invisible(x))
  }

  cat(sprintf("  m_used = %s (rungs completed: %d of %d attempted)\n",
              x$m_used, x$n_rungs_completed, nrow(x$ladder_topologies)))
  cat("\n")
  cat(sprintf("  certified              %-5s\n", x$certified))
  cat(sprintf("  |- lambda_case         %-4s (lambda = %s; gap %s)\n",
              x$lambda_case, .twostage_fmt(x$model@lambda), .twostage_fmt(x$gap)))
  cat(sprintf("  |- leaves              %s <= %s  (%s transition leaves collapsed; slack %s)\n",
              x$n_leaves_collapsed, x$leaf_budget,
              x$n_transition_leaves_collapsed, x$budget_slack))
  cat(sprintf("  |- feasible            %-5s (re-checked on the REFINED tree, group-aware)\n",
              x$feasible))
  cat(sprintf("  |- local certificate   %-4s (margin %s at lambda = %s)\n",
              if (isTRUE(x$local_certified)) "PASS" else "FAIL",
              .twostage_fmt(x$stage_b_local_margin), .twostage_fmt(x$model@lambda)))
  cat(sprintf("  \\- topology            %s\n",
              if (isTRUE(x$topology_stable)) {
                "STABLE (agreed across >= 2 completed rungs)"
              } else {
                sprintf("not established (%d rung(s) completed)", x$n_rungs_completed)
              }))
  if (!is.na(x$reason)) {
    cat(sprintf("\n  certified = FALSE, reason: %s\n", x$reason))
  }

  cat(sprintf("\n  certified_full_class   %-5s (= certified && depth_sufficient)\n",
              x$certified_full_class))
  if (!isTRUE(x$depth_sufficient)) {
    if (identical(x$d_0_source, "default_balanced")) {
      cat("\n  depth_sufficient = FALSE BY DESIGN, not a fit failure.\n")
      cat(sprintf(
        "  depth_budget = NULL used the STAGED cap d_0 = ceiling(log2(%d)) = %d,\n",
        x$leaf_budget, x$d_0
      ))
      cat(sprintf(
        "  so Stage A searched to depth %d. Full compliance at this leaf budget\n",
        x$max_depth
      ))
      cat(sprintf(
        "  needs depth %d (a %d-leaf chain), which is not the\n",
        x$depth_required, x$L_A
      ))
      cat("  computational regime this default is validated for. Deep chain-shaped\n")
      cat("  topologies were therefore EXCLUDED from the search.\n")
      cat("    -> every check above applies to the DECLARED class.\n")
      cat("    -> for the full-class claim, re-run with depth_budget = \"full\".\n")
    } else {
      cat("\n  depth_sufficient = FALSE (an explicit, deliberately restricted depth_budget was used).\n")
    }
  }

  cat("\n")
  if (x$leaf_budget %in% c(4L, 8L)) {
    cat(sprintf("  Leaf budget %d is inside the validated staged range {4, 8}.\n", x$leaf_budget))
  } else if (x$leaf_budget < 4L) {
    # Smaller than the validated range: NOT separately benchmarked, but
    # Stage-A cost is monotone increasing in leaf_budget (via L_A/d_A), so
    # a genuinely smaller budget is expected to be at least as tractable
    # as leaf_budget = 4 -- the "expect materially worse" framing below is
    # specifically about the OTHER direction and would be misleading here.
    cat(sprintf(
      "  Leaf budget %d is smaller than the validated staged range {4, 8} -- not\n",
      x$leaf_budget
    ))
    cat("  separately benchmarked, but Stage-A cost is monotone increasing in\n")
    cat("  leaf_budget, so this is expected to be at least as tractable as leaf_budget = 4.\n")
  } else {
    cat(sprintf(
      "  Leaf budget %d is OUTSIDE the validated staged range {4, 8}. No cost or\n",
      x$leaf_budget
    ))
    cat("  correctness evidence exists in this package above leaf_budget = 8 -- measured\n")
    cat(sprintf(
      "  Stage-A cost grew 64s -> 307s across p = 5 -> 20 AT leaf_budget = 8 alone (d_A = %d\n",
      x$max_depth
    ))
    cat("  here); expect materially worse. `time_budget` is your only guard.\n")
  }

  cat("\n  NOT guaranteed: m_used, topology_stable, budget_slack, lambda_binding.\n")
  cat("  DISCLAIMED: ass:global over the full continuum class, the (Grid) condition,\n")
  cat("  L^cont_j <= leaf_budget. The local certificate is LOCAL ONLY (theory.tex's\n")
  cat("  prop:greedy is a proven counterexample) -- it is NOT global optimality.\n")

  invisible(x)
}

#' Summarize a `fit_twostage()` result: the print output plus the full ladder trace
#'
#' @description
#' [print.optimaltrees_twostage_fit()] plus the complete
#' `$ladder_topologies` data.frame -- every rung attempted, including
#' refused/timed-out ones, with the fields recorded at that rung.
#'
#' @param object An `optimaltrees_twostage_fit`, from [fit_twostage()].
#' @param ... Unused.
#' @return `object`, invisibly.
#' @export
summary.optimaltrees_twostage_fit <- function(object, ...) {
  print(object)
  cat("\nLadder trace:\n")
  print(object$ladder_topologies)
  invisible(object)
}

#' Predict from a `fit_twostage()` result
#'
#' @description
#' Delegates to the winning rung's [RefinedTreeModel] `predict` method.
#' Aborts loudly, rather than returning garbage, when no rung ever
#' completed (`$model` is `NULL`) -- check `$stop_reason` and
#' `$ladder_topologies` for why.
#'
#' @param object An `optimaltrees_twostage_fit`, from [fit_twostage()].
#' @param newdata Data.frame or matrix with the coordinates `object$model`
#'   splits on.
#' @param ... Forwarded to `predict(object$model, newdata, ...)`.
#' @return Numeric vector of predictions.
#' @export
predict.optimaltrees_twostage_fit <- function(object, newdata, ...) {
  if (is.null(object$model)) {
    cli::cli_abort(c(
      "predict.optimaltrees_twostage_fit: no rung completed -- {.arg model} is NULL.",
      "i" = "stop_reason was {.val {object$stop_reason}}; see {.code object$ladder_topologies} for why every rung failed."
    ))
  }
  predict(object$model, newdata, ...)
}
