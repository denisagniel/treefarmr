#' Assign observations to leaves of a fitted single-tree model
#'
#' @description
#' Traverses the raw tree structure (the same `list(prediction=, feature=,
#' true=, false=)` representation used internally by [fit_tree()] and by
#' [get_fitted_from_tree()] / [get_probabilities_from_tree()]) and returns,
#' for each row of `X`, an integer identifying which leaf it falls into.
#' Mirrors the exact split-evaluation logic those prediction functions use
#' (binary features only: go to the `true` branch iff the feature value is
#' `1`/`TRUE`, else `false`), so leaf membership computed here is always
#' consistent with what the model actually predicts.
#'
#' @param tree_json Tree structure, or an `OptimalTreesModel` (its first tree
#'   is used).
#' @param X Data.frame or matrix with the same binary features used at fit
#'   time.
#' @return Integer vector of length `nrow(X)`, one leaf id per row. Leaf ids
#'   are assigned in traversal order and are not stable across refits or
#'   across different trees; use only within a single call.
#' @keywords internal
assign_leaf_ids <- function(tree_json, X) {
  if (S7::S7_inherits(tree_json, OptimalTreesModel)) {
    if (tree_json@n_trees == 0) {
      stop("assign_leaf_ids: model has no trees.", call. = FALSE)
    }
    tree_json <- tree_json@trees[[1]]
  }
  if (is.null(tree_json$feature) && is.null(tree_json$prediction) &&
      length(tree_json) >= 1 && is.list(tree_json[[1]])) {
    tree_json <- tree_json[[1]]
  }

  n <- nrow(X)
  leaf_id <- integer(n)
  next_id <- 1L

  walk <- function(node, row_indices) {
    if (length(row_indices) == 0) {
      return(invisible())
    }
    if (!is.null(node$prediction)) {
      leaf_id[row_indices] <<- next_id
      next_id <<- next_id + 1L
      return(invisible())
    }
    feature_idx <- as.integer(as.numeric(node$feature) + 1)
    if (feature_idx < 1 || feature_idx > ncol(X)) {
      stop("assign_leaf_ids: split references feature index ", feature_idx,
           " but X has ", ncol(X), " columns. This indicates a malformed ",
           "tree or a mismatch between the tree and X.", call. = FALSE)
    }
    feature_vals <- if (is.data.frame(X)) {
      X[row_indices, feature_idx, drop = TRUE]
    } else {
      X[row_indices, feature_idx]
    }
    go_true <- (feature_vals == 1 | feature_vals == TRUE)
    walk(node$true, row_indices[go_true])
    walk(node$false, row_indices[!go_true])
  }

  walk(tree_json, seq_len(n))
  leaf_id
}

#' Minimum per-leaf subgroup fraction of a fitted single-tree model
#'
#' @description
#' Computes \eqn{\min_\ell |\ell|_0 / |\ell|} over the leaves of a fitted tree:
#' the smallest fraction of a leaf's members that belong to subgroup 0. For a
#' propensity tree fit with `y = A`, this is the **empirical analogue of the
#' positivity/overlap constant** \eqn{c} in \eqn{1 - e_0(x) \ge c}: a value of
#' `0` means some leaf holds only treated units, so the fitted propensity there
#' is exactly 1 and the ATT weight \eqn{\hat p/(1 - \hat p)} diverges. Values
#' near 0 are a warning that the fit is close to violating positivity even when
#' it does not violate it outright. Reported on the fitted model as
#' `@min_leaf_subgroup_fraction`; the solver-side floor that keeps it away from
#' 0 is `subgroup_min_count_0`. See
#' `quality_reports/specs/2026-08-20_leaf-size-subgroup-enforcement.md` in the
#' `global-scholars` project.
#'
#' @param tree_json Tree structure, or an `OptimalTreesModel` (its first tree is
#'   used) — the same representation [assign_leaf_ids()] accepts.
#' @param X Data.frame or matrix of the binary features used at fit time.
#' @param y Outcome vector of length `nrow(X)`.
#' @param subgroup_value The outcome value defining subgroup 0 (default `0`,
#'   i.e. control units).
#' @return A single double in `[0, 1]`, or `NA_real_` when the diagnostic does
#'   not apply or cannot be computed (no tree, no data, length mismatch, or a
#'   tree that does not match `X`).
#' @keywords internal
min_leaf_subgroup_fraction <- function(tree_json, X, y, subgroup_value = 0) {
  if (is.null(tree_json) || is.null(X) || is.null(y)) return(NA_real_)
  if (!is.data.frame(X) && !is.matrix(X)) return(NA_real_)
  if (nrow(X) == 0L || length(y) != nrow(X)) return(NA_real_)

  leaf_id <- tryCatch(assign_leaf_ids(tree_json, X), error = function(e) NULL)
  if (is.null(leaf_id) || length(leaf_id) != nrow(X)) return(NA_real_)

  n_tab <- table(leaf_id)
  if (length(n_tab) == 0L) return(NA_real_)

  in_subgroup <- !is.na(y) & y == subgroup_value
  subgroup_tab <- table(factor(leaf_id[in_subgroup], levels = names(n_tab)))
  min(as.numeric(subgroup_tab) / as.numeric(n_tab))
}

#' Check per-leaf sample-size feasibility of a fitted single-tree model
#'
#' @description
#' Verifies that every leaf of a fitted tree carries at least `m_n`
#' observations, and, if `group` is supplied, that every leaf also carries at
#' least `m_n_group` observations with `group == group_value` (e.g. control
#' units, `A == 0`, for the outcome tree's feasible set in the theory).
#'
#' This is an R-side, post-fit check. The solver's
#' `Configuration::minimum_captured_points` floor is a *total*-count floor with
#' no group awareness (and is not threaded through from R). Since
#' 2026-08-20 the solver additionally enforces a group-aware floor natively via
#' `subgroup_target_index` / `subgroup_min_count_0` / `subgroup_min_count_1` (passed
#' through `...` of [optimaltrees()]; see
#' `quality_reports/specs/2026-08-20_leaf-size-subgroup-enforcement.md` in the
#' `global-scholars` project), so when those are set this function is a cheap
#' independent confirmation rather than the only line of defence. It remains the
#' only check for fits that do not set them.
#'
#' @param fit An `OptimalTreesModel` (e.g. the return value of [fit_tree()]).
#' @param X Data.frame or matrix used to fit `fit`.
#' @param m_n Integer. Minimum total observations per leaf.
#' @param group Optional vector of length `nrow(X)` (e.g. a treatment
#'   indicator). If supplied, `m_n_group` is also enforced.
#' @param group_value The value of `group` that must meet `m_n_group`
#'   (default `0`, i.e. control units).
#' @param m_n_group Integer. Minimum `group == group_value` observations per
#'   leaf. Defaults to `m_n`.
#' @return A list with `feasible` (logical) and `leaf_counts` (a data.frame
#'   with one row per leaf: `leaf_id`, `n`, and `n_group` if `group` was
#'   supplied).
#' @keywords internal
check_leaf_feasibility <- function(fit, X, m_n, group = NULL, group_value = 0,
                                    m_n_group = m_n) {
  leaf_id <- assign_leaf_ids(fit, X)
  n_tab <- table(leaf_id)
  ok_total <- all(n_tab >= m_n)

  leaf_counts <- data.frame(leaf_id = as.integer(names(n_tab)),
                             n = as.integer(n_tab))

  ok_group <- TRUE
  if (!is.null(group)) {
    if (length(group) != nrow(X)) {
      stop("check_leaf_feasibility: `group` must have length nrow(X).",
           call. = FALSE)
    }
    in_group <- group == group_value
    n_group_tab <- table(factor(leaf_id[in_group], levels = names(n_tab)))
    leaf_counts$n_group <- as.integer(n_group_tab)
    ok_group <- all(leaf_counts$n_group >= m_n_group)
  }

  list(feasible = ok_total && ok_group, leaf_counts = leaf_counts)
}

#' Classify a fit against the leaf-budgeted selection certificate
#'
#' @description
#' Given a fit at some `lambda` with `n_leaves` leaves, determines whether it
#' is *certified* to solve the theoretical selection rule
#' \eqn{\that_j\in\argmin_{\tau\in\cT^{(j)}_n}\{R^{(j)}_n(\tau)+\lambda_n\card\tau\}}
#' exactly (theory.tex Lemma "Certificate for the computed selector", cases
#' (i)/(ii)), or only up to a computable slack (case (iii)), or not at all.
#' All three cases require `feasible` to hold; the lemma's premises do not
#' apply otherwise.
#'
#' @param leaf_budget The budget the fit is being classified against. Callers
#'   under a declared depth restriction (see [bisect_lambda_to_budget()]'s
#'   `depth_restricted`) pass the *effective* budget
#'   (`min(leaf_budget, 2^max_depth)`), not the analyst's declared
#'   `leaf_budget`, so `gap` (case iii) is measured against what the
#'   depth-restricted search could actually have produced, not against a
#'   leaf count it structurally cannot reach.
#' @keywords internal
classify_lambda_fit <- function(fit, lambda, n_leaves, feasible, lambda_n,
                                 leaf_budget) {
  if (!feasible) {
    return(list(fit = fit, lambda = lambda, n_leaves = n_leaves,
                feasible = FALSE, certified = FALSE, gap = NA_real_))
  }
  if (n_leaves > leaf_budget) {
    # Over budget: the lemma's "card(t) <= Lbar" precondition fails outright,
    # so no case applies and no certificate (nor slack bound) is available.
    return(list(fit = fit, lambda = lambda, n_leaves = n_leaves,
                feasible = TRUE, certified = FALSE, gap = NA_real_))
  }
  if (isTRUE(lambda == lambda_n) || n_leaves == leaf_budget) {
    # Case (ii) (lambda == lambda_n, any n_leaves <= leaf_budget) or
    # case (i) (n_leaves == leaf_budget, any lambda >= lambda_n).
    return(list(fit = fit, lambda = lambda, n_leaves = n_leaves,
                feasible = TRUE, certified = TRUE, gap = 0))
  }
  # Case (iii): staircase skip. Slack is (lambda - lambda_n) * (Lbar - L),
  # not lambda * (Lbar - L) -- the gap is measured against the analyst's own
  # lambda_n, per the lemma's proof.
  list(fit = fit, lambda = lambda, n_leaves = n_leaves, feasible = TRUE,
       certified = FALSE, gap = (lambda - lambda_n) * (leaf_budget - n_leaves))
}

#' Fit a leaf-budgeted tree by bisecting the regularization penalty
#'
#' @description
#' Recovers the leaf-budget-constrained selector
#' \eqn{\that_j\in\argmin_{\tau:\card\tau\le\bar L}\{R^{(j)}_n(\tau)+\lambda_n\card\tau\}}
#' by calling [fit_tree()] repeatedly at different `regularization` values,
#' rather than adding a native leaf-count cap to the branch-and-bound search
#' (rejected on cache-fragmentation/soundness grounds; see
#' `quality_reports/specs/2026-08-20_leaf-budget-lambda-corollary.md` in the
#' `global-scholars` project). The supporting theory is in `theory.tex`'s
#' `Proposition` "The budget binds asymptotically" and Lemmas "Monotone leaf
#' count" / "Certificate for the computed selector".
#'
#' The search order reflects that theory directly: `lambda_n` is tried FIRST,
#' unmodified. Under standard sparsity/margin conditions this already
#' respects the budget with probability tending to one, in which case no
#' search happens at all (`used_search = FALSE`) and the returned tree is
#' certified to equal the theoretical selector at the analyst's own
#' `lambda_n`. Bisection upward from `lambda_n` is only a finite-sample
#' fallback for the residual (and, per the theory, unverifiable-from-data-in-
#' advance) event that the first try overshoots the budget.
#'
#' @param X,y Training data, as for [fit_tree()].
#' @param leaf_budget Integer. Maximum number of leaves (\eqn{\bar L}).
#' @param lambda_n Numeric. The analyst's regularization value, tried first.
#'   Defaults to [fit_tree()]'s own default (0.1). The fitted objective is
#'   mean-normalized (\eqn{n^{-1}\sum_i L(y_i,f(x_i)) + \lambda\card{\mathrm{leaves}}}),
#'   so `lambda_n` sits on the mean-loss scale regardless of sample size;
#'   see the roxygen note on [fit_tree()]'s `regularization` argument.
#' @param m_n Integer. Minimum observations per leaf (default `1L`, i.e. no
#'   floor beyond whatever the solver enforces natively).
#' @param group,group_value,m_n_group Optional group-specific leaf-size
#'   floor; see [check_leaf_feasibility()]. Use this for the outcome tree's
#'   control-count requirement (`group` = treatment indicator, `group_value
#'   = 0`).
#' @param loss_function Passed to [fit_tree()].
#' @param hi_init,hi_growth,tol_iter Bisection controls: the upper bound
#'   tried if `lambda_n` overshoots the budget (default `max(2 * lambda_n,
#'   0.01)`), the multiplicative factor by which that bound is grown if it
#'   also overshoots, and the maximum number of additional fits allowed in
#'   the bisection phase (growth and bisection share this budget).
#' @param max_depth Integer or `NULL` (default). The search depth passed to
#'   [fit_tree()]. **A feasible tree with exactly `leaf_budget` leaves can be
#'   as deep as `leaf_budget - 1`** (a chain-shaped topology), NOT
#'   `ceiling(log2(leaf_budget))` (a balanced tree, merely the *shallowest*
#'   topology that can reach `leaf_budget` leaves at all) -- never conflate
#'   the two. `NULL` (default) resolves to `leaf_budget - 1` (clamped to at
#'   least `1L`), the depth required to search the *full* feasible class,
#'   including chain-shaped topologies -- full theoretical compliance, no
#'   restriction, no flag needed. Pass an explicit smaller `max_depth`
#'   together with `depth_restricted = TRUE` to deliberately trade the
#'   full-class guarantee for speed (see `depth_sufficient`/`depth_restricted`
#'   in the return value). \strong{`max_depth = 0L` is rejected outright}: as
#'   of the 2026-09-01 fix to `treefarms()`'s single-tree depth-cap safety
#'   net, `0` no longer reliably means "unlimited" once the discretized
#'   binary feature count exceeds 8 -- it silently hands control of the
#'   search depth to that unrelated, feature-count-triggered heuristic
#'   instead of to `leaf_budget`'s own requirement, exactly the gap this
#'   function exists to close. An explicit `max_depth` below
#'   `ceiling(log2(leaf_budget))` is also rejected unconditionally
#'   (regardless of `depth_restricted`): at that depth `leaf_budget` leaves
#'   are not merely restricted but *mechanically unattainable at any
#'   regularization* -- a misconfiguration, not a tradeoff.
#' @param depth_restricted Logical, default `FALSE`. Declares that an
#'   explicit `max_depth` between `ceiling(log2(leaf_budget))` and
#'   `leaf_budget - 1` is a deliberate, known restriction of the search to a
#'   strict subclass of the feasible trees (excluding some deep,
#'   chain-shaped topologies) -- required to silence the error that would
#'   otherwise fire for such a `max_depth`. Ignored (and an error) if
#'   `max_depth` is left `NULL`, since there is then nothing to declare a
#'   restriction of.
#' @param ... Additional arguments passed to [fit_tree()]. Since `max_depth`
#'   is a named formal argument above (not left to `...`), passing
#'   `max_depth = ` here is impossible in practice -- R's own argument
#'   matching always binds it to the formal parameter first, even when
#'   forwarded through an intermediate wrapper's `...`.
#'
#' @return A list:
#'   \item{fit}{The fitted `OptimalTreesModel`.}
#'   \item{lambda}{The regularization value used for the returned fit.}
#'   \item{n_leaves}{Its leaf count.}
#'   \item{feasible}{Whether the per-leaf size floor(s) are met.}
#'   \item{certified}{`TRUE` iff the returned fit is proven (not just
#'     observed) to solve the theoretical selection rule exactly *over the
#'     declared problem* `(leaf_budget, max_depth)`. The claim over the full,
#'     depth-unrestricted feasible class additionally requires
#'     `depth_sufficient`; assert `certified && depth_sufficient` for that
#'     stronger guarantee.}
#'   \item{gap}{Computable suboptimality slack when `certified` is `FALSE`
#'     but the fit is otherwise feasible and within budget (the integer
#'     leaf-count staircase skipped the effective budget,
#'     `min(leaf_budget, 2^max_depth)`); `NA` if infeasible or the budget
#'     could not be met at all; `0` when `certified`.}
#'   \item{used_search}{`FALSE` if `lambda_n` alone already worked (the
#'     asymptotically-typical case); `TRUE` if bisection beyond `lambda_n`
#'     was needed.}
#'   \item{leaf_budget}{The analyst's declared budget, echoed back.}
#'   \item{max_depth}{The search depth actually used (never `0`).}
#'   \item{depth_required}{`leaf_budget - 1` (clamped to at least `1L`), the
#'     depth needed to cover the full feasible class.}
#'   \item{depth_sufficient}{`TRUE` iff `max_depth >= depth_required`, i.e.
#'     the search was NOT depth-restricted below full compliance.}
#'   \item{depth_restricted}{Echoes the `depth_restricted` argument.}
#'
#' @export
bisect_lambda_to_budget <- function(X, y, leaf_budget, lambda_n = 0.1,
                                     m_n = 1L, group = NULL, group_value = 0,
                                     m_n_group = m_n,
                                     loss_function = "misclassification",
                                     hi_init = max(2 * lambda_n, 0.01),
                                     hi_growth = 4, tol_iter = 40,
                                     max_depth = NULL, depth_restricted = FALSE,
                                     ...) {
  if (!is.numeric(leaf_budget) || length(leaf_budget) != 1 || leaf_budget < 1) {
    stop("bisect_lambda_to_budget: `leaf_budget` must be a positive integer.",
         call. = FALSE)
  }
  leaf_budget <- as.integer(leaf_budget)

  # Two distinct depth thresholds -- never conflate them (2026-09-01 Oracle
  # consult, quality_reports/plans/2026-09-01_two-stage-package-defaults-
  # session.md §3.2):
  #   depth_reachable: the shallowest depth at which `leaf_budget` leaves are
  #     even POSSIBLE (a balanced tree of this depth has 2^depth_reachable >=
  #     leaf_budget leaves). Below this, leaf_budget is mechanically
  #     unattainable at ANY lambda -- a misconfiguration, not a tradeoff.
  #   depth_required: the depth needed to cover the FULL feasible class,
  #     including the worst-case chain-shaped topology (depth = leaf_budget -
  #     1). Between depth_reachable and depth_required, leaf_budget is
  #     attainable but the search excludes some feasible trees -- a real,
  #     nameable restriction, never silent, only via depth_restricted = TRUE.
  # Clamped to >= 1L: fit_tree()/treefarms() overload max_depth = 0 to mean
  # "unlimited" (see the roxygen note above), so this function must never
  # resolve to a literal 0 even for the degenerate leaf_budget = 1 case --
  # depth 1 is a harmless superset (any depth-1 cap already permits a
  # 1-leaf tree; it just doesn't additionally REQUIRE one).
  depth_reachable <- max(as.integer(ceiling(log2(leaf_budget))), 1L)
  depth_required <- max(leaf_budget - 1L, 1L)

  # NB: no `"max_depth" %in% names(list(...))` guard here. Since `max_depth`
  # is a formal parameter of this function (not left to flow through `...`),
  # R's own argument matching binds any `max_depth =` argument to it directly
  # -- including when forwarded through an intermediate wrapper's `...` --
  # so it can never end up in this function's own `...` to begin with.
  # Confirmed empirically (2026-09-01): a wrapper `g <- function(...)
  # f(a = 1, ...)` called as `g(max_depth = 5)` still binds `max_depth` to
  # `f`'s formal, not to `f`'s `...`.

  if (is.null(max_depth)) {
    if (isTRUE(depth_restricted)) {
      stop("bisect_lambda_to_budget: `depth_restricted = TRUE` requires an ",
           "explicit `max_depth` to restrict to; got `max_depth = NULL`. ",
           "Pass a `max_depth` in [", depth_reachable, ", ", depth_required,
           ") together with `depth_restricted = TRUE`.", call. = FALSE)
    }
    max_depth <- depth_required
    depth_sufficient <- TRUE
    if (depth_required > 8L) {
      message(
        "bisect_lambda_to_budget: leaf_budget = ", leaf_budget,
        " requires searching to depth ", depth_required, " for full ",
        "theoretical compliance (the worst-case chain-shaped topology) -- ",
        "defaulting max_depth to ", depth_required, ". With ", ncol(X),
        " raw covariate(s) (more after discretization), this bisection can ",
        "run up to ", 2L * tol_iter + 1L, " fits, each searching an ",
        "unbounded-feature-count tree at depth ", depth_required, " -- can ",
        "be slow (see quality_reports/plans/2026-09-01_two-stage-package-",
        "defaults-session.md §1.3 for measured costs at this scale). Pass a ",
        "smaller `max_depth` together with `depth_restricted = TRUE` to ",
        "trade the full-class guarantee for speed."
      )
    }
  } else {
    max_depth <- as.integer(max_depth)
    if (max_depth == 0L) {
      stop("bisect_lambda_to_budget: `max_depth = 0L` (\"unlimited\") is not ",
           "accepted here. Since treefarms()'s single-tree depth-cap safety ",
           "net auto-restricts to depth 2 once the discretized binary ",
           "feature count exceeds 8, and fires precisely when ",
           "max_depth == 0L, passing 0 here would silently hand control of ",
           "the search depth to that unrelated heuristic instead of to ",
           "leaf_budget's own requirement -- exactly the gap this function ",
           "exists to close. Pass an explicit positive max_depth (",
           depth_required, " for full compliance) instead.", call. = FALSE)
    }
    if (max_depth < depth_reachable) {
      stop("bisect_lambda_to_budget: max_depth = ", max_depth, " is below ",
           "the minimum depth at which leaf_budget = ", leaf_budget,
           " leaves are even possible (need max_depth >= ceiling(log2(",
           leaf_budget, ")) = ", depth_reachable, "; a depth-", max_depth,
           " binary tree has at most 2^", max_depth, " = ",
           2^max_depth, " leaves). leaf_budget is mechanically unreachable ",
           "at ANY regularization with this max_depth, regardless of ",
           "depth_restricted. Either raise max_depth to at least ",
           depth_reachable, ", or lower leaf_budget to at most 2^max_depth ",
           "= ", 2^max_depth, ".", call. = FALSE)
    }
    if (max_depth < depth_required && !isTRUE(depth_restricted)) {
      stop("bisect_lambda_to_budget: max_depth = ", max_depth, " is below ",
           "leaf_budget - 1 = ", depth_required, " (the depth of the ",
           "worst-case chain-shaped topology with ", leaf_budget,
           " leaves). Trees with ", leaf_budget, " leaves and depth > ",
           max_depth, " ARE excluded from the search, so the returned fit ",
           "would be a global optimum over a depth-restricted class only, ",
           "not the full feasible class. leaf_budget = ", leaf_budget,
           " remains mechanically attainable at this depth (2^max_depth = ",
           2^max_depth, " >= leaf_budget), so this is a deliberate ",
           "restriction, not a misconfiguration -- either raise max_depth ",
           "to ", depth_required, " for full compliance, or pass ",
           "`depth_restricted = TRUE` to declare the restriction ",
           "deliberately.", call. = FALSE)
    }
    depth_sufficient <- max_depth >= depth_required
  }

  # Achievable leaf count given max_depth: a binary tree of depth max_depth
  # has at most 2^max_depth leaves. The depth_reachable check above already
  # guarantees 2^max_depth >= leaf_budget on every path that reaches this
  # line, so effective_budget always equals leaf_budget in practice -- kept
  # as an explicit min() (rather than assumed) so gap/the search target stay
  # correct even if that invariant is ever loosened upstream.
  effective_budget <- min(leaf_budget, as.integer(2^max_depth))

  finalize <- function(fit, lambda, n_leaves, feasible, used_search) {
    out <- classify_lambda_fit(fit, lambda, n_leaves, feasible, lambda_n,
                                effective_budget)
    out$used_search <- used_search
    out$leaf_budget <- leaf_budget
    out$max_depth <- max_depth
    out$depth_required <- depth_required
    out$depth_sufficient <- depth_sufficient
    out$depth_restricted <- isTRUE(depth_restricted)
    out
  }

  fit_and_check <- function(lambda) {
    fit <- fit_tree(X, y, loss_function = loss_function,
                     regularization = lambda, max_depth = max_depth, ...)
    n_leaves <- count_tree_leaves(fit)
    feas <- check_leaf_feasibility(fit, X, m_n = m_n, group = group,
                                    group_value = group_value,
                                    m_n_group = m_n_group)
    list(fit = fit, lambda = lambda, n_leaves = n_leaves,
         feasible = feas$feasible)
  }

  # Case (ii): try the analyst's own lambda_n first. Per
  # Proposition "The budget binds asymptotically", this already respects
  # the budget with probability tending to one -- no search is the
  # *expected* outcome, not a shortcut around the theory.
  r0 <- fit_and_check(lambda_n)
  if (r0$n_leaves <= effective_budget) {
    return(finalize(r0$fit, lambda_n, r0$n_leaves, r0$feasible, FALSE))
  }

  # Fallback: bisect upward for the smallest lambda >= lambda_n giving
  # <= effective_budget leaves. Lemma "Monotone leaf count" makes this well
  # posed: leaf count is weakly decreasing in lambda.
  lo <- lambda_n
  hi <- hi_init
  r_hi <- fit_and_check(hi)
  budget_iter <- 0L
  while (r_hi$n_leaves > effective_budget && budget_iter < tol_iter) {
    hi <- hi * hi_growth
    r_hi <- fit_and_check(hi)
    budget_iter <- budget_iter + 1L
  }
  if (r_hi$n_leaves > effective_budget) {
    warning("bisect_lambda_to_budget: could not reach leaf_budget = ",
            leaf_budget, " by growing lambda up to ", hi, "; returning the ",
            "most-regularized fit tried. Check whether leaf_budget * m_n ",
            "is compatible with nrow(X)", if (!depth_sufficient) {
              paste0(", and note max_depth = ", max_depth, " is a ",
                     "deliberate restriction below the full-compliance ",
                     "depth of ", depth_required, " -- raising max_depth ",
                     "may also help")
            } else "", ".", call. = FALSE)
    return(finalize(r_hi$fit, hi, r_hi$n_leaves, r_hi$feasible, TRUE))
  }

  best <- r_hi
  for (i in seq_len(tol_iter)) {
    mid <- sqrt(lo * hi)  # geometric bisection; scale is multiplicative
    r_mid <- fit_and_check(mid)
    if (r_mid$n_leaves == effective_budget) {
      return(finalize(r_mid$fit, mid, r_mid$n_leaves, r_mid$feasible, TRUE))
    } else if (r_mid$n_leaves > effective_budget) {
      lo <- mid
    } else {
      hi <- mid
      best <- r_mid
    }
  }

  # Lemma "Certificate for the computed selector", case (iii): the integer
  # leaf-count staircase skipped effective_budget exactly.
  finalize(best$fit, best$lambda, best$n_leaves, best$feasible, TRUE)
}
