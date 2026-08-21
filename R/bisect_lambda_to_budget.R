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

#' Check per-leaf sample-size feasibility of a fitted single-tree model
#'
#' @description
#' Verifies that every leaf of a fitted tree carries at least `m_n`
#' observations, and, if `group` is supplied, that every leaf also carries at
#' least `m_n_group` observations with `group == group_value` (e.g. control
#' units, `A == 0`, for the outcome tree's feasible set in the theory). This
#' is an R-side, post-fit check: the solver's native
#' `Configuration::minimum_captured_points` floor is a *total*-count floor
#' with no group awareness (and is not currently threaded through from R at
#' all), so the group-specific condition cannot be enforced during the
#' search itself and must be confirmed here. See
#' `quality_reports/specs/2026-08-20_leaf-size-subgroup-enforcement.md` in
#' the `global-scholars` project for the native-C++ alternative (not yet
#' implemented) that would make this a cheap confirmation rather than the
#' only check.
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
#' @param ... Additional arguments passed to [fit_tree()].
#'
#' @return A list:
#'   \item{fit}{The fitted `OptimalTreesModel`.}
#'   \item{lambda}{The regularization value used for the returned fit.}
#'   \item{n_leaves}{Its leaf count.}
#'   \item{feasible}{Whether the per-leaf size floor(s) are met.}
#'   \item{certified}{`TRUE` iff the returned fit is proven (not just
#'     observed) to solve the theoretical selection rule exactly.}
#'   \item{gap}{Computable suboptimality slack when `certified` is `FALSE`
#'     but the fit is otherwise feasible and within budget (the integer
#'     leaf-count staircase skipped `leaf_budget`); `NA` if infeasible or
#'     the budget could not be met at all; `0` when `certified`.}
#'   \item{used_search}{`FALSE` if `lambda_n` alone already worked (the
#'     asymptotically-typical case); `TRUE` if bisection beyond `lambda_n`
#'     was needed.}
#'
#' @export
bisect_lambda_to_budget <- function(X, y, leaf_budget, lambda_n = 0.1,
                                     m_n = 1L, group = NULL, group_value = 0,
                                     m_n_group = m_n,
                                     loss_function = "misclassification",
                                     hi_init = max(2 * lambda_n, 0.01),
                                     hi_growth = 4, tol_iter = 40, ...) {
  if (!is.numeric(leaf_budget) || length(leaf_budget) != 1 || leaf_budget < 1) {
    stop("bisect_lambda_to_budget: `leaf_budget` must be a positive integer.",
         call. = FALSE)
  }
  leaf_budget <- as.integer(leaf_budget)

  fit_and_check <- function(lambda) {
    fit <- fit_tree(X, y, loss_function = loss_function,
                     regularization = lambda, ...)
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
  if (r0$n_leaves <= leaf_budget) {
    out <- classify_lambda_fit(r0$fit, lambda_n, r0$n_leaves, r0$feasible,
                                lambda_n, leaf_budget)
    out$used_search <- FALSE
    return(out)
  }

  # Fallback: bisect upward for the smallest lambda >= lambda_n giving
  # <= leaf_budget leaves. Lemma "Monotone leaf count" makes this well
  # posed: leaf count is weakly decreasing in lambda.
  lo <- lambda_n
  hi <- hi_init
  r_hi <- fit_and_check(hi)
  budget_iter <- 0L
  while (r_hi$n_leaves > leaf_budget && budget_iter < tol_iter) {
    hi <- hi * hi_growth
    r_hi <- fit_and_check(hi)
    budget_iter <- budget_iter + 1L
  }
  if (r_hi$n_leaves > leaf_budget) {
    warning("bisect_lambda_to_budget: could not reach leaf_budget = ",
            leaf_budget, " by growing lambda up to ", hi, "; returning the ",
            "most-regularized fit tried. Check whether leaf_budget * m_n ",
            "is compatible with nrow(X).", call. = FALSE)
    out <- classify_lambda_fit(r_hi$fit, hi, r_hi$n_leaves, r_hi$feasible,
                                lambda_n, leaf_budget)
    out$used_search <- TRUE
    return(out)
  }

  best <- r_hi
  for (i in seq_len(tol_iter)) {
    mid <- sqrt(lo * hi)  # geometric bisection; scale is multiplicative
    r_mid <- fit_and_check(mid)
    if (r_mid$n_leaves == leaf_budget) {
      out <- classify_lambda_fit(r_mid$fit, mid, r_mid$n_leaves,
                                  r_mid$feasible, lambda_n, leaf_budget)
      out$used_search <- TRUE
      return(out)
    } else if (r_mid$n_leaves > leaf_budget) {
      lo <- mid
    } else {
      hi <- mid
      best <- r_mid
    }
  }

  # Lemma "Certificate for the computed selector", case (iii): the integer
  # leaf-count staircase skipped leaf_budget exactly.
  out <- classify_lambda_fit(best$fit, best$lambda, best$n_leaves,
                              best$feasible, lambda_n, leaf_budget)
  out$used_search <- TRUE
  out
}
