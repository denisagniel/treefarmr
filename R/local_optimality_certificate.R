#' Local-optimality certificate for a RefinedTreeModel (Milestone B)
#'
#' @description
#' \code{theory.tex} explicitly defers this mechanism to "companion
#' package-implementation work" (\code{rem:grid-practice}): "After Stage B,
#' a data-computable local-optimality check substitutes for an unobservable
#' population check." This file is that mechanism, designed via an
#' Oracle consult (see \code{quality_reports/plans/2026-09-01_two-stage-
#' package-defaults-session.md} Milestone B in the \code{global-scholars}
#' project for the full consult and its three framing corrections).
#'
#' \strong{What this certifies, precisely}: [certify_local_optimality()]
#' evaluates a [RefinedTreeModel] against every ONE-SPLIT edit of four
#' kinds -- \code{move} (re-optimize an existing split's cut on its own
#' coordinate), \code{recoord} (swap an existing PRUNABLE split's
#' coordinate), \code{add} (introduce a new split at an existing leaf),
#' \code{delete} (remove an existing PRUNABLE split) -- on the SAME
#' penalized objective Stage A minimized (\eqn{n^{-1}\sum_i(y_i-f(x_i))^2 +
#' \lambda\card{\mathrm{leaves}}}, on the raw-SSE scale internally). It
#' passes iff no such edit improves that objective by more than a numerical
#' tolerance.
#'
#' \strong{What this does NOT certify -- explicit, load-bearing limitation}:
#' \code{theory.tex}'s \code{prop:greedy} ("Greedy splitting has no
#' first-split signal") is a PROVEN counterexample showing a distant
#' topology can have strictly lower population risk than a
#' locally-unimprovable one, with every one-split deviation's risk
#' reduction being EXACTLY ZERO at the witness. This certificate is
#' therefore LOCAL ONLY. Passing it is necessary evidence against a
#' specific, cheap class of one-step deviations -- it is NOT global
#' optimality, NOT \code{ass:global} compliance, and must never be reported
#' to a caller as either.
#'
#' \strong{Explicitly excluded from the one-split search space} (so the
#' contract stays honest, per the Oracle consult): multi-split subtree
#' collapse (a \code{k}-step deviation for \code{k > 1}, already what
#' Stage A's branch-and-bound explored globally); a coordinate change at a
#' NON-prunable node (fair evaluation would require re-optimizing both
#' subtrees, not a one-step edit); a cut MOVE outside a split's own
#' identifying bracket (\code{= delete} composed with \code{move}, a
#' two-step deviation, not one); any simultaneous change to two splits; and
#' the distant-topology class from \code{prop:greedy} itself.
#'
#' @keywords internal
#' @name local_optimality_certificate
NULL

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Convert a model's stored `@lambda` to the raw-SSE-scale per-leaf penalty
#'
#' @description
#' [fit_tree()]'s objective is mean-normalized:
#' \eqn{n^{-1}\sum_i L(y_i,f(x_i)) + \lambda\card{\mathrm{leaves}}}
#' (confirmed directly from its own roxygen, not assumed). Every delta in
#' this file is computed on the RAW-SSE scale (matching [scan_cutoff()]'s
#' and [refine_tree_cuts()]'s existing arithmetic), so the leaf-count
#' penalty term must be converted: multiplying the mean-normalized
#' objective through by \code{n} gives \code{lambda_sse = n * lambda} for
#' \code{objective_scale = "mse"} (the package-wide default and the scale
#' [fit_tree()] actually uses). \code{objective_scale = "sse"} is provided
#' for a caller who has already converted \code{lambda} to that scale --
#' \strong{getting this wrong silently makes every pass/fail threshold in
#' this file wrong by a factor of \code{n_train}}, so it is a required,
#' explicit choice, not a free-form tuning knob.
#'
#' @keywords internal
.objective_lambda_sse <- function(model, lambda = NULL, objective_scale = NULL) {
  lam <- if (is.null(lambda)) model@lambda else lambda
  scale <- if (is.null(objective_scale)) model@objective_scale else objective_scale
  if (is.null(lam) || length(lam) != 1L || !is.finite(lam) || lam < 0) {
    cli::cli_abort("`lambda` must be a single non-negative finite number.")
  }
  switch(scale,
    mse = model@n_train * lam,
    sse = lam,
    cli::cli_abort("`objective_scale` must be 'mse' or 'sse', got {.val {scale}}.")
  )
}

#' Empty perturbation-delta data.frame, for a kind with zero candidate sites
#' @keywords internal
.empty_perturb_df <- function() {
  data.frame(kind = character(0), node_id = integer(0), coord = character(0),
             cut = numeric(0), delta = numeric(0), feasible = logical(0),
             stringsAsFactors = FALSE)
}

#' Assemble the plain-list return value of [certify_local_optimality()]
#' @keywords internal
.certificate_result <- function(certified, reason, margin, margin_rel, objective, sse,
                                 n_leaves, lambda_sse, tol, worst, by_kind, detail, notes) {
  structure(
    list(
      certified = certified, reason = reason, margin = margin, margin_rel = margin_rel,
      objective = objective, sse = sse, n_leaves = n_leaves, lambda_sse = lambda_sse,
      tol = tol, worst = worst, by_kind = by_kind,
      detail = if (length(detail) > 0L) detail else NULL, notes = notes
    ),
    class = "optimaltrees_local_certificate"
  )
}

# ---------------------------------------------------------------------------
# Canonical topology signature (Q3)
# ---------------------------------------------------------------------------

#' Per-leaf axis-aligned box constraints of a coordinate-space tree
#'
#' @description
#' Root-to-leaf walk maintaining, per coordinate, an open-below/closed-above
#' interval \code{(lo, hi]} (matching the tree's own left := \code{X[[coord]]
#' <= cut} convention). Coordinates never split on, on the path to a given
#' leaf, keep their trivial \code{(-Inf, Inf)} bound and are OMITTED from
#' that leaf's rows -- there is nothing to constrain and including a
#' trivial row would only add noise to any downstream comparison.
#'
#' @param tree A coordinate-space tree (see \code{\link{stage_b}}).
#' @param coord_names Character vector of coordinates to track bounds for.
#' @return Data.frame with one row per (leaf, constrained coordinate):
#'   \code{leaf_id}, \code{coord}, \code{lo}, \code{hi}.
#' @keywords internal
tree_leaf_boxes <- function(tree, coord_names) {
  rows <- list()
  init <- stats::setNames(
    lapply(coord_names, function(cn) list(lo = -Inf, hi = Inf)), coord_names
  )

  walk <- function(node, bounds) {
    if (identical(node$kind, "leaf")) {
      for (cn in coord_names) {
        b <- bounds[[cn]]
        if (identical(b$lo, -Inf) && identical(b$hi, Inf)) next   # trivial, omit
        rows[[length(rows) + 1L]] <<- data.frame(
          leaf_id = node$id, coord = cn, lo = b$lo, hi = b$hi,
          stringsAsFactors = FALSE
        )
      }
      return(invisible())
    }
    left_bounds <- bounds
    left_bounds[[node$coord]]$hi <- min(left_bounds[[node$coord]]$hi, node$cut)
    walk(node$left, left_bounds)

    right_bounds <- bounds
    right_bounds[[node$coord]]$lo <- max(right_bounds[[node$coord]]$lo, node$cut)
    walk(node$right, right_bounds)
  }
  walk(tree, init)

  if (length(rows) == 0L) {
    return(data.frame(leaf_id = integer(0), coord = character(0),
                       lo = numeric(0), hi = numeric(0)))
  }
  do.call(rbind, rows)
}

#' Canonical row-partition signature of a coordinate-space tree
#'
#' @description
#' The EXACT, tolerance-free notion of "same logical partition": two trees
#' induce the same partition of the training rows iff the training rows
#' are grouped into IDENTICAL blocks by leaf membership, regardless of leaf
#' `id` numbering, split order, or orientation. No numeric cut-value
#' tolerance is needed anywhere in this comparison, because two independent
#' fits recovering "the same" off-grid boundary at different discretization
#' resolutions will essentially never produce bit-identical cuts, while the
#' training rows themselves are identical, discrete, and unambiguous.
#'
#' @param tree A coordinate-space tree.
#' @param X Training data.frame the tree is evaluated against.
#' @return List: \code{blocks} (list of sorted row-index vectors, one per
#'   non-empty leaf, ordered by their own signature), \code{key} (a single
#'   hash of the sorted block signatures -- identical iff the partitions
#'   are identical), \code{n_leaves} (non-empty leaves found), \code{n_empty}
#'   (leaves declared by the tree's own structure but reached by zero rows
#'   of \code{X} -- can be nonzero if \code{X} differs from the tree's
#'   original training data).
#' @keywords internal
canonical_partition <- function(tree, X) {
  a <- coord_tree_assign(tree, X)
  blocks <- split(seq_along(a), a)
  blocks <- lapply(blocks, sort)
  # USE.NAMES = FALSE is load-bearing, not cosmetic: vapply() otherwise
  # carries split()'s leaf-id names onto `keys`, and digest::digest() DOES
  # hash the names attribute -- confirmed directly (two named vectors with
  # byte-identical VALUES but different NAMES hashed differently; stripped
  # of names, they hashed identically). Two trees inducing the SAME row
  # partition via DIFFERENT leaf-id-to-block assignment would otherwise be
  # reported as unstable purely from id-labeling, which is exactly the
  # false positive this signature exists to avoid.
  keys <- vapply(blocks, function(b) paste0(b, collapse = ","), character(1),
                  USE.NAMES = FALSE)
  ord <- order(keys)
  n_leaves_declared <- coord_tree_count_leaves(tree)
  list(
    blocks = unname(blocks[ord]),
    key = digest::digest(unname(keys[ord])),
    n_leaves = length(blocks),
    n_empty = n_leaves_declared - length(blocks)
  )
}

#' Canonical rank-based box signature of a coordinate-space tree
#'
#' @description
#' Secondary, human-readable-adjacent signature: canonicalizes every
#' leaf's box (see [tree_leaf_boxes()]) by replacing each numeric bound with
#' its integer RANK among \code{X}'s observed values of that coordinate
#' (sentinel \code{0L} for \code{-Inf}, \code{nrow(X)} for \code{Inf}), so
#' two off-grid cuts falling in the same inter-observation gap compare
#' EQUAL. Per-leaf constraints are sorted by coordinate name; leaves are
#' sorted by their own canonicalized signature string. This can disagree
#' with [canonical_partition()] only under near-perfect collinearity
#' between two coordinates (same rows separated, different coordinate
#' used) -- report it as a secondary diagnostic, never let it drive
#' \code{topology_stable}.
#'
#' @keywords internal
canonical_box_signature <- function(tree, X) {
  coord_names <- coord_tree_coords_used(tree)
  if (length(coord_names) == 0L) return("<single-leaf>")

  boxes <- tree_leaf_boxes(tree, coord_names)
  n <- nrow(X)
  rank_of <- function(coord, val) {
    if (identical(val, -Inf)) return(0L)
    if (identical(val, Inf)) return(n)
    sum(X[[coord]] <= val)
  }
  if (nrow(boxes) == 0L) return("<single-leaf>")

  boxes$rank_lo <- mapply(rank_of, boxes$coord, boxes$lo)
  boxes$rank_hi <- mapply(rank_of, boxes$coord, boxes$hi)

  by_leaf <- split(boxes, boxes$leaf_id)
  leaf_sigs <- vapply(by_leaf, function(df) {
    df <- df[order(df$coord), ]
    paste0(sprintf("%s:[%d,%d]", df$coord, df$rank_lo, df$rank_hi), collapse = "|")
  }, character(1))
  paste0(sort(leaf_sigs), collapse = ";")
}

#' Order-sensitive structural fingerprint (coord + rank-canonicalized cut at
#' every split, in pre-order) -- distinguishes "same partition, different
#' split order/orientation" from "literally the same tree shape".
#' @keywords internal
.structural_fingerprint <- function(tree, X) {
  walk <- function(node) {
    if (identical(node$kind, "leaf")) return("L")
    r <- sum(X[[node$coord]] <= node$cut)
    sprintf("S(%s,%d,%s,%s)", node$coord, r, walk(node$left), walk(node$right))
  }
  walk(tree)
}

#' Compare the collapsed topology of two `RefinedTreeModel`s
#'
#' @description
#' The cross-rung comparison the `m`-ladder needs: does the tree fit and
#' refined at one discretization resolution induce the SAME partition of
#' the training rows as one fit and refined at a different resolution?
#' Uses [canonical_partition()] -- exact, tolerance-free -- as the
#' authoritative \code{topology_stable} signal; [canonical_box_signature()]
#' and [.structural_fingerprint()] are reported as secondary diagnostics,
#' never as the driver of \code{topology_stable} (see their own docs for
#' why: box-signature agreement can be a false negative under collinearity,
#' and structural agreement is strictly stronger than partition agreement
#' and would report false instability on a mere reordering).
#'
#' Also reports the REFINEMENT relationship between the two partitions
#' (from one cross-tabulation of leaf assignments, no extra data pass):
#' \code{"identical"}, \code{"curr_refines_prev"} (every \code{model_curr}
#' leaf's rows fall inside exactly one \code{model_prev} leaf -- the common
#' benign outcome when increasing \code{m} recovers one additional
#' boundary), \code{"prev_refines_curr"} (the dual), or
#' \code{"incomparable"}.
#'
#' @param model_prev,model_curr [RefinedTreeModel] objects from two
#'   `m`-ladder rungs.
#' @param X Data.frame or matrix with the coordinates both models split on.
#'   MUST be the same underlying rows for both (this function has no way to
#'   detect a silently different \code{X} beyond the coordinate-presence
#'   check below -- pass the same object for both fits).
#' @return List: \code{comparable} (logical; \code{FALSE} only for an
#'   \code{X} that is missing a coordinate either model needs --
#'   \code{topology_stable} is never a bare \code{FALSE} in that case, to
#'   avoid it reading as detected instability), \code{reason} (populated
#'   iff \code{!comparable}), \code{topology_stable}, \code{refinement},
#'   \code{n_leaves_prev}, \code{n_leaves_curr}, \code{n_empty_prev},
#'   \code{n_empty_curr}, \code{coords_used_prev}, \code{coords_used_curr},
#'   \code{structure_identical}, \code{box_signature_equal}.
#' @export
compare_topology <- function(model_prev, model_curr, X) {
  if (!S7::S7_inherits(model_prev, RefinedTreeModel) ||
      !S7::S7_inherits(model_curr, RefinedTreeModel)) {
    cli::cli_abort("compare_topology: both arguments must be RefinedTreeModel objects.")
  }
  if (is.matrix(X)) X <- as.data.frame(X)

  missing <- setdiff(union(model_prev@coords, model_curr@coords), names(X))
  if (length(missing) > 0L) {
    return(list(
      comparable = FALSE, reason = "X_mismatch", topology_stable = NA,
      refinement = NA_character_, n_leaves_prev = NA_integer_, n_leaves_curr = NA_integer_,
      n_empty_prev = NA_integer_, n_empty_curr = NA_integer_,
      coords_used_prev = model_prev@coords, coords_used_curr = model_curr@coords,
      structure_identical = NA, box_signature_equal = NA
    ))
  }

  part_prev <- canonical_partition(model_prev@tree, X)
  part_curr <- canonical_partition(model_curr@tree, X)
  topology_stable <- identical(part_prev$key, part_curr$key)

  a_prev <- coord_tree_assign(model_prev@tree, X)
  a_curr <- coord_tree_assign(model_curr@tree, X)
  tab <- table(prev = a_prev, curr = a_curr)
  curr_refines_prev <- all(colSums(tab > 0) <= 1L)
  prev_refines_curr <- all(rowSums(tab > 0) <= 1L)
  refinement <- if (curr_refines_prev && prev_refines_curr) {
    "identical"
  } else if (curr_refines_prev) {
    "curr_refines_prev"
  } else if (prev_refines_curr) {
    "prev_refines_curr"
  } else {
    "incomparable"
  }

  box_prev <- canonical_box_signature(model_prev@tree, X)
  box_curr <- canonical_box_signature(model_curr@tree, X)
  fp_prev <- .structural_fingerprint(model_prev@tree, X)
  fp_curr <- .structural_fingerprint(model_curr@tree, X)

  list(
    comparable = TRUE, reason = NA_character_, topology_stable = topology_stable,
    refinement = refinement,
    n_leaves_prev = part_prev$n_leaves, n_leaves_curr = part_curr$n_leaves,
    n_empty_prev = part_prev$n_empty, n_empty_curr = part_curr$n_empty,
    coords_used_prev = model_prev@coords, coords_used_curr = model_curr@coords,
    structure_identical = identical(fp_prev, fp_curr),
    box_signature_equal = identical(box_prev, box_curr)
  )
}

# ---------------------------------------------------------------------------
# One-split perturbation enumerators (Q1)
# ---------------------------------------------------------------------------

#' DELETE: collapse every prunable (both children are leaves) split
#'
#' @description
#' Closed form from each pair's already-stored leaf `stats` -- no data
#' pass. \code{delta_sse = s1^2/n1 + s2^2/n2 - (s1+s2)^2/(n1+n2)} is the
#' SSE the split currently buys (always \code{>= 0}, by convexity of
#' \code{s^2/n}); \code{delta = delta_sse - lambda_sse} is the change in the
#' penalized objective from deleting it (removing one leaf saves exactly
#' \code{lambda_sse}). Restricted to prunable nodes: collapsing an internal
#' split whose children are themselves subtrees would delete more than one
#' split's worth of structure in one "perturbation", which is exactly what
#' Stage A's own branch-and-bound already explored globally -- not a
#' one-step edit.
#'
#' @keywords internal
perturb_delete_deltas <- function(model, lambda_sse = .objective_lambda_sse(model)) {
  tree <- model@tree
  rows <- list()
  for (p in coord_tree_split_paths(tree)) {
    node <- coord_tree_node_at(tree, p)
    if (!identical(node$left$kind, "leaf") || !identical(node$right$kind, "leaf")) next

    l <- node$left$stats; r <- node$right$stats
    delta_sse <- l$sum^2 / l$n + r$sum^2 / r$n - (l$sum + r$sum)^2 / (l$n + r$n)
    delta <- delta_sse - lambda_sse
    rows[[length(rows) + 1L]] <- data.frame(
      kind = "delete", node_id = node$id, coord = node$coord, cut = node$cut,
      delta = delta, feasible = TRUE, stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) return(.empty_perturb_df())
  do.call(rbind, rows)
}

#' MOVE: re-check every existing split's cut against its own bracket
#'
#' @description
#' \strong{Not vacuous.} [refine_tree_cuts()] refines root-first, so a
#' node's bracket/leaf-position map at the moment IT was refined reflects
#' its descendants' THEN-current (possibly still-unrefined) cuts. This
#' function re-derives every node's candidate set and leaf-position map
#' from the FINAL, fully-refined \code{model@tree} and re-runs
#' [scan_cutoff()] -- i.e. it is the fixed-point check a single root-first
#' pass does not itself guarantee. Does NOT widen the search past the
#' node's own [node_bracket()]: a cut moved past a bracket boundary makes a
#' descendant region empty, which is DELETE composed with MOVE (a two-step
#' deviation), not a one-split edit.
#'
#' @keywords internal
perturb_move_deltas <- function(model, X, y, min_leaf_n = model@min_leaf_n) {
  if (is.matrix(X)) X <- as.data.frame(X)
  tree <- model@tree
  y <- as.numeric(y)
  y_centered <- y - mean(y)

  rows <- list()
  for (p in coord_tree_split_paths(tree)) {
    node <- coord_tree_node_at(tree, p)
    idx  <- coord_tree_rows_at(tree, X, p)

    if (length(idx) < 2L * min_leaf_n) {
      rows[[length(rows) + 1L]] <- data.frame(
        kind = "move", node_id = node$id, coord = node$coord, cut = NA_real_,
        delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
      )
      next
    }

    Xn <- X[idx, , drop = FALSE]
    yn <- y_centered[idx]
    xj <- Xn[[node$coord]]

    br <- node_bracket(tree, p)
    cands <- bracket_candidates(xj, node, br)

    incumbent_present <- any(vapply(
      cands, function(c) isTRUE(all.equal(c, node$cut, tolerance = .Machine$double.eps^0.5)),
      logical(1)
    ))
    if (!incumbent_present) {
      cli::cli_abort(c(
        "perturb_move_deltas: node {node$id}'s incumbent cut is not a member of \\
         its own freshly-recomputed candidate set.",
        "i" = "This indicates the tree's incumbent cut is inconsistent with `X` \\
               (a different X than the model was refined on) or a genuinely \\
               corrupted/degenerate tree -- not a normal outcome."
      ))
    }

    n_leq <- vapply(cands, function(c) sum(xj <= c), integer(1))
    n_gt  <- length(xj) - n_leq
    feasible_cand <- cands[n_leq >= min_leaf_n & n_gt >= min_leaf_n]

    if (length(feasible_cand) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        kind = "move", node_id = node$id, coord = node$coord, cut = NA_real_,
        delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
      )
      next
    }

    n_left  <- coord_tree_count_leaves(node$left)
    lid_left  <- coord_tree_leaf_index(node$left,  Xn)
    lid_right <- n_left + coord_tree_leaf_index(node$right, Xn)
    K <- n_left + coord_tree_count_leaves(node$right)

    res <- scan_cutoff(xj, yn, lid_left, lid_right, K, feasible_cand,
                        incumbent_cut = node$cut)
    if (is.na(res$sse_before)) {
      cli::cli_abort(
        "perturb_move_deltas: node {node$id}'s incumbent cut survived the \\
         candidate check but scan_cutoff() could not locate its SSE -- this \\
         should be unreachable; please report."
      )
    }

    rows[[length(rows) + 1L]] <- data.frame(
      kind = "move", node_id = node$id, coord = node$coord, cut = res$cut,
      delta = res$sse - res$sse_before, feasible = TRUE, stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) return(.empty_perturb_df())
  do.call(rbind, rows)
}

#' ADD: introduce one new split at every leaf, on every coordinate
#'
#' @description
#' Exhaustive over leaves x ALL \code{p} coordinates (including coordinates
#' already used elsewhere in the tree -- do NOT restrict to unused ones: a
#' coordinate used elsewhere is frequently the informative one in a
#' DIFFERENT leaf too, and excluding it would let the certificate pass a
#' tree a single greedy step beats). Cost is one CART growth step,
#' O(p * n log n) via [scan_cutoff()] with a trivial 2-leaf assignment --
#' affordable, so no restriction is a real efficiency tradeoff.
#'
#' Respects the Stage-A model class: a candidate that would exceed
#' \code{max_depth} or push \code{n_leaves} past \code{max_leaves} is
#' marked \code{feasible = FALSE} (an OUT-OF-CLASS split says nothing about
#' the model's optimality WITHIN the class it was fit over), never scored
#' as an improving perturbation.
#'
#' @keywords internal
perturb_add_deltas <- function(model, X, y, min_leaf_n = model@min_leaf_n,
                                max_depth = model@max_depth, max_leaves = model@max_leaves) {
  if (is.matrix(X)) X <- as.data.frame(X)
  tree <- model@tree
  y <- as.numeric(y)
  y_centered <- y - mean(y)
  lambda_sse <- .objective_lambda_sse(model)

  all_coords <- names(X)
  n_leaves_total <- coord_tree_count_leaves(tree)
  leaves_capped <- length(max_leaves) == 1L && n_leaves_total >= max_leaves

  rows <- list()
  walk <- function(node, path, depth) {
    if (!identical(node$kind, "leaf")) {
      walk(node$left,  c(path, "left"),  depth + 1L)
      walk(node$right, c(path, "right"), depth + 1L)
      return(invisible())
    }

    idx <- coord_tree_rows_at(tree, X, path)
    depth_capped <- length(max_depth) == 1L && (depth + 1L) > max_depth
    row_infeasible <- length(idx) < 2L * min_leaf_n

    for (cn in all_coords) {
      if (row_infeasible || leaves_capped || depth_capped) {
        rows[[length(rows) + 1L]] <<- data.frame(
          kind = "add", node_id = node$id, coord = cn, cut = NA_real_,
          delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
        )
        next
      }
      xj <- X[[cn]][idx]
      yn <- y_centered[idx]
      cands <- sort(unique(xj))
      n_leq <- vapply(cands, function(c) sum(xj <= c), integer(1))
      n_gt  <- length(xj) - n_leq
      feasible_cand <- cands[n_leq >= min_leaf_n & n_gt >= min_leaf_n]

      if (length(feasible_cand) == 0L) {
        rows[[length(rows) + 1L]] <<- data.frame(
          kind = "add", node_id = node$id, coord = cn, cut = NA_real_,
          delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
        )
        next
      }

      sse_leaf <- sum(yn * yn) - sum(yn)^2 / length(yn)
      lid_left  <- rep(1L, length(idx))
      lid_right <- rep(2L, length(idx))
      res <- scan_cutoff(xj, yn, lid_left, lid_right, K = 2L, feasible_cand,
                          incumbent_cut = feasible_cand[[1L]])
      gain <- sse_leaf - res$sse

      rows[[length(rows) + 1L]] <<- data.frame(
        kind = "add", node_id = node$id, coord = cn, cut = res$cut,
        delta = -gain + lambda_sse, feasible = TRUE, stringsAsFactors = FALSE
      )
    }
    invisible()
  }
  walk(tree, character(0), 0L)
  if (length(rows) == 0L) return(.empty_perturb_df())
  do.call(rbind, rows)
}

#' RECOORD: swap the coordinate of every prunable split
#'
#' @description
#' The fourth one-split class the plan's original "move/add/delete" prose
#' missed (Oracle consult correction C3): at a PRUNABLE split (both
#' children leaves), re-optimize using a DIFFERENT coordinate entirely,
#' holding leaf count fixed (so \code{lambda_sse} cancels out of the
#' delta). Restricted to prunable nodes for the same reason DELETE is: a
#' coordinate swap at a non-prunable node would require re-optimizing both
#' subtrees to be a fair comparison, which is not a one-step edit.
#'
#' @keywords internal
perturb_recoord_deltas <- function(model, X, y, min_leaf_n = model@min_leaf_n) {
  if (is.matrix(X)) X <- as.data.frame(X)
  tree <- model@tree
  y <- as.numeric(y)
  y_centered <- y - mean(y)
  all_coords <- names(X)

  rows <- list()
  for (p in coord_tree_split_paths(tree)) {
    node <- coord_tree_node_at(tree, p)
    if (!identical(node$left$kind, "leaf") || !identical(node$right$kind, "leaf")) next

    idx <- coord_tree_rows_at(tree, X, p)
    l <- node$left$stats; r <- node$right$stats
    sse_current <- (l$sumsq - l$sum^2 / l$n) + (r$sumsq - r$sum^2 / r$n)

    other_coords <- setdiff(all_coords, node$coord)
    for (cn in other_coords) {
      if (length(idx) < 2L * min_leaf_n) {
        rows[[length(rows) + 1L]] <- data.frame(
          kind = "recoord", node_id = node$id, coord = cn, cut = NA_real_,
          delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
        )
        next
      }
      xj <- X[[cn]][idx]
      yn <- y_centered[idx]
      cands <- sort(unique(xj))
      n_leq <- vapply(cands, function(c) sum(xj <= c), integer(1))
      n_gt  <- length(xj) - n_leq
      feasible_cand <- cands[n_leq >= min_leaf_n & n_gt >= min_leaf_n]

      if (length(feasible_cand) == 0L) {
        rows[[length(rows) + 1L]] <- data.frame(
          kind = "recoord", node_id = node$id, coord = cn, cut = NA_real_,
          delta = NA_real_, feasible = FALSE, stringsAsFactors = FALSE
        )
        next
      }

      lid_left  <- rep(1L, length(idx))
      lid_right <- rep(2L, length(idx))
      res <- scan_cutoff(xj, yn, lid_left, lid_right, K = 2L, feasible_cand,
                          incumbent_cut = feasible_cand[[1L]])

      rows[[length(rows) + 1L]] <- data.frame(
        kind = "recoord", node_id = node$id, coord = cn, cut = res$cut,
        delta = res$sse - sse_current, feasible = TRUE, stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(.empty_perturb_df())
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

#' Certify local optimality of a `RefinedTreeModel` (Milestone B)
#'
#' @description
#' See \code{\link{local_optimality_certificate}} for the full contract,
#' scope, and the explicit \code{prop:greedy} limitation this certificate
#' does NOT overcome. In one line: passes iff no single MOVE, RECOORD, ADD,
#' or DELETE edit of \code{model} improves the penalized objective Stage A
#' minimized by more than a numerical tolerance.
#'
#' \code{certified} is NEVER \code{NA} -- every code path returns
#' \code{TRUE} or \code{FALSE} with a \code{reason} (only populated when
#' \code{FALSE}): \code{"improving_move"}, \code{"improving_recoord"},
#' \code{"improving_add"}, \code{"improving_delete"} (the worst offending
#' kind, from \code{worst$kind}); \code{"lambda_zero_uncapped"} (\code{add}
#' requested with \code{lambda_sse == 0} and neither \code{max_depth} nor
#' \code{max_leaves} binding -- almost any \code{add} then improves the
#' unpenalized objective, so this is reported as a structural
#' mis-specification rather than a confusing improving-add result);
#' \code{"vacuous_no_feasible_perturbations"} (every requested perturbation
#' kind was infeasible everywhere it was tried -- e.g. a single-leaf stump,
#' or \code{min_leaf_n} blocking every candidate -- a certificate that
#' "passes" only because nothing could be checked is a silent degradation,
#' so it is reported as a failure, not a pass); \code{"input_mismatch"}
#' (\code{X}/\code{y} inconsistent with \code{model}); \code{"degenerate_node"}
#' (a perturbation enumerator hit an internal invariant violation --
#' the underlying error is preserved in \code{notes}).
#'
#' @param model A [RefinedTreeModel].
#' @param X,y The SAME training data \code{model} was refined on (or, at
#'   minimum, data consistent with \code{model@coords}).
#' @param lambda,objective_scale,min_leaf_n,max_depth,max_leaves Default to
#'   the values recorded on \code{model} at [refine_tree()] time. Override
#'   only to intentionally certify against a DIFFERENT model class or
#'   penalty than \code{model} was actually fit under (rare; the model's
#'   own recorded values are the honest default).
#' @param perturbations Character vector, subset of
#'   \code{c("move","recoord","add","delete")} (default: all four). Restrict
#'   for speed, but every excluded kind narrows what the certificate can
#'   claim -- state which kinds were checked when reporting a pass.
#' @param tol_abs,tol_rel Absolute and relative (to the null-model SSE)
#'   numerical tolerance; a perturbation is "improving" only if it beats the
#'   incumbent by more than \code{max(tol_abs, tol_rel * sse_null)}. This is
#'   numerical-noise tolerance ONLY -- deliberately NOT a statistical-
#'   significance slack (that would double-count what \code{lambda} already
#'   does, and would convert a deterministic, data-computable statement into
#'   an unstated hypothesis test with no level -- exactly what
#'   \code{theory.tex} says this mechanism cannot assert).
#' @param detail Logical (default \code{FALSE}). If \code{TRUE}, include
#'   every evaluated perturbation (not just the worst per kind) in the
#'   returned \code{detail} list.
#' @return A list (class \code{"optimaltrees_local_certificate"}):
#'   \code{certified}, \code{reason}, \code{margin} (the worst-case delta;
#'   negative means an improving perturbation exists), \code{margin_rel}
#'   (\code{margin} scaled by the null-model SSE), \code{objective},
#'   \code{sse}, \code{n_leaves}, \code{lambda_sse}, \code{tol}, \code{worst}
#'   (list: \code{kind}, \code{node_id}, \code{coord}, \code{cut},
#'   \code{delta}), \code{by_kind} (data.frame: \code{kind},
#'   \code{n_evaluated}, \code{n_infeasible}, \code{min_delta},
#'   \code{argmin_node_id}), \code{detail} (per-kind data.frames, or
#'   \code{NULL} unless requested), \code{notes} (character vector).
#' @export
certify_local_optimality <- function(model, X, y,
                                      lambda = model@lambda,
                                      objective_scale = model@objective_scale,
                                      min_leaf_n = model@min_leaf_n,
                                      max_depth = model@max_depth,
                                      max_leaves = model@max_leaves,
                                      perturbations = c("move", "recoord", "add", "delete"),
                                      tol_abs = 1e-9, tol_rel = 1e-9,
                                      detail = FALSE) {
  if (!S7::S7_inherits(model, RefinedTreeModel)) {
    cli::cli_abort("certify_local_optimality: {.arg model} must be a RefinedTreeModel.")
  }
  if (is.matrix(X)) X <- as.data.frame(X)
  if (nrow(X) != length(y)) {
    cli::cli_abort("certify_local_optimality: nrow(X) must equal length(y).")
  }
  perturbations <- match.arg(
    perturbations, c("move", "recoord", "add", "delete"), several.ok = TRUE
  )

  missing_coords <- setdiff(model@coords, names(X))
  if (length(missing_coords) > 0L) {
    return(.certificate_result(
      FALSE, "input_mismatch", NA_real_, NA_real_, NA_real_, NA_real_,
      NA_integer_, NA_real_, NA_real_, NULL,
      data.frame(kind = character(0), n_evaluated = integer(0), n_infeasible = integer(0),
                 min_delta = numeric(0), argmin_node_id = integer(0)),
      list(), sprintf("X is missing coordinate(s): %s", paste(missing_coords, collapse = ", "))
    ))
  }

  lambda_sse <- .objective_lambda_sse(model, lambda, objective_scale)
  y <- as.numeric(y)
  sse_null <- sum((y - mean(y))^2)
  tol <- max(tol_abs, tol_rel * sse_null)

  sse <- coord_tree_training_sse(model@tree, X, y)
  n_leaves <- coord_tree_count_leaves(model@tree)
  objective <- sse + lambda_sse * n_leaves

  if ("add" %in% perturbations && lambda_sse == 0 &&
      length(max_depth) == 0L && length(max_leaves) == 0L) {
    return(.certificate_result(
      FALSE, "lambda_zero_uncapped", NA_real_, NA_real_, objective, sse, n_leaves,
      lambda_sse, tol, NULL,
      data.frame(kind = character(0), n_evaluated = integer(0), n_infeasible = integer(0),
                 min_delta = numeric(0), argmin_node_id = integer(0)),
      list(), "lambda_sse == 0 with no max_depth/max_leaves cap: almost any add \\
improves the unpenalized objective, so certification against 'add' is structurally \\
unattainable rather than a meaningful diagnostic."
    ))
  }

  by_kind_list <- list()
  worst <- list(kind = NA_character_, node_id = NA_integer_, coord = NA_character_,
                cut = NA_real_, delta = Inf)
  detail_list <- list()
  notes <- character(0)

  run_kind <- function(kind, fn) {
    d <- tryCatch(fn(), error = function(e) e)
    if (inherits(d, "error")) {
      notes <<- c(notes, sprintf("%s: %s", kind, conditionMessage(d)))
      return(NULL)
    }
    if (isTRUE(detail)) detail_list[[kind]] <<- d
    feas <- d[d$feasible, , drop = FALSE]
    by_kind_list[[kind]] <<- data.frame(
      kind = kind, n_evaluated = nrow(d), n_infeasible = sum(!d$feasible),
      min_delta = if (nrow(feas) > 0L) min(feas$delta) else NA_real_,
      argmin_node_id = if (nrow(feas) > 0L) feas$node_id[[which.min(feas$delta)]] else NA_integer_,
      stringsAsFactors = FALSE
    )
    if (nrow(feas) > 0L) {
      i <- which.min(feas$delta)
      if (feas$delta[[i]] < worst$delta) {
        worst <<- list(kind = kind, node_id = feas$node_id[[i]], coord = feas$coord[[i]],
                        cut = feas$cut[[i]], delta = feas$delta[[i]])
      }
    }
    invisible()
  }

  if ("move" %in% perturbations) {
    run_kind("move", function() perturb_move_deltas(model, X, y, min_leaf_n = min_leaf_n))
  }
  if ("recoord" %in% perturbations) {
    run_kind("recoord", function() perturb_recoord_deltas(model, X, y, min_leaf_n = min_leaf_n))
  }
  if ("add" %in% perturbations) {
    run_kind("add", function() {
      perturb_add_deltas(model, X, y, min_leaf_n = min_leaf_n,
                          max_depth = max_depth, max_leaves = max_leaves)
    })
  }
  if ("delete" %in% perturbations) {
    run_kind("delete", function() perturb_delete_deltas(model, lambda_sse = lambda_sse))
  }

  if (length(notes) > 0L && length(by_kind_list) < length(perturbations)) {
    return(.certificate_result(
      FALSE, "degenerate_node", NA_real_, NA_real_, objective, sse, n_leaves,
      lambda_sse, tol, NULL,
      if (length(by_kind_list)) do.call(rbind, by_kind_list) else
        data.frame(kind = character(0), n_evaluated = integer(0), n_infeasible = integer(0),
                   min_delta = numeric(0), argmin_node_id = integer(0)),
      detail_list, notes
    ))
  }

  by_kind <- if (length(by_kind_list) > 0L) do.call(rbind, by_kind_list) else
    data.frame(kind = character(0), n_evaluated = integer(0), n_infeasible = integer(0),
               min_delta = numeric(0), argmin_node_id = integer(0))

  n_feasible_total <- if (nrow(by_kind) > 0L) sum(by_kind$n_evaluated - by_kind$n_infeasible) else 0L
  if (n_feasible_total == 0L) {
    return(.certificate_result(
      FALSE, "vacuous_no_feasible_perturbations", NA_real_, NA_real_, objective, sse,
      n_leaves, lambda_sse, tol, NULL, by_kind, detail_list, notes
    ))
  }

  margin <- worst$delta
  certified <- margin >= -tol
  reason <- if (certified) {
    NA_character_
  } else {
    switch(worst$kind,
      move = "improving_move", recoord = "improving_recoord",
      add = "improving_add", delete = "improving_delete"
    )
  }

  .certificate_result(
    certified, reason, margin,
    margin_rel = if (sse_null > 0) margin / sse_null else NA_real_,
    objective = objective, sse = sse, n_leaves = n_leaves, lambda_sse = lambda_sse,
    tol = tol, worst = worst, by_kind = by_kind, detail = detail_list, notes = notes
  )
}
