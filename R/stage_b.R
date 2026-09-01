#' Stage B: coordinate-space tree representation
#'
#' @description
#' Stage A (the existing GOSDT/OSRT solver via [fit_tree()]/[optimaltrees()])
#' fits over a discretized BINARY design matrix, so its split thresholds are
#' floored at the quantile grid it discretized on -- error \eqn{O(n^{-\rho})}
#' at the grid mesh, no matter how large \code{n} gets. Stage B removes that
#' floor by moving each threshold to an exact, off-grid data value: for each
#' split, identify a bracket bounded only by the nearest OTHER split on the
#' same coordinate in that node's own subtree (the bracket's job is
#' IDENTIFICATION -- isolating one true boundary from any other -- not
#' localization), then scan every observed value of that coordinate strictly
#' inside the bracket and keep whichever exact value minimizes empirical risk.
#'
#' This file provides the coordinate-space tree representation Stage B
#' operates on, and the machinery to build one from a fitted
#' \code{OptimalTreesModel} ([as_coordinate_tree()]) and to predict from one
#' ([coord_tree_predict()]). Threshold refinement itself
#' ([refine_tree_cuts()], not yet implemented as of this file) and
#' transition-leaf collapse (\code{collapse_transitions()}, not yet
#' implemented) are separate, later pieces of the same mechanism -- see
#' \code{quality_reports/plans/2026-09-01_two-stage-package-defaults-session.md}
#' Milestone A in the \code{global-scholars} project for the full design and
#' its provenance (an Oracle-consulted architecture decision, verified
#' empirically before being accepted -- see that file for what was checked
#' and why).
#'
#' \strong{Scope, 2026-09-01 (Milestone A): regression (\code{squared_error})
#' only, disclosed as such, not silently unsupported.} Classification support
#' is deferred to a later milestone.
#'
#' \strong{Why a new representation, not the fitted model's own binary-feature
#' JSON tree.} The fitted tree's split nodes reference an absolute 0-based
#' column index into the discretized binary design matrix. The SAME column
#' index can be (and empirically is, confirmed by direct inspection of a real
#' fit before this design was accepted) split on by more than one node,
#' including two sibling subtrees -- so once refinement gives those two nodes
#' DIFFERENT off-grid cut values, one stored per-coordinate threshold in
#' \code{@discretization_metadata} cannot represent both. A distinct
#' coordinate-space tree, where every split node carries its OWN real-valued
#' \code{cut} rather than sharing a metadata-level threshold, has no such
#' collision.
#'
#' \strong{Node schema} (nested list; every node has \code{kind} and a unique,
#' pre-order-assigned integer \code{id}):
#' \preformatted{
#' split: list(kind="split", id=<int>, coord=<chr>, cut=<dbl>,
#'             grid_cuts=<dbl[]>, collapsed=<lgl>, k_lo=<int|NA>, k_hi=<int|NA>,
#'             left=<node>, right=<node>)   # left := X[[coord]] <= cut
#' leaf:  list(kind="leaf", id=<int>, prediction=<dbl>, n=<int>,
#'             stats=list(n=, sum=, sumsq=), original_prediction=<dbl>)
#' }
#' \code{grid_cuts} records the ORIGINAL grid threshold value(s) this split
#' came from (length 1 for an ordinary split, length 2 for a collapsed
#' transition-leaf pair, once \code{collapse_transitions()} exists).
#' \code{stats} on leaves is the seam for a later classification variant (a
#' second accumulator, not a new schema). \code{original_prediction} is
#' provenance: the fitted model's OWN stored leaf value, used as a
#' cross-check when the coordinate tree is first built (see
#' [as_coordinate_tree()]) and otherwise unused.
#'
#' @keywords internal
#' @name stage_b
NULL

#' Positional lookup from binary column index to (original coordinate, threshold)
#'
#' @description
#' [fit_tree()]/[optimaltrees()] discretize each continuous coordinate into
#' indicator columns \eqn{1\{X_j \le \tau_{j,k}\}} and the fitted tree refers
#' to them by a ZERO-BASED integer index into that binary design matrix. This
#' rebuilds the mapping.
#'
#' The rebuild is POSITIONAL because it mirrors exactly how the binary matrix
#' is assembled by [discretize_features()] (a loop over \code{names(X)}, and
#' within each coordinate a loop over its thresholds, combined with
#' \code{cbind}). It is then cross-checked against \code{md$binary_names}, so
#' a change in that assembly order fails loudly here rather than silently
#' mislabelling which coordinate a split is on.
#'
#' @param md A model's \code{@discretization_metadata}.
#' @return Data frame with one row per binary column: \code{coord}, \code{k}
#'   (\code{NA} for a non-continuous/binary column), \code{cut} (\code{NA}
#'   likewise).
#' @keywords internal
bin_lookup <- function(md) {
  if (isTRUE(md$all_binary)) {
    cli::cli_abort(
      "bin_lookup: metadata says all features are binary -- there are no \\
       continuous thresholds to look up, and nothing for Stage B to refine."
    )
  }
  rows <- list()
  for (cn in names(md$features)) {
    fm <- md$features[[cn]]
    if (identical(fm$type, "continuous")) {
      for (k in seq_along(fm$thresholds)) {
        rows[[length(rows) + 1L]] <- data.frame(
          coord = cn, k = k, cut = fm$thresholds[[k]],
          new_name = fm$new_names[[k]], stringsAsFactors = FALSE
        )
      }
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        coord = cn, k = NA_integer_, cut = NA_real_,
        new_name = fm$new_names[[1L]], stringsAsFactors = FALSE
      )
    }
  }
  lk <- do.call(rbind, rows)

  # Assert the positional reconstruction lines up with the recorded column names.
  if (nrow(lk) != length(md$binary_names)) {
    cli::cli_abort(paste0(
      "bin_lookup: rebuilt ", nrow(lk), " binary columns but metadata records ",
      length(md$binary_names), ". Binary-matrix assembly order has changed."
    ), call. = FALSE)
  }
  ok <- mapply(function(nm, suffix) endsWith(nm, suffix),
               md$binary_names, lk$new_name)
  if (!all(ok)) {
    bad <- which(!ok)[1L]
    cli::cli_abort(paste0(
      "bin_lookup: binary column ", bad, " is named '", md$binary_names[[bad]],
      "' but was reconstructed as '", lk$new_name[[bad]],
      "'. Binary-matrix assembly order has changed."
    ), call. = FALSE)
  }
  lk
}

#' Build the raw coordinate-space node structure (ids assigned, no leaf stats yet)
#'
#' @description
#' Internal helper for [as_coordinate_tree()]. Walks a fitted JSON tree node
#' (from \code{model@trees[[tree_index]]}) and produces the parallel
#' coordinate-space structure, assigning every node a pre-order integer
#' \code{id} as it goes. Leaves get \code{original_prediction} (the fitted
#' model's own stored value) but not yet \code{prediction}/\code{n}/
#' \code{stats} -- those require a second pass over training data, done by
#' [as_coordinate_tree()] after this returns, because computing a leaf's row
#' set requires the FULL structure to exist first (root-to-leaf routing),
#' not the reverse.
#'
#' In the fitted JSON tree the split is always \code{binary_feature == 1},
#' and the binary feature is the indicator \eqn{1\{X_j \le \tau\}}, so the
#' fitted \code{$true} branch is the \eqn{\{X_j \le \tau\}} side --
#' asserted, not assumed, via the \code{relation}/\code{reference} checks
#' below.
#'
#' @param node A node of \code{model@trees[[tree_index]]}.
#' @param lk Output of [bin_lookup()].
#' @param md A model's \code{@discretization_metadata}.
#' @return Nested list per the node schema documented in \code{\link{stage_b}}
#'   (leaves missing \code{prediction}/\code{n}/\code{stats}).
#' @keywords internal
build_coord_node <- function(node, lk, md) {
  next_id <- local({
    counter <- 0L
    function() {
      counter <<- counter + 1L
      counter
    }
  })

  recurse <- function(node) {
    id <- next_id()

    if (!is.null(node$prediction)) {
      return(list(kind = "leaf", id = id,
                  original_prediction = as.numeric(node$prediction)))
    }
    if (is.null(node$feature)) {
      cli::cli_abort("build_coord_node: tree node has neither a prediction nor a feature.")
    }
    # These two are what make the branch orientation below correct; assert, do not assume.
    if (!identical(node$relation, "==")) {
      cli::cli_abort("build_coord_node: expected relation '==' on a binary feature, got {node$relation}.")
    }
    if (!identical(as.numeric(node$reference), 1)) {
      cli::cli_abort("build_coord_node: expected reference 1 on a binary feature, got {node$reference}.")
    }

    j <- as.integer(node$feature) + 1L   # fitted index is ZERO-based
    if (j < 1L || j > nrow(lk)) {
      cli::cli_abort("build_coord_node: split feature index {node$feature} is outside the binary design matrix.")
    }
    coord <- lk$coord[[j]]
    k     <- lk$k[[j]]
    if (is.na(k)) {
      cli::cli_abort("build_coord_node: split lands on a non-continuous column ('{coord}'); nothing to refine.")
    }
    cut <- md$features[[coord]]$thresholds[[k]]

    list(
      kind = "split", id = id, coord = coord, cut = cut,
      grid_cuts = cut, collapsed = FALSE, k_lo = k, k_hi = NA_integer_,
      left  = recurse(node$true),
      right = recurse(node$false)
    )
  }

  recurse(node)
}

#' Leaf `id` reached by each row of `X` under a coordinate-space tree
#'
#' @param tree A coordinate-space tree (see \code{\link{stage_b}} for the
#'   node schema).
#' @param X Data.frame with the coordinates the tree splits on, as raw
#'   (non-discretized) values.
#' @return Integer vector of length \code{nrow(X)}, one leaf \code{id} per row.
#' @keywords internal
coord_tree_assign <- function(tree, X) {
  n <- nrow(X)
  out <- integer(n)

  walk <- function(node, idx) {
    if (length(idx) == 0L) return(invisible())
    if (identical(node$kind, "leaf")) {
      out[idx] <<- node$id
      return(invisible())
    }
    go_left <- X[[node$coord]][idx] <= node$cut
    walk(node$left,  idx[go_left])
    walk(node$right, idx[!go_left])
  }

  walk(tree, seq_len(n))
  out
}

#' Named vector of leaf predictions, keyed by leaf `id` (as character)
#'
#' @param tree A coordinate-space tree with \code{prediction} populated at
#'   every leaf (i.e. after [as_coordinate_tree()], not the raw structure
#'   from [build_coord_node()] alone).
#' @return Named numeric vector; names are \code{as.character(id)}.
#' @keywords internal
coord_tree_leaf_predictions <- function(tree) {
  out <- list()
  walk <- function(node) {
    if (identical(node$kind, "leaf")) {
      out[[as.character(node$id)]] <<- node$prediction
      return(invisible())
    }
    walk(node$left)
    walk(node$right)
  }
  walk(tree)
  unlist(out)
}

#' Fitted values from a coordinate-space tree (regression only)
#'
#' @param tree A coordinate-space tree with \code{prediction} populated at
#'   every leaf.
#' @param X Data.frame with the coordinates the tree splits on, as raw
#'   (non-discretized) values.
#' @return Numeric vector of length \code{nrow(X)}.
#' @keywords internal
coord_tree_predict <- function(tree, X) {
  ids <- coord_tree_assign(tree, X)
  preds <- coord_tree_leaf_predictions(tree)
  unname(preds[as.character(ids)])
}

#' Convert a fitted `OptimalTreesModel` into a coordinate-space tree
#'
#' @description
#' Builds the coordinate-space representation [stage_b] documents, then
#' populates every leaf's \code{n}/\code{stats}/\code{prediction} from the
#' supplied training data -- and cross-checks the data-derived leaf mean
#' against the fitted model's OWN stored leaf value as a correctness gate.
#' `X`/`y` are required explicitly (not read from \code{model@X_train}/
#' \code{@y_train}, even when those happen to be populated): silently
#' refining against data that does not actually match what the model was fit
#' on would invalidate every downstream number, so this function verifies
#' the supplied data reconciles with the model rather than trusting it (see
#' the two \code{cli_abort} checks below) -- trusting stored training data
#' unconditionally would skip exactly that check.
#'
#' @param model An \code{OptimalTreesModel} (S7), fit with
#'   \code{loss_function = "squared_error"} (Milestone A scope; see
#'   \code{\link{stage_b}}).
#' @param X,y The training data `model` was fit on. `X` as raw (continuous),
#'   not pre-discretized, covariates.
#' @param tree_index Which tree in \code{model@trees} to convert (default
#'   \code{1L}; only relevant for a Rashomon set).
#' @return A coordinate-space tree (nested list; see \code{\link{stage_b}}
#'   for the node schema), fully populated (every leaf has \code{prediction},
#'   \code{n}, \code{stats}, and \code{original_prediction}).
#' @keywords internal
as_coordinate_tree <- function(model, X, y, tree_index = 1L) {
  if (!S7::S7_inherits(model, OptimalTreesModel)) {
    cli::cli_abort("as_coordinate_tree: {.arg model} must be an OptimalTreesModel.")
  }
  if (!identical(model@loss_function, "squared_error")) {
    cli::cli_abort(c(
      "as_coordinate_tree: only {.val squared_error} models are supported.",
      "i" = "Got {.val {model@loss_function}}. Stage B's threshold-refinement \\
             scan has no classification variant yet -- see \\
             quality_reports/plans/2026-09-01_two-stage-package-defaults-\\
             session.md (Milestone A scope) in the global-scholars project."
    ))
  }
  if (model@n_trees == 0L) {
    cli::cli_abort("as_coordinate_tree: {.arg model} has no trees.")
  }
  tree_index <- as.integer(tree_index)
  if (is.na(tree_index) || tree_index < 1L || tree_index > model@n_trees) {
    cli::cli_abort(
      "as_coordinate_tree: {.arg tree_index} = {tree_index} is out of range \\
       (model has {model@n_trees} tree(s))."
    )
  }
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("as_coordinate_tree: {.arg X} must be a data.frame or matrix.")
  }
  if (is.matrix(X)) X <- as.data.frame(X)
  if (nrow(X) != length(y)) {
    cli::cli_abort(
      "as_coordinate_tree: nrow(X) ({nrow(X)}) must equal length(y) ({length(y)})."
    )
  }

  md <- model@discretization_metadata
  if (is.null(md)) {
    cli::cli_abort(
      "as_coordinate_tree: {.arg model} has no @discretization_metadata -- \\
       was it fit on already-binary features? There is nothing for Stage B \\
       to refine."
    )
  }

  # Verify X reconciles with the model's own discretization -- structurally
  # (column names/order), not just by trusting the caller. This is exactly
  # the check Oracle's consult (see the plan file's Milestone A addendum)
  # flagged as required: "silently refining against the wrong data is the
  # failure mode that would invalidate every downstream number."
  X_binary_check <- apply_discretization(X, md)
  expected_names <- md$binary_names
  if (!identical(colnames(X_binary_check), expected_names)) {
    cli::cli_abort(c(
      "as_coordinate_tree: {.arg X} does not reconcile with {.arg model}'s own discretization.",
      "i" = "Re-discretizing {.arg X} against the model's stored metadata \\
             produced columns {.val {colnames(X_binary_check)}}, but the \\
             model was fit on {.val {expected_names}}.",
      "i" = "This usually means X is not the same data (or feature \\
             set/order) the model was actually fit on."
    ))
  }

  lk <- bin_lookup(md)
  raw_tree <- build_coord_node(model@trees[[tree_index]], lk, md)

  leaf_ids <- coord_tree_assign(raw_tree, X)
  y <- as.numeric(y)

  attach_stats <- function(node) {
    if (identical(node$kind, "leaf")) {
      rows <- which(leaf_ids == node$id)
      n_leaf <- length(rows)
      if (n_leaf == 0L) {
        cli::cli_abort(c(
          "as_coordinate_tree: leaf {node$id} received zero training rows \\
           under the supplied X.",
          "i" = "A correctly-reproduced fit should never leave a leaf empty \\
                 on its own training data -- this means X/y do not match \\
                 what {.arg model} was actually fit on."
        ))
      }
      yl <- y[rows]
      s  <- sum(yl)
      ss <- sum(yl * yl)
      pred <- s / n_leaf
      # ABSOLUTE, not relative, tolerance: confirmed by direct inspection of
      # a raw fitted tree's JSON that the C++ solver serializes leaf
      # predictions rounded to a fixed number of DECIMAL PLACES (~6dp,
      # observed absolute error up to ~3e-7), not significant figures. A
      # relative-tolerance check (base all.equal()'s default) fails on
      # small-magnitude leaf means even though the discrepancy is exactly
      # this known, harmless serialization rounding -- e.g. a leaf mean of
      # -0.0026303 stored as "-0.00263" is a ~1e-4 RELATIVE difference but
      # only ~3e-7 in absolute terms. 1e-5 comfortably covers the observed
      # rounding with margin; a genuine X/y mismatch would differ by orders
      # of magnitude more than this.
      if (abs(pred - node$original_prediction) >= 1e-5) {
        cli::cli_abort(c(
          "as_coordinate_tree: leaf {node$id}'s data-derived mean ({pred}) \\
           disagrees with the fitted model's own stored prediction \\
           ({node$original_prediction}).",
          "i" = "This means X/y do not match what {.arg model} was actually \\
                 fit on, or there is a bug in the coordinate-tree conversion \\
                 -- not something to paper over."
        ))
      }
      node$n <- n_leaf
      node$prediction <- pred
      node$stats <- list(n = n_leaf, sum = s, sumsq = ss)
      return(node)
    }
    node$left  <- attach_stats(node$left)
    node$right <- attach_stats(node$right)
    node
  }

  attach_stats(raw_tree)
}

# ---------------------------------------------------------------------------
# Tree-structure utilities shared by collapse_transitions() and (later)
# refine_tree_cuts(). Operate on the coordinate-space schema documented in
# `\link{stage_b}`, addressed by a "path" -- a character vector of
# "left"/"right" steps from the root (root itself is `character(0)`).
# ---------------------------------------------------------------------------

#' Number of split (internal) nodes in a coordinate-space tree
#'
#' Used as a structural termination bound for [collapse_transitions()]: each
#' collapse round removes at least one split node, so this count is a real
#' upper bound on the number of rounds needed, unlike a fixed magic number
#' calibrated to one DGP's tree size.
#'
#' @param node A coordinate-space (sub)tree.
#' @return Integer count.
#' @keywords internal
coord_tree_count_splits <- function(node) {
  if (identical(node$kind, "leaf")) return(0L)
  1L + coord_tree_count_splits(node$left) + coord_tree_count_splits(node$right)
}

#' Paths to every split node of a coordinate-space tree, root first
#'
#' @param node A coordinate-space (sub)tree.
#' @param path Internal recursion accumulator; leave at the default.
#' @return List of character-vector paths (each a sequence of "left"/"right").
#' @keywords internal
coord_tree_split_paths <- function(node, path = character(0)) {
  if (identical(node$kind, "leaf")) return(list())
  c(list(path),
    coord_tree_split_paths(node$left,  c(path, "left")),
    coord_tree_split_paths(node$right, c(path, "right")))
}

#' Get the node at `path` (see [coord_tree_split_paths()] for the path format)
#' @keywords internal
coord_tree_node_at <- function(tree, path) {
  for (step in path) tree <- tree[[step]]
  tree
}

#' Return `tree` with the node at `path` replaced by `value`
#' @keywords internal
coord_tree_set_node_at <- function(tree, path, value) {
  if (length(path) == 0L) return(value)
  tree[[path[[1L]]]] <- coord_tree_set_node_at(tree[[path[[1L]]]], path[-1L], value)
  tree
}

#' Detect and collapse transition-leaf pairs (`prop:transition-leaves`)
#'
#' @description
#' A true boundary that lands interior to one grid atom forces a
#' grid-optimal Stage-A tree to split the SAME coordinate at both grid
#' cutpoints bracketing that atom, sandwiching a thin spurious leaf (the
#' "transition leaf") in between -- an extra leaf that is not estimating a
#' second true boundary, but half of the machinery for locating one. This
#' function detects that pattern and merges each pair into a single node
#' spanning both original grid cutpoints, so a later identifying-bracket
#' computation naturally treats the pair as one identification problem with
#' no special-casing anywhere else in Stage B.
#'
#' Detection is by GRID ADJACENCY (consecutive \code{k} indices on the same
#' coordinate, immediate parent/child, matching side) -- NOT by leaf mass,
#' which at ~n/n_bins observations is not "very low" and would both miss
#' real pairs and flag ordinary small leaves. Adjacency is exactly what the
#' separation-floor assumption (theory.tex's clause (Sep)) plus mesh =
#' o(separation) guarantees: two same-coordinate cuts one grid atom apart
#' cannot be two DISTINCT true boundaries. If a DGP ever has two true
#' boundaries on one coordinate closer than one grid atom, this function is
#' wrong by construction -- the chained-pair abort below is the only
#' tripwire for that, and it is a real, reachable case at depth >= 3, not
#' merely a defensive placeholder: two true boundaries in NEARBY (not the
#' same) grid atoms produce two DISTINCT transition-collapse candidates
#' whose k-ranges are themselves adjacent, and detecting/erroring on that is
#' this function's job, not something a depth cap coincidentally rules out.
#' A same-coordinate split separated by an INTERVENING split on a DIFFERENT
#' coordinate is correctly NOT treated as a pair, because the pairwise check
#' below only ever looks at a node's DIRECT child -- see
#' \code{test-stage-b.R}'s negative test for this.
#'
#' \strong{The node sandwiched between a candidate pair being itself a split
#' (not a leaf) is NOT a rare, exotic case reserved for pathological DGPs --
#' confirmed empirically (2026-09-01) on an ordinary two-informative-
#' covariate fit at \code{max_depth = 3}.} The transition band between two
#' adjacent same-coordinate grid cuts can itself contain enough observations,
#' and enough genuine remaining structure from a DIFFERENT coordinate, that
#' the solver profitably splits again inside it -- e.g. a further split on
#' \eqn{X_2} nested inside an \eqn{X_1} transition band, producing two leaves
#' with clearly different fitted values, not one thin uninformative sliver.
#' That is real structure, not a grid artifact, and collapsing across it
#' would destroy it -- exactly why this function ERRORS on that pattern
#' rather than silently merging regardless of what is inside. Callers
#' should expect to see this error in ordinary use once more than one
#' coordinate carries signal and depth exceeds 2, not treat it as a bug
#' report; there is no handling for it yet (a scoped, later milestone), and
#' the honest response until there is one is to stop, not guess.
#'
#' @param tree Coordinate-space tree from [as_coordinate_tree()], UNREFINED
#'   (cuts are still exactly the grid values [as_coordinate_tree()] assigned;
#'   this function must run before any refinement moves them, per the
#'   grid-adjacency detection rule above).
#' @param max_iter Safety cap on collapse rounds. Default \code{NULL}
#'   resolves to [coord_tree_count_splits()] -- a real structural bound
#'   (every round removes >=1 split node), not a fixed number calibrated to
#'   one DGP's tree size. Hitting this cap indicates a bug in the detector,
#'   not a legitimate input; it is not meant to be tuned by callers.
#' @return List(tree, n_collapses).
#' @keywords internal
collapse_transitions <- function(tree, max_iter = NULL) {
  if (is.null(max_iter)) {
    max_iter <- coord_tree_count_splits(tree)
  }
  n_collapses <- 0L
  for (iter in seq_len(max(max_iter, 1L))) {
    paths <- coord_tree_split_paths(tree)
    did_collapse <- FALSE

    for (p in paths) {
      P <- coord_tree_node_at(tree, p)
      j <- P$coord

      for (side in c("right", "left")) {
        C <- P[[side]]
        if (!identical(C$kind, "split") || !identical(C$coord, j)) next

        if (isTRUE(P$collapsed) || isTRUE(C$collapsed)) {
          cli::cli_abort(c(
            "collapse_transitions: node id {P$id} (coord {.val {j}}) is \\
             adjacent, on its {.val {side}} side, to a split that is ALREADY \\
             part of a collapsed transition pair (node id {C$id}).",
            "i" = "This is a chained (three-way, or deeper) collapse: two \\
                   distinct transition-leaf collapses whose grid-index \\
                   ranges are themselves adjacent. It is a real, reachable \\
                   case once search depth exceeds 2 (two true boundaries in \\
                   NEARBY, not the same, grid atoms) -- not a depth-cap \\
                   coincidence to wave away.",
            "i" = "Not supported: a three-node merge would widen the \\
                   identifying bracket and merge two leaves, behavior this \\
                   collapse rule and the single-pass root-first refinement \\
                   it feeds were not derived for. Handling it silently \\
                   would be an unproven claim; this error is the honest \\
                   alternative until it is."
          ))
        }
        k_parent <- P$k_lo; k_child <- C$k_lo
        d <- k_child - k_parent
        if (side == "right" && d == -1L) {
          cli::cli_abort(c(
            "collapse_transitions: degenerate split at node id {C$id}.",
            "i" = "Its right child cuts BELOW its parent (id {P$id}) on \\
                   coordinate {.val {j}} (k_parent = {k_parent}, k_child = \\
                   {k_child}), meaning the {{X <= cut}} child is empty.",
            "i" = "This is a bin_lookup()/branch-orientation bug, not a \\
                   transition leaf."
          ))
        }
        if (side == "left" && d == 1L) {
          cli::cli_abort(c(
            "collapse_transitions: degenerate split at node id {C$id}.",
            "i" = "Its left child cuts ABOVE its parent (id {P$id}) on \\
                   coordinate {.val {j}} (k_parent = {k_parent}, k_child = \\
                   {k_child}), meaning the {{X > cut}} child is empty.",
            "i" = "This is a bin_lookup()/branch-orientation bug, not a \\
                   transition leaf."
          ))
        }
        is_pair <- (side == "right" && d == 1L) || (side == "left" && d == -1L)
        if (!is_pair) next   # different, genuinely distinct boundary

        # The transition leaf sits between P's cut and C's cut: it is C's
        # LEFT child if C is P's right child (interval (tau_k, tau_{k+1}]),
        # or C's RIGHT child if C is P's left child (interval
        # (tau_{k-1}, tau_k]).
        trans_side <- if (side == "right") "left" else "right"
        L <- C[[trans_side]]
        if (!identical(L$kind, "leaf")) {
          same_coord <- identical(L$coord, j)
          cli::cli_abort(c(
            "collapse_transitions: the node sandwiched between id {P$id} \\
             and id {C$id} (coord {.val {j}}) is itself a split (id \\
             {L$id}, coord {.val {L$coord}}), not a leaf.",
            "i" = if (same_coord) {
              "This is a chained (three-way, or deeper) same-coordinate \\
               collapse -- see the collapsed-node-adjacency abort above for \\
               why this is not merely a depth-cap coincidence and not \\
               handled silently."
            } else {
              "The transition band between these two grid cuts contains a \\
               genuine further split on a DIFFERENT coordinate -- not a \\
               grid artifact to merge away, but real structure this \\
               function has no rule for collapsing across. This is an \\
               expected outcome once more than one coordinate carries \\
               signal and depth exceeds 2, not a bug report."
            }
          ))
        }

        k_lo <- min(k_parent, k_child); k_hi <- max(k_parent, k_child)
        tau_lo <- min(P$cut, C$cut); tau_hi <- max(P$cut, C$cut)
        collapsed_node <- list(
          kind = "split", id = P$id, coord = j,
          k_lo = k_lo, k_hi = k_hi,
          grid_cuts = c(tau_lo, tau_hi),
          cut = mean(c(tau_lo, tau_hi)),   # starting value only; refinement moves it
          collapsed = TRUE,
          left  = if (side == "right") P$left  else C$left,
          right = if (side == "right") C$right else P$right
        )
        tree <- coord_tree_set_node_at(tree, p, collapsed_node)
        n_collapses  <- n_collapses + 1L
        did_collapse <- TRUE
        break
      }
      if (did_collapse) break   # re-derive coord_tree_split_paths() fresh before the next collapse
    }
    if (!did_collapse) return(list(tree = tree, n_collapses = n_collapses))
  }
  cli::cli_abort(
    "collapse_transitions: did not reach a fixed point within {max_iter} \\
     iterations (a structural bound on the number of split nodes) -- this \\
     indicates a bug in the detector, not a legitimate input."
  )
}
