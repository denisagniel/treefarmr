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
      # Classed (2026-09-01, Milestone E): a caller orchestrating a search
      # over multiple discretization resolutions (fit_twostage()'s m-ladder)
      # needs to distinguish "the solver picked a binary-passthrough column
      # to split on -- a different m will not help, since the column is
      # binary regardless of resolution" from an ordinary R error. Escalate
      # at most once on this class, then stop (Oracle consult, plan file's
      # Milestone E addendum, Q5.4) -- not a retriable-forever condition.
      cli::cli_abort(
        "build_coord_node: split lands on a non-continuous column ('{coord}'); nothing to refine.",
        class = "optimaltrees_stage_b_binary_split"
      )
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
      # COMBINED absolute+relative tolerance (the standard atol + rtol *
      # |value| pattern), not either alone -- confirmed necessary by direct
      # measurement across magnitudes, not assumed. The C++ solver's own
      # stored leaf values disagree with an independently-computed R-side
      # mean by an amount that scales with magnitude in a way neither a
      # fixed absolute nor a fixed relative tolerance alone covers safely:
      # observed ~3e-7 ABSOLUTE at a leaf mean near zero (-0.0026303 stored
      # as -0.00263 -- a LARGE ~1e-4 relative difference a relative-only
      # check would wrongly reject), and observed ~0.03 ABSOLUTE at a leaf
      # mean of ~1e6 (1000001.0293 stored as the bare integer 1000001 -- a
      # tiny ~3e-8 relative difference a fixed-1e-5-absolute check would
      # wrongly reject, discovered when testing the y-centering invariant
      # at a deliberately large offset). Both observations are comfortably
      # explained by ~1e-7 relative precision (consistent with the solver
      # storing/accumulating leaf values in single, not double, precision
      # internally) -- atol=1e-4 + rtol=1e-4 covers both with a full 3
      # orders of magnitude of margin, while a genuine X/y mismatch would
      # differ by far more than either term.
      atol <- 1e-4; rtol <- 1e-4
      tol <- atol + rtol * max(abs(pred), abs(node$original_prediction))
      if (abs(pred - node$original_prediction) > tol) {
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
          # Classed (2026-09-01, Milestone E): this is the "ordinary case, a
          # different grid resolution may resolve it" refusal (Oracle
          # consult, plan file's Milestone E addendum, sub-step E0b) --
          # distinct from the degenerate-split aborts below, which indicate
          # a genuine bin_lookup()/branch-orientation BUG and must NOT be
          # silently retried by a caller escalating m.
          ), class = "optimaltrees_collapse_unsupported")
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
          # Classed (2026-09-01, Milestone E): same retriable-refusal class
          # as the adjacency abort above -- see that comment.
          ), class = "optimaltrees_collapse_unsupported")
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

# ---------------------------------------------------------------------------
# Off-grid threshold refinement (refine_tree_cuts()). Run AFTER
# collapse_transitions() on the same tree -- refinement must never see a
# still-uncollapsed transition pair, since collapse's detection relies on
# grid-exact cuts (see collapse_transitions()'s roxygen).
# ---------------------------------------------------------------------------

#' Number of leaves in a coordinate-space (sub)tree
#' @keywords internal
coord_tree_count_leaves <- function(node) {
  if (identical(node$kind, "leaf")) return(1L)
  coord_tree_count_leaves(node$left) + coord_tree_count_leaves(node$right)
}

#' Position (1..K) of the leaf each row of `X` reaches under a (sub)tree
#'
#' Bookkeeping-only positional index (NOT the node schema's own `id`) for
#' [scan_cutoff()]'s per-leaf running sufficient statistics arrays, which
#' only need a dense 1..K index local to the two subtrees being compared at
#' one split -- ported near-verbatim from the original.
#'
#' @keywords internal
coord_tree_leaf_index <- function(node, X) {
  if (identical(node$kind, "leaf")) return(rep(1L, nrow(X)))
  n_left <- coord_tree_count_leaves(node$left)
  ifelse(X[[node$coord]] <= node$cut,
         coord_tree_leaf_index(node$left, X),
         n_left + coord_tree_leaf_index(node$right, X))
}

#' Row indices of `X` reaching the node at `path`, under the tree's CURRENT cuts
#'
#' Walks from the root using whatever cut each ancestor CURRENTLY holds
#' (grid value if not yet refined this pass, refined value if it is) -- see
#' [refine_tree_cuts()]'s roxygen for why this asymmetry is safe and
#' intentional, not a bug.
#'
#' @keywords internal
coord_tree_rows_at <- function(tree, X, path) {
  idx <- seq_len(nrow(X))
  node <- tree
  for (step in path) {
    keep <- if (identical(step, "left")) {
      X[[node$coord]][idx] <= node$cut
    } else {
      X[[node$coord]][idx] > node$cut
    }
    idx <- idx[keep]
    node <- node[[step]]
  }
  idx
}

#' All cuts on `coord` anywhere within a coordinate-space subtree
#'
#' Used by [node_bracket()] to find the nearest OTHER split on the same
#' coordinate inside a node's own children.
#'
#' @keywords internal
coord_tree_cuts_on <- function(node, coord) {
  if (identical(node$kind, "leaf")) return(numeric(0))
  c(if (identical(node$coord, coord)) node$cut else numeric(0),
    coord_tree_cuts_on(node$left,  coord),
    coord_tree_cuts_on(node$right, coord))
}

#' Identifying bracket for the split at `path`
#'
#' @description
#' The nearest other same-coordinate cut in this node's OWN subtree, open at
#' both ends (+/-Inf if none). This bounds IDENTIFICATION -- isolating
#' exactly one true boundary from any other -- not LOCALIZATION, which is
#' [scan_cutoff()]'s job. Ancestor bounds are not computed here because the
#' row set [refine_tree_cuts()] passes in is already restricted to the
#' ancestor-consistent range (via [coord_tree_rows_at()]) -- adding an
#' explicit ancestor bound here would be redundant, not merely harmless.
#'
#' @param tree Full coordinate-space tree (current -- possibly partially
#'   refined by earlier, shallower calls in the same [refine_tree_cuts()] pass).
#' @param path Path to the split node.
#' @return List(lo, hi): an open interval, +/-Inf at either end if no
#'   bounding descendant cut exists on that side.
#' @keywords internal
node_bracket <- function(tree, path) {
  node  <- coord_tree_node_at(tree, path)
  below <- coord_tree_cuts_on(node$left,  node$coord)
  above <- coord_tree_cuts_on(node$right, node$coord)
  list(lo = if (length(below)) max(below) else -Inf,
       hi = if (length(above)) min(above) else  Inf)
}

#' Candidate cutoffs for a node: observed values strictly inside the bracket
#'
#' @description
#' \code{br} (from [node_bracket()]) is the identifying bracket: open at
#' both ends, strict inequalities. A candidate EQUAL to a bounding
#' descendant cut would empty one of that descendant's children, so it is
#' excluded, not merely redundant. The node's own grid cutpoint(s)
#' (\code{node$grid_cuts} -- length 1 normally, length 2 if
#' \code{collapsed}) are always added as candidates when they fall inside
#' the bracket, so [refine_tree_cuts()] never scores worse than its
#' pre-refinement starting point at an ORDINARY node (this guarantee is
#' intentionally NOT claimed at a \code{collapsed} node -- see
#' [collapse_transitions()]'s docs: the collapsed starting cut is a
#' midpoint, not a value that was ever actually fit).
#'
#' @keywords internal
bracket_candidates <- function(xj, node, br) {
  v  <- xj[xj > br$lo & xj < br$hi]
  gc <- node$grid_cuts
  gc <- gc[gc > br$lo & gc < br$hi]
  sort(unique(c(v, gc)))
}

#' Exact empirical-risk-minimizing cutoff over observed values inside a bracket
#'
#' @description
#' Sweeps the candidate cut upward. At each distinct candidate value all
#' rows at or below it belong to the left subtree, so rows migrate from the
#' right subtree's leaves to the left subtree's leaves monotonically and
#' per-leaf sufficient statistics update incrementally -- O(\eqn{|S| \log|S|}
#' + \code{#candidates}) per node instead of O(\eqn{|S|} * \code{#candidates}).
#' Ties in \code{xj} are handled correctly because \code{candidates} is
#' already deduplicated to unique observed values (by [bracket_candidates()])
#' and every row sharing a candidate's value migrates together in the same
#' sweep step, never split across it.
#'
#' \strong{Tie-break among candidates with equal (within \code{sse_tol}) risk}:
#' prefer the one closest to \code{incumbent_cut}, then the smallest value --
#' deterministic and reproducible, unlike the strict \code{<} comparison this
#' was ported from (which silently preferred whichever tied candidate the
#' ascending sweep visited first, with no stated rationale).
#'
#' @param xj Coordinate values of the rows reaching this node.
#' @param y Outcomes of those rows (should be pre-centered by the caller --
#'   see [refine_tree_cuts()] -- SSE is exactly invariant to a global
#'   additive shift, and centering avoids catastrophic cancellation in
#'   \code{sumsq - sum^2/n} when \code{y} is far from zero).
#' @param lid_left,lid_right Leaf position (1..K) each row would take under
#'   the left / right subtree, on a common 1..K id space.
#' @param K Total number of leaf positions.
#' @param candidates Ascending candidate cutoffs (observed values in the
#'   bracket, plus the incumbent grid cut(s)).
#' @param incumbent_cut The node's cut BEFORE this call (for the tie-break
#'   and for computing \code{sse_before}).
#' @param sse_tol Absolute SSE difference below which two candidates are
#'   considered tied (not a difference worth preferring on risk alone).
#' @return List(cut, sse, sse_before, n_candidates, all_candidates
#'   (data.frame(candidate, sse))).
#' @keywords internal
scan_cutoff <- function(xj, y, lid_left, lid_right, K, candidates,
                         incumbent_cut, sse_tol = 1e-9) {
  stopifnot(length(candidates) >= 1L)
  ord <- order(xj, method = "radix")   # stable, platform-independent order
  xs  <- xj[ord]; ys <- y[ord]
  lL  <- lid_left[ord]; lR <- lid_right[ord]

  cnt <- tabulate(lR, nbins = K)
  s   <- numeric(K); ss <- numeric(K)
  for (k in which(cnt > 0L)) {
    sel <- lR == k
    s[[k]]  <- sum(ys[sel]); ss[[k]] <- sum(ys[sel] * ys[sel])
  }

  sse_now <- function() {
    # Clamp tiny negative SSE (floating-point noise, not a real negative
    # variance) to exactly 0 rather than letting it propagate.
    terms <- ifelse(cnt > 0L, ss - s^2 / pmax(cnt, 1L), 0)
    sum(pmax(terms, 0))
  }

  best_cut <- NA_real_; best_sse <- Inf
  sse_before <- NA_real_
  all_sse <- numeric(length(candidates))
  pos <- 1L; m <- length(xs)

  for (ci in seq_along(candidates)) {
    cand <- candidates[[ci]]
    while (pos <= m && xs[[pos]] <= cand) {
      kR <- lR[[pos]]; kL <- lL[[pos]]; yv <- ys[[pos]]
      cnt[[kR]] <- cnt[[kR]] - 1L; s[[kR]] <- s[[kR]] - yv; ss[[kR]] <- ss[[kR]] - yv * yv
      cnt[[kL]] <- cnt[[kL]] + 1L; s[[kL]] <- s[[kL]] + yv; ss[[kL]] <- ss[[kL]] + yv * yv
      pos <- pos + 1L
    }
    v <- sse_now()
    all_sse[[ci]] <- v
    if (isTRUE(all.equal(cand, incumbent_cut, tolerance = .Machine$double.eps^0.5))) {
      sse_before <- v
    }
    if (v < best_sse - sse_tol) {
      best_sse <- v; best_cut <- cand
    } else if (abs(v - best_sse) <= sse_tol) {
      # Tied within tolerance: prefer closer to incumbent_cut, then smaller.
      if (abs(cand - incumbent_cut) < abs(best_cut - incumbent_cut) ||
          (abs(cand - incumbent_cut) == abs(best_cut - incumbent_cut) && cand < best_cut)) {
        best_sse <- min(best_sse, v); best_cut <- cand
      }
    }
  }

  list(cut = best_cut, sse = best_sse, sse_before = sse_before,
       n_candidates = length(candidates),
       all_candidates = data.frame(candidate = candidates, sse = all_sse))
}

#' Refine every split threshold of a coordinate-space tree, root first
#'
#' @description
#' Top-down: the root is refined first, then children are refined on the
#' row sets induced by the already-refined ancestors. One pass -- this is a
#' bracket refinement, not an alternating-minimization search.
#'
#' Each node's bracket is recomputed from the CURRENT tree at the moment it
#' is refined ([node_bracket()]), not stored statically: refining an
#' ancestor first means a later same-coordinate descendant's row set
#' ([coord_tree_rows_at()]) is already filtered by the ancestor's REFINED
#' cut, while a not-yet-refined descendant node still contributes its GRID
#' cut as the bounding value seen by its parent. This asymmetry is
#' monotone-safe and requires no second pass, because after
#' [collapse_transitions()] any two same-coordinate splits that remain
#' distinct nodes are separated by the (Sep) margin, not by one grid atom --
#' do not "fix" this into an alternating minimization.
#'
#' \strong{Robustness fixes relative to the ported original} (Oracle
#' consult, plan file's Milestone A addendum, §Q4): \code{min_leaf_n} as an
#' explicit floor on every candidate (protects against a refined ancestor
#' cut leaving a DESCENDANT leaf with zero rows -- the original had a
#' node-local \code{< 2} guard only, not a floor propagated through the
#' candidate set); a deterministic tie-break in [scan_cutoff()]; \code{y}
#' centered once, globally, before any scan (SSE-exact, removes a
#' catastrophic-cancellation risk for \code{y} far from zero); a full
#' \code{refined} log with \code{sse_before}/\code{sse_after} per node.
#'
#' @param tree Coordinate-space tree from [as_coordinate_tree()], already
#'   passed through [collapse_transitions()].
#' @param X,y Training data. \code{y} is centered internally (once, by its
#'   overall mean) before any SSE is computed; leaf \code{prediction}s in
#'   the RETURNED tree are on the ORIGINAL scale (the centering is undone
#'   before values are written back).
#' @param min_leaf_n Integer floor on rows per side of every candidate cut
#'   (default \code{1L} -- no floor beyond "non-empty"). Set higher to
#'   guard against unstable leaf means.
#' @return List(tree, refined) where \code{refined} is a data.frame with one
#'   row per split: \code{path, coord, grid_cut_lo, grid_cut_hi, incumbent_cut,
#'   refined_cut, bracket_lo, bracket_hi, n_node, n_candidates, sse_before,
#'   sse_after, collapsed, refined, reason}.
#' @keywords internal
refine_tree_cuts <- function(tree, X, y, min_leaf_n = 1L) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    cli::cli_abort("refine_tree_cuts: {.arg X} must be a data.frame or matrix.")
  }
  if (is.matrix(X)) X <- as.data.frame(X)
  if (nrow(X) != length(y)) {
    cli::cli_abort(
      "refine_tree_cuts: nrow(X) ({nrow(X)}) must equal length(y) ({length(y)})."
    )
  }
  min_leaf_n <- as.integer(min_leaf_n)
  if (is.na(min_leaf_n) || min_leaf_n < 1L) {
    cli::cli_abort("refine_tree_cuts: {.arg min_leaf_n} must be a positive integer.")
  }

  y <- as.numeric(y)
  y_mean <- mean(y)
  y_centered <- y - y_mean   # SSE-exact shift; see roxygen and scan_cutoff() docs

  paths <- coord_tree_split_paths(tree)
  paths <- paths[order(vapply(paths, length, integer(1)))]   # root-first

  log_rows <- list()

  for (p in paths) {
    idx  <- coord_tree_rows_at(tree, X, p)
    node <- coord_tree_node_at(tree, p)
    br   <- node_bracket(tree, p)

    grid_lo <- if (length(node$grid_cuts) >= 1L) node$grid_cuts[[1L]] else NA_real_
    grid_hi <- if (length(node$grid_cuts) >= 2L) node$grid_cuts[[2L]] else NA_real_
    base_row <- function(refined_cut, sse_before, sse_after, n_cand, refined, reason) {
      data.frame(
        path = paste(p, collapse = "/"), coord = node$coord,
        grid_cut_lo = grid_lo, grid_cut_hi = grid_hi,
        incumbent_cut = node$cut, refined_cut = refined_cut,
        bracket_lo = br$lo, bracket_hi = br$hi,
        n_node = length(idx), n_candidates = n_cand,
        sse_before = sse_before, sse_after = sse_after,
        collapsed = isTRUE(node$collapsed), refined = refined, reason = reason,
        stringsAsFactors = FALSE
      )
    }

    if (length(idx) < 2L * min_leaf_n) {
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "too_few_rows_at_node"
      )
      next
    }

    Xn <- X[idx, , drop = FALSE]
    yn <- y_centered[idx]
    xj <- Xn[[node$coord]]

    cands_raw <- bracket_candidates(xj, node, br)
    if (length(cands_raw) == 0L) {
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "no_candidates_in_bracket"
      )
      next
    }

    # min_leaf_n floor: exclude any candidate that would leave either side
    # of THIS split with fewer than min_leaf_n rows. By induction (root-
    # first, one node at a time, using each node's ACTUAL realised row set)
    # this is sufficient to guarantee every eventual LEAF has >= min_leaf_n
    # rows -- the "descendant left empty by an ancestor's refined cut"
    # failure mode Oracle's consult flagged as highest priority.
    n_leq <- vapply(cands_raw, function(c) sum(xj <= c), integer(1))
    n_gt  <- length(xj) - n_leq
    feasible_cand <- cands_raw[n_leq >= min_leaf_n & n_gt >= min_leaf_n]

    if (length(feasible_cand) == 0L) {
      log_rows[[length(log_rows) + 1L]] <- base_row(
        node$cut, NA_real_, NA_real_, 0L, FALSE, "min_leaf_n_infeasible"
      )
      next
    }

    n_left  <- coord_tree_count_leaves(node$left)
    lid_left  <- coord_tree_leaf_index(node$left,  Xn)
    lid_right <- n_left + coord_tree_leaf_index(node$right, Xn)
    K <- n_left + coord_tree_count_leaves(node$right)

    res <- scan_cutoff(xj, yn, lid_left, lid_right, K, feasible_cand,
                        incumbent_cut = node$cut)

    log_rows[[length(log_rows) + 1L]] <- base_row(
      res$cut, res$sse_before, res$sse, res$n_candidates,
      !isTRUE(all.equal(res$cut, node$cut, tolerance = .Machine$double.eps^0.5)),
      "refined"
    )

    node$cut <- res$cut
    tree <- coord_tree_set_node_at(tree, p, node)
  }

  # Re-attach leaf statistics on the ORIGINAL y scale: refinement can move
  # rows between leaves, so every leaf's n/prediction/stats must be
  # recomputed from the FINAL tree, not carried over from as_coordinate_tree().
  leaf_ids <- coord_tree_assign(tree, X)
  attach_final_stats <- function(node) {
    if (identical(node$kind, "leaf")) {
      rows <- which(leaf_ids == node$id)
      n_leaf <- length(rows)
      if (n_leaf == 0L) {
        cli::cli_abort(c(
          "refine_tree_cuts: leaf {node$id} has zero rows after refinement.",
          "i" = "This should be prevented by the min_leaf_n floor applied \\
                 at every split; its occurrence indicates a bug in that \\
                 floor's propagation, not a legitimate outcome."
        ))
      }
      yl <- y[rows]   # ORIGINAL scale, not centered
      node$n <- n_leaf
      node$prediction <- mean(yl)
      node$stats <- list(n = n_leaf, sum = sum(yl), sumsq = sum(yl * yl))
      return(node)
    }
    node$left  <- attach_final_stats(node$left)
    node$right <- attach_final_stats(node$right)
    node
  }
  tree <- attach_final_stats(tree)

  list(tree = tree, refined = do.call(rbind, log_rows))
}

# ---------------------------------------------------------------------------
# Public entry point and consumer contract. Everything above this point is
# internal machinery (`@keywords internal`); `refine_tree()`,
# `RefinedTreeModel` (in R/s7_classes.R), its predict/print/summary
# methods, and `leaf_assignments()`/`n_leaves()`/`split_table()` are the
# supported public surface Milestone A promises -- doubletree and other
# callers should use these, not reach into the coordinate-tree internals
# directly.
# ---------------------------------------------------------------------------

#' Coordinate names actually used by splits in a coordinate-space tree
#' @keywords internal
coord_tree_coords_used <- function(node) {
  if (identical(node$kind, "leaf")) return(character(0))
  unique(c(node$coord,
           coord_tree_coords_used(node$left),
           coord_tree_coords_used(node$right)))
}

#' Total training risk (SSE) of a coordinate-space tree
#' @keywords internal
coord_tree_training_sse <- function(tree, X, y) {
  preds <- coord_tree_predict(tree, X)
  sum((as.numeric(y) - preds)^2)
}

#' Run Stage B (off-grid threshold refinement) on a fitted model
#'
#' @description
#' The public entry point for Stage B: converts \code{model} to
#' coordinate-space ([as_coordinate_tree()]), collapses transition-leaf
#' pairs ([collapse_transitions()]), refines every remaining threshold to
#' an exact, off-grid data value ([refine_tree_cuts()]), and wraps the
#' result in a [RefinedTreeModel]. See \code{\link{stage_b}} for the
#' overall mechanism and \code{quality_reports/plans/2026-09-01_two-stage-
#' package-defaults-session.md} Milestone A in the \code{global-scholars}
#' project for the design history.
#'
#' \strong{Scope, 2026-09-01 (Milestone A): regression (\code{squared_error})
#' only}, enforced by [as_coordinate_tree()] (loudly rejects any other
#' \code{loss_function}, not silently). Classification support is a
#' separate, later milestone.
#'
#' @param model A fitted \code{OptimalTreesModel} (S7), from [fit_tree()]
#'   with \code{loss_function = "squared_error"}.
#' @param X,y The training data \code{model} was fit on (required
#'   explicitly, and verified to reconcile with \code{model}'s own
#'   discretization -- see [as_coordinate_tree()]).
#' @param min_leaf_n Integer floor on rows per side of every refined cut
#'   (default \code{1L}). Passed through to [refine_tree_cuts()], and
#'   recorded on the returned model for [certify_local_optimality()].
#' @param tree_index Which tree in \code{model@trees} to refine (default
#'   \code{1L}; only relevant for a Rashomon set).
#' @param max_depth,max_leaves The Stage-A model-class constraints
#'   \code{model} was originally fit under (\code{NULL}, the default,
#'   means that constraint was uncapped). \strong{Not recoverable from
#'   \code{model} itself} -- \code{OptimalTreesModel} does not retain
#'   \code{max_depth}/\code{max_leaves} post-fit -- so pass the SAME
#'   values used at the original [fit_tree()] call if you plan to run
#'   [certify_local_optimality()] on the result; otherwise its \code{add}
#'   perturbation cannot distinguish an out-of-class candidate split (which
#'   should be marked infeasible, not a certificate failure) from an
#'   in-class one.
#' @return A [RefinedTreeModel].
#' @export
refine_tree <- function(model, X, y, min_leaf_n = 1L, tree_index = 1L,
                         max_depth = NULL, max_leaves = NULL) {
  ct <- as_coordinate_tree(model, X, y, tree_index = tree_index)
  collapsed <- collapse_transitions(ct)
  refined <- refine_tree_cuts(collapsed$tree, X, y, min_leaf_n = min_leaf_n)

  RefinedTreeModel(
    tree = refined$tree,
    coords = coord_tree_coords_used(refined$tree),
    loss = model@loss_function,
    source = model,
    refinement_log = refined$refined,
    n_train = nrow(if (is.matrix(X)) as.data.frame(X) else X),
    training_risk = coord_tree_training_sse(refined$tree, X, y),
    min_leaf_n = as.integer(min_leaf_n),
    lambda = model@regularization,
    max_depth = if (is.null(max_depth)) NULL else as.integer(max_depth),
    max_leaves = if (is.null(max_leaves)) NULL else as.integer(max_leaves)
  )
}

#' Leaf assigned to each row of `newdata` by a `RefinedTreeModel`
#'
#' @param object A [RefinedTreeModel].
#' @param newdata Data.frame or matrix with the coordinates \code{object}
#'   splits on (\code{object@coords}), as raw (non-discretized) values.
#' @return Integer vector of length \code{nrow(newdata)}: the leaf `id`
#'   (see \code{\link{stage_b}}'s node schema) each row falls into.
#' @export
leaf_assignments <- function(object, newdata) {
  if (!S7::S7_inherits(object, RefinedTreeModel)) {
    cli::cli_abort("leaf_assignments: {.arg object} must be a RefinedTreeModel.")
  }
  newdata <- .refined_tree_check_newdata(object, newdata)
  coord_tree_assign(object@tree, newdata)
}

#' Number of leaves in a `RefinedTreeModel`
#' @param object A [RefinedTreeModel].
#' @return Integer.
#' @export
n_leaves <- function(object) {
  if (!S7::S7_inherits(object, RefinedTreeModel)) {
    cli::cli_abort("n_leaves: {.arg object} must be a RefinedTreeModel.")
  }
  coord_tree_count_leaves(object@tree)
}

#' Flat table of every split in a `RefinedTreeModel`
#'
#' @param object A [RefinedTreeModel].
#' @return Data.frame with one row per split: \code{id}, \code{coord},
#'   \code{cut}, \code{collapsed}, \code{grid_cut_lo}, \code{grid_cut_hi}.
#' @export
split_table <- function(object) {
  if (!S7::S7_inherits(object, RefinedTreeModel)) {
    cli::cli_abort("split_table: {.arg object} must be a RefinedTreeModel.")
  }
  rows <- list()
  walk <- function(node) {
    if (identical(node$kind, "leaf")) return(invisible())
    rows[[length(rows) + 1L]] <<- data.frame(
      id = node$id, coord = node$coord, cut = node$cut,
      collapsed = isTRUE(node$collapsed),
      grid_cut_lo = if (length(node$grid_cuts) >= 1L) node$grid_cuts[[1L]] else NA_real_,
      grid_cut_hi = if (length(node$grid_cuts) >= 2L) node$grid_cuts[[2L]] else NA_real_,
      stringsAsFactors = FALSE
    )
    walk(node$left); walk(node$right)
  }
  walk(object@tree)
  do.call(rbind, rows)
}

#' Validate `newdata` for a `RefinedTreeModel` (internal helper)
#' @keywords internal
.refined_tree_check_newdata <- function(object, newdata) {
  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    cli::cli_abort("{.arg newdata} must be a data.frame or matrix.")
  }
  if (is.matrix(newdata)) newdata <- as.data.frame(newdata)
  missing_coords <- setdiff(object@coords, names(newdata))
  if (length(missing_coords) > 0L) {
    cli::cli_abort(
      "newdata is missing coordinate(s) {.val {missing_coords}} that \\
       {.arg object} splits on."
    )
  }
  newdata
}

# Predict from a RefinedTreeModel. Walks the coordinate-space tree
# DIRECTLY on newdata's raw (non-discretized) coordinate values -- no
# binary design matrix, no @discretization_metadata step. This is the
# whole reason RefinedTreeModel is a separate representation from
# OptimalTreesModel (see its class docs): a refined, off-grid `cut` is
# not expressible as a grid threshold at all.
S7::method(predict, RefinedTreeModel) <- function(object, newdata, ...) {
  newdata <- .refined_tree_check_newdata(object, newdata)
  coord_tree_predict(object@tree, newdata)
}

# Print a RefinedTreeModel.
S7::method(print, RefinedTreeModel) <- function(x, ...) {
  cat("<RefinedTreeModel>\n")
  cat(sprintf("  loss: %s | n_train: %d | n_leaves: %d | training_risk (SSE): %s\n",
              x@loss, x@n_train, coord_tree_count_leaves(x@tree),
              format(x@training_risk, digits = 6)))
  cat(sprintf("  coords: %s | min_leaf_n: %d\n",
              paste(x@coords, collapse = ", "), x@min_leaf_n))
  n_refined <- if (!is.null(x@refinement_log)) sum(x@refinement_log$refined) else NA_integer_
  n_collapsed <- if (!is.null(x@refinement_log)) sum(x@refinement_log$collapsed) else NA_integer_
  cat(sprintf("  splits refined off-grid: %s | collapsed transition pairs: %s\n",
              n_refined, n_collapsed))
  invisible(x)
}

# Summarize a RefinedTreeModel: prints the model plus the full
# @refinement_log -- the per-split checkable record of what moved, from
# where, and by how much risk. DIAGNOSTIC AND PROVENANCE ONLY: this makes
# no claim about global optimality, full ass:global compliance, or the
# (Grid) condition -- see the plan file's Milestone A/§4.2 for the
# explicit disclaimer categories fit_twostage() (a later milestone) will
# surface formally.
S7::method(summary, RefinedTreeModel) <- function(object, ...) {
  print(object)
  cat("\nRefinement log:\n")
  print(object@refinement_log)
  invisible(object)
}
