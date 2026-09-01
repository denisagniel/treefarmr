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
      "i" = "Got {.val {model@loss_function}}. Stage B's threshold-refinement ",
      "scan has no classification variant yet -- see quality_reports/plans/",
      "2026-09-01_two-stage-package-defaults-session.md (Milestone A scope) ",
      "in the global-scholars project."
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
      "i" = "Re-discretizing {.arg X} against the model's stored metadata produced ",
      "columns {.val {colnames(X_binary_check)}}, but the model was fit on ",
      "{.val {expected_names}}.",
      "i" = "This usually means X is not the same data (or feature set/order) ",
      "the model was actually fit on."
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
          "as_coordinate_tree: leaf {node$id} received zero training rows ",
          "under the supplied X.",
          "i" = "A correctly-reproduced fit should never leave a leaf empty ",
          "on its own training data -- this means X/y do not match what ",
          "{.arg model} was actually fit on."
        ))
      }
      yl <- y[rows]
      s  <- sum(yl)
      ss <- sum(yl * yl)
      pred <- s / n_leaf
      if (!isTRUE(all.equal(pred, node$original_prediction, tolerance = 1e-6))) {
        cli::cli_abort(c(
          "as_coordinate_tree: leaf {node$id}'s data-derived mean ({pred}) ",
          "disagrees with the fitted model's own stored prediction ",
          "({node$original_prediction}).",
          "i" = "This means X/y do not match what {.arg model} was actually ",
          "fit on, or there is a bug in the coordinate-tree conversion -- ",
          "not something to paper over."
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
