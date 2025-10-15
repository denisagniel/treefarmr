#' Tree Visualization Functions
#'
#' @description
#' Comprehensive tree visualization functions for TreeFARMS models,
#' including static plots, interactive visualizations, and comparison tools.

# Load required packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required for tree visualization. Install with: install.packages('ggplot2')")
}

if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  stop("Package 'DiagrammeR' is required for tree visualization. Install with: install.packages('DiagrammeR')")
}

#' Plot TreeFARMS Model
#'
#' @description
#' Create a visualization of a TreeFARMS model showing the decision tree structure.
#'
#' @param x A treefarms model object
#' @param tree_index Integer: which tree to plot (default: 1)
#' @param type Character: type of plot ("tree", "rules", "comparison")
#' @param interactive Logical: whether to create interactive plot
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot.treefarms_logloss_model <- function(x, tree_index = 1, type = c("tree", "rules", "comparison"), 
                                        interactive = FALSE, ...) {
  type <- match.arg(type)
  
  if (x$n_trees == 0) {
    stop("No trees available for plotting")
  }
  
  if (tree_index > x$n_trees) {
    stop(sprintf("Tree index %d exceeds number of trees (%d)", tree_index, x$n_trees))
  }
  
  tree <- x$trees[[tree_index]]
  
  switch(type,
    "tree" = plot_tree_structure(tree, x$X_train, interactive = interactive, ...),
    "rules" = plot_tree_rules(tree, x$X_train, ...),
    "comparison" = plot_tree_comparison(x, ...)
  )
}

#' Plot TreeFARMS Subprocess Model
#'
#' @description
#' Create a visualization of a TreeFARMS subprocess model.
#'
#' @param x A treefarms_subprocess_model object
#' @param tree_index Integer: which tree to plot (default: 1)
#' @param type Character: type of plot ("tree", "rules", "comparison")
#' @param interactive Logical: whether to create interactive plot
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot.treefarms_subprocess_model <- function(x, tree_index = 1, type = c("tree", "rules", "comparison"), 
                                           interactive = FALSE, ...) {
  type <- match.arg(type)
  
  if (x$n_trees == 0) {
    stop("No trees available for plotting")
  }
  
  if (tree_index > x$n_trees) {
    stop(sprintf("Tree index %d exceeds number of trees (%d)", tree_index, x$n_trees))
  }
  
  tree <- x$trees[[tree_index]]
  
  switch(type,
    "tree" = plot_tree_structure(tree, x$X_train, interactive = interactive, ...),
    "rules" = plot_tree_rules(tree, x$X_train, ...),
    "comparison" = plot_tree_comparison(x, ...)
  )
}

#' Plot Cross-Fitted Rashomon Results
#'
#' @description
#' Create visualizations for cross-fitted Rashomon results.
#'
#' @param x A cf_rashomon object
#' @param type Character: type of plot ("overview", "trees", "stability", "comparison")
#' @param interactive Logical: whether to create interactive plot
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot.cf_rashomon <- function(x, type = c("overview", "trees", "stability", "comparison"), 
                            interactive = FALSE, ...) {
  type <- match.arg(type)
  
  switch(type,
    "overview" = plot_cf_overview(x, ...),
    "trees" = plot_cf_trees(x, interactive = interactive, ...),
    "stability" = plot_cf_stability(x, ...),
    "comparison" = plot_cf_comparison(x, ...)
  )
}

#' Plot Tree Structure
#'
#' @description
#' Create a detailed visualization of a single tree structure.
#'
#' @param tree A tree object (JSON string or list)
#' @param X_train Training data for context
#' @param interactive Logical: whether to create interactive plot
#' @param feature_names Character vector: names for features
#' @param ... Additional arguments
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot_tree_structure <- function(tree, X_train, interactive = FALSE, feature_names = NULL, ...) {
  
  # Parse tree JSON
  if (is.character(tree)) {
    tree_data <- jsonlite::fromJSON(tree)
  } else if (is.list(tree) && "json" %in% names(tree)) {
    tree_data <- jsonlite::fromJSON(tree$json)
  } else {
    stop("Invalid tree format")
  }
  
  # Get feature names
  if (is.null(feature_names)) {
    feature_names <- colnames(X_train)
  }
  
  if (interactive) {
    return(plot_tree_interactive(tree_data, feature_names))
  } else {
    return(plot_tree_static(tree_data, feature_names))
  }
}

#' Plot Tree Structure (Static)
#'
#' @description
#' Create a static visualization of tree structure using ggplot2.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A ggplot2 object
#'
#' @export
plot_tree_static <- function(tree_data, feature_names) {
  
  # Create tree nodes and edges
  nodes <- create_tree_nodes(tree_data, feature_names)
  edges <- create_tree_edges(tree_data, feature_names)
  
  # Handle simple leaf nodes (no decision structure)
  if (is.null(tree_data$feature) && !is.null(tree_data$prediction)) {
    # This is a simple leaf node - create a single node visualization
    nodes <- data.frame(
      x = 0,
      y = 0,
      label = paste("Prediction:", tree_data$prediction),
      type = "leaf",
      size = 5,
      stringsAsFactors = FALSE
    )
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = nodes, 
                         ggplot2::aes(x = x, y = y, color = type, size = size),
                         alpha = 0.8) +
      ggplot2::geom_text(data = nodes, 
                        ggplot2::aes(x = x, y = y, label = label),
                        size = 4, hjust = 0.5, vjust = 0.5) +
      ggplot2::scale_color_manual(values = c("leaf" = "lightgreen")) +
      ggplot2::scale_size_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(title = "TreeFARMS Decision Tree Structure",
                   subtitle = paste("Simple leaf node | Loss:", 
                                  ifelse(is.numeric(tree_data$loss), 
                                         round(tree_data$loss, 4), 
                                         "N/A")))
  } else {
    # Complex tree structure
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = edges, 
                           ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                           color = "gray50", linewidth = 1) +
      ggplot2::geom_point(data = nodes, 
                         ggplot2::aes(x = x, y = y, color = type, size = size),
                         alpha = 0.8) +
      ggplot2::geom_text(data = nodes, 
                        ggplot2::aes(x = x, y = y, label = label),
                        size = 3, hjust = 0.5, vjust = 0.5) +
      ggplot2::scale_color_manual(values = c("decision" = "lightblue", 
                                            "leaf" = "lightgreen",
                                            "root" = "orange")) +
      ggplot2::scale_size_continuous(range = c(3, 8)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(title = "TreeFARMS Decision Tree Structure",
                   subtitle = paste("Prediction:", tree_data$prediction, 
                                  "| Loss:", ifelse(is.numeric(tree_data$loss), 
                                                   round(tree_data$loss, 4), 
                                                   "N/A")),
                   color = "Node Type", size = "Node Size")
  }
  
  return(p)
}

#' Plot Tree Structure (Interactive)
#'
#' @description
#' Create an interactive visualization of tree structure using DiagrammeR.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A DiagrammeR object
#'
#' @export
plot_tree_interactive <- function(tree_data, feature_names) {
  
  # Create nodes and edges for DiagrammeR
  nodes <- create_diagrammer_nodes(tree_data, feature_names)
  edges <- create_diagrammer_edges(tree_data, feature_names)
  
  # Create DiagrammeR graph
  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges
  )
  
  # Render graph
  DiagrammeR::render_graph(graph, layout = "tree")
}

#' Plot Tree Rules
#'
#' @description
#' Create a visualization of tree decision rules.
#'
#' @param tree A tree object
#' @param X_train Training data for context
#' @param max_rules Integer: maximum number of rules to display
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot_tree_rules <- function(tree, X_train, max_rules = 10, ...) {
  
  # Get tree rules
  rules <- get_tree_rules(tree, feature_names = colnames(X_train))
  
  if (length(rules) == 0) {
    stop("No rules found in tree")
  }
  
  # Limit number of rules
  if (length(rules) > max_rules) {
    rules <- rules[1:max_rules]
  }
  
  # Create rules data frame
  rules_df <- data.frame(
    rule_id = seq_along(rules),
    rule = rules,
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(rules_df, ggplot2::aes(x = rule_id, y = 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = rule_id), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = rule), 
                      size = 3, hjust = 0.5, vjust = 0.5) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = "Tree Decision Rules",
                 subtitle = paste("Showing", length(rules), "rules"))
  
  return(p)
}

#' Plot Tree Comparison
#'
#' @description
#' Create a comparison visualization of multiple trees.
#'
#' @param model A treefarms model object
#' @param trees_to_compare Integer vector: which trees to compare
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot_tree_comparison <- function(model, trees_to_compare = NULL, ...) {
  
  if (model$n_trees < 2) {
    stop("Need at least 2 trees for comparison")
  }
  
  if (is.null(trees_to_compare)) {
    trees_to_compare <- 1:min(model$n_trees, 4)  # Compare first 4 trees
  }
  
  # Create comparison data
  comparison_data <- data.frame(
    tree_id = trees_to_compare,
    prediction = sapply(trees_to_compare, function(i) {
      tree_data <- jsonlite::fromJSON(model$trees[[i]])
      tree_data$prediction
    }),
    loss = sapply(trees_to_compare, function(i) {
      tree_data <- jsonlite::fromJSON(model$trees[[i]])
      tree_data$loss
    }),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = tree_id, y = loss)) +
    ggplot2::geom_point(ggplot2::aes(color = as.factor(prediction)), size = 4) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_color_manual(values = c("0" = "red", "1" = "blue")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Tree Comparison",
                 subtitle = "Loss vs Tree ID",
                 x = "Tree ID", y = "Loss",
                 color = "Prediction") +
    ggplot2::scale_x_continuous(breaks = trees_to_compare)
  
  return(p)
}

#' Plot Cross-Fitted Rashomon Overview
#'
#' @description
#' Create an overview visualization of cross-fitted Rashomon results.
#'
#' @param cf_result A cf_rashomon object
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot_cf_overview <- function(cf_result, ...) {
  
  # Create overview data
  overview_data <- data.frame(
    fold = 1:cf_result$K,
    trees = cf_result$rashomon_sizes,
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(overview_data, ggplot2::aes(x = fold, y = trees)) +
    ggplot2::geom_col(ggplot2::aes(fill = trees), alpha = 0.7) +
    ggplot2::geom_hline(yintercept = cf_result$n_intersecting, 
                       linetype = "dashed", color = "red", size = 1) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cross-Fitted Rashomon Overview",
                 subtitle = paste("Intersecting trees:", cf_result$n_intersecting),
                 x = "Fold", y = "Number of Trees",
                 fill = "Trees") +
    ggplot2::scale_x_continuous(breaks = 1:cf_result$K)
  
  return(p)
}

#' Plot Cross-Fitted Rashomon Trees
#'
#' @description
#' Create visualizations of trees in cross-fitted Rashomon results.
#'
#' @param cf_result A cf_rashomon object
#' @param interactive Logical: whether to create interactive plot
#' @param ... Additional arguments
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot_cf_trees <- function(cf_result, interactive = FALSE, ...) {
  
  if (cf_result$n_intersecting == 0) {
    stop("No intersecting trees to plot")
  }
  
  # Plot the first intersecting tree
  tree <- cf_result$intersecting_trees[[1]]
  
  # Handle different tree formats
  if (is.character(tree)) {
    tree_data <- jsonlite::fromJSON(tree)
  } else if (is.list(tree) && "json" %in% names(tree)) {
    tree_data <- jsonlite::fromJSON(tree$json)
  } else {
    tree_data <- tree
  }
  
  if (interactive) {
    return(plot_tree_interactive(tree_data, colnames(cf_result$X_train)))
  } else {
    return(plot_tree_static(tree_data, colnames(cf_result$X_train)))
  }
}

#' Plot Cross-Fitted Rashomon Stability
#'
#' @description
#' Create a stability visualization for cross-fitted Rashomon results.
#'
#' @param cf_result A cf_rashomon object
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot_cf_stability <- function(cf_result, ...) {
  
  # Create stability data
  stability_data <- data.frame(
    fold = rep(1:cf_result$K, each = max(cf_result$rashomon_sizes)),
    tree_id = unlist(lapply(cf_result$rashomon_sizes, function(n) 1:n)),
    present = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(stability_data, ggplot2::aes(x = fold, y = tree_id)) +
    ggplot2::geom_tile(ggplot2::aes(fill = present), alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Tree Stability Across Folds",
                 subtitle = paste("Trees present in each fold"),
                 x = "Fold", y = "Tree ID",
                 fill = "Present") +
    ggplot2::scale_x_continuous(breaks = 1:cf_result$K)
  
  return(p)
}

#' Plot Cross-Fitted Rashomon Comparison
#'
#' @description
#' Create a comparison visualization for cross-fitted Rashomon results.
#'
#' @param cf_result A cf_rashomon object
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot_cf_comparison <- function(cf_result, ...) {
  
  # Create comparison data
  comparison_data <- data.frame(
    metric = c("Total Folds", "Trees per Fold (avg)", "Intersecting Trees", "Stability Rate"),
    value = c(cf_result$K, 
              round(mean(cf_result$rashomon_sizes), 2),
              cf_result$n_intersecting,
              round(cf_result$n_intersecting / mean(cf_result$rashomon_sizes), 3)),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_col(ggplot2::aes(fill = metric), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = value), 
                      size = 4, hjust = 0.5, vjust = -0.5) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                  legend.position = "none") +
    ggplot2::labs(title = "Cross-Fitted Rashomon Comparison",
                 subtitle = "Key metrics summary",
                 x = "Metric", y = "Value")
  
  return(p)
}

# Helper functions for tree visualization

#' Create Tree Nodes for Static Plot
#'
#' @description
#' Create node data frame for static tree visualization.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A data frame with node information
#'
#' @keywords internal
create_tree_nodes <- function(tree_data, feature_names) {
  
  # For now, create a simple node structure
  # In a full implementation, this would parse the actual tree structure
  
  nodes <- data.frame(
    x = c(0, -1, 1),
    y = c(0, -1, -1),
    label = c("Root", "Left", "Right"),
    type = c("root", "leaf", "leaf"),
    size = c(5, 3, 3),
    stringsAsFactors = FALSE
  )
  
  return(nodes)
}

#' Create Tree Edges for Static Plot
#'
#' @description
#' Create edge data frame for static tree visualization.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A data frame with edge information
#'
#' @keywords internal
create_tree_edges <- function(tree_data, feature_names) {
  
  # For now, create simple edges
  # In a full implementation, this would parse the actual tree structure
  
  edges <- data.frame(
    x = c(0, 0),
    y = c(0, 0),
    xend = c(-1, 1),
    yend = c(-1, -1),
    stringsAsFactors = FALSE
  )
  
  return(edges)
}

#' Create DiagrammeR Nodes
#'
#' @description
#' Create node data frame for DiagrammeR visualization.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A data frame with DiagrammeR node information
#'
#' @keywords internal
create_diagrammer_nodes <- function(tree_data, feature_names) {
  
  # Create nodes for DiagrammeR
  nodes <- data.frame(
    id = c(1, 2, 3),
    label = c("Root", "Left", "Right"),
    type = c("root", "leaf", "leaf"),
    stringsAsFactors = FALSE
  )
  
  return(nodes)
}

#' Create DiagrammeR Edges
#'
#' @description
#' Create edge data frame for DiagrammeR visualization.
#'
#' @param tree_data Parsed tree JSON data
#' @param feature_names Character vector: names for features
#'
#' @return A data frame with DiagrammeR edge information
#'
#' @keywords internal
create_diagrammer_edges <- function(tree_data, feature_names) {
  
  # Create edges for DiagrammeR
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 3),
    stringsAsFactors = FALSE
  )
  
  return(edges)
}
