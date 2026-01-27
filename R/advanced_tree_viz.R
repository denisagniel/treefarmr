#' Advanced Tree Visualization Functions
#'
#' @description
#' Advanced tree visualization functions that can parse and display
#' the actual tree structure from TreeFARMS JSON output.

#' Parse Tree JSON Structure
#'
#' @description
#' Parse TreeFARMS tree JSON into a structured format for visualization.
#'
#' @param tree_json Character string: JSON representation of tree
#' @param feature_names Character vector: names for features
#'
#' @return A list containing parsed tree structure
#'
#' @export
parse_tree_structure <- function(tree_json, feature_names = NULL) {
  
  # Parse JSON
  tree_data <- jsonlite::fromJSON(tree_json)
  
  # Initialize tree structure
  tree_structure <- list(
    nodes = data.frame(),
    edges = data.frame(),
    root = NULL
  )
  
  # Parse tree recursively
  tree_structure <- parse_tree_recursive(tree_data, tree_structure, feature_names)
  
  return(tree_structure)
}

#' Parse Tree Recursively
#'
#' @description
#' Recursively parse tree structure from JSON data.
#'
#' @param node_data List: current node data
#' @param tree_structure List: accumulating tree structure
#' @param feature_names Character vector: names for features
#' @param node_id Integer: current node ID
#' @param parent_id Integer: parent node ID
#' @param depth Integer: current depth
#'
#' @return Updated tree structure
#'
#' @keywords internal
parse_tree_recursive <- function(node_data, tree_structure, feature_names = NULL, 
                                node_id = 1, parent_id = NULL, depth = 0) {
  
  # Create node information
  node_info <- data.frame(
    id = node_id,
    parent_id = parent_id,
    depth = depth,
    type = determine_node_type(node_data),
    label = create_node_label(node_data, feature_names),
    prediction = get_node_prediction(node_data),
    loss = get_node_loss(node_data),
    feature = get_node_feature(node_data, feature_names),
    threshold = get_node_threshold(node_data),
    stringsAsFactors = FALSE
  )
  
  # Add node to structure
  tree_structure$nodes <- rbind(tree_structure$nodes, node_info)
  
  # Add edge if not root
  if (!is.null(parent_id)) {
    edge_info <- data.frame(
      from = parent_id,
      to = node_id,
      label = get_edge_label(node_data),
      stringsAsFactors = FALSE
    )
    tree_structure$edges <- rbind(tree_structure$edges, edge_info)
  }
  
  # Set root if this is the first node
  if (is.null(tree_structure$root)) {
    tree_structure$root <- node_id
  }
  
  # Process children if they exist
  if ("true" %in% names(node_data) && is.list(node_data$true)) {
    tree_structure <- parse_tree_recursive(node_data$true, tree_structure, 
                                         feature_names, node_id + 1, node_id, depth + 1)
  }
  
  if ("false" %in% names(node_data) && is.list(node_data$false)) {
    tree_structure <- parse_tree_recursive(node_data$false, tree_structure, 
                                         feature_names, node_id + 2, node_id, depth + 1)
  }
  
  return(tree_structure)
}

#' Determine Node Type
#'
#' @description
#' Determine the type of a tree node.
#'
#' @param node_data List: node data
#'
#' @return Character string: node type
#'
#' @keywords internal
determine_node_type <- function(node_data) {
  if ("feature" %in% names(node_data)) {
    return("decision")
  } else if ("prediction" %in% names(node_data)) {
    return("leaf")
  } else {
    return("unknown")
  }
}

#' Create Node Label
#'
#' @description
#' Create a label for a tree node.
#'
#' @param node_data List: node data
#' @param feature_names Character vector: names for features
#'
#' @return Character string: node label
#'
#' @keywords internal
create_node_label <- function(node_data, feature_names = NULL) {
  if ("feature" %in% names(node_data)) {
    feature_idx <- node_data$feature
    if (!is.null(feature_names) && feature_idx < length(feature_names)) {
      feature_name <- feature_names[feature_idx + 1]  # 0-based to 1-based indexing
    } else {
      feature_name <- paste("Feature", feature_idx)
    }
    return(feature_name)
  } else if ("prediction" %in% names(node_data)) {
    return(paste("Pred:", node_data$prediction))
  } else {
    return("Unknown")
  }
}

#' Get Node Prediction
#'
#' @description
#' Get the prediction value for a node.
#'
#' @param node_data List: node data
#'
#' @return Numeric: prediction value
#'
#' @keywords internal
get_node_prediction <- function(node_data) {
  if ("prediction" %in% names(node_data)) {
    return(node_data$prediction)
  } else {
    return(NA)
  }
}

#' Get Node Loss
#'
#' @description
#' Get the loss value for a node.
#'
#' @param node_data List: node data
#'
#' @return Numeric: loss value
#'
#' @keywords internal
get_node_loss <- function(node_data) {
  if ("loss" %in% names(node_data)) {
    return(node_data$loss)
  } else {
    return(NA)
  }
}

#' Get Node Feature
#'
#' @description
#' Get the feature index for a decision node.
#'
#' @param node_data List: node data
#' @param feature_names Character vector: names for features
#'
#' @return Character string: feature name or index
#'
#' @keywords internal
get_node_feature <- function(node_data, feature_names = NULL) {
  if ("feature" %in% names(node_data)) {
    feature_idx <- node_data$feature
    if (!is.null(feature_names) && feature_idx < length(feature_names)) {
      return(feature_names[feature_idx + 1])  # 0-based to 1-based indexing
    } else {
      return(paste("Feature", feature_idx))
    }
  } else {
    return(NA)
  }
}

#' Get Node Threshold
#'
#' @description
#' Get the threshold value for a decision node.
#'
#' @param node_data List: node data
#'
#' @return Numeric: threshold value
#'
#' @keywords internal
get_node_threshold <- function(node_data) {
  if ("reference" %in% names(node_data)) {
    return(node_data$reference)
  } else {
    return(NA)
  }
}

#' Get Edge Label
#'
#' @description
#' Get the label for an edge.
#'
#' @param node_data List: node data
#'
#' @return Character string: edge label
#'
#' @keywords internal
get_edge_label <- function(node_data) {
  if ("relation" %in% names(node_data)) {
    return(node_data$relation)
  } else {
    return("")
  }
}

#' Plot Advanced Tree Structure
#'
#' @description
#' Create an advanced visualization of tree structure using parsed data.
#'
#' @param tree_structure List: parsed tree structure
#' @param title Character: plot title
#' @param interactive Logical: whether to create interactive plot
#'
#' @return A ggplot2 object or interactive plot
#'
#' @export
plot_advanced_tree <- function(tree_structure, title = "TreeFARMS Decision Tree", 
                              interactive = FALSE) {
  
  if (nrow(tree_structure$nodes) == 0) {
    stop("No nodes found in tree structure")
  }
  
  # Calculate node positions
  tree_structure$nodes <- calculate_node_positions(tree_structure$nodes)
  
  if (interactive) {
    return(plot_advanced_tree_interactive(tree_structure, title))
  } else {
    return(plot_advanced_tree_static(tree_structure, title))
  }
}

#' Calculate Node Positions
#'
#' @description
#' Calculate x,y positions for tree nodes.
#'
#' @param nodes Data frame: node information
#'
#' @return Data frame: nodes with position information
#'
#' @keywords internal
calculate_node_positions <- function(nodes) {
  
  # Simple positioning algorithm
  # In a full implementation, this would use a proper tree layout algorithm
  
  max_depth <- max(nodes$depth)
  
  # Calculate y positions (depth-based)
  nodes$y <- -nodes$depth * 2
  
  # Calculate x positions (breadth-based)
  nodes$x <- 0
  for (depth in 0:max_depth) {
    depth_nodes <- nodes[nodes$depth == depth, ]
    if (nrow(depth_nodes) > 0) {
      x_positions <- seq(-nrow(depth_nodes), nrow(depth_nodes), length.out = nrow(depth_nodes))
      nodes[nodes$depth == depth, "x"] <- x_positions
    }
  }
  
  return(nodes)
}

#' Plot Advanced Tree Structure (Static)
#'
#' @description
#' Create a static advanced tree visualization.
#'
#' @param tree_structure List: parsed tree structure
#' @param title Character: plot title
#'
#' @return A ggplot2 object
#'
#' @export
plot_advanced_tree_static <- function(tree_structure, title = "TreeFARMS Decision Tree") {
  
  nodes <- tree_structure$nodes
  edges <- tree_structure$edges
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
  
  # Add edges
  if (nrow(edges) > 0) {
    # Get edge coordinates
    edge_coords <- merge(edges, nodes, by.x = "from", by.y = "id")
    edge_coords <- merge(edge_coords, nodes, by.x = "to", by.y = "id", 
                        suffixes = c("_from", "_to"))
    
    p <- p + ggplot2::geom_segment(data = edge_coords,
                                  ggplot2::aes(x = x_from, y = y_from, 
                                             xend = x_to, yend = y_to),
                                  color = "gray50", size = 1)
    
    # Add edge labels
    if ("label" %in% names(edge_coords)) {
      edge_coords$mid_x <- (edge_coords$x_from + edge_coords$x_to) / 2
      edge_coords$mid_y <- (edge_coords$y_from + edge_coords$y_to) / 2
      
      p <- p + ggplot2::geom_text(data = edge_coords,
                                 ggplot2::aes(x = mid_x, y = mid_y, label = label),
                                 size = 2.5, hjust = 0.5, vjust = -0.5)
    }
  }
  
  # Add nodes
  p <- p + ggplot2::geom_point(data = nodes,
                              ggplot2::aes(x = x, y = y, color = type, size = type),
                              alpha = 0.8)
  
  # Add node labels
  p <- p + ggplot2::geom_text(data = nodes,
                             ggplot2::aes(x = x, y = y, label = label),
                             size = 3, hjust = 0.5, vjust = 0.5)
  
  # Customize appearance
  p <- p + ggplot2::scale_color_manual(values = c("decision" = "lightblue", 
                                                "leaf" = "lightgreen",
                                                "root" = "orange")) +
    ggplot2::scale_size_manual(values = c("decision" = 4, 
                                        "leaf" = 3,
                                        "root" = 5)) +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

#' Plot Advanced Tree Structure (Interactive)
#'
#' @description
#' Create an interactive advanced tree visualization.
#'
#' @param tree_structure List: parsed tree structure
#' @param title Character: plot title
#'
#' @return A DiagrammeR object
#'
#' @export
plot_advanced_tree_interactive <- function(tree_structure, title = "TreeFARMS Decision Tree") {
  
  nodes <- tree_structure$nodes
  edges <- tree_structure$edges
  
  # Prepare nodes for DiagrammeR
  diagrammer_nodes <- data.frame(
    id = nodes$id,
    label = nodes$label,
    type = nodes$type,
    stringsAsFactors = FALSE
  )
  
  # Prepare edges for DiagrammeR
  diagrammer_edges <- data.frame(
    from = edges$from,
    to = edges$to,
    label = edges$label,
    stringsAsFactors = FALSE
  )
  
  # Create DiagrammeR graph
  graph <- DiagrammeR::create_graph(
    nodes_df = diagrammer_nodes,
    edges_df = diagrammer_edges
  )
  
  # Render graph
  DiagrammeR::render_graph(graph, layout = "tree")
}

#' Create Tree Summary Visualization
#'
#' @description
#' Create a summary visualization showing tree statistics.
#'
#' @param model A treefarms model object
#' @param tree_index Integer: which tree to summarize
#'
#' @return A ggplot2 object
#'
#' @export
plot_tree_summary <- function(model, tree_index = 1) {
  
  if (model$n_trees == 0) {
    stop("No trees available for summary")
  }
  
  if (tree_index > model$n_trees) {
    stop(sprintf("Tree index %d exceeds number of trees (%d)", tree_index, model$n_trees))
  }
  
  # Parse tree structure
  tree_json <- model$trees[[tree_index]]
  tree_structure <- parse_tree_structure(tree_json, colnames(model$X_train))
  
  # Create summary data
  summary_data <- data.frame(
    metric = c("Total Nodes", "Decision Nodes", "Leaf Nodes", "Max Depth", "Avg Loss"),
    value = c(nrow(tree_structure$nodes),
              sum(tree_structure$nodes$type == "decision"),
              sum(tree_structure$nodes$type == "leaf"),
              max(tree_structure$nodes$depth),
              round(mean(tree_structure$nodes$loss, na.rm = TRUE), 4)),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_col(ggplot2::aes(fill = metric), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = value), 
                      size = 4, hjust = 0.5, vjust = -0.5) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                  legend.position = "none") +
    ggplot2::labs(title = paste("Tree", tree_index, "Summary"),
                 subtitle = "Key statistics",
                 x = "Metric", y = "Value")
  
  return(p)
}


