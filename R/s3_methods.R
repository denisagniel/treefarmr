#' S3 Methods for TreeFARMS Models
#'
#' @description
#' S3 methods for TreeFARMS model objects to provide standard R interface
#' for predict, print, summary, and other generic functions.

# Load required packages (checked at runtime within functions that need them)
# if (!requireNamespace("jsonlite", quietly = TRUE)) {
#   stop("Package 'jsonlite' is required for S3 methods")
# }

#' Parse Tree JSON Structure
#'
#' @description
#' Parse a tree JSON structure into a readable R format for printing and plotting.
#' Handles different tree formats (single leaf, splits, nested).
#'
#' @param tree_json A JSON object (list) representing a tree structure
#' @param feature_names Optional vector of feature names for display
#'
#' @return A list with parsed tree structure containing:
#'   - type: "split" or "leaf"
#'   - feature: feature name or index
#'   - condition: split condition (e.g., "== 1")
#'   - false: false branch (list or leaf)
#'   - true: true branch (list or leaf)
#'   - For leaves: prediction, loss, complexity, probabilities
#'
#' @noRd
parse_tree_json <- function(tree_json, feature_names = NULL) {
  if (is.null(tree_json)) {
    return(NULL)
  }
  
  # If this is a leaf node (has prediction)
  if (!is.null(tree_json$prediction)) {
    return(list(
      type = "leaf",
      prediction = tree_json$prediction,
      loss = if (!is.null(tree_json$loss)) tree_json$loss else NA,
      complexity = if (!is.null(tree_json$complexity)) tree_json$complexity else NA,
      probabilities = if (!is.null(tree_json$probabilities)) tree_json$probabilities else NULL
    ))
  }
  
  # If this is a split node (has feature)
  if (!is.null(tree_json$feature)) {
    # Get feature name
    feature_idx <- tree_json$feature
    feature_name <- if (!is.null(tree_json$name)) {
      # Remove quotes from name if present
      gsub('"', '', tree_json$name)
    } else if (!is.null(feature_names) && feature_idx < length(feature_names)) {
      feature_names[feature_idx + 1]  # R uses 1-based indexing
    } else {
      paste0("V", feature_idx + 1)  # Default to V1, V2, etc.
    }
    
    # Build condition string
    relation <- if (!is.null(tree_json$relation)) tree_json$relation else "=="
    reference <- if (!is.null(tree_json$reference)) tree_json$reference else ""
    condition <- paste(relation, reference)
    
    # Parse children
    false_branch <- if (!is.null(tree_json$false)) {
      parse_tree_json(tree_json$false, feature_names)
    } else {
      NULL
    }
    
    true_branch <- if (!is.null(tree_json$true)) {
      parse_tree_json(tree_json$true, feature_names)
    } else {
      NULL
    }
    
    return(list(
      type = "split",
      feature = feature_name,
      feature_idx = feature_idx,
      condition = condition,
      relation = relation,
      reference = reference,
      false = false_branch,
      true = true_branch,
      model_objective = if (!is.null(tree_json$model_objective)) tree_json$model_objective else NA
    ))
  }
  
  # Unknown structure
  return(NULL)
}

#' Print Tree Structure Recursively
#'
#' @description
#' Recursively print a parsed tree structure in a readable format.
#'
#' @param tree Parsed tree structure from parse_tree_json()
#' @param indent Current indentation level
#' @param feature_names Optional vector of feature names
#'
#' @noRd
print_tree_recursive <- function(tree, indent = 0, feature_names = NULL) {
  if (is.null(tree)) return()
  
  indent_str <- paste(rep("  ", indent), collapse = "")
  
  if (tree$type == "leaf") {
    # Leaf node
    cat(indent_str, "Leaf: prediction=", tree$prediction, sep = "")
    if (!is.na(tree$loss)) {
      cat(", loss=", sprintf("%.3f", tree$loss), sep = "")
    }
    if (!is.na(tree$complexity)) {
      cat(", complexity=", sprintf("%.3f", tree$complexity), sep = "")
    }
    if (!is.null(tree$probabilities) && length(tree$probabilities) > 0) {
      cat(", probabilities=[", paste(sprintf("%.3f", tree$probabilities), collapse = ", "), "]", sep = "")
    }
    cat("\n")
  } else if (tree$type == "split") {
    # Split node
    cat(indent_str, "Split on feature ", tree$feature, " ", tree$relation, " ", tree$reference, "\n", sep = "")
    
    # Print false branch
    cat(indent_str, "  If FALSE:\n", sep = "")
    if (!is.null(tree$false)) {
      print_tree_recursive(tree$false, indent + 2, feature_names)
    }
    
    # Print true branch
    cat(indent_str, "  If TRUE:\n", sep = "")
    if (!is.null(tree$true)) {
      print_tree_recursive(tree$true, indent + 2, feature_names)
    }
  }
}

#' Extract Tree JSON from stdout Output
#'
#' @description
#' Extract tree JSON structures from captured stdout output.
#' Looks for JSON objects that start with curly braces and contain tree structure.
#'
#' @param stdout_lines Character vector of lines from captured stdout
#'
#' @return List of parsed tree JSON objects, or NULL if none found
#'
#' @noRd
extract_tree_from_stdout <- function(stdout_lines) {
  if (is.null(stdout_lines) || length(stdout_lines) == 0) {
    return(NULL)
  }
  
  # Combine all lines
  full_output <- paste(stdout_lines, collapse = "\n")
  
  # Find JSON objects - look for lines that start with { and contain tree structure
  # The tree JSON is printed after "Finding Optimal Objective..." or similar messages
  # and before the ModelSet serialization
  
  # Try to find JSON objects that look like trees
  # Pattern: starts with {, contains either "feature" or "prediction"
  tree_jsons <- list()
  
  # Look for JSON blocks - they start with { and end with }
  # We need to find balanced braces
  lines <- stdout_lines
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    # Check if this line starts a JSON object
    if (grepl("^\\s*\\{", line)) {
      # Found start of JSON - collect until matching }
      json_lines <- character(0)
      brace_count <- 0
      start_line <- i
      
      for (j in i:length(lines)) {
        current_line <- lines[j]
        json_lines <- c(json_lines, current_line)
        
        # Count braces
        brace_count <- brace_count + 
          nchar(gsub("[^{]", "", current_line)) - 
          nchar(gsub("[^}]", "", current_line))
        
        if (brace_count == 0 && j > start_line) {
          # Found complete JSON object
          json_str <- paste(json_lines, collapse = "\n")
          
          # Check if this looks like a tree (has feature or prediction)
          if (grepl('"feature"|"prediction"', json_str)) {
            tryCatch({
              tree_json <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
              # Only add if it's actually a tree structure
              if (!is.null(tree_json$feature) || !is.null(tree_json$prediction)) {
                tree_jsons[[length(tree_jsons) + 1]] <- tree_json
              }
            }, error = function(e) {
              # Not valid JSON, skip
            })
          }
          
          i <- j
          break
        }
      }
    }
    i <- i + 1
  }
  
  if (length(tree_jsons) == 0) {
    return(NULL)
  } else if (length(tree_jsons) == 1) {
    return(tree_jsons[[1]])
  } else {
    return(tree_jsons)
  }
}

#' Print method for treefarms_model
#'
#' @param x A treefarms_model object
#' @param ... Additional arguments (unused)
#'
#' @export
print.treefarms_model <- function(x, ...) {
  cat("TreeFARMS Model\n")
  cat("===============\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("Number of trees:", x$n_trees, "\n")
  cat("Training accuracy:", round(x$accuracy, 4), "\n")
  n_samp <- if (!is.null(x$n_train)) x$n_train else nrow(x$X_train)
  cat("Training samples:", n_samp, "\n")
  cat("Features:", ncol(x$X_train), "\n")
  
  # Get feature names from column names if available
  feature_names <- if (!is.null(colnames(x$X_train))) {
    colnames(x$X_train)
  } else {
    paste0("V", seq_len(ncol(x$X_train)))
  }
  
  # Print tree structure if available
  if (!is.null(x$model$tree_json)) {
    cat("\nTree Structure:\n")
    cat("---------------\n")
    
    # Handle both single tree and list of trees
    trees <- if (is.list(x$model$tree_json) && !is.null(x$model$tree_json$type)) {
      # Single tree
      list(x$model$tree_json)
    } else if (is.list(x$model$tree_json) && length(x$model$tree_json) > 0) {
      # List of trees
      x$model$tree_json
    } else {
      NULL
    }
    
    if (!is.null(trees)) {
      for (i in seq_along(trees)) {
        if (i > 1) cat("\n")
        if (length(trees) > 1) {
          cat("Tree", i, ":\n")
        }
        # Parse and print tree
        parsed_tree <- parse_tree_json(trees[[i]], feature_names)
        if (!is.null(parsed_tree)) {
          print_tree_recursive(parsed_tree, indent = 0, feature_names)
        }
      }
    }
  } else if (x$n_trees > 0) {
    cat("\nNote: Tree structure not available for display.\n")
  }
}

#' Plot method for treefarms_model
#'
#' @param x A treefarms_model object
#' @param tree_index Integer: which tree to plot (default: 1)
#' @param ... Additional arguments (unused)
#'
#' @export
plot.treefarms_model <- function(x, tree_index = 1, ...) {
  if (x$n_trees == 0) {
    stop("No trees available for plotting")
  }
  
  if (!is.null(x$model$tree_json)) {
    # Get feature names from column names if available
    feature_names <- if (!is.null(colnames(x$X_train))) {
      colnames(x$X_train)
    } else {
      paste0("V", seq_len(ncol(x$X_train)))
    }
    
    # Handle both single tree and list of trees
    trees <- if (is.list(x$model$tree_json) && !is.null(x$model$tree_json$type)) {
      # Single tree
      list(x$model$tree_json)
    } else if (is.list(x$model$tree_json) && length(x$model$tree_json) > 0) {
      # List of trees
      x$model$tree_json
    } else {
      NULL
    }
    
    if (!is.null(trees) && tree_index <= length(trees)) {
      # Parse tree
      parsed_tree <- parse_tree_json(trees[[tree_index]], feature_names)
      
      if (!is.null(parsed_tree)) {
        # Simple text-based tree plot
        cat("\nTree Visualization (Tree", tree_index, "):\n")
        cat("==========================================\n")
        print_tree_recursive(parsed_tree, indent = 0, feature_names)
        
        # If DiagrammeR is available, create interactive plot
        if (requireNamespace("DiagrammeR", quietly = TRUE)) {
          return(plot_tree_diagrammer(parsed_tree, feature_names))
        } else {
          cat("\n(Install 'DiagrammeR' package for interactive tree plots)\n")
          return(invisible(NULL))
        }
      }
    }
  }
  
  stop("Tree structure not available for plotting")
}

#' Plot Tree Using DiagrammeR
#'
#' @description
#' Create an interactive tree plot using DiagrammeR.
#'
#' @param tree Parsed tree structure
#' @param feature_names Character vector: names for features
#'
#' @return A DiagrammeR graph object
#'
#' @noRd
plot_tree_diagrammer <- function(tree, feature_names = NULL) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required for interactive tree plots")
  }
  
  # Build nodes and edges from tree structure
  nodes_list <- list()
  edges_list <- list()
  node_id_counter <- 0
  
  # Recursive function to build graph
  build_graph <- function(node, parent_id = NULL, edge_label = NULL) {
    if (is.null(node)) return(NULL)
    
    current_id <- node_id_counter
    node_id_counter <<- node_id_counter + 1
    
    if (node$type == "leaf") {
      # Leaf node
      label <- paste0("Prediction: ", node$prediction)
      if (!is.na(node$loss)) {
        label <- paste0(label, "\nLoss: ", sprintf("%.3f", node$loss))
      }
      if (!is.null(node$probabilities) && length(node$probabilities) > 0) {
        label <- paste0(label, "\nProb: [", paste(sprintf("%.2f", node$probabilities), collapse = ", "), "]")
      }
      
      nodes_list[[length(nodes_list) + 1]] <<- list(
        id = current_id,
        label = label,
        type = "leaf"
      )
    } else if (node$type == "split") {
      # Split node
      label <- paste0(node$feature, "\n", node$relation, " ", node$reference)
      
      nodes_list[[length(nodes_list) + 1]] <<- list(
        id = current_id,
        label = label,
        type = "split"
      )
      
      # Add edge from parent if exists
      if (!is.null(parent_id)) {
        edges_list[[length(edges_list) + 1]] <<- list(
          from = parent_id,
          to = current_id,
          label = edge_label
        )
      }
      
      # Process children
      if (!is.null(node$false)) {
        build_graph(node$false, current_id, "FALSE")
      }
      if (!is.null(node$true)) {
        build_graph(node$true, current_id, "TRUE")
      }
    }
    
    return(current_id)
  }
  
  # Build graph
  build_graph(tree)
  
  # Convert to DiagrammeR format
  if (length(nodes_list) > 0) {
    nodes_df <- data.frame(
      id = sapply(nodes_list, function(x) x$id),
      label = sapply(nodes_list, function(x) x$label),
      type = sapply(nodes_list, function(x) x$type),
      stringsAsFactors = FALSE
    )
    
    if (length(edges_list) > 0) {
      edges_df <- data.frame(
        from = sapply(edges_list, function(x) x$from),
        to = sapply(edges_list, function(x) x$to),
        label = sapply(edges_list, function(x) x$label),
        stringsAsFactors = FALSE
      )
    } else {
      edges_df <- data.frame(from = integer(0), to = integer(0), label = character(0))
    }
    
    # Create DiagrammeR graph
    graph <- DiagrammeR::create_graph(
      nodes_df = nodes_df,
      edges_df = edges_df
    )
    
    # Render with tree layout
    return(DiagrammeR::render_graph(graph, layout = "tree"))
  }
  
  return(invisible(NULL))
}

#' Print method for treefarms_logloss_model
#'
#' @param x A treefarms_logloss_model object
#' @param ... Additional arguments (unused)
#'
#' @export
print.treefarms_logloss_model <- function(x, ...) {
  cat("TreeFARMS Log-Loss Model (Subprocess)\n")
  cat("=====================================\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("Number of trees:", x$n_trees, "\n")
  cat("Training accuracy:", round(x$accuracy, 4), "\n")
  n_samp <- if (!is.null(x$n_train)) x$n_train else nrow(x$X_train)
  cat("Training samples:", n_samp, "\n")
  cat("Features:", ncol(x$X_train), "\n")
  
  if (x$n_trees > 0) {
    cat("\nTree Summary:\n")
    for (i in seq_len(min(x$n_trees, 3))) {  # Show first 3 trees
      tree_json <- jsonlite::fromJSON(x$trees[[i]])
      if ("prediction" %in% names(tree_json)) {
        cat(sprintf("  Tree %d: Prediction = %s\n", i, tree_json$prediction))
      }
    }
    if (x$n_trees > 3) {
      cat(sprintf("  ... and %d more trees\n", x$n_trees - 3))
    }
  }
}


#' Print method for cf_rashomon
#'
#' @param x A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
print.cf_rashomon <- function(x, ...) {
  cat("Cross-Fitted Rashomon Set Analysis\n")
  cat("==================================\n")
  cat("Number of folds:", x$K, "\n")
  cat("Loss function:", x$loss_function, "\n")
  cat("Regularization:", x$regularization, "\n")
  cat("\nRashomon set sizes per fold:\n")
  for (i in seq_along(x$rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", i, x$rashomon_sizes[i]))
  }
  cat("\nIntersecting trees:", x$n_intersecting, "\n")
  
  if (x$n_intersecting > 0) {
    cat("\n✓ Found stable tree(s) appearing in all folds!\n")
    cat("  Use predict() to make predictions with the stable model.\n")
  } else {
    cat("\n✗ No trees found in all folds.\n")
    cat("  Consider adjusting regularization or rashomon_bound_multiplier.\n")
  }
}

#' Summary method for treefarms_logloss_model
#'
#' @param object A treefarms_logloss_model object
#' @param ... Additional arguments (unused)
#'
#' @export
summary.treefarms_logloss_model <- function(object, ...) {
  cat("TreeFARMS Log-Loss Model Summary\n")
  cat("===============================\n\n")
  
  # Model information
  cat("Model Configuration:\n")
  cat("  Loss function:", object$loss_function, "\n")
  cat("  Regularization:", object$regularization, "\n")
  cat("  Number of trees:", object$n_trees, "\n\n")
  
  # Training data information
  cat("Training Data:\n")
  n_samp <- if (!is.null(object$n_train)) object$n_train else nrow(object$X_train)
  cat("  Samples:", n_samp, "\n")
  cat("  Features:", ncol(object$X_train), "\n")
  cat("  Class distribution:\n")
  class_counts <- table(object$y_train)
  for (i in seq_along(class_counts)) {
    cat(sprintf("    Class %s: %d (%.1f%%)\n", 
                names(class_counts)[i], 
                class_counts[i], 
                100 * class_counts[i] / sum(class_counts)))
  }
  cat("\n")
  
  # Performance
  cat("Performance:\n")
  cat("  Training accuracy:", round(object$accuracy, 4), "\n")
  cat("  Loss function:", object$loss_function, "\n\n")
  
  # Tree details
  if (object$n_trees > 0) {
    cat("Tree Details:\n")
    for (i in seq_len(min(object$n_trees, 5))) {  # Show first 5 trees
      tree_json <- jsonlite::fromJSON(object$trees[[i]])
      cat(sprintf("  Tree %d:\n", i))
      if ("prediction" %in% names(tree_json)) {
        cat(sprintf("    Prediction: %s\n", tree_json$prediction))
      }
      if ("loss" %in% names(tree_json)) {
        cat(sprintf("    Loss: %.4f\n", tree_json$loss))
      }
    }
    if (object$n_trees > 5) {
      cat(sprintf("  ... and %d more trees\n", object$n_trees - 5))
    }
  }
}

#' Summary method for cf_rashomon
#'
#' @param object A cf_rashomon object
#' @param ... Additional arguments (unused)
#'
#' @export
summary.cf_rashomon <- function(object, ...) {
  cat("Cross-Fitted Rashomon Set Summary\n")
  cat("=================================\n\n")
  
  # Configuration
  cat("Configuration:\n")
  cat("  K-fold cross-fitting: K =", object$K, "\n")
  cat("  Loss function:", object$loss_function, "\n")
  cat("  Regularization:", object$regularization, "\n\n")
  
  # Data information
  cat("Data:\n")
  n_samp <- if (!is.null(object$n_train)) object$n_train else nrow(object$X_train)
  cat("  Samples:", n_samp, "\n")
  cat("  Features:", ncol(object$X_train), "\n\n")
  
  # Fold results
  cat("Fold Results:\n")
  for (i in seq_along(object$rashomon_sizes)) {
    cat(sprintf("  Fold %d: %d trees\n", i, object$rashomon_sizes[i]))
  }
  cat("\n")
  
  # Intersection results
  cat("Intersection Results:\n")
  cat("  Trees in all folds:", object$n_intersecting, "\n")
  
  if (object$n_intersecting > 0) {
    cat("  ✓ Stable trees found!\n")
    cat("  Use predict() to make predictions with stable model.\n")
  } else {
    cat("  ✗ No stable trees found.\n")
    cat("  Consider:\n")
    cat("    - Increasing regularization\n")
    cat("    - Adjusting rashomon_bound_multiplier\n")
    cat("    - Using different K value\n")
  }
}

#' Predict method for treefarms_logloss_model
#'
#' @param object A treefarms_logloss_model object
#' @param newdata A data.frame or matrix of new features
#' @param type Character string: "class" or "prob"
#' @param ... Additional arguments (unused)
#'
#' @export
predict.treefarms_logloss_model <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  
  # Use the same prediction logic as treefarms_model
  # Get tree structure from model object
  tree_to_use <- if (!is.null(object$model$tree_json)) {
    object$model$tree_json
  } else if (!is.null(object$model$result_data)) {
    object$model$result_data
  } else {
    NULL
  }
  
  # Convert to data.frame if matrix
  if (is.matrix(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  
  # Validate features if training data is available
  if (!is.null(object$X_train)) {
    if (!identical(names(newdata), names(object$X_train))) {
      stop("Feature names in newdata must match training data")
    }
  }
  
  # Check for binary features
  for (col in names(newdata)) {
    if (!all(newdata[[col]] %in% c(0, 1))) {
      stop(paste("Feature", col, "must contain only binary values (0 and 1)"))
    }
  }
  
  # Extract probabilities from tree structure
  if (!is.null(tree_to_use)) {
    # Use the get_probabilities_from_tree function from treefarms.R
    # This function is available in the package namespace
    # We need to access it via the parent environment or use do.call
    get_probabilities_from_tree <- get("get_probabilities_from_tree", envir = asNamespace("treefarmr"))
    probabilities <- get_probabilities_from_tree(tree_to_use, newdata)
    
    if (type == "class") {
      # Derive predictions from probabilities (argmax)
      predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
      return(predictions)
    } else {
      return(probabilities)
    }
  } else {
    # No tree available, return default values
    n_samples <- nrow(newdata)
    if (type == "class") {
      return(rep(0, n_samples))
    } else {
      return(matrix(c(0.5, 0.5), nrow = n_samples, ncol = 2, byrow = TRUE))
    }
  }
}



