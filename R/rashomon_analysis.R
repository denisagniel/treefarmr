#' Rashomon Set Analysis Functions
#'
#' @description
#' Analysis and visualization functions for Rashomon sets and cross-fitted results.
#' Provides diagnostic tools for understanding model stability and tree characteristics.

# Load required packages (checked at runtime within functions that need them)
# if (!requireNamespace("ggplot2", quietly = TRUE)) {
#   stop("Package 'ggplot2' is required for Rashomon analysis. Install with: install.packages('ggplot2')")
# }

#' Plot Rashomon Set Sizes
#'
#' @description
#' Create a bar plot showing the size of Rashomon sets across different folds.
#'
#' @param cf_result A cf_rashomon object from cross_fitted_rashomon()
#' @param title Character: plot title
#' @param ... Additional arguments passed to ggplot2
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Run cross-fitted Rashomon analysis
#' result <- cross_fitted_rashomon(X, y, K = 5)
#' 
#' # Plot Rashomon set sizes
#' plot_rashomon_sizes(result)
#' }
#'
#' @export
plot_rashomon_sizes <- function(cf_result, title = "Rashomon Set Sizes Across Folds", ...) {
  
  if (!inherits(cf_result, "cf_rashomon")) {
    stop("cf_result must be a cf_rashomon object")
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    fold = 1:cf_result$K,
    size = cf_result$rashomon_sizes
  )
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(fold), y = size)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = size), vjust = -0.5, size = 3) +
    ggplot2::labs(
      title = title,
      x = "Fold",
      y = "Rashomon Set Size",
      subtitle = sprintf("Found %d intersecting tree(s) across all folds", cf_result$n_intersecting)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  return(p)
}

#' Compare Rashomon Sets
#'
#' @description
#' Compare two Rashomon sets to find common and unique trees.
#'
#' @param set1 First Rashomon set (list of tree JSON strings)
#' @param set2 Second Rashomon set (list of tree JSON strings)
#' @param verbose Logical: whether to print detailed comparison
#'
#' @return A list containing:
#'   \item{common_trees}{Trees appearing in both sets}
#'   \item{unique_to_set1}{Trees only in set1}
#'   \item{unique_to_set2}{Trees only in set2}
#'   \item{set1_size}{Size of set1}
#'   \item{set2_size}{Size of set2}
#'   \item{common_size}{Number of common trees}
#'
#' @examples
#' \dontrun{
#' # Compare two Rashomon sets
#' comparison <- compare_rashomon_sets(set1, set2, verbose = TRUE)
#' print(comparison)
#' }
#'
#' @export
compare_rashomon_sets <- function(set1, set2, verbose = FALSE) {
  
  # Validate inputs
  if (!is.list(set1) || !is.list(set2)) {
    stop("set1 and set2 must be lists of tree objects")
  }
  
  # Convert trees to JSON strings for comparison
  if (length(set1) > 0) {
    if (is.character(set1[[1]])) {
      set1_json <- set1
    } else {
      set1_json <- sapply(set1, function(tree) {
        if (is.list(tree) && "json" %in% names(tree)) {
          return(as.character(tree$json))
        } else if ("json" %in% names(tree)) {
          return(as.character(tree$json()))
        } else {
          stop("Tree object does not have a json() method or json field")
        }
      })
    }
  } else {
    set1_json <- character(0)
  }
  
  if (length(set2) > 0) {
    if (is.character(set2[[1]])) {
      set2_json <- set2
    } else {
      set2_json <- sapply(set2, function(tree) {
        if (is.list(tree) && "json" %in% names(tree)) {
          return(as.character(tree$json))
        } else if ("json" %in% names(tree)) {
          return(as.character(tree$json()))
        } else {
          stop("Tree object does not have a json() method or json field")
        }
      })
    }
  } else {
    set2_json <- character(0)
  }
  
  # Find common trees
  common_trees <- intersect(set1_json, set2_json)
  
  # Find unique trees
  unique_to_set1 <- setdiff(set1_json, set2_json)
  unique_to_set2 <- setdiff(set2_json, set1_json)
  
  # Create result
  result <- list(
    common_trees = common_trees,
    unique_to_set1 = unique_to_set1,
    unique_to_set2 = unique_to_set2,
    set1_size = length(set1_json),
    set2_size = length(set2_json),
    common_size = length(common_trees)
  )
  
  # Print summary if verbose
  if (verbose) {
    cat("Rashomon Set Comparison\n")
    cat("======================\n")
    cat("Set 1 size:", result$set1_size, "\n")
    cat("Set 2 size:", result$set2_size, "\n")
    cat("Common trees:", result$common_size, "\n")
    cat("Unique to set 1:", length(result$unique_to_set1), "\n")
    cat("Unique to set 2:", length(result$unique_to_set2), "\n")
    cat("Overlap percentage:", round(100 * result$common_size / min(result$set1_size, result$set2_size), 1), "%\n")
  }
  
  return(result)
}

#' Get Tree Rules
#'
#' @description
#' Extract human-readable rules from a tree's JSON representation.
#'
#' @param tree_json Character: JSON string representation of a tree
#' @param feature_names Character vector: names of features (optional)
#'
#' @return A list containing:
#'   \item{rules}{List of decision rules}
#'   \item{leaves}{List of leaf node predictions}
#'   \item{features_used}{Vector of features used in the tree}
#'
#' @examples
#' \dontrun{
#' # Get rules from a tree
#' rules <- get_tree_rules(tree_json, feature_names = colnames(X))
#' print(rules)
#' }
#'
#' @export
get_tree_rules <- function(tree_json, feature_names = NULL) {
  
  # Handle different tree formats
  if (is.list(tree_json)) {
    if ("json" %in% names(tree_json)) {
      tree_json <- tree_json$json
    } else if (length(tree_json) == 1 && is.character(tree_json[[1]])) {
      # Tree is a list containing a single JSON string
      tree_json <- tree_json[[1]]
    } else if (length(tree_json) == 1 && is.list(tree_json[[1]]) && length(tree_json[[1]]) == 0) {
      # Tree is a list containing an empty list - no tree structure
      return(list(
        rules = list(),
        features_used = character(0),
        n_rules = 0
      ))
    } else {
      stop("Cannot extract JSON from tree object")
    }
  }
  
  # Parse the JSON
  tree_data <- jsonlite::fromJSON(tree_json, simplifyVector = FALSE)
  
  # Extract rules and leaves
  rules <- list()
  leaves <- list()
  features_used <- character(0)
  
  # Recursive function to extract rules
  extract_rules <- function(node, path = "") {
    if (is.null(node$children) || length(node$children) == 0) {
      # Leaf node
      leaf_id <- length(leaves) + 1
      leaves[[leaf_id]] <<- list(
        path = path,
        prediction = node$prediction,
        loss = if (!is.null(node$loss)) node$loss else NA
      )
      return()
    }
    
    # Internal node
    feature_idx <- node$feature
    feature_name <- if (!is.null(feature_names) && feature_idx < length(feature_names)) {
      feature_names[feature_idx + 1]  # Feature index is 0-based in JSON, convert to 1-based for R
    } else {
      paste0("feature_", feature_idx)
    }
    
    features_used <<- unique(c(features_used, feature_name))
    
    # Process children
    for (i in seq_along(node$children)) {
      child <- node$children[[i]]
      child_path <- if (path == "") {
        paste0(feature_name, " == ", child$value)
      } else {
        paste0(path, " AND ", feature_name, " == ", child$value)
      }
      extract_rules(child, child_path)
    }
  }
  
  # Extract rules from root
  extract_rules(tree_data)
  
  # Create result
  result <- list(
    rules = leaves,
    features_used = features_used,
    n_rules = length(leaves)
  )
  
  return(result)
}

#' Plot Tree Rules
#'
#' @description
#' Create a visualization of tree rules.
#'
#' @param tree_json Character: JSON string representation of a tree
#' @param feature_names Character vector: names of features (optional)
#' @param title Character: plot title
#' @param ... Additional arguments passed to ggplot2
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Plot tree rules
#' plot_tree_rules(tree_json, feature_names = colnames(X))
#' }
#'
#' @export
plot_tree_rules <- function(tree_json, feature_names = NULL, title = "Tree Decision Rules", ...) {
  
  # Get tree rules
  rules_data <- get_tree_rules(tree_json, feature_names)
  
  if (length(rules_data$rules) == 0) {
    stop("No rules found in tree")
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    rule_id = 1:length(rules_data$rules),
    prediction = sapply(rules_data$rules, function(x) x$prediction),
    loss = sapply(rules_data$rules, function(x) if (!is.na(x$loss)) x$loss else 0)
  )
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(rule_id), y = prediction)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = prediction), vjust = -0.5, size = 3) +
    ggplot2::labs(
      title = title,
      x = "Rule ID",
      y = "Prediction",
      subtitle = sprintf("Tree uses %d features: %s", 
                        length(rules_data$features_used),
                        paste(rules_data$features_used, collapse = ", "))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  return(p)
}

#' Analyze Cross-Fitted Rashomon Results
#'
#' @description
#' Provide a comprehensive analysis of cross-fitted Rashomon results.
#'
#' @param cf_result A cf_rashomon object from cross_fitted_rashomon()
#' @param verbose Logical: whether to print detailed analysis
#'
#' @return A list containing analysis results
#'
#' @examples
#' \dontrun{
#' # Analyze cross-fitted results
#' analysis <- analyze_cf_rashomon(result, verbose = TRUE)
#' }
#'
#' @export
analyze_cf_rashomon <- function(cf_result, verbose = FALSE) {
  
  if (!inherits(cf_result, "cf_rashomon")) {
    stop("cf_result must be a cf_rashomon object")
  }
  
  # Basic statistics
  analysis <- list(
    n_folds = cf_result$K,
    n_intersecting = cf_result$n_intersecting,
    rashomon_sizes = cf_result$rashomon_sizes,
    min_rashomon_size = min(cf_result$rashomon_sizes),
    max_rashomon_size = max(cf_result$rashomon_sizes),
    mean_rashomon_size = mean(cf_result$rashomon_sizes),
    sd_rashomon_size = sd(cf_result$rashomon_sizes)
  )
  
  # Stability analysis
  if (cf_result$n_intersecting > 0) {
    analysis$stability_found <- TRUE
    analysis$stability_percentage <- 100 * cf_result$n_intersecting / min(cf_result$rashomon_sizes)
  } else {
    analysis$stability_found <- FALSE
    analysis$stability_percentage <- 0
  }
  
  # Feature usage analysis
  if (cf_result$n_intersecting > 0) {
    # Analyze features used in intersecting trees
    features_used <- list()
    for (i in seq_len(cf_result$n_intersecting)) {
      tree_json <- cf_result$intersecting_trees[[i]]
      # Handle case where tree_json might be a list with json field
      if (is.list(tree_json) && "json" %in% names(tree_json)) {
        tree_json <- tree_json$json
      }
      tryCatch({
        tree_rules <- get_tree_rules(tree_json)
        features_used[[i]] <- tree_rules$features_used
      }, error = function(e) {
        # Skip this tree if there's an error
        features_used[[i]] <<- character(0)
      })
    }
    analysis$features_used <- unique(unlist(features_used))
    analysis$n_features_used <- length(analysis$features_used)
  } else {
    analysis$features_used <- character(0)
    analysis$n_features_used <- 0
  }
  
  # Print analysis if verbose
  if (verbose) {
    cat("Cross-Fitted Rashomon Analysis\n")
    cat("==============================\n")
    cat("Number of folds:", analysis$n_folds, "\n")
    cat("Rashomon set sizes:", paste(analysis$rashomon_sizes, collapse = ", "), "\n")
    cat("Min/Max/Mean size:", analysis$min_rashomon_size, "/", 
        analysis$max_rashomon_size, "/", round(analysis$mean_rashomon_size, 2), "\n")
    cat("Standard deviation:", round(analysis$sd_rashomon_size, 2), "\n")
    cat("\nStability Analysis:\n")
    cat("Intersecting trees:", analysis$n_intersecting, "\n")
    cat("Stability found:", analysis$stability_found, "\n")
    if (analysis$stability_found) {
      cat("Stability percentage:", round(analysis$stability_percentage, 1), "%\n")
      cat("Features used:", analysis$n_features_used, "\n")
      if (analysis$n_features_used > 0) {
        cat("Feature names:", paste(analysis$features_used, collapse = ", "), "\n")
      }
    }
  }
  
  return(analysis)
}
