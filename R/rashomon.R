#' Extract Trees from Rashomon Set
#'
#' @description
#' Extract all trees from a trained TreeFARMS model's Rashomon set.
#'
#' @param model A trained TreeFARMS model object (class `treefarms_model`)
#'
#' @return A list of tree objects from the model's Rashomon set
#'
#' @examples
#' \dontrun{
#' model <- treefarms(X, y, regularization = 0.1)
#' trees <- get_rashomon_trees(model)
#' cat("Number of trees:", length(trees), "\n")
#' }
#'
#' @export
get_rashomon_trees <- function(model) {
  if (!inherits(model, c("treefarms_model", "treefarms_logloss_model"))) {
    stop("model must be a treefarms_model or treefarms_logloss_model object")
  }
  
  if (inherits(model, "treefarms_logloss_model")) {
    # For log_loss models, trees are stored in model$trees
    if (model$n_trees == 0) {
      warning("No trees in Rashomon set")
      return(list())
    }
    return(model$trees)
  } else {
    # For regular models, use model_set
    model_set <- model$model$model_set
    tree_count <- model_set$get_tree_count()
    
    if (tree_count == 0) {
      warning("No trees in Rashomon set")
      return(list())
    }
    
    # Extract all trees using 0-based indexing
    trees <- lapply(0:(tree_count - 1), function(i) {
      model_set[i]
    })
    
    return(trees)
  }
}

#' Convert Tree to JSON String
#'
#' @description
#' Convert a TreeClassifier object to its JSON string representation.
#' This is used for comparing tree structures.
#'
#' @param tree A TreeClassifier object from the Rashomon set
#'
#' @return A character string containing the tree's JSON representation
#'
#' @examples
#' \dontrun{
#' trees <- get_rashomon_trees(model)
#' tree_json <- tree_to_json(trees[[1]])
#' }
#'
#' @export
tree_to_json <- function(tree) {
  # Handle both TreeClassifier objects and log_loss model trees
  if (is.character(tree)) {
    # This is already a JSON string (log_loss model)
    return(tree)
  } else if (is.list(tree) && "json" %in% names(tree)) {
    # This is a log_loss model tree with json field
    return(as.character(tree$json))
  } else if ("json" %in% names(tree)) {
    # This is a TreeClassifier object
    json_str <- as.character(tree$json())
    return(json_str)
  } else {
    stop("Tree object does not have a json() method or json field")
  }
}

#' Compare Two Trees for Structural Equality
#'
#' @description
#' Check if two tree structures are identical by comparing their JSON representations.
#'
#' @param tree1 First TreeClassifier object
#' @param tree2 Second TreeClassifier object
#'
#' @return Logical: TRUE if trees have identical structure, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' trees <- get_rashomon_trees(model)
#' are_same <- compare_trees(trees[[1]], trees[[2]])
#' }
#'
#' @export
compare_trees <- function(tree1, tree2) {
  json1 <- tree_to_json(tree1)
  json2 <- tree_to_json(tree2)
  
  return(identical(json1, json2))
}

#' Find Intersection of Trees Across Rashomon Sets
#'
#' @description
#' Find trees that appear in ALL Rashomon sets from cross-fitting.
#' This is the core function for identifying stable trees.
#'
#' @param rashomon_list A list of Rashomon sets, where each element is a list of trees
#' @param verbose Logical. Print progress information. Default: TRUE
#'
#' @return A list containing:
#'   \item{intersecting_trees}{List of tree objects appearing in all Rashomon sets}
#'   \item{n_intersecting}{Number of intersecting trees}
#'   \item{tree_jsons}{JSON representations of intersecting trees}
#'
#' @details
#' This function compares trees across K Rashomon sets by converting each tree
#' to its JSON representation and finding common trees across all sets.
#'
#' @examples
#' \dontrun{
#' # Get Rashomon sets from K models
#' rashomon_sets <- lapply(models, get_rashomon_trees)
#' 
#' # Find trees in all sets
#' result <- find_tree_intersection(rashomon_sets)
#' cat("Found", result$n_intersecting, "stable trees\n")
#' }
#'
#' @export
find_tree_intersection <- function(rashomon_list, verbose = TRUE) {
  K <- length(rashomon_list)
  
  if (K == 0) {
    stop("rashomon_list is empty")
  }
  
  if (K == 1) {
    if (verbose) {
      cat("Only one Rashomon set provided, returning all trees\n")
    }
    trees <- rashomon_list[[1]]
    if (length(trees) == 0) {
      tree_jsons <- character(0)
    } else if (is.character(trees[[1]])) {
      tree_jsons <- trees
    } else {
      tree_jsons <- sapply(trees, tree_to_json)
    }
    
    return(list(
      intersecting_trees = trees,
      n_intersecting = length(trees),
      tree_jsons = tree_jsons
    ))
  }
  
  if (verbose) {
    cat(sprintf("Finding intersection across %d Rashomon sets...\n", K))
    set_sizes <- sapply(rashomon_list, length)
    cat(sprintf("Rashomon set sizes: %s\n", paste(set_sizes, collapse = ", ")))
  }
  
  # Convert all trees to JSON for comparison
  json_sets <- lapply(rashomon_list, function(trees) {
    if (length(trees) == 0) {
      return(character(0))
    }
    # Check if trees are already JSON strings
    if (is.character(trees[[1]])) {
      return(trees)
    } else {
      return(sapply(trees, tree_to_json))
    }
  })
  
  # Start with first set as candidates
  intersection_jsons <- json_sets[[1]]
  
  if (verbose) {
    cat(sprintf("Starting with %d candidates from set 1\n", length(intersection_jsons)))
  }
  
  # Iteratively intersect with remaining sets
  for (k in 2:K) {
    # Keep only JSON strings that appear in set k
    intersection_jsons <- intersection_jsons[intersection_jsons %in% json_sets[[k]]]
    
    if (verbose) {
      cat(sprintf("After intersecting with set %d: %d trees remain\n", 
                  k, length(intersection_jsons)))
    }
    
    # Early exit if no intersection
    if (length(intersection_jsons) == 0) {
      if (verbose) {
        cat("No trees appear in all sets\n")
      }
      return(list(
        intersecting_trees = list(),
        n_intersecting = 0,
        tree_jsons = character(0)
      ))
    }
  }
  
  # Get the actual tree objects from the first set
  # (they're identical across sets, so we can use any)
  first_set_jsons <- json_sets[[1]]
  intersecting_indices <- which(first_set_jsons %in% intersection_jsons)
  intersecting_trees <- rashomon_list[[1]][intersecting_indices]
  
  if (verbose) {
    cat(sprintf("Found %d tree(s) in all %d Rashomon sets\n", 
                length(intersecting_trees), K))
  }
  
  return(list(
    intersecting_trees = intersecting_trees,
    n_intersecting = length(intersecting_trees),
    tree_jsons = intersection_jsons
  ))
}

#' Get Number of Trees in Rashomon Set
#'
#' @description
#' Get the count of trees in a model's Rashomon set.
#'
#' @param model A trained TreeFARMS model object (class `treefarms_model`)
#'
#' @return Integer: number of trees in the Rashomon set
#'
#' @examples
#' \dontrun{
#' model <- treefarms(X, y, regularization = 0.1)
#' n_trees <- count_trees(model)
#' }
#'
#' @export
count_trees <- function(model) {
  if (!inherits(model, "treefarms_model")) {
    stop("model must be a treefarms_model object")
  }
  
  return(model$n_trees)
}

#' Get Human-Readable Rules from Tree
#'
#' @description
#' Convert a tree to human-readable if-then rules.
#'
#' @param tree A TreeClassifier object
#' @param feature_names Optional vector of feature names. If NULL, uses generic names.
#'
#' @return Character string with tree rules
#'
#' @examples
#' \dontrun{
#' trees <- get_rashomon_trees(model)
#' rules <- get_tree_rules(trees[[1]], feature_names = colnames(X))
#' cat(rules)
#' }
#'
#' @export
get_tree_rules <- function(tree, feature_names = NULL) {
  # Get JSON representation
  json_str <- tree_to_json(tree)
  tree_list <- jsonlite::fromJSON(json_str)
  
  # Recursive function to build rules
  build_rules <- function(node, path = character(0), depth = 0) {
    indent <- paste(rep("  ", depth), collapse = "")
    
    if ("prediction" %in% names(node)) {
      # Leaf node
      rule <- paste(path, collapse = " AND ")
      if (rule == "") rule <- "TRUE"
      return(sprintf("%sIF %s THEN predict %s\n", 
                     indent, rule, node$prediction))
    } else {
      # Internal node
      feature_name <- if (!is.null(feature_names)) {
        feature_names[node$feature + 1]  # Convert 0-based to 1-based
      } else {
        paste0("feature_", node$feature)
      }
      
      # True branch
      true_path <- c(path, sprintf("%s == 1", feature_name))
      true_rules <- build_rules(node$`true`, true_path, depth + 1)
      
      # False branch
      false_path <- c(path, sprintf("%s == 0", feature_name))
      false_rules <- build_rules(node$`false`, false_path, depth + 1)
      
      return(paste0(true_rules, false_rules))
    }
  }
  
  rules <- build_rules(tree_list)
  return(rules)
}

