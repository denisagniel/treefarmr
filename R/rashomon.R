#' Count leaves in a single tree
#'
#' @description
#' Returns the number of leaf nodes in a tree (list or JSON string).
#' Used for \code{max_leaves} sieve in \code{get_rashomon_trees}.
#'
#' @param tree A single tree as list or JSON string
#' @return Integer number of leaves
#' @export
count_leaves_tree <- function(tree) {
  if (is.character(tree)) {
    tree <- jsonlite::fromJSON(tree, simplifyVector = FALSE)
  }
  if (!is.list(tree)) return(0L)
  if (!is.null(tree$prediction)) return(1L)
  n <- 0L
  if (!is.null(tree$true))  n <- n + count_leaves_tree(tree$true)
  if (!is.null(tree$false)) n <- n + count_leaves_tree(tree$false)
  as.integer(n)
}

#' Extract Trees from Rashomon Set
#'
#' @description
#' Extract all trees from a trained TreeFARMS model's Rashomon set.
#' Optionally restrict to trees with at most \code{max_leaves} leaves (sieve for
#' theory-consistent complexity; see \file{docs/Implementation-requirements-Rashomon-DML.md}).
#'
#' @param model A trained TreeFARMS model object (class `treefarms_model`)
#' @param max_leaves Optional integer. If set, only trees with number of leaves \eqn{\le}
#'   \code{max_leaves} are returned (R-side sieve).
#'
#' @return A list of tree objects from the model's Rashomon set (possibly filtered by \code{max_leaves})
#'
#' @examples
#' \dontrun{
#' model <- treefarms(X, y, regularization = 0.1)
#' trees <- get_rashomon_trees(model)
#' trees_small <- get_rashomon_trees(model, max_leaves = 5)
#' }
#'
#' @export
get_rashomon_trees <- function(model, max_leaves = NULL) {
  if (!inherits(model, c("treefarms_model", "treefarms_logloss_model"))) {
    stop("model must be a treefarms_model or treefarms_logloss_model object")
  }
  
  if (inherits(model, "treefarms_logloss_model")) {
    # For log_loss models, trees are stored in model$trees
    if (model$n_trees == 0) {
      warning("No trees in Rashomon set")
      return(list())
    }
    trees <- model$trees
  } else {
    # For regular models, extract from JSON structure
    # Note: Current implementation stores trees as JSON, not C++ objects
    # If C++ model_set exists in the future, we can add backward compatibility here
    {
      tree_json <- model$model$tree_json
      
      if (is.null(tree_json)) {
        # Try result_data as fallback
        tree_json <- model$model$result_data
      }
      
      if (is.null(tree_json)) {
        warning("No tree structure found in model")
        trees <- list()
      } else if (is.list(tree_json)) {
        # Check if it's a single tree or a rashomon set
        if (!is.null(tree_json$feature) || !is.null(tree_json$prediction)) {
          trees <- list(tree_json)
        } else if (length(tree_json) > 0) {
          # Check if it's a list of trees
          if (length(tree_json) == 1 && 
              is.list(tree_json[[1]]) &&
              (!is.null(tree_json[[1]]$feature) || !is.null(tree_json[[1]]$prediction))) {
            trees <- list(tree_json[[1]])
          } else if (length(tree_json) > 1) {
            # Multiple trees - check if they're valid tree structures
            valid_trees <- list()
            for (i in seq_along(tree_json)) {
              if (is.list(tree_json[[i]]) && 
                  (!is.null(tree_json[[i]]$feature) || !is.null(tree_json[[i]]$prediction))) {
                valid_trees[[length(valid_trees) + 1]] <- tree_json[[i]]
              }
            }
            if (length(valid_trees) > 0) {
              trees <- valid_trees
            } else {
              trees <- list()
            }
          } else {
            trees <- list()
          }
        } else {
          trees <- list()
        }
      } else {
        warning("Could not extract trees from model structure")
        trees <- list()
      }
    }
  }
  if (!is.null(max_leaves) && length(trees) > 0) {
    nleaves <- vapply(trees, count_leaves_tree, integer(1))
    trees <- trees[nleaves <= max_leaves]
  }
  trees
}

#' Tree structure to canonical form (splits only)
#'
#' @description
#' Returns a canonical representation of a tree that includes only the split
#' structure (feature, relation, reference, and recursive true/false branches).
#' Leaf nodes are represented as a placeholder so that two trees with the same
#' splits but different leaf values compare equal. Used for intersection by
#' structure in DML.
#'
#' @param tree A tree as list (with \code{feature}/\code{prediction}) or JSON string
#' @return A list with structure only (no prediction/probabilities/loss at leaves)
#' @export
tree_structure_to_canonical <- function(tree) {
  if (is.character(tree)) {
    tree <- jsonlite::fromJSON(tree, simplifyVector = FALSE)
  }
  if (!is.list(tree)) {
    stop("tree must be a list or JSON string")
  }
  if (!is.null(tree$prediction)) {
    return(list(leaf = TRUE))
  }
  if (!is.null(tree$feature)) {
    return(list(
      feature = tree$feature,
      relation = if (is.null(tree$relation)) "==" else tree$relation,
      reference = if (is.null(tree$reference)) "" else tree$reference,
      true = tree_structure_to_canonical(tree$true),
      false = tree_structure_to_canonical(tree$false)
    ))
  }
  stop("Tree node has neither feature nor prediction")
}

#' Get maximum feature index referenced in tree structure
#' @noRd
get_max_feature_index <- function(node) {
  if (is.null(node) || !is.list(node)) {
    return(NULL)
  }

  max_idx <- NULL

  # Check current node
  if (!is.null(node$feature)) {
    max_idx <- node$feature
  }

  # Recursively check children
  if (!is.null(node$left)) {
    left_max <- get_max_feature_index(node$left)
    if (!is.null(left_max)) {
      max_idx <- if (is.null(max_idx)) left_max else max(max_idx, left_max)
    }
  }

  if (!is.null(node$right)) {
    right_max <- get_max_feature_index(node$right)
    if (!is.null(right_max)) {
      max_idx <- if (is.null(max_idx)) right_max else max(max_idx, right_max)
    }
  }

  return(max_idx)
}

#' Refit a tree structure on data (leaf values only)
#'
#' @description
#' Given a tree structure (splits), route each row of \code{X} to a leaf and set
#' leaf predictions and probabilities from the empirical class distribution in
#' that leaf. Used for DML: the same structure is refit on \eqn{\mathcal{D}^{(-k)}}
#' per fold to obtain \eqn{\tilde{\eta}^{(-k)}}.
#'
#' @param structure A tree (list or JSON string) with the same split structure;
#'   leaf values are ignored and replaced.
#' @param X Data.frame or matrix of features (binary 0/1)
#' @param y Vector of binary class labels (0/1)
#' @return A tree (list) with the same structure and leaf \code{prediction} and
#'   \code{probabilities} fitted on \code{(X, y)}.
#' @export
refit_structure_on_data <- function(structure, X, y) {
  # Validate structure input
  if (is.null(structure)) {
    stop("structure cannot be NULL", call. = FALSE)
  }

  if (is.character(structure)) {
    structure <- jsonlite::fromJSON(structure, simplifyVector = FALSE)
  }

  if (!is.list(structure)) {
    stop("structure must be a list or tree object, got: ",
         class(structure)[1], call. = FALSE)
  }

  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }

  if (!is.data.frame(X)) {
    stop("X must be a data.frame or matrix", call. = FALSE)
  }

  if (!is.numeric(y) && !is.logical(y)) {
    stop("y must be numeric or logical", call. = FALSE)
  }

  n <- length(y)
  if (nrow(X) != n) {
    stop("nrow(X) must equal length(y)", call. = FALSE)
  }

  # Check that structure's feature indices don't exceed ncol(X)
  max_feature <- get_max_feature_index(structure)
  if (!is.null(max_feature) && max_feature > ncol(X)) {
    stop("structure references feature index ", max_feature,
         " but X only has ", ncol(X), " columns", call. = FALSE)
  }
  row_indices <- seq_len(n)

  is_regression <- !all(y %in% c(0L, 1L)) && is.numeric(y)
  fill_leaf_values <- function(node, indices) {
    if (is.null(node) || !is.list(node)) {
      if (is_regression) return(list(prediction = 0))
      return(list(prediction = 0L, probabilities = c(1, 0)))
    }
    if (!is.null(node$prediction)) {
      if (is_regression) {
        leaf_mean <- if (length(indices) > 0) mean(y[indices], na.rm = TRUE) else 0
        return(list(prediction = leaf_mean))
      }
      p1 <- mean(y[indices], na.rm = TRUE)
      if (length(indices) == 0) p1 <- 0.5
      p1 <- max(0, min(1, p1))
      pred <- as.integer(p1 >= 0.5)
      return(list(prediction = pred, probabilities = c(1 - p1, p1)))
    }
    if (!is.null(node$feature)) {
      feat_idx <- node$feature + 1L
      if (feat_idx < 1L || feat_idx > ncol(X)) {
        stop("Tree feature index out of range for X")
      }
      col <- X[[feat_idx]]
      if (is.null(col)) {
        col <- X[, feat_idx]
      }
      true_idx <- indices[col == 1]
      false_idx <- indices[col == 0]
      true_child <- fill_leaf_values(node$true, true_idx)
      false_child <- fill_leaf_values(node$false, false_idx)
      return(list(
        feature = node$feature,
        relation = if (is.null(node$relation)) "==" else node$relation,
        reference = if (is.null(node$reference)) "" else node$reference,
        true = true_child,
        false = false_child
      ))
    }
    stop("Node has neither feature nor prediction")
  }

  out <- fill_leaf_values(structure, row_indices)
  out
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
  } else if (is.list(tree)) {
    # Check if it's a JSON tree structure (has feature or prediction)
    if (!is.null(tree$feature) || !is.null(tree$prediction)) {
      # This is a JSON tree structure - convert to JSON string
      return(jsonlite::toJSON(tree, auto_unbox = TRUE))
    } else if ("json" %in% names(tree)) {
      # This is a log_loss model tree with json field or TreeClassifier object
      if (is.character(tree$json)) {
        return(as.character(tree$json))
      } else if (is.function(tree$json)) {
        # This is a TreeClassifier object with json() method
        json_str <- as.character(tree$json())
        return(json_str)
      }
    }
  }
  
  # If we get here, couldn't convert
  stop("Tree object does not have a recognizable structure. Expected: JSON tree (list with feature/prediction), JSON string, or TreeClassifier object with json() method")
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

#' Find Intersection of Trees Across Rashomon Sets (by structure)
#'
#' @description
#' Find tree *structures* that appear in ALL Rashomon sets from cross-fitting.
#' Comparison is by structure only (splits), so the same splits with different
#' leaf values (e.g. from different folds) count as one. This is required for
#' DML so that each fold can refit the chosen structure on its training data.
#'
#' @param rashomon_list A list of Rashomon sets, where each element is a list of trees
#' @param verbose Logical. Print progress information. Default: TRUE
#'
#' @return A list containing:
#'   \item{intersecting_trees}{List of tree objects (from first set) with structures in all sets}
#'   \item{n_intersecting}{Number of intersecting structures}
#'   \item{tree_jsons}{JSON representations of those trees}
#'   \item{intersecting_structures}{Same as intersecting_trees; full trees usable for \code{refit_structure_on_data}}
#'
#' @details
#' Trees are compared by canonical structure only (feature, relation, reference,
#' and recursive branches; leaf values ignored). So the same structure refit on
#' different folds matches. One representative full tree per structure (from the
#' first set) is returned for backward compatibility and for refitting.
#'
#' @examples
#' \dontrun{
#' rashomon_sets <- lapply(models, get_rashomon_trees)
#' result <- find_tree_intersection(rashomon_sets)
#' cat("Found", result$n_intersecting, "stable structure(s)\n")
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
      tree_jsons = tree_jsons,
      intersecting_structures = trees
    ))
  }
  
  if (verbose) {
    cat(sprintf("Finding intersection across %d Rashomon sets (by structure)...\n", K))
    set_sizes <- sapply(rashomon_list, length)
    cat(sprintf("Rashomon set sizes: %s\n", paste(set_sizes, collapse = ", ")))
  }
  
  # OPTIMIZATION: Use hash-based comparison instead of JSON string comparison
  # For large Rashomon sets (1000+ trees), JSON serialization + string matching is O(n*m) and very slow
  # Hashing is O(n) and comparison is O(1) per lookup

  # Extract structure hashes for each fold (digest of canonical structure)
  structure_hash_sets <- lapply(rashomon_list, function(trees) {
    if (length(trees) == 0) return(character(0))
    vapply(trees, function(t) {
      # Get canonical structure (no prediction values, just splits)
      if (is.character(t)) {
        canonical <- tree_structure_to_canonical(jsonlite::fromJSON(t, simplifyVector = FALSE))
      } else {
        canonical <- tree_structure_to_canonical(t)
      }
      # Fast hash instead of full JSON string
      digest::digest(canonical, algo = "xxhash64")
    }, character(1))
  })

  # Intersect by hash (much faster than string matching)
  intersection_hashes <- structure_hash_sets[[1]]
  for (k in 2:K) {
    intersection_hashes <- intersection_hashes[intersection_hashes %in% structure_hash_sets[[k]]]
    if (verbose) {
      cat(sprintf("After intersecting with set %d: %d structure(s) remain\n", k, length(intersection_hashes)))
    }
    if (length(intersection_hashes) == 0) {
      if (verbose) cat("No structures appear in all sets\n")
      return(list(
        intersecting_trees = list(),
        n_intersecting = 0L,
        tree_jsons = character(0),
        intersecting_structures = list()
      ))
    }
  }

  # One representative full tree per structure from set 1 (first occurrence of each hash)
  first_hashes <- structure_hash_sets[[1]]
  idx <- match(unique(intersection_hashes), first_hashes)
  idx <- idx[!is.na(idx)]
  intersecting_trees <- rashomon_list[[1]][idx]

  # Only serialize to JSON at the end for the final intersecting trees
  tree_jsons <- sapply(intersecting_trees, tree_to_json)
  
  if (verbose) {
    cat(sprintf("Found %d structure(s) in all %d Rashomon sets\n", length(intersecting_trees), K))
  }
  
  list(
    intersecting_trees = intersecting_trees,
    n_intersecting = length(intersecting_trees),
    tree_jsons = tree_jsons,
    intersecting_structures = intersecting_trees
  )
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

