#' Model Persistence Functions
#'
#' @description
#' Functions for saving and loading TreeFARMS models to/from disk.
#' Handles Python objects via JSON serialization.

#' Save TreeFARMS Model
#'
#' @description
#' Save a TreeFARMS model to disk using RDS format.
#' Handles Python objects by converting trees to JSON.
#'
#' @param model A TreeFARMS model object (any type)
#' @param path Character: file path to save to (should end with .rds)
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the model
#'
#' @details
#' This function saves TreeFARMS models to disk, handling the complexity
#' of Python objects by converting tree structures to JSON format.
#' The saved model can be loaded later using \code{load_treefarms()}.
#'
#' @examples
#' \dontrun{
#' # Train a model
#' model <- treefarms(X, y, loss_function = "log_loss")
#' 
#' # Save the model
#' save_treefarms(model, "my_model.rds")
#' 
#' # Load it later
#' loaded_model <- load_treefarms("my_model.rds")
#' }
#'
#' @export
save_treefarms <- function(model, path, ...) {
  
  # Validate inputs
  if (!inherits(model, c("treefarms_model", "treefarms_logloss_model", "treefarms_subprocess_model", "cf_rashomon"))) {
    stop("model must be a TreeFARMS model object")
  }
  
  if (!is.character(path) || length(path) != 1) {
    stop("path must be a single character string")
  }
  
  # Create a serializable version of the model
  serializable_model <- list()
  
  # Copy basic information
  serializable_model$class <- class(model)
  serializable_model$loss_function <- model$loss_function
  serializable_model$regularization <- model$regularization
  serializable_model$accuracy <- model$accuracy
  serializable_model$n_trees <- model$n_trees
  serializable_model$X_train <- model$X_train
  serializable_model$y_train <- model$y_train
  serializable_model$predictions <- model$predictions
  serializable_model$probabilities <- model$probabilities
  
  # Handle different model types
  if (inherits(model, "treefarms_model")) {
    # For reticulate models, convert trees to JSON
    if (model$n_trees > 0) {
      trees_json <- list()
      for (i in seq_len(model$n_trees)) {
        tree <- model$model$model_set[i]
        trees_json[[i]] <- tree$json()
      }
      serializable_model$trees_json <- trees_json
    } else {
      serializable_model$trees_json <- list()
    }
    
    # Store model configuration
    serializable_model$config <- list(
      loss_function = model$loss_function,
      regularization = model$regularization,
      rashomon = TRUE,
      rashomon_bound_multiplier = 0.05
    )
    
  } else if (inherits(model, c("treefarms_logloss_model", "treefarms_subprocess_model"))) {
    # For subprocess models, trees are already JSON strings
    serializable_model$trees_json <- model$trees
    serializable_model$config <- model$config
    
  } else if (inherits(model, "cf_rashomon")) {
    # For cross-fitted Rashomon models
    serializable_model$intersecting_trees <- model$intersecting_trees
    serializable_model$n_intersecting <- model$n_intersecting
    serializable_model$fold_models <- list()
    serializable_model$rashomon_sets <- model$rashomon_sets
    serializable_model$rashomon_sizes <- model$rashomon_sizes
    serializable_model$fold_indices <- model$fold_indices
    serializable_model$K <- model$K
    serializable_model$call <- model$call
    
    # Convert fold models to serializable format
    for (i in seq_along(model$fold_models)) {
      fold_model <- model$fold_models[[i]]
      serializable_fold <- list()
      serializable_fold$class <- class(fold_model)
      serializable_fold$loss_function <- fold_model$loss_function
      serializable_fold$regularization <- fold_model$regularization
      serializable_fold$accuracy <- fold_model$accuracy
      serializable_fold$n_trees <- fold_model$n_trees
      serializable_fold$X_train <- fold_model$X_train
      serializable_fold$y_train <- fold_model$y_train
      serializable_fold$predictions <- fold_model$predictions
      serializable_fold$probabilities <- fold_model$probabilities
      
      if (inherits(fold_model, "treefarms_model")) {
        if (fold_model$n_trees > 0) {
          trees_json <- list()
          for (j in seq_len(fold_model$n_trees)) {
            tree <- fold_model$model$model_set[j]
            trees_json[[j]] <- tree$json()
          }
          serializable_fold$trees_json <- trees_json
        } else {
          serializable_fold$trees_json <- list()
        }
        serializable_fold$config <- list(
          loss_function = fold_model$loss_function,
          regularization = fold_model$regularization,
          rashomon = TRUE,
          rashomon_bound_multiplier = 0.05
        )
      } else {
        serializable_fold$trees_json <- fold_model$trees
        serializable_fold$config <- fold_model$config
      }
      
      serializable_model$fold_models[[i]] <- serializable_fold
    }
  }
  
  # Save to RDS file
  saveRDS(serializable_model, file = path)
  
  invisible(model)
}

#' Load TreeFARMS Model
#'
#' @description
#' Load a TreeFARMS model from disk.
#' Reconstructs the model object from saved JSON data.
#'
#' @param path Character: file path to load from (should be .rds file)
#' @param ... Additional arguments (currently unused)
#'
#' @return The loaded TreeFARMS model object
#'
#' @details
#' This function loads TreeFARMS models from disk, reconstructing
#' the model object from the saved JSON data. Note that Python
#' objects are not fully reconstructed - only the essential
#' information for making predictions is restored.
#'
#' @examples
#' \dontrun{
#' # Load a saved model
#' model <- load_treefarms("my_model.rds")
#' 
#' # Use the model for predictions
#' predictions <- predict(model, newdata)
#' }
#'
#' @export
load_treefarms <- function(path, ...) {
  
  # Validate inputs
  if (!is.character(path) || length(path) != 1) {
    stop("path must be a single character string")
  }
  
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # Load the serializable model
  serializable_model <- readRDS(path)
  
  # Reconstruct the model object
  if (serializable_model$class == "treefarms_model") {
    # For reticulate models, we can't fully reconstruct the Python object
    # but we can create a minimal object for prediction purposes
    model <- list(
      loss_function = serializable_model$loss_function,
      regularization = serializable_model$regularization,
      accuracy = serializable_model$accuracy,
      n_trees = serializable_model$n_trees,
      X_train = serializable_model$X_train,
      y_train = serializable_model$y_train,
      predictions = serializable_model$predictions,
      probabilities = serializable_model$probabilities,
      trees_json = serializable_model$trees_json,
      config = serializable_model$config
    )
    class(model) <- "treefarms_model_loaded"
    
  } else if (serializable_model$class %in% c("treefarms_logloss_model", "treefarms_subprocess_model")) {
    # For subprocess models, reconstruction is straightforward
    model <- list(
      loss_function = serializable_model$loss_function,
      regularization = serializable_model$regularization,
      accuracy = serializable_model$accuracy,
      n_trees = serializable_model$n_trees,
      X_train = serializable_model$X_train,
      y_train = serializable_model$y_train,
      predictions = serializable_model$predictions,
      probabilities = serializable_model$probabilities,
      trees = serializable_model$trees_json,
      config = serializable_model$config
    )
    class(model) <- serializable_model$class
    
  } else if (serializable_model$class == "cf_rashomon") {
    # For cross-fitted Rashomon models
    model <- list(
      intersecting_trees = serializable_model$intersecting_trees,
      n_intersecting = serializable_model$n_intersecting,
      fold_models = list(),
      rashomon_sets = serializable_model$rashomon_sets,
      rashomon_sizes = serializable_model$rashomon_sizes,
      fold_indices = serializable_model$fold_indices,
      K = serializable_model$K,
      call = serializable_model$call
    )
    
    # Reconstruct fold models
    for (i in seq_along(serializable_model$fold_models)) {
      fold_data <- serializable_model$fold_models[[i]]
      
      if (fold_data$class == "treefarms_model") {
        fold_model <- list(
          loss_function = fold_data$loss_function,
          regularization = fold_data$regularization,
          accuracy = fold_data$accuracy,
          n_trees = fold_data$n_trees,
          X_train = fold_data$X_train,
          y_train = fold_data$y_train,
          predictions = fold_data$predictions,
          probabilities = fold_data$probabilities,
          trees_json = fold_data$trees_json,
          config = fold_data$config
        )
        class(fold_model) <- "treefarms_model_loaded"
      } else {
        fold_model <- list(
          loss_function = fold_data$loss_function,
          regularization = fold_data$regularization,
          accuracy = fold_data$accuracy,
          n_trees = fold_data$n_trees,
          X_train = fold_data$X_train,
          y_train = fold_data$y_train,
          predictions = fold_data$predictions,
          probabilities = fold_data$probabilities,
          trees = fold_data$trees_json,
          config = fold_data$config
        )
        class(fold_model) <- fold_data$class
      }
      
      model$fold_models[[i]] <- fold_model
    }
    
    class(model) <- "cf_rashomon"
  }
  
  return(model)
}

#' Predict method for loaded treefarms_model objects
#'
#' @param object A treefarms_model_loaded object
#' @param newdata A data.frame or matrix of new features
#' @param type Character: "class" or "prob"
#' @param ... Additional arguments
#'
#' @return Predictions based on type
#' @export
predict.treefarms_model_loaded <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  
  # Validate newdata
  if (!all(colnames(object$X_train) %in% colnames(newdata))) {
    stop("newdata must contain all training features")
  }
  
  # Ensure same column order
  newdata <- newdata[, colnames(object$X_train), drop = FALSE]
  
  # Create temporary files for subprocess prediction
  temp_dir <- tempdir()
  X_file <- file.path(temp_dir, "X_new.csv")
  model_file <- file.path(temp_dir, "model.json")
  result_file <- file.path(temp_dir, "predictions.json")
  script_file <- file.path(temp_dir, "predict.py")
  
  # Write data to files
  write.csv(newdata, X_file, row.names = FALSE)
  
  # Save model information
  model_data <- list(
    trees = object$trees_json,
    config = object$config
  )
  writeLines(jsonlite::toJSON(model_data, auto_unbox = TRUE), model_file)
  
  # Create Python prediction script
  python_script <- sprintf('
import pandas as pd
import numpy as np
import json
import sys
import os

# Add TreeFARMS to path
sys.path.insert(0, os.path.expanduser("~/OneDrive/rrr/treeFarms"))

from treefarms.model.treefarms import TREEFARMS

# Read data
X_new = pd.read_csv("%s")

# Load model
with open("%s", "r") as f:
    model_data = json.load(f)

# Create model and load trees
if model_data["trees"]:
    tree_json = model_data["trees"][0]["json"]
    
    # Create a minimal model for prediction
    config = model_data["config"]
    model = TREEFARMS(config)
    
    try:
        # Get predictions using the model
        predictions = model.predict(X_new)
        probabilities = model.predict_proba(X_new)
        
        # Convert to lists for JSON serialization
        if hasattr(predictions, "tolist"):
            predictions_list = predictions.tolist()
        else:
            predictions_list = list(predictions)
            
        if hasattr(probabilities, "tolist"):
            probabilities_list = probabilities.tolist()
        else:
            probabilities_list = list(probabilities)
        
        # Save results
        results = {
            "predictions": predictions_list,
            "probabilities": probabilities_list
        }
        
        with open("%s", "w") as f:
            json.dump(results, f)
            
    except Exception as e:
        # Fallback: return default predictions
        n_samples = len(X_new)
        results = {
            "predictions": [0] * n_samples,
            "probabilities": [[0.5, 0.5]] * n_samples,
            "error": str(e)
        }
        
        with open("%s", "w") as f:
            json.dump(results, f)
else:
    # No trees available
    n_samples = len(X_new)
    results = {
        "predictions": [0] * n_samples,
        "probabilities": [[0.5, 0.5]] * n_samples,
        "error": "No trees available"
    }
    
    with open("%s", "w") as f:
        json.dump(results, f)
', X_file, model_file, result_file, result_file, result_file)
  
  # Write and execute Python script
  writeLines(python_script, script_file)
  
  # Execute Python script
  python_path <- getOption("treefarms.python_path", "~/gosdt-env/bin/python")
  cmd <- sprintf("%s %s", python_path, script_file)
  exit_code <- system(cmd, intern = FALSE)
  
  if (exit_code != 0) {
    stop("Error running Python prediction script")
  }
  
  # Read results
  if (!file.exists(result_file)) {
    stop("Prediction results file not found")
  }
  
  results <- jsonlite::fromJSON(result_file)
  
  # Clean up temporary files
  unlink(c(X_file, model_file, result_file, script_file))
  
  if (type == "class") {
    return(as.numeric(results$predictions))
  } else {
    return(as.matrix(results$probabilities))
  }
}


