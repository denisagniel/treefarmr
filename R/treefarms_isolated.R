#' Find gosdt Executable
#'
#' @description
#' Helper function to locate the gosdt standalone executable for process isolation.
#' Checks package installation directory first, then system PATH.
#'
#' @return Path to executable, or NULL if not found
#' @keywords internal
find_gosdt_executable <- function() {
  # Check package installation directory
  pkg_dir <- system.file(package = "treefarmr")
  if (nchar(pkg_dir) > 0) {
    # Try Unix/Linux/macOS executable
    exec_path <- file.path(pkg_dir, "bin", "gosdt")
    if (file.exists(exec_path) && file.access(exec_path, 1) == 0) {
      return(exec_path)
    }
    
    # Try Windows executable
    exec_path_win <- file.path(pkg_dir, "bin", "gosdt.exe")
    if (file.exists(exec_path_win) && file.access(exec_path_win, 1) == 0) {
      return(exec_path_win)
    }
  }
  
  # Check system PATH
  exec_name <- if (.Platform$OS.type == "windows") "gosdt.exe" else "gosdt"
  exec_path <- Sys.which(exec_name)
  if (nchar(exec_path) > 0 && file.exists(exec_path)) {
    return(exec_path)
  }
  
  return(NULL)
}

#' TreeFARMS with Process Isolation
#'
#' @description
#' Wrapper for treefarms that uses process isolation to prevent C++ crashes
#' from affecting the R session. This is the recommended approach for
#' production use, especially when dealing with large datasets or complex models.
#'
#' @param X A data.frame or matrix of features. Must contain only binary (0/1) features.
#' @param y A vector of binary class labels (0/1).
#' @param loss_function Character string: "misclassification" or "log_loss" (default: "misclassification").
#' @param regularization Numeric value controlling model complexity (default: 0.1).
#' @param rashomon_bound_multiplier Numeric value controlling Rashomon set size (default: 0.05).
#' @param worker_limit Integer: number of parallel workers (default: 1).
#' @param verbose Logical: print training progress (default: FALSE).
#' @param single_tree Logical: if TRUE, fit exactly one tree (default: TRUE).
#' @param timeout Numeric: timeout in seconds for subprocess (default: 3600, 1 hour).
#' @param store_training_data Logical: whether to store training data in model (default: FALSE).
#' @param compute_probabilities Logical: whether to compute probabilities immediately (default: FALSE).
#' @param ... Additional parameters passed to TreeFARMS configuration.
#'
#' @return Same as \code{\link{treefarms}}: list with model, predictions, probabilities, etc.
#'
#' @details
#' This function provides process isolation by:
#' 1. Writing data and configuration to temporary files
#' 2. Calling the standalone gosdt executable in a separate process
#' 3. Reading results from output file
#' 4. Cleaning up temporary files
#'
#' If the executable is not found, falls back to direct Rcpp call with a warning.
#'
#' @export
treefarms_isolated <- function(X, y, 
                                loss_function = "misclassification",
                                regularization = 0.1,
                                rashomon_bound_multiplier = 0.05,
                                worker_limit = 1,
                                verbose = FALSE,
                                single_tree = TRUE,
                                timeout = 3600,
                                store_training_data = FALSE,
                                compute_probabilities = FALSE,
                                ...) {
  
  # Find the executable
  exec_path <- find_gosdt_executable()
  
  if (is.null(exec_path)) {
    warning("Standalone gosdt executable not found. Falling back to direct Rcpp call.\n",
            "To build the executable, reinstall the package or run:\n",
            "  cd src && make -f Makevars all")
    # Fall back to direct call
    return(treefarms(X = X, y = y, 
                     loss_function = loss_function,
                     regularization = regularization,
                     rashomon_bound_multiplier = rashomon_bound_multiplier,
                     worker_limit = worker_limit,
                     verbose = verbose,
                     single_tree = single_tree,
                     store_training_data = store_training_data,
                     compute_probabilities = compute_probabilities,
                     ...))
  }
  
  if (verbose) {
    cat("Using process isolation with executable:", exec_path, "\n")
  }
  
  # Convert to data.frame if matrix
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }
  
  # Check for binary features
  for (col in names(X)) {
    if (!all(X[[col]] %in% c(0, 1))) {
      stop(paste("Feature", col, "must contain only binary values (0 and 1)"))
    }
  }
  
  # Convert y to numeric if logical
  if (is.logical(y)) {
    y <- as.numeric(y)
  }
  
  # Create temporary files
  temp_dir <- tempdir()
  data_file <- tempfile(pattern = "treefarms_data_", tmpdir = temp_dir, fileext = ".csv")
  config_file <- tempfile(pattern = "treefarms_config_", tmpdir = temp_dir, fileext = ".json")
  output_file <- tempfile(pattern = "treefarms_output_", tmpdir = temp_dir, fileext = ".json")
  stderr_file <- tempfile(pattern = "treefarms_stderr_", tmpdir = temp_dir, fileext = ".txt")
  
  # Cleanup function
  cleanup <- function() {
    tryCatch({
      if (file.exists(data_file)) unlink(data_file)
      if (file.exists(config_file)) unlink(config_file)
      if (file.exists(output_file)) unlink(output_file)
      if (file.exists(stderr_file)) unlink(stderr_file)
    }, error = function(e) {
      # Ignore cleanup errors
    })
  }
  
  # Ensure cleanup on exit
  on.exit(cleanup(), add = TRUE)
  
  tryCatch({
    # Prepare data CSV
    data_df <- X
    data_df$class <- y
    write.csv(data_df, data_file, row.names = FALSE)
    
    if (verbose) {
      cat("Data file:", data_file, "\n")
      cat("Config file:", config_file, "\n")
      cat("Output file:", output_file, "\n")
    }
    
    # Create configuration JSON
    config <- list(
      loss_function = loss_function,
      regularization = regularization,
      verbose = verbose,
      worker_limit = as.integer(worker_limit),
      model = output_file,  # Output to file
      ...
    )
    
    # Add rashomon parameters
    if (single_tree) {
      config$rashomon <- FALSE
    } else {
      config$rashomon <- TRUE
      config$rashomon_bound_multiplier <- rashomon_bound_multiplier
    }
    
    # Write configuration JSON
    config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)
    writeLines(config_json, config_file)
    
    if (verbose) {
      cat("Calling executable:", exec_path, "\n")
      cat("Arguments: data =", data_file, ", config =", config_file, "\n")
    }
    
    # Call executable
    # Format: gosdt data.csv config.json
    result <- system2(exec_path,
                     args = c(data_file, config_file),
                     stdout = TRUE,
                     stderr = stderr_file,
                     wait = TRUE,
                     timeout = timeout)
    
    # Check exit status
    exit_status <- attr(result, "status")
    if (!is.null(exit_status) && exit_status != 0) {
      # Read stderr for error messages
      stderr_content <- if (file.exists(stderr_file)) {
        paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
      } else {
        "No error output captured"
      }
      
      stop("gosdt executable failed with exit status ", exit_status, 
           "\nError output:\n", stderr_content)
    }
    
    # Read output file
    if (!file.exists(output_file)) {
      # Check if there's useful info in stderr
      stderr_content <- if (file.exists(stderr_file)) {
        paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
      } else {
        ""
      }
      stop("Output file was not created. Check stderr for errors.\n",
           if (nchar(stderr_content) > 0) paste("Stderr:", stderr_content) else "")
    }
    
    # Read JSON result
    json_output <- readChar(output_file, file.info(output_file)$size)
    
    if (verbose) {
      cat("Result file size:", file.info(output_file)$size, "bytes\n")
      cat("JSON output length:", nchar(json_output), "characters\n")
    }
    
    # Parse JSON and process result (reuse existing logic from treefarms.R)
    # We need to access the helper functions from treefarms.R
    # Get the extract_tree_from_stdout function if available
    extract_tree_from_stdout <- tryCatch({
      get("extract_tree_from_stdout", envir = asNamespace("treefarmr"))
    }, error = function(e) {
      function(stdout_lines) { NULL }
    })
    
    # Parse the JSON result
    if (is.null(json_output) || json_output == "" || trimws(json_output) == "{}") {
      if (verbose) {
        cat("DEBUG: json_output is null or empty\n")
      }
      result_data <- NULL
    } else {
      tryCatch({
        result_data <- jsonlite::fromJSON(json_output, simplifyVector = FALSE)
        if (verbose) {
          cat("DEBUG: result_data parsed successfully\n")
        }
      }, error = function(e) {
        warning("Failed to parse JSON result. Error: ", e$message)
        result_data <- NULL
      })
    }
    
    # Extract tree JSON from result_data (similar to treefarms.R logic)
    tree_json <- NULL
    if (!is.null(result_data)) {
      # Check if result_data has a "trees" field
      if (!is.null(result_data$trees) && is.list(result_data$trees) && length(result_data$trees) > 0) {
        if (single_tree && length(result_data$trees) == 1) {
          tree_json <- result_data$trees[[1]]
        } else if (!single_tree) {
          tree_json <- result_data$trees
        } else {
          # Multiple trees but single_tree=TRUE, use first
          tree_json <- result_data$trees[[1]]
        }
      } else if (!is.null(result_data$feature) || !is.null(result_data$prediction)) {
        # result_data itself is a tree structure
        tree_json <- result_data
      } else if (!is.null(result_data$storage) && is.list(result_data$storage) && length(result_data$storage) > 0) {
        # result_data is a ModelSet structure - extract the first tree
        for (item in result_data$storage) {
          if (is.list(item) && (!is.null(item$feature) || !is.null(item$prediction))) {
            tree_json <- item
            break
          }
        }
      }
    }
    
    # Count trees
    n_trees <- 0
    if (!is.null(result_data)) {
      if (is.list(result_data) && !is.null(result_data$trees)) {
        if (is.list(result_data$trees)) {
          n_trees <- length(result_data$trees)
        } else {
          n_trees <- 1
        }
      } else if (is.list(result_data) && (!is.null(result_data$feature) || !is.null(result_data$prediction))) {
        n_trees <- 1
      }
    }
    if (n_trees == 0 && !is.null(tree_json)) {
      if (is.list(tree_json) && (!is.null(tree_json$feature) || !is.null(tree_json$prediction))) {
        n_trees <- 1
      }
    }
    
    # Get probabilities if requested
    probabilities <- NULL
    predictions <- NULL
    accuracy <- NULL
    
    tree_to_use <- if (!is.null(tree_json)) tree_json else result_data
    has_tree <- !is.null(tree_to_use) && is.list(tree_to_use) && 
                (!is.null(tree_to_use$feature) || !is.null(tree_to_use$prediction))
    
    if (compute_probabilities && has_tree) {
      # Use existing get_probabilities_from_tree function
      get_probabilities_from_tree <- get("get_probabilities_from_tree", 
                                        envir = asNamespace("treefarmr"))
      probabilities <- get_probabilities_from_tree(tree_to_use, X)
      predictions <- ifelse(probabilities[, 2] >= 0.5, 1, 0)
      accuracy <- mean(predictions == y)
    } else if (has_tree) {
      # Lazy evaluation - probabilities will be computed on demand
    } else {
      # No tree available
      predictions <- rep(0, length(y))
      accuracy <- mean(predictions == y)
    }
    
    # Build result object (same structure as treefarms())
    model_obj <- list(
      tree_json = tree_json,
      result_data = result_data,
      loss_function = loss_function,
      regularization = regularization,
      n_trees = n_trees
    )
    
    result_list <- list(
      model = list(model = model_obj),
      predictions = predictions,
      probabilities = probabilities,
      accuracy = accuracy,
      loss_function = loss_function,
      regularization = regularization,
      n_trees = n_trees
    )
    
    # Set class for S3 methods
    if (loss_function == "log_loss") {
      class(result_list) <- "treefarms_logloss_model"
    } else {
      class(result_list) <- "treefarms_model"
    }
    
    return(result_list)
    
  }, error = function(e) {
    # Read stderr if available
    stderr_content <- if (file.exists(stderr_file)) {
      paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    
    stop("Error in treefarms_isolated: ", e$message,
         if (nchar(stderr_content) > 0) paste("\nStderr output:\n", stderr_content) else "")
  })
}



