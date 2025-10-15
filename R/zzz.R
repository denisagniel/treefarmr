#' Package Initialization
#'
#' @description
#' Package initialization hooks for treefarms package

#' Package onLoad hook
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set package options
  options(
    treefarms.verbose = FALSE,
    treefarms.default_regularization = 0.1,
    treefarms.default_rashomon_bound_multiplier = 0.05
  )
  
  # Initialize RcppParallel for proper TBB initialization
  if (requireNamespace("RcppParallel", quietly = TRUE)) {
    RcppParallel::setThreadOptions(numThreads = "auto")
  }
  
  # Check for required packages
  required_packages <- c("Rcpp", "jsonlite", "RcppParallel")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    packageStartupMessage(
      "Warning: The following required packages are not installed: ",
      paste(missing_packages, collapse = ", "),
      "\nPlease install them with: install.packages(c('",
      paste(missing_packages, collapse = "', '"),
      "'))"
    )
  }
  
  invisible()
}

#' Package onAttach hook
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "TreeFARMS R Package v", 
    utils::packageVersion(pkgname),
    "\n",
    "Tree-based Fast and Accurate Rule Set Models with Log-Loss and Probabilities\n",
    "Using RcppParallel for thread-safe parallel execution\n",
    "For help, see: help(package = '", pkgname, "')"
  )
  
  invisible()
}

#' Package onUnload hook
#'
#' @param libpath Library path
#'
#' @keywords internal
.onUnload <- function(libpath) {
  # CRITICAL: Clean up C++ static state BEFORE RcppParallel shuts down TBB
  tryCatch({
    .Call("_treefarms_cleanup_static_state")
  }, error = function(e) {
    # Ignore if already cleaned or if function doesn't exist yet
  })
  
  # Clean up any temporary files
  temp_files <- list.files(tempdir(), pattern = "^temp_.*\\.(csv|json)$", full.names = TRUE)
  if (length(temp_files) > 0) {
    tryCatch({
      unlink(temp_files)
    }, error = function(e) {
      # Ignore cleanup errors
    })
  }
  
  invisible()
}
