#' Package Initialization
#'
#' @description
#' Package initialization hooks for treefarms package
#'
#' @import Rcpp
#' @import jsonlite
#' @useDynLib treefarmr, .registration = TRUE
NULL

#' Package onLoad hook
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # CHECKPOINT: Entry to .onLoad()
  tryCatch({
    log_file <- file("/tmp/treefarmr_load.log", "a")
    cat("[CHECKPOINT] .onLoad() ENTRY\n", file = log_file)
    close(log_file)
  }, error = function(e) {})
  
  # Set package options (minimal, safe operations only)
  options(
    treefarms.verbose = FALSE,
    treefarms.default_regularization = 0.1,
    treefarms.default_rashomon_bound_multiplier = 0.05
  )
  
  # CHECKPOINT: Before exit from .onLoad()
  tryCatch({
    log_file <- file("/tmp/treefarmr_load.log", "a")
    cat("[CHECKPOINT] .onLoad() EXIT\n", file = log_file)
    close(log_file)
  }, error = function(e) {})
  
  invisible()
}

#' Package onAttach hook
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # CHECKPOINT: Entry to .onAttach()
  tryCatch({
    log_file <- file("/tmp/treefarmr_load.log", "a")
    cat("[CHECKPOINT] .onAttach() ENTRY\n", file = log_file)
    close(log_file)
  }, error = function(e) {})
  
  packageStartupMessage(
    "TreeFARMS R Package v", 
    utils::packageVersion(pkgname),
    "\n",
    "Tree-based Fast and Accurate Rule Set Models with Log-Loss and Probabilities\n",
    "For help, see: help(package = '", pkgname, "')"
  )
  
  # CHECKPOINT: Exit from .onAttach()
  tryCatch({
    log_file <- file("/tmp/treefarmr_load.log", "a")
    cat("[CHECKPOINT] .onAttach() EXIT\n", file = log_file)
    close(log_file)
  }, error = function(e) {})
  
  invisible()
}

#' Package onUnload hook
#'
#' @param libpath Library path
#'
#' @keywords internal
.onUnload <- function(libpath) {
  # Note: State is now instance-based, not static, so no cleanup needed
  # Each Optimizer instance manages its own State, which is cleaned up automatically
  
  # Clean up any temporary files (from both direct calls and subprocess calls)
  temp_files <- list.files(tempdir(), pattern = "^(temp_|treefarms_).*\\.(csv|json|txt)$", full.names = TRUE)
  if (length(temp_files) > 0) {
    tryCatch({
      unlink(temp_files)
    }, error = function(e) {
      # Ignore cleanup errors
    })
  }
  
  invisible()
}
