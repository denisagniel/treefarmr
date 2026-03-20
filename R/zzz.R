#' Package Initialization
#'
#' @description
#' Package initialization hooks for optimaltrees package
#'
#' @import Rcpp
#' @import jsonlite
#' @useDynLib optimaltrees, .registration = TRUE
NULL

#' Package onLoad hook
#'
#' @param libname Library name
#' @param pkgname Package name
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Register S7 methods with S3 dispatch system
  # This enables S7 methods to work with S3 generics like predict()
  S7::methods_register()

  # Set package options (minimal, safe operations only)
  options(
    treefarms.verbose = FALSE,
    treefarms.default_regularization = 0.1,
    treefarms.default_rashomon_bound_multiplier = 0.05
  )

  # Cache package availability (check once at load, use many times)
  # Eliminates repeated requireNamespace() calls across 5 files (18 total occurrences)
  ns <- asNamespace(pkgname)
  assign(".has_furrr", requireNamespace("furrr", quietly = TRUE), envir = ns)
  assign(".has_future", requireNamespace("future", quietly = TRUE), envir = ns)
  assign(".has_cli", requireNamespace("cli", quietly = TRUE), envir = ns)

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
    "optimaltrees v",
    utils::packageVersion(pkgname),
    "\n",
    "Optimal Decision Trees with Flexible Loss Functions\n",
    "For help, see: help(package = '", pkgname, "') or vignette('treefarms-introduction', package = '", pkgname, "')"
  )

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
