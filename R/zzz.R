#' Package Initialization
#'
#' @description
#' Package initialization hooks for optimaltrees package
#'
#' @import Rcpp
#' @importFrom jsonlite fromJSON toJSON
#' @useDynLib optimaltrees, .registration = TRUE
#' @name optimaltrees-package-imports
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

  # Explicit S3 re-registration for plain-list (non-S7) classes (2026-09-01,
  # Milestone E sub-step E4). CONFIRMED EMPIRICALLY, not assumed: NAMESPACE's
  # declarative S3method(print, optimaltrees_twostage_fit) (and, pre-
  # existing, the SAME entries for optimaltrees_logloss_model/cf_rashomon)
  # does NOT take effect after a real R CMD INSTALL + library() load --
  # methods("print") lists none of them, not even as a hidden/starred
  # entry, and getS3method() reports "not found" even though the function
  # itself is present in the namespace. Consistent with S7::methods_register()
  # above rebuilding/overwriting the S3 dispatch table rather than merging
  # into it, though the exact S7-internal mechanism was not traced further.
  # Fixed HERE, AFTER S7::methods_register(), for the classes THIS
  # milestone's own code needs generic dispatch to actually work for.
  # The pre-existing optimaltrees_logloss_model/cf_rashomon cases are a
  # separate, out-of-scope bug -- flagged, not fixed, since those classes'
  # print/summary/predict are apparently only ever called explicitly (as
  # plain helper functions) from within their own S7 method wrappers, not
  # via generic dispatch, so nothing currently depends on fixing them.
  registerS3method("print", "optimaltrees_twostage_fit",
                    print.optimaltrees_twostage_fit)
  registerS3method("summary", "optimaltrees_twostage_fit",
                    summary.optimaltrees_twostage_fit)
  registerS3method("predict", "optimaltrees_twostage_fit",
                    predict.optimaltrees_twostage_fit)

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
