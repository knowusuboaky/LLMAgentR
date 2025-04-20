#' Require Suggested Package or Retrieve Function at Runtime
#'
#' This utility ensures a package listed in `Suggests:` is available,
#' and optionally returns a function from it. If no function is provided,
#' it just checks the package presence (like a safe `requireNamespace()`).
#'
#' Base R packages are automatically considered available.
#'
#' @param pkg Character string. Name of the package.
#' @param fun Optional character string. Name of the function to retrieve from the package.
#'
#' @return If \code{fun} is provided, returns the function object from the package namespace.
#'         Otherwise, invisibly returns TRUE if the package is available.
#' @export
get_suggested <- function(pkg, fun = NULL) {
  base_pkgs <- c("base", "utils", "stats", "graphics", "grDevices", "methods", "datasets")

  # Always pass base packages
  if (pkg %in% base_pkgs) {
    return(if (is.null(fun)) invisible(TRUE) else get(fun, envir = asNamespace(pkg)))
  }

  # Ensure package is available
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "The '%s' package is required for this functionality. Please install it or move it to Imports.",
      pkg
    ), call. = FALSE)
  }

  # Return function if requested, or TRUE
  if (!is.null(fun)) {
    return(get(fun, envir = asNamespace(pkg)))
  }

  invisible(TRUE)
}
