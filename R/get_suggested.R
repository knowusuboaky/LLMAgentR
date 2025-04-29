get_suggested <- function(pkg, fun = NULL) {
  base_pkgs <- c("base", "utils", "stats", "graphics", "grDevices", "methods", "datasets")

  # Always pass base packages
  if (pkg %in% base_pkgs) {
    return(if (is.null(fun)) invisible(TRUE) else get(fun, envir = asNamespace(pkg)))
  }

  # Ensure package is available
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(sprintf(
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
