#' Dynamic registration for package interoperability
#'
#' @seealso See `vignette("xtending", package = "emmeans")` for background.
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) { # nolint
  register_on_load(
    "emmeans", c("1.6", NA),
    callback = function() emmeans::.emm_register("mmrm", pkgname),
    message = "mmrm() registered as emmeans extension"
  )

  register_on_load(
    "parsnip", c(NA, NA),
    callback = parsnip_add_mmrm,
    message = "mmrm() registered as tidymodel linear_reg model"
  )
}



#' Helper function for registering functionality with suggests packages
#'
#' @inheritParams check_package_version
#' @param message An optional message to print if mmrm functionality is
#'  successfully registered
#' @return A logical (invisibly) indicating whether registration was successful.
#'  If not, a onLoad hook was set for the next time the package is loaded.
#'
#' @keywords ineternal
register_on_load <- function(pkg, ver = c(NA, NA), callback, message = NULL) {
  if (isNamespaceLoaded(pkg) && check_package_version(pkg, ver)) {
    callback()
    if (!is.null(message)) packageStartupMessage(message)
    return(invisible(TRUE))
  }

  setHook(
    packageEvent(pkg, event = "onLoad"),
    action = "append",
    function(...) {
      register_on_load(
        pkg = pkg, 
        ver = ver, 
        callback = callback, 
        message = message
      )
    }
  )

  invisible(FALSE)
}



#' Check suggested dependency against version requirements
#'
#' @param pkg A package name
#' @param ver A vector of length two, specifying minimum and maximum (inclusive)
#'   versions for interoperability.
#' @return A logical (invisibly) indicating whether the loaded package meets
#'   the version requirements. A warning is emitted otherwise.
#'
#' @keywords internal
check_package_version <- function(pkg, ver = c(NA, NA)) {
  pkg_ver <- utils::packageVersion(pkg)
  ver <- lapply(ver, numeric_version, strict = FALSE)

  warn_version <- function(pkg, pkg_ver, ver) {
    ver_na <- is.na(vapply(ver, is.na, logical(1L)))
    warning(sprintf(
      "Cannot register mmrm for use with %s (v%s). Version %s required.",
      pkg, pkg_ver,
      if (!any(ver_na)) sprintf("%s to %s", ver[[1]], ver[[2]])
      else if (!is.na(ver[[1]])) paste0(">= ", ver[[1]])
      else if (!is.na(ver[[2]])) paste0("<= ", ver[[1]])
    ))
  }

  if (identical(pkg_ver < ver[[1]], TRUE) || identical(pkg_ver > ver[[2]], TRUE)) {
    warn_version(pkg, pkg_ver, ver)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}
