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
    message = emit_tidymodels_register_msg
  )
}



#' Helper function for registering functionality with suggests packages
#'
#' @inheritParams check_package_version
#' @param callback A function to execute upon package load. Note that no
#'   arguments are passed to this function. Any necessary data must be provided
#'   upon construction.
#' @param message An optional message to print if mmrm functionality is
#'  successfully registered
#' @return A logical (invisibly) indicating whether registration was successful.
#'  If not, a onLoad hook was set for the next time the package is loaded.
#'
#' @keywords internal
register_on_load <- function(pkg, ver = c(NA, NA), callback, message = NULL) {
  if (isNamespaceLoaded(pkg) && check_package_version(pkg, ver)) {
    callback()
    if (is.character(message)) packageStartupMessage(message)
    if (is.function(message)) packageStartupMessage(message())
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
#' @importFrom utils packageVersion
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




#' Format a message to emit when tidymodels is loaded
#'
#' @return A character message to emit. Either a ansi-formatted cli output if
#'   package 'cli' is available or a plain-text message otherwise.
#'
#' @importFrom utils packageName packageVersion
#' @keywords internal
emit_tidymodels_register_msg <- function() {
  pkg <- utils::packageName()
  ver <- utils::packageVersion(pkg)

  if (isTRUE(getOption("tidymodels.quiet")))
    return()

  # if tidymodels is attached, cli packages come as a dependency
  has_cli <- requireNamespace("cli", quietly = TRUE)
  if (has_cli) {
    # unfortunately, cli does not expose many formatting tools for emitting
    # messages (only via conitions via stderr) which can't be suppressed using
    # suppressPackageStartupMessages() so formatting must be done adhoc,
    # similar to how it's done in {tidymodels} R/attach.R
    paste0(
      cli::rule(
        left = cli::style_bold("Model Registration"),
        right = paste(pkg, ver)
      ),
      "\n",
      cli::col_green(cli::symbol$tick), " ",
      cli::col_blue("mmrm"), "::", cli::col_green("mmrm()")
    )
  } else {
    paste0(pkg, "::mmrm() registered for use with tidymodels")
  }
}
