# Predicate whether currently running R version is under development.
is_r_devel <- function() {
  grepl("devel", R.version$status)
}

# Predicate whether currently running on a Linux operating system.
is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}

# Predicate whether currently running on R compiled with clang.
is_using_clang <- function() {
  grepl("clang", R_compiled_by()["C"])
}

# A `data.frame` giving default clang versions for each OS version of the
# Fedora Linux distribution.
# Source: https://packages.fedoraproject.org/pkgs/clang/clang/
# See Updates section for older Fedora versions.
fedora_clang_defaults <- data.frame(
  os = as.integer(c(36, 37, 38, 39, 40)),
  clang = as.integer(c(14, 15, 16, 17, 17))
)

# A `data.frame` giving default clang versions for each OS version of the
# Debian Linux distribution.
# Source: https://packages.debian.org/search?keywords=clang
debian_clang_defaults <- data.frame(
  os = c("bullseye", "bookworm", "trixie"),
  clang = as.integer(c(11, 14, 16))
)

# Parse the major clang version as integer (e.g. 17) from
# the full clang string (e.g. "Debian clang version 17.0.6 (3)")
parse_clang_major <- function(clang_string) {
  assert_string(clang_string, pattern = "clang")
  clang_version <- gsub(pattern = "[^0-9.]", replacement = "", x = clang_string)
  as.integer(gsub(pattern = "([0-9]+).+", replacement = "\\1", x = clang_version))
}

# Predicate whether a non-standard clang version is used, specifically
# a higher than default clang version. Assumes that clang is used, otherwise fails.
# If not Fedora or Debian of the known versions are used, always returns `FALSE`.
is_non_standard_clang <- function() {
  os <- utils::osVersion
  clang_major_version <- parse_clang_major(R_compiled_by()["C"])
  assert_int(clang_major_version)
  if (grepl("Fedora", os)) {
    os_version <- as.integer(gsub(pattern = "[^0-9]", replacement = "", x = os))
    assert_int(os_version)
    which_os <- match(os_version, fedora_clang_defaults$os)
    if (is.na(which_os)) return(FALSE)
    clang_major_version > fedora_clang_defaults$clang[which_os]
  } else if (grepl("Debian", os)) {
    os_codename <- gsub(pattern = "debian gnu/linux ([a-z]+)/*[a-z]*", replacement = "\\1", x = os)
    assert_string(os_codename)
    which_os <- match(os_codename, debian_clang_defaults$os)
    if (is.na(which_os)) return(FALSE)
    clang_major_version > debian_clang_defaults$clang[which_os]
  } else {
    FALSE
  }
}

# Custom skipping function, specializing testthat::skip.
# This will skip the test if an R-devel version is running on Linux Fedora or
# Debian with a non-standard clang compiler.
skip_if_r_devel_linux_clang <- function() {
  do_skip <- is_r_devel() &&
    is_linux() &&
    is_using_clang() &&
    is_non_standard_clang()

  if (do_skip) {
    skip("Not run on R-devel Linux systems with non-standard clang")
  } else {
    invisible()
  }
}
