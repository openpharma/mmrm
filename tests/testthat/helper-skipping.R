is_r_devel <- function() {
  grepl("devel", R.version$status)
}

is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}

is_using_clang <- function() {
  grepl("clang", R_compiled_by()["C"])
}

# Source: https://packages.fedoraproject.org/pkgs/clang/clang/
# See Updates section for older Fedora versions.
fedora_clang_defaults <- data.frame(
  os = as.integer(c(36, 37, 38, 39, 40)),
  clang = as.integer(c(14, 15, 16, 17, 17))
)

# Source: https://packages.debian.org/search?keywords=clang
debian_clang_defaults <- data.frame(
  os = c("bullseye", "bookworm", "trixie"),
  clang = as.integer(c(11, 14, 16))
)

parse_clang_major <- function(clang_string) {
  assert_string(clang_string, pattern = "clang")
  clang_version <- gsub(pattern = "[^0-9.]", replacement = "", x = clang)
  as.integer(gsub(pattern = "([0-9]+).+", replacement = "\\1", x = clang_version))
}

is_non_standard_clang <- function() {
  os <- tolower(utils::osVersion)
  clang_major_version <- parse_clang_major(R_compiled_by()["C"])
  assert_int(clang_major_version)
  if (grepl("fedora", os)) {
    os_version <- as.integer(gsub(pattern = "[^0-9]", replacement = "", x = os))
    assert_int(os_version)
    which_os <- match(os_version, fedora_clang_defaults$os)
    if (is.na(which_os)) return(FALSE)
    clang_major_version > fedora_clang_defaults$clang[which_os]
  } else if (grepl("debian", os)) {
    os_codename <- gsub(pattern = "debian gnu/linux ([a-z]+)/*[a-z]*", replacement = "\\1", x = os)
    assert_string(os_codename)
    which_os <- match(os_codename, debian_clang_defaults$os)
    if (is.na(which_os)) return(FALSE)
    clang_major_version > debian_clang_defaults$clang[which_os]
  } else {
    FALSE
  }
}

skip_if_cran_devel_clang <- function() {
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
