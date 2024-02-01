# Internal functions used for skipping tests or examples.

# Predicate whether currently running R version is under development.
is_r_devel <- function() {
  grepl("devel", R.version$status)
}

# Predicate whether currently running on a Linux operating system.
is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}

# Get the compiler information. Workaround for older R versions
# where R_compiled_by() is not available.
get_compiler <- function() {
  r_cmd <- file.path(R.home("bin"), "R")
  system2(r_cmd, args = "CMD config CC", stdout = TRUE)
}

# Predicate whether currently using a clang compiler.
is_using_clang <- function() {
  grepl("clang", get_compiler())
}

# Predicate whether an R-devel version is running on Linux Fedora or
# Debian with a clang compiler.
is_r_devel_linux_clang <- function() {
  is_r_devel() &&
    is_linux() &&
    is_using_clang()
}
