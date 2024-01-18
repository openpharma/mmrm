# Custom skipping function, specializing testthat::skip.
skip_if_r_devel_linux_clang <- function() {
  do_skip <- is_r_devel_linux_clang()
  if (do_skip) {
    skip("On R-devel Linux system with non-standard clang")
  } else {
    invisible()
  }
}
