h_get_empirical <- function(tmb_data, theta, beta, beta_vcov, jackknife) {
  .Call(`_mmrm_get_empirical`, PACKAGE = "mmrm", tmb_data, theta, beta, beta_vcov, jackknife)
}