################################################################################
## Evaluator Functions
################################################################################

## Extract correlation matrix from model fits
get_covar_mat <- function(fit) {

  ## extract the covariance matrix from the fit object
  if (class(fit)[1] == "gls") {
    corr <- coef(fit$modelStruct$corStruct, unconstrained = FALSE)
    vars <- coef(fit$modelStruct$varStruct, unconstrained = FALSE)
    covar_mat <- tcrossprod(sqrt(vars), sqrt(vars)) * corr
    diag(covar_mat) <- vars
  } else if (class(fit)[1] == "glmmTMB") {
    corr_fit_attributes <- attributes(var_corr[["cond"]]$participant)
    corr_mat <- corr_fit_attributes$correlation
    out_stddev_mat <- outer(corr_fit_attributes$stddev, corr_fit_attributes$stddev)
    covar_mat <- corr_mat * out_stddev_mat
  } else if (class(fit)[1] == "mmrm") {
    covar_mat <- fit$cov
  }

  ## standardize row and column names
  rownames(covar_mat) <- NULL
  colnames(covar_mat) <- NULL

  return(covar_mat)
}
