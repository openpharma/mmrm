#' Getting Number of Variance Parameters in Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param model (`list`)\cr model from glmmTMB package.
#' @return the number of variance parameters in the model as well as one ID which has the maximum number of visits.
#' @export
#'
#' @examples
#' dat <- fev_data
#' mod <- glmmTMB(
#' FEV1 ~ ar1(0 + AVISIT | USUBJID),
#' data = dat,
#' dispformula = ~0,
#' REML = TRUE
#' )
#' class(mod) <- c("mmrm_fit", "glmmTMB")
#' h_cov_estimate(mod)
h_cov_estimate <- function(model) {
  assert_class(model, "mmrm_fit")
  cov_est <- VarCorr(model)$cond[[1L]]
  theta <- getME(model, "theta")
  id_per_obs <- model$modelInfo$reTrms$cond$flist[[1L]]
  n_visits <- length(model$modelInfo$reTrms$cond$cnms[[1L]])
  which_id <- which(table(id_per_obs) == n_visits)[1L]
  structure(
    cov_est,
    id = levels(id_per_obs)[which_id],
    n_parameters = length(theta)
  )
}
