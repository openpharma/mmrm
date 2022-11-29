################################################################################
## Method Wrapper Functions
################################################################################

mmrm_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- mmrm::mmrm(
    formula = y ~ base_cov + trt*time + csh(time | participant), data = df
  )

  return(fit)
}

glmmTMB_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- glmmTMB::glmmTMB(
    formula = y ~ base_cov + trt*time + cs(time + 0 | participant), data = df
  )

}

nlme_wrapper_fun <- function(
  participant,
  time,
  y,
  trt,
  base_cov
) {

  ## assemble the vectors into a data.frame
  df <- data.frame(
    "participant" = participant,
    "time" = time,
    "y" = y,
    "trt" = trt,
    "base_cov" = base_cov
  )

  fit <- nlme::gls(
    y ~ base_cov + trt*time,
    correlation = nlme::corCompSymm(form = ~ 1 | participant),
    weights = nlme::varIdent(form = ~ 1 | time), data = df
  )

  return(fit)

}
