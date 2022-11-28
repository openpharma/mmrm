# get kr comp
test_that("h_get_kr_comp works as expected on grouped/ungrouped mmrm", {
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = TRUE)
    expect_snapshot(h_get_kr_comp(fit$tmb_data, fit$theta_est))
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE)
    expect_snapshot(h_get_kr_comp(fit$tmb_data, fit$theta_est))
  }
)
