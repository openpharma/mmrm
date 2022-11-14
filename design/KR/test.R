devtools::load_all()

data = subset(fev_data, USUBJID %in% c("PT1", "PT2"))
data$FEV1[1] = 22
data$FEV1[3] = 32
data$FEV1[5] = 31

fit <- mmrm(
  formula = FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
  data = data
)




test2(fit$tmb_data$x_matrix, fit$tmb_data$subject_zero_inds, fit$tmb_data$visits_zero_inds,
fit$tmb_data$n_subjects, fit$tmb_data$subject_n_visits, fit$tmb_data$n_visits, fit$tmb_data$cov_type, fit$tmb_data$is_spatial_int, fit$theta_est)
