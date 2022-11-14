devtools::load_all()

data <- subset(fev_data, USUBJID %in% c("PT1"))
fit <- mmrm(
  formula = FEV1 ~ ar1(AVISIT | USUBJID),
  data = data
)
# run the rmarkdown part of kenward.Rmd
res1 = test2(fit$tmb_data$x_matrix, fit$tmb_data$subject_zero_inds, fit$tmb_data$visits_zero_inds,
fit$tmb_data$n_subjects, fit$tmb_data$subject_n_visits, fit$tmb_data$n_visits, fit$tmb_data$cov_type, fit$tmb_data$is_spatial_int, fit$theta_est)


res1[[4]] - sigma_sparse

res1[[2]] - do.call(rbind, sigma_d1_sparse)

res1[[3]] - rbind(sigma_d2_sparse[[1]][[1]], sigma_d2_sparse[[1]][[2]], sigma_d2_sparse[[2]][[1]], sigma_d2_sparse[[2]][[2]])

do.call(rbind, p_sparse) - res1[[5]]
rbind(r_sparse[[1]][[1]],r_sparse[[1]][[2]],r_sparse[[2]][[1]],r_sparse[[2]][[2]]) - res1[[7]]
rbind(q_sparse[[1]][[1]],q_sparse[[1]][[2]],q_sparse[[2]][[1]],q_sparse[[2]][[2]]) - res1[[6]]
