test2 <- function(x,
                  subject_zero_inds,
                  visits_zero_inds,
                  n_subjects,
                  subject_n_visits,
                  n_visits,
                  cov_type,
                  is_spatial,
                  theta) {
  .Call(
    `_mmrm_test2`,
    x,
    subject_zero_inds,
    visits_zero_inds,
    n_subjects,
    subject_n_visits,
    n_visits,
    cov_type,
    is_spatial,
    theta
  )
}
