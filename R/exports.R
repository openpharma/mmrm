#' obtain P, Q, R matix for the mmrm fit
get_pqr <- function(x,
                    subject_zero_inds,
                    visits_zero_inds,
                    n_subjects,
                    subject_n_visits,
                    n_visits,
                    cov_type,
                    is_spatial,
                    theta) {
  .Call(
    `_mmrm_get_pqr`,
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
