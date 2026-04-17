# h_get_par_from_emp errors for sp_exp

    Code
      h_get_par_from_emp(mat, cov_type = "sp_exp")
    Condition
      Error in `h_get_par_from_emp()`:
      ! Empirical start not implemented for spatial covariance.

# emp_start errors for sp_exp

    Code
      emp_start(y_vector = full_frame$FEV1, x_matrix = model.matrix(model_formula,
        full_frame), full_frame = full_frame, visit_var = "AVISIT", subject_var = "USUBJID",
      subject_groups = subject_groups, cov_type = "sp_exp")
    Condition
      Error in `FUN()`:
      ! Empirical start not implemented for spatial covariance.

