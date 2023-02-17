test_that(paste(
  "assemble_df produces a data.frame object with participant, visit_num,",
  "base_bcva, strata, trt and bcva_change columns"
), {
  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(10),
  )
  df <- assemble_df(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change
  )

  expect_equal(nrow(df), 1000)
  expect_equal(
    colnames(df),
    c("participant", "visit_num", "base_bcva", "strata", "trt", "bcva_change")
  )
})

test_that(paste(
  "assemble_df turns the participant variable into a factor when asked"
), {

  dgp_output <- rct_dgp_fun(
    n_obs = 100,
    outcome_covar_mat = diag(3),
  )
  df <- assemble_df(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    participant_as_factor = TRUE
  )

  expect_equal(class(df$participant), "factor")
  expect_equal(levels(df$participant), as.character(seq_len(100)))
})

test_that(paste(
  "assemble_df turns the visit_num variable into a factor when asked"
), {

  dgp_output <- rct_dgp_fun(
    n_obs = 10,
    outcome_covar_mat = diag(15),
  )
  df <- assemble_df(
    participant = dgp_output$participant,
    visit_num = dgp_output$visit_num,
    base_bcva = dgp_output$base_bcva,
    strata = dgp_output$strata,
    trt = dgp_output$trt,
    bcva_change = dgp_output$bcva_change,
    visit_num_as_factor = TRUE
  )

  expect_equal(class(df$visit_num), "factor")
  expect_equal(levels(df$visit_num), as.character(seq_len(15)))
})
