# h_assert_one_rec_pt_visit ----

test_that("h_assert_one_rec_pt_visit passes as expected if data is ok", {
  vars <- list(visit = "AVISIT", id = "MYID")
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 2, 1, 2, 1, 2)
  )
  expect_silent(h_assert_one_rec_pt_visit(vars, data))
})

test_that("h_assert_one_rec_pt_visit fails as expected if data is wrong", {
  vars <- list(visit = "AVISIT", id = "MYID")

  # Patient 1 at visit 2 twice observed.
  data <- data.frame(
    MYID = c(1, 1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 2, 2, 1, 2, 1, 2)
  )
  expect_error(
    h_assert_one_rec_pt_visit(vars, data),
    "There are 1 subjects with more than one record per visit: MYID = 1 with AVISIT = 2",
    fixed = TRUE
  )

  # Patient 1 at visit 2, Patient 3 at visit 1 twice observed.
  data <- data.frame(
    MYID = c(3, 1, 1, 1, 2, 2, 3, 3),
    AVISIT = c(1, 1, 2, 2, 1, 2, 1, 2)
  )
  expect_error(
    h_assert_one_rec_pt_visit(vars, data),
    paste(
      "There are 2 subjects with more than one record per visit:",
      "MYID = 1 with AVISIT = 2, MYID = 3 with AVISIT = 1"
    ),
    fixed = TRUE
  )
})

test_that("h_assert_one_rec_pt_visit works with just one patient and one visit", {
  vars <- list(visit = "AVISIT", id = "MYID")
  data <- data.frame(
    MYID = 1,
    AVISIT = 5
  )
  expect_silent(h_assert_one_rec_pt_visit(vars, data))
})

# h_assert_rsp_var ----

test_that("h_assert_rsp_var works as expected", {
  vars <- list(response = "bla")

  data <- data.frame(bla = numeric(1))
  expect_silent(h_assert_rsp_var(vars, data))

  data2 <- data.frame(foo = numeric(1))
  expect_error(h_assert_rsp_var(vars, data2))

  data3 <- data.frame(bla = factor(1))
  expect_error(h_assert_rsp_var(vars, data3))
})

# h_assert_visit_var ----

test_that("h_assert_visit_var works as expected", {
  vars <- list(visit = "vis")

  data <- data.frame(vis = factor(1))
  expect_silent(h_assert_visit_var(vars, data))

  data2 <- data.frame(boo = factor(1))
  expect_error(h_assert_visit_var(vars, data2))

  data3 <- data.frame(vis = numeric(1))
  expect_error(h_assert_visit_var(vars, data3))
})

# assert_data ----

test_that("assert_data passes as expected", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP")
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    AVISIT = factor(c(1, 2, 1, 2, 1, 2)),
    RSP = c(25.3245, 234.34, 5.1, 35.2, 24.24, 346.32)
  )
  expect_silent(assert_data(vars, data))
})

test_that("assert_data does not look at rows with incomplete regressors for checking duplicates", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", covariates = "BLA")
  data <- data.frame(
    MYID = c(1, 1, 2, 2, 3, 3),
    AVISIT = factor(c(1, 2, 1, 2, 1, 1)), # Duplicate visit 1 for id 3.
    BLA = c(1, 1, 1, 1, 1, NA), # But regressor is missing there.
    RSP = c(25.3245, 234.34, 5.1, 35.2, 24.24, 346.32)
  )
  expect_silent(assert_data(vars, data))
})

test_that("assert_data fails when less than 5 rows in complete data set without arm", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", covariates = "BLA")
  data <- data.frame( # Only 4 rows with complete data.
    MYID = c(1, 1, 2, 2, 3, 3),
    AVISIT = factor(c(1, 2, 1, 2, 1, 1)),
    BLA = c(1, 1, 1, 1, 1, NA),
    RSP = c(25.3245, 234.34, 5.1, NA, 24.24, 346.32)
  )
  expect_error(
    assert_data(vars, data),
    "Must have at least 5 rows, but has 4 rows"
  )
})

test_that("assert_data fails when less than 5 rows in complete data set per arm", {
  vars <- list(visit = "AVISIT", id = "MYID", response = "RSP", arm = "TRT")
  data <- data.frame( # Only 4 rows with complete data for TRT 1.
    MYID = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    AVISIT = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
    TRT = factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)),
    RSP = c(25.3245, 234.34, 5.1, NA, 24.24, 346.32, 1.2, 1.3, 1.4, 1.5)
  )
  expect_error(assert_data(vars, data))
})
