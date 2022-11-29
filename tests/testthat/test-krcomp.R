# get kr comp
test_that("h_get_kr_comp works as expected on grouped/ungrouped mmrm", {
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = TRUE)
    expect_snapshot(h_get_kr_comp(fit$tmb_data, fit$theta_est))
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE)
    expect_snapshot(h_get_kr_comp(fit$tmb_data, fit$theta_est))
  }
)

# test adjusted covariance matrix and degree of freedom

test_that("kr give similar results as SAS", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data)
  res <- kr(fit, contrast = matrix(c(0, 1), ncol = 1))
  expected <- c(0.95865439662225, 188.46934887972)
  expect_equal(res$df$m, expected[2], tolerance = 1e-4)
  expect_equal(sqrt(res$v_adj[2, 2]), expected[1], tolerance = 1e-4)

  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data)
  res <- kr(fit, contrast = matrix(c(0, 1), ncol = 1))
  expected <- c(0.7590316099633, 188.225339095373)
  expect_equal(res$df$m, expected[2], tolerance = 1e-4)
  expect_equal(sqrt(res$v_adj[2, 2]), expected[1], tolerance = 1e-1)

  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data)
  res <- kr(fit, contrast = matrix(c(0, 1), ncol = 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df$m, expected[2], tolerance = 1e-4)
  expect_equal(sqrt(res$v_adj[2, 2]), expected[1], tolerance = 1e-3)

  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data)
  res <- kr(fit, contrast = matrix(c(0, 1), ncol = 1))
  expected <- c(0.67414806011886, 190.737701349941)
  expect_equal(res$df$m, expected[2], tolerance = 1e-3)
  expect_equal(sqrt(res$v_adj[2, 2]), expected[1], tolerance = 1e-1)

  fit <- mmrm(FEV1 ~ ARMCD + ad(AVISIT | USUBJID), data = fev_data)
  res <- kr(fit, contrast = matrix(c(0, 1), ncol = 1))
  expected <- c(0.66172017349971, 162.39338528175)
  # ante has large difference
})
