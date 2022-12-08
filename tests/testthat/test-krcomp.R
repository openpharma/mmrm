# h_get_kr_comp ----
test_that("h_get_kr_comp works as expected on ungrouped mmrm", {
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
    expect_snapshot_tolerance(fit$kr_comp)
  }
)

test_that("h_get_kr_comp works as expected on grouped mmrm", {
    fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
    expect_snapshot_tolerance(fit$kr_comp)
  }
)

# df_1d ----

test_that("kr give similar results as SAS for ar1", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.95865439662225, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

test_that("kr linear give similar results as SAS for ar1", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger-Linear")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.96058142305176, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

test_that("kr give similar results as SAS for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7590316099633, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

test_that("kr linear give similar results as SAS for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger-Linear")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.75924807546934, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

test_that("kr give similar results as SAS for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

test_that("kr linear give similar results as SAS for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger-Linear")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

test_that("kr give similar results as SAS for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67414806011886, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

test_that("kr linear give similar results as SAS for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger-Linear")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67403858183242, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

test_that("kr give similar results as SAS for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66172017349971, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

test_that("kr linear give similar results as SAS for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger-Linear")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66158550758897, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

# h_df_1d_kr ----

test_that("h_df_1d_kr works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_df_1d_kr(object_mmrm_kr, c(0, 1), TRUE))
  expect_snapshot_tolerance(h_df_1d_kr(object_mmrm_kr, c(1, 1), FALSE))
})

# h_df_md_kr ----

test_that("h_df_md_kr works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_df_md_kr(object_mmrm_kr, matrix(c(0, 1, 1, 0), nrow = 2), TRUE))
  expect_snapshot_tolerance(h_df_md_kr(object_mmrm_kr, matrix(c(0, -1, 1, 0), nrow = 2), FALSE))
})

# h_kr_df ----

test_that("h_kr_df works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  kr_comp <- object_mmrm_kr$kr_comp
  w <- component(object_mmrm_kr, "theta_vcov")
  v_adj <- object_mmrm_kr$beta_vcov_adj
  expect_snapshot_tolerance(
    h_kr_df(v0 = object_mmrm_kr$beta_vcov, l = matrix(c(0, 1), nrow = 1), w = w, p = kr_comp$P),
    style = "deparse"
  )
})

# h_var_adj ----

test_that("h_var_adj works as expected in the standard case for Kenward-Roger", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_var_adj(
    v = object_mmrm_kr$beta_vcov,
    w = component(object_mmrm_kr, "theta_vcov"),
    p = object_mmrm_kr$kr_comp$P,
    q = object_mmrm_kr$kr_comp$Q,
    r = object_mmrm_kr$kr_comp$R,
    linear = TRUE
  ))
})

test_that("h_var_adj works as expected in the standard case for Kenward-Roger-Linear", {
  expect_snapshot_tolerance(h_var_adj(
    v = object_mmrm_kr$beta_vcov,
    w = component(object_mmrm_kr, "theta_vcov"),
    p = object_mmrm_kr$kr_comp$P,
    q = object_mmrm_kr$kr_comp$Q,
    r = object_mmrm_kr$kr_comp$R,
    linear = FALSE
  ))
})
