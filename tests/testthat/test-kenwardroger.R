# h_get_kr_comp ----
test_that("h_get_kr_comp works as expected on ar1 ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1 grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1h ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1h grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on cs ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on cs grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on csh ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on csh grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toep ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toep grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toeph ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toeph grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on us ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on us grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on adh ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on adh grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ad ungrouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ad(AVISIT | USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ad grouped mmrm", {
  fit <- mmrm(FEV1 ~ ARMCD + ad(AVISIT | SEX / USUBJID), data = fev_data, reml = TRUE, method = "Kenward-Roger")
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on spatial mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on grouped spatial mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | SEX / USUBJID),
    data = fev_data, reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

# df_1d ----

## auto-regressive ----

### kr ----

test_that("kr give similar results as SAS for ar1", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.95865439662225, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## kr linear ----

test_that("kr linear give similar results as SAS for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.96058142305176, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## Heterogeneous auto-regressive ----

### kr ----

test_that("kr give similar results as SAS for ar1h", {
  fit <- mmrm(FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7590316099633, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for ar1h", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.75924807546934, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## compound symmetry ----

### kr ----

test_that("kr give similar results as SAS for cs", {
  fit <- mmrm(FEV1 ~ ARMCD + cs(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

### kr linear ----

test_that("kr linear give similar results as SAS for cs", {
  fit <- mmrm(
    FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## Heterogeneous compound symmetry ----

### kr ----

test_that("kr give similar results as SAS for csh", {
  fit <- mmrm(FEV1 ~ ARMCD + csh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67414806011886, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for csh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67403858183242, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

## Heterogeneous ante-dependence ----

### kr ----

test_that("kr give similar results as SAS for adh", {
  fit <- mmrm(FEV1 ~ ARMCD + adh(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66172017349971, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for adh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66158550758897, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Toeplitz ----

### kr ----

test_that("kr give similar results as SAS for toep", {
  fit <- mmrm(FEV1 ~ ARMCD + toep(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.87839805519623, 160.027408337368)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear

test_that("kr linear give similar results as SAS for toep", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.87839805519623, 160.027408337368)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Heterogeneous Toeplitz ----

### kr ----

test_that("kr give similar results as SAS for toeph", {
  fit <- mmrm(FEV1 ~ ARMCD + toeph(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.72543828853831, 180.062730071701)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear

test_that("kr linear give similar results as SAS for toeph", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.72537324518435, 180.062730071701)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Unstructured ----

### kr ----

test_that("kr give similar results as SAS for unstructured", {
  # Please note that in SAS, for unstructure covariance, Kenward-Roger and Kenward-Roger-Linear
  # are identical because in their parameterization the second order derivatives are zero matrices.
  # In `mmrm`, we are using different parameterization so the second order derivatives are non-zero.
  # This will lead to differences in Kenward-Roger and Kenward-Roger-Linear.
  fit <- mmrm(FEV1 ~ ARMCD + us(AVISIT | USUBJID), data = fev_data, method = "Kenward-Roger")
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66124382270307, 160.733266403768)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-1)
})

### kr linear

test_that("kr linear give similar results as SAS for unstructured", {
  fit <- mmrm(
    FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66124382270307, 160.73326640376)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Spatial Exponential ----

### kr

test_that("kr give similar results as SAS for spatial exponential", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.90552903839818, 195.584197921463)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

### kr linear

test_that("kr linear give similar results as SAS for spatial exponential", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.90527620094771, 195.584197921463)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

# h_df_1d_kr ----

test_that("h_df_1d_kr works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_df_1d_kr(object_mmrm_kr, c(0, 1)))
  expect_snapshot_tolerance(h_df_1d_kr(object_mmrm_kr, c(1, 1)))
})

# h_df_md_kr ----

test_that("h_df_md_kr works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_df_md_kr(object_mmrm_kr, matrix(c(0, 1, 1, 0), nrow = 2)))
  expect_snapshot_tolerance(h_df_md_kr(object_mmrm_kr, matrix(c(0, -1, 1, 0), nrow = 2)))
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
  object_mmrm_kr <- get_mmrm_kr()
  expect_snapshot_tolerance(h_var_adj(
    v = object_mmrm_kr$beta_vcov,
    w = component(object_mmrm_kr, "theta_vcov"),
    p = object_mmrm_kr$kr_comp$P,
    q = object_mmrm_kr$kr_comp$Q,
    r = object_mmrm_kr$kr_comp$R,
    linear = FALSE
  ))
})

# df_md ----

test_that("df_md works as expected for Kenward-Roger", {
  object_mmrm_kr <- get_mmrm_kr()
  contrast <- matrix(c(0, 1, 1, 0), nrow = 2)
  result <- expect_silent(df_md(object_mmrm_kr, contrast))
  expected <- list(
    num_df = 2L,
    denom_df = 188.65,
    f_stat = 3913.72,
    p_val = 2.576e-154
  )
  expect_equal(
    result,
    expected,
    tolerance = 1e-4
  )
})
