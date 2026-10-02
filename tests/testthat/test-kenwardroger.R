# h_get_kr_comp ----
test_that("h_get_kr_comp works as expected on ar1 ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1 grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1h ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ar1h grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1h(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on cs ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on cs grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + cs(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on csh ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on csh grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + csh(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toep ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toep grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toep(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toeph ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on toeph grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toeph(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on us ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on us grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + us(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on adh ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on adh grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + adh(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ad ungrouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ad(AVISIT | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on ad grouped mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ad(AVISIT | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on spatial mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on grouped spatial mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_exp(VISITN, VISITN2 | SEX / USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

test_that("h_get_kr_comp works as expected on spatial Gaussian mmrm", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_gau(VISITN, VISITN2 | USUBJID),
    data = fev_data,
    reml = TRUE,
    method = "Kenward-Roger"
  )
  expect_snapshot_tolerance(fit$kr_comp)
})

# df_1d ----

## auto-regressive ----

### kr ----

test_that("kr give similar results as SAS for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.95865439662225, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## kr linear ----

test_that("kr linear give similar results as SAS for ar1", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.96058142305176, 188.46934887972)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## Heterogeneous auto-regressive ----

### kr ----

test_that("kr give similar results as SAS for ar1h", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7590316099633, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for ar1h", {
  fit <- mmrm(
    FEV1 ~ ARMCD + ar1h(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.75924807546934, 188.225339095373)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## compound symmetry ----

### kr ----

test_that("kr give similar results as SAS for cs", {
  fit <- mmrm(
    FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7965, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for cs", {
  fit <- mmrm(
    FEV1 ~ ARMCD + cs(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.7964696053595, 177.038485931223)
  expect_equal(res$df, expected[2], tolerance = 1e-4)
  expect_equal(res$se, expected[1], tolerance = 1e-4)
})

## Heterogeneous compound symmetry ----

### kr ----

test_that("kr give similar results as SAS for csh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67414806011886, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for csh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + csh(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.67403858183242, 190.737701349941)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

## Heterogeneous ante-dependence ----

### kr ----

test_that("kr give similar results as SAS for adh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66172017349971, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear ----

test_that("kr linear give similar results as SAS for adh", {
  fit <- mmrm(
    FEV1 ~ ARMCD + adh(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66158550758897, 162.393385281755)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Toeplitz ----

### kr ----

test_that("kr give similar results as SAS for toep", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.87839805519623, 160.027408337368)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear

test_that("kr linear give similar results as SAS for toep", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toep(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.87839805519623, 160.027408337368)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Heterogeneous Toeplitz ----

### kr ----

test_that("kr give similar results as SAS for toeph", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.72543828853831, 180.062730071701)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-2)
})

### kr linear

test_that("kr linear give similar results as SAS for toeph", {
  fit <- mmrm(
    FEV1 ~ ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
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
  fit <- mmrm(
    FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.66124382270307, 160.733266403768)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-1)
})

### kr linear

test_that("kr linear give similar results as SAS for unstructured", {
  fit <- mmrm(
    FEV1 ~ ARMCD + us(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
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
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
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
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  expected <- c(0.90527620094771, 195.584197921463)
  expect_equal(res$df, expected[2], tolerance = 1e-3)
  expect_equal(res$se, expected[1], tolerance = 1e-3)
})

## Spatial Gaussian ----

### kr linear

test_that("kr linear give similar results as SAS for spatial Gaussian", {
  fit <- mmrm(
    FEV1 ~ ARMCD + sp_gau(VISITN, VISITN2 | USUBJID),
    data = fev_data,
    method = "Kenward-Roger",
    vcov = "Kenward-Roger-Linear"
  )
  res <- df_1d(fit, contrast = c(0, 1))
  # See design/SAS/sas_sp_gau_kr.txt for the source of numbers
  # (ddfm=kenwardroger(firstorder)).
  expected <- c(0.9028, 250.2326)
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
  expect_snapshot_tolerance(h_df_md_kr(
    object_mmrm_kr,
    matrix(c(0, 1, 1, 0), nrow = 2)
  ))
  expect_snapshot_tolerance(h_df_md_kr(
    object_mmrm_kr,
    matrix(c(0, -1, 1, 0), nrow = 2)
  ))
})

# h_kr_df ----

test_that("h_kr_df works as expected in the standard case", {
  object_mmrm_kr <- get_mmrm_kr()
  kr_comp <- object_mmrm_kr$kr_comp
  w <- component(object_mmrm_kr, "theta_vcov")
  v_adj <- object_mmrm_kr$beta_vcov_adj
  expect_snapshot_tolerance(
    h_kr_df(
      v0 = object_mmrm_kr$beta_vcov,
      l = matrix(c(0, 1), nrow = 1),
      w = w,
      p = kr_comp$P
    ),
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
    linear = FALSE
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
    linear = TRUE
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

# First-derivative-only preparation ----

test_that("h_get_kr_comp with linear = TRUE preserves P, Q and inference while omitting R", {
  formulas <- list(
    FEV1 ~ ARMCD * AVISIT + us(AVISIT | USUBJID),
    FEV1 ~ ARMCD * AVISIT + us(AVISIT | SEX / USUBJID),
    FEV1 ~ ARMCD * AVISIT + ar1(AVISIT | USUBJID),
    FEV1 ~ ARMCD * AVISIT + sp_exp(VISITN, VISITN2 | USUBJID),
    FEV1 ~ ARMCD * AVISIT + sp_gau(VISITN, VISITN2 | SEX / USUBJID)
  )
  for (formula in formulas) {
    for (weighted in c(FALSE, TRUE)) {
      info <- paste(format(formula), if (weighted) "(weighted)" else "(unweighted)")
      weights <- if (weighted) seq(0.5, 2, length.out = nrow(fev_data)) else rep(1, nrow(fev_data))
      fit <- mmrm(formula, fev_data, weights = weights,
        control = mmrm_control(method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"))
      full <- h_get_kr_comp(fit$tmb_data, fit$theta_est)
      expect_null(fit$kr_comp$R, info = info)
      expect_equal(fit$kr_comp$P, full$P, tolerance = 1e-12, info = info)
      expect_equal(fit$kr_comp$Q, full$Q, tolerance = 1e-12, info = info)
      # The previous implementation discarded R by replacing it with zeros.
      reference <- fit
      reference$kr_comp <- full
      reference$beta_vcov_adj <- h_var_adj(fit$beta_vcov, component(fit, "theta_vcov"),
        full$P, full$Q, matrix(0, nrow(full$R), ncol(full$R)))
      expect_equal(fit$beta_vcov_adj, reference$beta_vcov_adj, tolerance = 1e-12, info = info)
      p <- length(coef(fit))
      expect_equal(df_1d(fit, diag(p)[p, ]), df_1d(reference, diag(p)[p, ]),
        tolerance = 1e-10, info = info)
      expect_equal(df_md(fit, diag(p)[(p - 1):p, ]), df_md(reference, diag(p)[(p - 1):p, ]),
        tolerance = 1e-10, info = info)
    }
  }
})

test_that("h_var_adj requires R for full Kenward-Roger", {
  object_mmrm_kr <- get_mmrm_kr()
  expect_error(
    h_var_adj(
      v = object_mmrm_kr$beta_vcov,
      w = component(object_mmrm_kr, "theta_vcov"),
      p = object_mmrm_kr$kr_comp$P,
      q = object_mmrm_kr$kr_comp$Q,
      r = NULL,
      linear = FALSE
    ),
    "matrix"
  )
})

# Contrast-space degrees of freedom ----

test_that("contrast-space KR agrees with coefficient-space inference on fitted models", {
  skip_on_cran()
  complete_ids <- names(which(table(fev_data$USUBJID) == nlevels(fev_data$AVISIT)))
  complete_data <- droplevels(fev_data[fev_data$USUBJID %in% complete_ids, ])
  cases <- list(
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID), complete_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | SEX / USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + ar1(AVISIT | USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + sp_exp(VISITN, VISITN2 | SEX / USUBJID), fev_data),
    list(FEV1 ~ ARMCD * AVISIT + FEV1_BL + sp_gau(VISITN, VISITN2 | USUBJID), fev_data)
  )
  for (case in cases) {
    for (weighted in c(FALSE, TRUE)) {
      dat <- case[[2L]]
      weights <- if (weighted) seq(0.5, 2, length.out = nrow(dat)) else rep(1, nrow(dat))
      fit <- mmrm(case[[1L]], dat, weights = weights, method = "Kenward-Roger")
      v0 <- fit$beta_vcov
      w <- component(fit, "theta_vcov")
      p <- fit$kr_comp$P
      n_beta <- ncol(v0)
      # Dense, nonorthogonal contrasts exercise normalization and off-diagonal F entries.
      contrasts <- diag(n_beta) + matrix(seq_len(n_beta^2) / n_beta^3, n_beta)
      for (rank in c(1L, 2L, 3L, n_beta)) {
        contrast <- contrasts[seq_len(rank), , drop = FALSE]
        info <- paste(format(case[[1L]]), weighted, rank, nrow(dat))
        expected <- h_kr_df_coefficient_space(v0, contrast, w, p)
        actual <- h_kr_df(v0, contrast, w, p)
        expect_equal(actual, expected, tolerance = 1e-9, info = info)
        for (linear in c(FALSE, TRUE)) {
          reference <- fit
          if (linear) {
            reference$vcov <- "Kenward-Roger-Linear"
            reference$beta_vcov_adj <- h_var_adj(v0, w, p, fit$kr_comp$Q, NULL, linear = TRUE)
          }
          expect_equal(df_md(reference, contrast),
            h_test_md(reference, contrast, expected$m, expected$lambda),
            tolerance = 1e-9, info = info)
          if (rank == 1L) {
            scalar <- df_1d(reference, as.vector(contrast))
            expect_equal(scalar, h_test_1d(reference, as.vector(contrast), expected$m),
              tolerance = 1e-9, info = info)
            expect_equal(df_md(reference, contrast)$f_stat, scalar$t_stat^2, tolerance = 1e-12)
            expect_equal(df_md(reference, contrast)$p_val, scalar$p_val, tolerance = 1e-12)
          }
        }
      }
      # Invertible row transformations represent the same hypothesis.
      contrast <- contrasts[1:3, , drop = FALSE]
      transform <- matrix(c(2, 1, -1, 0, 3, 1, 1, 0, 2), 3)
      expect_equal(h_kr_df(v0, transform %*% contrast, w, p), h_kr_df(v0, contrast, w, p),
        tolerance = 1e-9)
      if (fit$tmb_data$n_groups > 1L) {
        group <- rep(seq_len(fit$tmb_data$n_groups), each = ncol(w) / fit$tmb_data$n_groups)
        block_w <- w
        block_w[outer(group, group, "!=")] <- 0
        expect_gt(max(abs(w - block_w)), 1e-8)
        expect_gt(abs(h_kr_df(v0, contrast, w, p)$m - h_kr_df(v0, contrast, block_w, p)$m), 1e-6)
      }
    }
  }
})

test_that("contrast-space KR agrees with coefficient-space inference on a linear KR fit", {
  fit <- mmrm(
    FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID), fev_data,
    method = "Kenward-Roger", vcov = "Kenward-Roger-Linear"
  )
  expect_null(fit$kr_comp$R)
  v0 <- fit$beta_vcov
  w <- component(fit, "theta_vcov")
  p <- fit$kr_comp$P
  n_beta <- ncol(v0)
  contrasts <- diag(n_beta) + matrix(seq_len(n_beta^2) / n_beta^3, n_beta)
  for (rank in c(1L, 3L)) {
    contrast <- contrasts[seq_len(rank), , drop = FALSE]
    expected <- h_kr_df_coefficient_space(v0, contrast, w, p)
    expect_equal(h_kr_df(v0, contrast, w, p), expected, tolerance = 1e-9)
    expect_equal(df_md(fit, contrast), h_test_md(fit, contrast, expected$m, expected$lambda), tolerance = 1e-9)
  }
  contrast <- as.vector(contrasts[1L, ])
  expected <- h_kr_df_coefficient_space(v0, matrix(contrast, nrow = 1), w, p)
  expect_equal(df_1d(fit, contrast), h_test_1d(fit, contrast, expected$m), tolerance = 1e-9)
})

test_that("scalar shortcut handles one coefficient, rescaling and boundary moments", {
  for (moment in c(0, 0.01, 1, 2)) {
    expected <- list(m = 2 / moment, lambda = 1)
    for (scale in c(-3, 1, 1e-8, 1e8)) {
      expect_equal(h_kr_df(matrix(2), matrix(scale), matrix(moment / 64), matrix(4)), expected)
    }
  }
})

test_that("contrast-space calculation supports one covariance parameter and ill-conditioned covariance", {
  v0 <- diag(c(1e-6, 0.2, 1, 10, 100))
  v0[2, 3] <- v0[3, 2] <- 0.1
  p <- diag(1 / diag(v0))
  contrast <- matrix(c(1, 2, -1, 0, 1, 0, 1, 3, -2, 1), 2, byrow = TRUE)
  w <- matrix(0.001)
  expected <- h_kr_df_coefficient_space(v0, contrast, w, p)
  expect_equal(h_kr_df(v0, contrast, w, p), expected, tolerance = 1e-9)
  scaled <- diag(c(1e-3, 1e3)) %*% contrast
  expect_equal(h_kr_df(v0, scaled, w, p), expected, tolerance = 1e-9)
  expect_equal(h_kr_df(v0, diag(5), w, p),
    h_kr_df_coefficient_space(v0, diag(5), w, p), tolerance = 1e-9)
})

test_that("contrast-space calculation rejects incompatible components and singular hypotheses", {
  expect_error(h_kr_df(diag(2), diag(2), diag(2), matrix(0, 4, 3)), "2 cols")
  expect_error(h_kr_df(diag(2), diag(2), diag(2), matrix(0, 2, 2)), "4 rows")
  expect_error(h_kr_df(diag(2), matrix(1, 2, 2), matrix(1), diag(2)), "numerically singular")
  expect_error(h_kr_df(diag(c(1, -1)), diag(2), matrix(1), diag(2)), "not positive definite")
  expect_error(h_kr_df(diag(2), matrix(0, 1, 2), matrix(1), diag(2)), "variance must be positive")
  expect_error(h_kr_df(diag(c(1, -1)), matrix(c(0, 1), 1), matrix(1), diag(2)), "variance must be positive")
})

test_that("scalar KR df equals Satterthwaite based on unadjusted covariance", {
  skip_on_cran()
  formulas <- list(
    FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | USUBJID),
    FEV1 ~ ARMCD * AVISIT + FEV1_BL + us(AVISIT | SEX / USUBJID),
    FEV1 ~ ARMCD * AVISIT + FEV1_BL + ar1(AVISIT | USUBJID),
    FEV1 ~ ARMCD * AVISIT + FEV1_BL + sp_exp(VISITN, VISITN2 | USUBJID)
  )
  for (formula in formulas) {
    weights <- seq(0.5, 2, length.out = nrow(fev_data))
    fit <- mmrm(formula, fev_data, weights = weights, method = "Satterthwaite")
    p <- h_get_kr_comp(fit$tmb_data, fit$theta_est, linear = TRUE)$P
    n_beta <- length(coef(fit))
    for (contrast in list(diag(n_beta)[n_beta, ], seq(-1, 1, length.out = n_beta))) {
      kr <- h_kr_df(fit$beta_vcov, matrix(contrast, nrow = 1), component(fit, "theta_vcov"), p)
      expect_equal(kr$m, df_1d(fit, contrast)$df, tolerance = 1e-10)
      expect_identical(kr$lambda, 1)
    }
  }
})
