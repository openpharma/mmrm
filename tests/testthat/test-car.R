# car loading ----
test_that("car emits a message about mmrm registration on load", {
  # Don't use skip_if_not_installed yet since it runs requireNamespace, which
  # will itself load the package without testing startup.
  skip_if(length(find.package("car")) < 1)

  # Detach car in case it was loaded in session or previous test run
  if (isNamespaceLoaded("car")) {
    unloadNamespace("car")
  }

  expect_message(library(car), "mmrm")
})


# h_get_contrast ----
test_that("h_get_contrast works as expected", {
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "log(FEV1_BL)", "3"),
    matrix(c(0, 1, rep(0, 7)), nrow = 1, byrow = TRUE)
  )
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "ARMCD", "3"),
    matrix(c(0, 0, 1, rep(0, 3), rep(0.25, 3)), nrow = 1, byrow = TRUE)
  )
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "ARMCD:AVISIT", "3"),
    matrix(rep(rep(c(0, 1), 3), c(6, 1, 9, 1, 9, 1)), nrow = 3, byrow = TRUE)
  )
})

# Anova ----
test_that("Anova works as expected", {
  skip_if_not_installed("car")
  expect_snapshot_tolerance(
    Anova(get_mmrm(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_weighted(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_rank_deficient(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_trans(), "3")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_weighted(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_rank_deficient(), "2")
  )
  expect_snapshot_tolerance(
    Anova(get_mmrm_trans(), "2")
  )
})

# Anova Integration Test ----

test_that("Anova give similar results to SAS", {
  skip_if_not_installed("car")
  fit <- mmrm(FEV1 ~ ARMCD * FEV1_BL + ar1(AVISIT | USUBJID), data = fev_data)
  df2 <- expect_silent(Anova(fit, "2"))
  df3 <- expect_silent(Anova(fit, "3"))
  # the SAS results from design/anova/test2_1.csv and design/anova/test3_1.csv
  # they shared the same model
  expect_equal(
    df2$`Denom Df`,
    c(199.582314485653, 196.41571750149, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F Statistic`,
    c(0.03099660681111, 11.5301174147263, 0.7644368094469),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Denom Df`,
    c(199.582314485653, 198.163532037936, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F Statistic`,
    c(0.03099660681111, 11.0435094615508, 0.7644368094469),
    tolerance = 1e-4
  )
})

test_that("Anova give similar results to SAS", {
  skip_if_not_installed("car")
  fit <- mmrm(FEV1 ~ ARMCD * SEX + ARMCD * FEV1_BL - FEV1_BL + ar1(AVISIT | USUBJID), data = fev_data)
  df2 <- expect_silent(Anova(fit, "2"))
  df3 <- expect_silent(Anova(fit, "3"))
  # the SAS results from design/anova/test2_1.csv and design/anova/test3_1.csv
  # they shared the same model
  expect_equal(
    df2$`Denom Df`,
    c(196.700026021882, 186.404412481085, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F Statistic`,
    c(0.03093826913613, 0.10489707559679, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Denom Df`,
    c(197.423866403271, 186.119018537661, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F Statistic`,
    c(0.04875135180324, 0.09277653315012, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
})
