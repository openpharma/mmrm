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

# h_first_contain_categorical ----

test_that("h_first_contain_categorical works as expected", {
  effect <- "FEV1_BL:AVISIT"
  factors <- matrix(
    c(2, 2),
    nrow = 2L,
    dimnames = list(c("FEV1_BL", "AVISIT"), c("FEV1_BL:AVISIT"))
  )
  categorical <- c("AVISIT")
  expect_true(h_first_contain_categorical(effect, factors, categorical))
  factors2 <- matrix(
    c(1, 0, 0, 0, 2, 2),
    nrow = 3L,
    dimnames = list(c("AGE", "FEV1_BL", "AVISIT"), c("AGE", "FEV1_BL:AVISIT"))
  )
  expect_true(h_first_contain_categorical(effect, factors2, categorical))
  categorical2 <- c("NONE")
  expect_false(h_first_contain_categorical(effect, factors, categorical2))
  categorical3 <- c("AGE")
  expect_false(h_first_contain_categorical(effect, factors, categorical3))
})

# h_obtain_lvls ----
test_that("h_obtain_lvls works as expected", {
  expect_identical(
    h_obtain_lvls(
      "a",
      c("b", "c"),
      list(a = letters[1:2], b = letters[1:3], c = letters[1:5])
    ),
    list(
      prior = setNames(integer(0), character(0)),
      post = c(b = 3L, c = 5L),
      total = 15
    )
  )
  expect_identical(
    h_obtain_lvls("a", c("b", "c"), list(b = letters[1:3], c = letters[1:5])),
    list(
      prior = c(b = 3L, c = 5L),
      post = 2L,
      total = 15
    )
  )
})

# h_get_index ----

test_that("h_get_index works as expected", {
  expect_identical(
    expect_silent(h_get_index(list(c(1, 2), c(3, 4)), list(c(1), c(3, 4)))),
    c(1L, 2L)
  )
  expect_identical(
    expect_silent(h_get_index(list(c(1, 2), c(1, 4)), list(c(1), c(1, 4)))),
    c(1L, NA)
  )
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
  expect_identical(
    h_get_contrast(get_mmrm_trans(), "ARMCD:AVISIT", "3"),
    matrix(rep(rep(c(0, 1), 3), c(6, 1, 9, 1, 9, 1)), nrow = 3, byrow = TRUE)
  )
})

test_that("h_get_contrast works even if the interaction term order changes", {
  mod1 <- mmrm(
    formula = FEV1 ~
      RACE + AVISIT + RACE * AVISIT + FEV1_BL + us(AVISIT | USUBJID),
    data = fev_data
  )
  ctr <- expect_silent(h_get_contrast(mod1, "AVISIT", "3"))
  colnames(ctr) <- names(coef(mod1))
  for (i in seq_len(nrow(ctr))) {
    expect_identical(
      names(ctr[i, ctr[i, ] != 0]),
      sprintf(
        c(
          "AVISITVIS%s",
          "RACEBlack or African American:AVISITVIS%s",
          "RACEWhite:AVISITVIS%s"
        ),
        i + 1
      )
    )
  }

  mod2 <- mmrm(
    formula = FEV1 ~
      AVISIT + RACE + AVISIT * RACE + FEV1_BL + us(AVISIT | USUBJID),
    data = fev_data
  )
  ctr <- expect_silent(h_get_contrast(mod2, "AVISIT", "3"))
  colnames(ctr) <- names(coef(mod2))

  for (i in seq_len(nrow(ctr))) {
    expect_identical(
      names(ctr[i, ctr[i, ] != 0]),
      sprintf(
        c(
          "AVISITVIS%s",
          "AVISITVIS%s:RACEBlack or African American",
          "AVISITVIS%s:RACEWhite"
        ),
        i + 1
      )
    )
  }
})

test_that("h_get_contrast works even if only interaction term exists", {
  mod1 <- mmrm(
    formula = FEV1 ~ FEV1_BL:AVISIT - 1 + ar1(AVISIT | USUBJID),
    data = fev_data
  )
  ctr <- expect_silent(h_get_contrast(mod1, "FEV1_BL:AVISIT", "3"))
  colnames(ctr) <- names(coef(mod1))
  for (i in seq_len(nrow(ctr))) {
    expect_identical(
      names(ctr[i, ctr[i, ] != 0]),
      sprintf(c("FEV1_BL:AVISITVIS%s"), c("1", i + 1))
    )
  }

  mod2 <- mmrm(
    formula = FEV1 ~ AVISIT + AVISIT:RACE + FEV1_BL + us(AVISIT | USUBJID),
    data = fev_data
  )
  ctr2 <- expect_silent(h_get_contrast(mod2, "AVISIT", "3"))
  colnames(ctr2) <- names(coef(mod2))

  for (i in seq_len(nrow(ctr2))) {
    expect_identical(
      names(ctr2[i, ctr2[i, ] != 0]),
      sprintf(
        c(
          "AVISITVIS%s",
          "AVISITVIS1:RACEBlack or African American",
          "AVISITVIS%s:RACEBlack or African American",
          "AVISITVIS1:RACEWhite",
          "AVISITVIS%s:RACEWhite"
        ),
        i + 1
      )
    )
  }

  mod3 <- mmrm(
    formula = FEV1 ~ AVISIT + AVISIT:RACE + FEV1_BL - 1 + us(AVISIT | USUBJID),
    data = fev_data
  )
  ctr3 <- expect_silent(h_get_contrast(mod3, "AVISIT", "3"))
  colnames(ctr3) <- names(coef(mod3))

  for (i in seq_len(nrow(ctr3))) {
    expect_identical(
      names(ctr3[i, ctr3[i, ] != 0]),
      sprintf(
        c(
          "AVISITVIS1",
          "AVISITVIS%s",
          "AVISITVIS1:RACEBlack or African American",
          "AVISITVIS%s:RACEBlack or African American",
          "AVISITVIS1:RACEWhite",
          "AVISITVIS%s:RACEWhite"
        ),
        as.character(i + 1)
      )
    )
  }
})

test_that("h_get_contrast works if intercept is not given", {
  fit <- mmrm(
    FEV1 ~ AVISIT * ARMCD - 1 + ar1(AVISIT | USUBJID),
    data = fev_data
  )
  expect_silent(h_get_contrast(fit, "ARMCD", "2"))
  expect_silent(h_get_contrast(fit, "AVISIT", "2"))
  expect_silent(h_get_contrast(fit, "AVISIT:ARMCD", "2"))
})

test_that("h_get_contrast works for higher-order interaction", {
  mod <- mmrm(
    formula = FEV1 ~
      ARMCD +
      RACE +
      AVISIT +
      RACE * AVISIT * ARMCD +
      FEV1_BL +
      ar1(AVISIT | USUBJID),
    data = fev_data
  )
  ctr <- expect_silent(h_get_contrast(mod, "ARMCD:AVISIT", "3"))
  colnames(ctr) <- names(coef(mod))
  for (i in seq_len(nrow(ctr))) {
    expect_identical(
      names(ctr[i, ctr[i, ] != 0]),
      sprintf(
        c(
          "ARMCDTRT:AVISITVIS%s",
          "ARMCDTRT:RACEBlack or African American:AVISITVIS%s",
          "ARMCDTRT:RACEWhite:AVISITVIS%s"
        ),
        as.character(i + 1)
      )
    )
  }

  ctr <- expect_silent(h_get_contrast(mod, "RACE:AVISIT", "3"))
  colnames(ctr) <- names(coef(mod))
  for (i in seq_len(nrow(ctr))) {
    expect_identical(
      names(ctr[i, ctr[i, ] != 0]),
      sprintf(
        c(
          "RACE%s:AVISITVIS%s",
          "ARMCDTRT:RACE%s:AVISITVIS%s"
        ),
        c("Black or African American", "White")[(i - 1) %% 2 + 1],
        as.character(ceiling(i / 2) + 1)
      )
    )
  }
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

## Anova with character covariates

test_that("Anova works if covariate are character", {
  skip_if_not_installed("car")
  fev2 <- fev_data
  fev2$ARMCD <- as.character(fev2$ARMCD)
  fit <- mmrm(FEV1 ~ ARMCD * AVISIT + ar1(AVISIT | USUBJID), data = fev2)
  fit2 <- mmrm(FEV1 ~ ARMCD * AVISIT + ar1(AVISIT | USUBJID), data = fev_data)
  expect_identical(
    Anova(fit, "III"),
    Anova(fit2, "III")
  )
  expect_identical(
    Anova(fit, "II"),
    Anova(fit2, "II")
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
  fit <- mmrm(
    FEV1 ~ ARMCD * SEX + ARMCD * FEV1_BL - FEV1_BL + ar1(AVISIT | USUBJID),
    data = fev_data
  )
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
