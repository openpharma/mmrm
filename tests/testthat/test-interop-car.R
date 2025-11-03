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
  expect_snapshot_tolerance(
    Anova(get_mmrm(), type = "III", test.statistic = "Chisq")
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
    df2$`Res.Df`,
    c(199.582314485653, 196.41571750149, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F`,
    c(0.03099660681111, 11.5301174147263, 0.7644368094469),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Res.Df`,
    c(199.582314485653, 198.163532037936, 198.163532037944),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F`,
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
    df2$`Res.Df`,
    c(196.700026021882, 186.404412481085, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df2$`F`,
    c(0.03093826913613, 0.10489707559679, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`Res.Df`,
    c(197.423866403271, 186.119018537661, 186.11901853766, 194.942672335072),
    tolerance = 1e-4
  )
  expect_equal(
    df3$`F`,
    c(0.04875135180324, 0.09277653315012, 0.46643535342468, 6.11482167585228),
    tolerance = 1e-4
  )
})

test_that("Anova results are compatible with broom::tidy", {
  skip_if_not_installed("car")
  skip_if_not_installed("broom")
  fit <- mmrm(
    FEV1 ~ ARMCD * SEX + ARMCD * FEV1_BL - FEV1_BL + ar1(AVISIT | USUBJID),
    data = fev_data
  )
  df2 <- expect_silent(car::Anova(fit, "2"))
  df2_chisq <- expect_silent(car::Anova(fit, "2", test.statistic = "Chisq"))

  df3 <- expect_silent(car::Anova(fit, "3"))
  df3_chisq <- expect_silent(car::Anova(fit, "3", test.statistic = "Chisq"))

  tidy2 <- expect_silent(broom::tidy(df2))
  tidy2_chisq <- expect_silent(broom::tidy(df2_chisq))

  tidy3 <- expect_silent(broom::tidy(df3))
  tidy3_chisq <- expect_silent(broom::tidy(df3_chisq))

  expect_names(
    names(tidy2),
    identical.to = c("term", "df", "df.residual", "statistic", "p.value")
  )
  expect_names(
    names(tidy2_chisq),
    identical.to = c("term", "df", "statistic", "p.value")
  )

  expect_names(
    names(tidy3),
    identical.to = c("term", "df", "df.residual", "statistic", "p.value")
  )
  expect_names(
    names(tidy3_chisq),
    identical.to = c("term", "df", "statistic", "p.value")
  )
})

test_that("Anova Type 3 results are compatible with emmeans::joint_tests", {
  skip_if_not_installed("car")
  skip_if_not_installed("emmeans")

  # Uses the data from https://github.com/openpharma/mmrm/issues/502
  # fmt: skip
  dane_post_part <- data.frame(
    PatientId = structure(c(44L, 12L, 58L, 55L, 51L, 
  5L, 41L, 48L, 20L, 37L, 8L, 27L, 30L, 26L, 33L, 21L, 13L, 33L, 
  18L, 52L, 39L, 38L, 6L, 58L, 25L, 35L, 1L, 4L, 42L, 46L, 38L, 
  40L, 1L, 28L, 26L, 9L), levels = c("1", "2", "3", "4", "5", "6", 
  "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
  "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", 
  "29", "30", "101", "102", "103", "104", "105", "106", "107", 
  "108", "109", "110", "111", "112", "113", "114", "115", "116", 
  "117", "118", "119", "120", "121", "122", "123", "124", "125", 
  "126", "127", "128", "129", "130"), class = "factor"), Timepoint = structure(c(2L, 
  1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 
  2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
  1L, 1L, 1L), levels = c("Month 6", "Month 12"), class = "factor"), 
      Response = c(5.5, 6.5, 5, 7, 6, 3, 6, 7.5, 5.5, 4, 3, 4.5, 
      3.5, 3.5, 4.5, 6.5, 4, 4, 6.5, 5, 5, 5, 3, 5.5, 6.5, 6.5, 
      6, 4, 7.5, 6.5, 5, 3, 3, 5.5, 3.5, 3.5), Method = structure(c(1L, 
      2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 
      2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 
      1L, 2L, 2L, 2L, 2L), levels = c("A", "B"), class = "factor"), 
      Response_Bas = c(5.5, 8.5, 5.5, 7.5, 6.5, 7, 8, 7.5, 7.5, 
      8, 7.5, 6.5, 5.5, 5.5, 5, 8.5, 8.5, 5, 8.5, 5.5, 6.5, 5.5, 
      5.5, 5.5, 8.5, 7, 5.5, 11.5, 8, 6.5, 5.5, 5, 5.5, 7.5, 5.5, 
      12), CFB = c(0, -2, -0.5, -0.5, -0.5, -4, -2, 0, -2, -4, 
      -4.5, -2, -2, -2, -0.5, -2, -4.5, -1, -2, -0.5, -1.5, -0.5, 
      -2.5, 0, -2, -0.5, 0.5, -7.5, -0.5, 0, -0.5, -2, -2.5, -2, 
      -2, -8.5), Response_Bas_cent = c(-1.14166666666667, 1.85833333333333, 
      -1.14166666666667, 0.858333333333333, -0.141666666666667, 
      0.358333333333333, 1.35833333333333, 0.858333333333333, 0.858333333333333, 
      1.35833333333333, 0.858333333333333, -0.141666666666667, 
      -1.14166666666667, -1.14166666666667, -1.64166666666667, 
      1.85833333333333, 1.85833333333333, -1.64166666666667, 1.85833333333333, 
      -1.14166666666667, -0.141666666666667, -1.14166666666667, 
      -1.14166666666667, -1.14166666666667, 1.85833333333333, 0.358333333333333, 
      -1.14166666666667, 4.85833333333333, 1.35833333333333, -0.141666666666667, 
      -1.14166666666667, -1.64166666666667, -1.14166666666667, 
      0.858333333333333, -1.14166666666667, 5.35833333333333)
  )

  fit <- mmrm(
    Response ~ Method *
      Timepoint *
      Response_Bas_cent +
      us(Timepoint | PatientId),
    data = dane_post_part,
    method = "Kenward-Roger"
  )

  car_result <- car::Anova(fit, type = "3")
  emmeans_result <- emmeans::joint_tests(fit)

  expect_equal(car_result$F, emmeans_result$F.ratio, tolerance = 1e-4)
})
