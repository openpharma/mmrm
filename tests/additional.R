# Additional tests for interop-car.R which don't work well with testthat::test_that

library(mmrm)
library(testthat)

# Type 3 tests are compatible with nlme::gls even if only interaction term exists

if (require("car", quietly = TRUE)) {
  fev_data_complete <- na.omit(fev_data)

  fev_data_complete_gls <- fev_data_complete
  contrasts(fev_data_complete_gls$AVISIT) <- contr.sum(length(levels(
    fev_data_complete_gls$AVISIT
  )))
  contrasts(fev_data_complete_gls$RACE) <- contr.sum(length(levels(
    fev_data_complete_gls$RACE
  )))
  contrasts(fev_data_complete_gls$ARMCD) <- contr.sum(length(levels(
    fev_data_complete_gls$ARMCD
  )))

  mod1 <- mmrm(
    formula = FEV1 ~ FEV1_BL:AVISIT - 1 + ar1(AVISIT | USUBJID),
    data = fev_data_complete
  )
  result1 <- car::Anova(mod1, type = "3", test.statistic = "Chisq")

  mod1_gls <- nlme::gls(
    FEV1 ~ FEV1_BL:AVISIT - 1,
    correlation = nlme::corAR1(form = ~ as.numeric(AVISIT) | USUBJID),
    data = fev_data_complete_gls,
    method = "REML"
  )
  result1_gls <- car::Anova(mod1_gls, type = "3")

  expect_equal(result1$Chisq, result1_gls$Chisq, tolerance = 1e-6)
  expect_equal(result1$Df, result1_gls$Df)

  mod2 <- mmrm(
    formula = FEV1 ~ AVISIT + AVISIT:RACE + FEV1_BL + us(AVISIT | USUBJID),
    data = fev_data
  )
  result2 <- car::Anova(mod2, type = "3", test.statistic = "Chisq")

  mod2_gls <- nlme::gls(
    FEV1 ~ AVISIT + AVISIT:RACE + FEV1_BL,
    correlation = nlme::corSymm(form = ~ as.numeric(AVISIT) | USUBJID),
    weights = nlme::varIdent(form = ~ 1 | as.numeric(AVISIT)),
    data = fev_data_complete_gls,
    method = "REML"
  )
  result2_gls <- car::Anova(mod2_gls, type = "3")

  # We don't get the same fit here therefore results are a little bit different.
  expect_equal(
    as.numeric(logLik(mod2)),
    as.numeric(logLik(mod2_gls)),
    tolerance = 1e-2
  )
  expect_equal(result2$Chisq, result2_gls$Chisq[-1], tolerance = 1e-3)

  # Check simplest example in more detail and compare with nlme and lme4 result.
  mod3 <- mmrm(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )
  result3 <- car::Anova(mod3, type = "3", test.statistic = "Chisq")

  mod3_gls <- nlme::gls(
    FEV1 ~ RACE + SEX + ARMCD * AVISIT,
    correlation = nlme::corSymm(form = ~ as.numeric(AVISIT) | USUBJID),
    weights = nlme::varIdent(form = ~ 1 | as.numeric(AVISIT)),
    data = fev_data_complete_gls,
    method = "REML"
  )
  result3_gls <- car::Anova(mod3_gls, type = "3")

  expect_equal(
    as.numeric(logLik(mod3)),
    as.numeric(logLik(mod3_gls)),
    tolerance = 1e-2
  )
  expect_equal(result3$Chisq, result3_gls$Chisq[-1], tolerance = 1e-4)

  if (require("lmerTest", quietly = TRUE)) {
    mod3_lmer_kr <- lmerTest::lmer(
      FEV1 ~ RACE + SEX + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
      data = fev_data_complete_gls,
      control = lme4::lmerControl(
        check.nobs.vs.nRE = "ignore",
        check.conv.grad = list(action = "warning", tol = 2e-2, relTol = NULL),
      )
    )
    mod3_kr <- mmrm(
      FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
      data = fev_data,
      method = "Kenward-Roger"
    )
    expect_equal(
      as.numeric(logLik(mod3_lmer_kr)),
      as.numeric(logLik(mod3_kr)),
      tolerance = 1e-2
    )
    result3_kr <- car::Anova(mod3_kr, type = "3", test.statistic = "F")
    result3_lmer_kr <- car::Anova(
      mod3_lmer_kr,
      type = "3",
      test.statistic = "F"
    )
    expect_equal(
      result3_kr$F,
      result3_lmer_kr$F[-1],
      tolerance = 1e-1
    )
    expect_equal(
      result3_kr$Res.Df,
      result3_lmer_kr$Df.res[-1],
      tolerance = 1e-1
    )
  }
}

# Type 3 tests are compatible with nlme::gls for higher-order interaction

if (require("car", quietly = TRUE)) {
  fev_data_complete <- na.omit(fev_data)

  fev_data_complete_gls <- fev_data_complete
  contrasts(fev_data_complete_gls$AVISIT) <- contr.sum(length(levels(
    fev_data_complete_gls$AVISIT
  )))
  contrasts(fev_data_complete_gls$RACE) <- contr.sum(length(levels(
    fev_data_complete_gls$RACE
  )))
  contrasts(fev_data_complete_gls$ARMCD) <- contr.sum(length(levels(
    fev_data_complete_gls$ARMCD
  )))

  mod <- mmrm(
    formula = FEV1 ~
      ARMCD +
      RACE +
      AVISIT +
      RACE * AVISIT * ARMCD +
      FEV1_BL +
      ar1(AVISIT | USUBJID),
    data = fev_data_complete
  )
  result <- car::Anova(mod, type = "3", test.statistic = "Chisq")

  mod_gls <- nlme::gls(
    FEV1 ~
      ARMCD +
      RACE +
      AVISIT +
      RACE * AVISIT * ARMCD +
      FEV1_BL,
    correlation = nlme::corAR1(form = ~ as.numeric(AVISIT) | USUBJID),
    data = fev_data_complete_gls,
    method = "REML"
  )
  result_gls <- car::Anova(mod_gls, type = "3")

  expect_equal(
    as.numeric(logLik(mod)),
    as.numeric(logLik(mod_gls)),
    tolerance = 1e-1
  )
  expect_equal(result$Chisq, result_gls$Chisq[-1], tolerance = 1e-5)
}
