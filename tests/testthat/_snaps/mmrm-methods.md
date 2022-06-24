# h_print_call works as expected

    Formula:  FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:     fev_data

# h_print_aic_list works as expected

         AIC      BIC   logLik deviance
       234.2    234.2   -252.2 345235.2

# print.summary.mmrm works as expected

    mmrm fit

    Formula:  FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:     fev_data
    Used 537 observations from 197 subjects with maximum 4 timepoints.

    Model selection criteria:
         AIC      BIC   logLik deviance
      3406.4   3439.3  -1693.2   3386.4

    Coefficients:
                                  Estimate Std. Error  df t value Pr(>|t|)
    (Intercept)                         31          1 219      35   <2e-16 ***
    RACEBlack or African American        2          1 169       2     0.02 *
    RACEWhite                            6          1 157       8    2e-14 ***
    SEXFemale                         0.33       0.53 166       1     0.54
    ARMCDTRT                             4          1 146       4    6e-04 ***
    AVISITVIS2                           5          1 144       6    1e-08 ***
    AVISITVIS3                          10          1 156      13   <2e-16 ***
    AVISITVIS4                          15          1 138      12   <2e-16 ***
    ARMCDTRT:AVISITVIS2              -0.04          1 139       0     0.97
    ARMCDTRT:AVISITVIS3                 -1          1 158      -1     0.56
    ARMCDTRT:AVISITVIS4                  1          2 130       0     0.74
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Covariance estimate:
         VIS1 VIS2 VIS3 VIS4
    VIS1   41   14    5   13
    VIS2   14   27    3    7
    VIS3    5    3   15    1
    VIS4   13    7    1   96
