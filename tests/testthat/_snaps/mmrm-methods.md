# h_print_call works as expected

    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 1 observations from 2 subjects with maximum 3
    timepoints)

# h_print_cov works as expected

    Covariance:  heterogeneous Toeplitz (3 variance parameters)

# h_print_aic_list works as expected

         AIC      BIC   logLik deviance
       234.2    234.2   -252.2 345235.2

# print.summary.mmrm works as expected

    mmrm fit

    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4
    timepoints)
    Covariance:  unstructured (10 variance parameters)

    Model selection criteria:
         AIC      BIC   logLik deviance
      3406.4   3439.3  -1693.2   3386.4

    Coefficients:
                                  Estimate Std. Error     df t value Pr(>|t|)
    (Intercept)                      30.78       0.89 219.00    34.7   <2e-16 ***
    RACEBlack or African American     1.53       0.62 169.00     2.5     0.02 *
    RACEWhite                         5.64       0.67 157.00     8.5    2e-14 ***
    SEXFemale                         0.33       0.53 166.00     0.6     0.54
    ARMCDTRT                          3.77       1.07 146.00     3.5    6e-04 ***
    AVISITVIS2                        4.84       0.80 144.00     6.0    1e-08 ***
    AVISITVIS3                       10.34       0.82 156.00    12.6   <2e-16 ***
    AVISITVIS4                       15.05       1.31 138.00    11.5   <2e-16 ***
    ARMCDTRT:AVISITVIS2              -0.04       1.13 139.00     0.0     0.97
    ARMCDTRT:AVISITVIS3              -0.69       1.19 158.00    -0.6     0.56
    ARMCDTRT:AVISITVIS4               0.62       1.85 130.00     0.3     0.74
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Covariance estimate:
         VIS1 VIS2 VIS3 VIS4
    VIS1 40.6 14.4  5.0 13.4
    VIS2 14.4 26.6  2.8  7.5
    VIS3  5.0  2.8 14.9  0.9
    VIS4 13.4  7.5  0.9 95.6
