# h_print_call works as expected

    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 1 observations from 2 subjects with maximum 3 
    timepoints)

# h_print_call works as expected for weighted fits

    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 1 observations from 2 subjects with maximum 3 
    timepoints)
    Weights:     .mmrm_weights

# h_print_cov works as expected

    Covariance:  Toeplitz (3 variance parameters)

---

    Covariance:  Toeplitz (6 variance parameters of 2 groups)

# h_print_aic_list works as expected

         AIC      BIC   logLik deviance 
       234.2    234.2   -252.2 345235.2 

# print.summary.mmrm works as expected

    mmrm fit
    
    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method:      Satterthwaite
    Vcov Method: Asymptotic
    Inference:   REML
    
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
    

# print.summary.mmrm works as expected for weighted models

    mmrm fit
    
    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Weights:     .mmrm_weights
    Covariance:  unstructured (10 variance parameters)
    Method:      Satterthwaite
    Vcov Method: Asymptotic
    Inference:   REML
    
    Model selection criteria:
         AIC      BIC   logLik deviance 
      4778.8   4811.6  -2379.4   4758.8 
    
    Coefficients: 
                                  Estimate Std. Error    df t value Pr(>|t|)    
    (Intercept)                       31.0        4.1 357.0     7.6    3e-13 ***
    RACEBlack or African American      2.7        4.2 325.0     0.7    0.512    
    RACEWhite                          4.7        4.6 282.0     1.0    0.307    
    SEXFemale                          1.0        3.6 309.0     0.3    0.774    
    ARMCDTRT                           3.2        3.8 367.0     0.8    0.400    
    AVISITVIS2                         4.7        1.7 133.0     2.8    0.006 ** 
    AVISITVIS3                         9.9        1.9 130.0     5.2    7e-07 ***
    AVISITVIS4                        16.1        2.3 149.0     7.0    7e-11 ***
    ARMCDTRT:AVISITVIS2                0.1        2.3 127.0     0.1    0.957    
    ARMCDTRT:AVISITVIS3               -0.8        2.8 139.0    -0.3    0.785    
    ARMCDTRT:AVISITVIS4                0.5        3.3 152.0     0.1    0.891    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
            VIS1    VIS2    VIS3    VIS4
    VIS1  7630.9  8208.8  9170.8 10413.5
    VIS2  8208.8  9769.0 10462.2 11568.9
    VIS3  9170.8 10462.2 12463.4 13206.4
    VIS4 10413.5 11568.9 13206.4 15973.8
    

# print.summary.mmrm works as expected for rank deficient fits

    mmrm fit
    
    Formula:     FEV1 ~ RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        .mmrm_dat_rank_deficient (used 537 observations from 197 
    subjects with maximum 4 timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method:      Satterthwaite
    Vcov Method: Asymptotic
    Inference:   REML
    
    Model selection criteria:
         AIC      BIC   logLik deviance 
      3406.4   3439.3  -1693.2   3386.4 
    
    Coefficients: (1 not defined because of singularities)
                                  Estimate Std. Error     df t value Pr(>|t|)    
    (Intercept)                      30.78       0.89 219.00    34.7   <2e-16 ***
    RACEBlack or African American     1.53       0.62 169.00     2.5     0.02 *  
    RACEWhite                         5.64       0.67 157.00     8.5    2e-14 ***
    SEXFemale                         0.33       0.53 166.00     0.6     0.54    
    SEX2Female                          NA         NA     NA      NA       NA    
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
    

# print.summary.mmrm works as expected for grouped fits

    mmrm fit
    
    Formula:     FEV1 ~ ARMCD + us(AVISIT | ARMCD/USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (20 variance parameters of 2 groups)
    Method:      Satterthwaite
    Vcov Method: Asymptotic
    Inference:   REML
    
    Model selection criteria:
         AIC      BIC   logLik deviance 
      3702.7   3768.3  -1831.3   3662.7 
    
    Coefficients: 
                Estimate Std. Error    df t value Pr(>|t|)    
    (Intercept)     41.2        0.4  93.0     101   <2e-16 ***
    ARMCDTRT         3.5        0.6 147.0       6    2e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
    Group: PBO
          VIS1  VIS2 VIS3  VIS4
    VIS1 110.7  49.2 -7.0 -47.0
    VIS2  49.2  40.1 -2.7 -22.4
    VIS3  -7.0  -2.7 23.5  17.7
    VIS4 -47.0 -22.4 17.7 131.9
    Group: TRT
          VIS1 VIS2 VIS3  VIS4
    VIS1 106.8 42.6  2.7 -46.3
    VIS2  42.6 40.7  4.6  -4.7
    VIS3   2.7  4.6 26.0  20.5
    VIS4 -46.3 -4.7 20.5 172.8
    

# print.summary.mmrm works as expected for spatial fits

    mmrm fit
    
    Formula:     FEV1 ~ ARMCD + sp_exp(VISITN | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  spatial exponential (2 variance parameters)
    Method:      Satterthwaite
    Vcov Method: Asymptotic
    Inference:   REML
    
    Model selection criteria:
         AIC      BIC   logLik deviance 
      4571.4   4577.9  -2283.7   4567.4 
    
    Coefficients: 
                Estimate Std. Error  df t value Pr(>|t|)    
    (Intercept)       40          4 195     9.5   <2e-16 ***
    ARMCDTRT           4          6 195     0.7      0.5    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
           0      1
    0 1925.4 1881.5
    1 1881.5 1925.4
    

