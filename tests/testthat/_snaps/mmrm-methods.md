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
      3446.0   3478.8  -1713.0   3426.0 
    
    Coefficients: 
                                  Estimate Std. Error     df t value Pr(>|t|)    
    (Intercept)                      30.34       0.91 222.00    33.5   <2e-16 ***
    RACEBlack or African American     1.91       0.61 180.00     3.1    0.002 ** 
    RACEWhite                         6.07       0.65 163.00     9.3   <2e-16 ***
    SEXFemale                         0.56       0.52 175.00     1.1    0.281    
    ARMCDTRT                          3.67       1.09 146.00     3.4    1e-03 ***
    AVISITVIS2                        4.86       0.83 144.00     5.8    4e-08 ***
    AVISITVIS3                       10.48       0.85 159.00    12.3   <2e-16 ***
    AVISITVIS4                       15.58       1.29 128.00    12.1   <2e-16 ***
    ARMCDTRT:AVISITVIS2              -0.03       1.15 140.00     0.0    0.977    
    ARMCDTRT:AVISITVIS3              -0.65       1.21 163.00    -0.5    0.596    
    ARMCDTRT:AVISITVIS4               0.02       1.82 120.00     0.0    0.990    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
          VIS1  VIS2 VIS3  VIS4
    VIS1 251.6  88.5 27.9  87.7
    VIS2  88.5 159.5 13.4  48.7
    VIS3  27.9  13.4 90.7   2.3
    VIS4  87.7  48.7  2.3 542.6
    

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
    (Intercept)     41.2        0.4  94.0     101   <2e-16 ***
    ARMCDTRT         3.5        0.6 147.0       6    2e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
    Group: PBO
          VIS1  VIS2 VIS3  VIS4
    VIS1 110.7  49.2 -7.0 -47.0
    VIS2  49.2  40.1 -2.7 -22.4
    VIS3  -7.0  -2.7 23.5  17.7
    VIS4 -47.0 -22.4 17.7 132.0
    Group: TRT
          VIS1 VIS2 VIS3  VIS4
    VIS1 106.8 42.6  2.8 -46.3
    VIS2  42.6 40.7  4.6  -4.8
    VIS3   2.8  4.6 26.0  20.5
    VIS4 -46.3 -4.8 20.5 172.9
    

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
      3859.1   3865.7  -1927.6   3855.1 
    
    Coefficients: 
                Estimate Std. Error    df t value Pr(>|t|)    
    (Intercept)     40.3        0.7 194.0      60   <2e-16 ***
    ARMCDTRT         4.2        1.0 188.0       4    2e-05 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Covariance estimate:
         0    1
    0 84.4 33.0
    1 33.0 84.4
    

