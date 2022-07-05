# print.mmrm_tmb works as expected

    mmrm fit
    
    Formula:     FEV1 ~ RACE + us(AVISIT | USUBJID)
    Data:        data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method: REML
    Deviance: 3642.395
    
    Coefficients:
                      (Intercept) RACEBlack or African American 
                        41.227284                      0.800180 
                        RACEWhite 
                         5.879103 
    
    Model Inference Optimization:
    Optimizer: BFGS
    Converged with code 0 and message: relative convergence (4)

---

    mmrm fit
    
    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method: REML
    Deviance: 3386.45
    
    Coefficients:
                      (Intercept) RACEBlack or African American 
                      30.77747548                    1.53049977 
                        RACEWhite                     SEXFemale 
                       5.64356535                    0.32606192 
                         ARMCDTRT                    AVISITVIS2 
                       3.77423004                    4.83958845 
                       AVISITVIS3                    AVISITVIS4 
                      10.34211288                   15.05389826 
              ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
                      -0.04192625                   -0.69368537 
              ARMCDTRT:AVISITVIS4 
                       0.62422703 
    
    Model Inference Optimization:
    Optimizer: BFGS
    Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch

# component works as expected

    $AIC
    [1] 3662.395
    
    $BIC
    [1] 3695.227
    
    $logLik
    [1] -1821.197
    
    $deviance
    [1] 3642.395
    
    $cov_type
    [1] "us"
    
    $n_theta
    [1] 10
    
    $n_subjects
    [1] 197
    
    $n_timepoints
    [1] 4
    
    $n_obs
    [1] 537
    
    $vcov
                                  (Intercept) RACEBlack or African American
    (Intercept)                     0.2361187                    -0.2361187
    RACEBlack or African American  -0.2361187                     0.4895745
    RACEWhite                      -0.2361187                     0.2361187
                                   RACEWhite
    (Intercept)                   -0.2361187
    RACEBlack or African American  0.2361187
    RACEWhite                      0.5674998
    
    $varcor
               VIS1        VIS2       VIS3      VIS4
    VIS1 109.755105  46.1794653 -3.6328809 -47.15173
    VIS2  46.179465  41.1774393  0.4916314 -14.88129
    VIS3  -3.632881   0.4916314 20.1079868  12.57760
    VIS4 -47.151726 -14.8812872 12.5775995 145.73248
    
    $formula
    [1] "FEV1 ~ RACE + us(AVISIT | USUBJID)"
    
    $dataset
    [1] "data"
    
    $reml
    [1] TRUE
    
    $method
    [1] "BFGS"
    
    $convergence
    [1] 0
    
    $evaluations
    function gradient 
          40       28 
    
    $conv_message
    [1] "relative convergence (4)"
    
    $call
    h_mmrm_tmb(formula = FEV1 ~ RACE + us(AVISIT | USUBJID), data = "data")
    

---

    $AIC
    [1] 3406.45
    
    $BIC
    [1] 3439.282
    
    $logLik
    [1] -1693.225
    
    $deviance
    [1] 3386.45
    
    $cov_type
    [1] "us"
    
    $n_theta
    [1] 10
    
    $n_subjects
    [1] 197
    
    $n_timepoints
    [1] 4
    
    $n_obs
    [1] 537
    
    $vcov
                                  (Intercept) RACEBlack or African American
    (Intercept)                     0.7859971                  -0.226212328
    RACEBlack or African American  -0.2262123                   0.389969478
    RACEWhite                      -0.1771113                   0.181466304
    SEXFemale                      -0.1684152                   0.031537926
    ARMCDTRT                       -0.5674809                   0.028374129
    AVISITVIS2                     -0.4227565                   0.002972514
    AVISITVIS3                     -0.5231223                   0.010825469
    AVISITVIS4                     -0.4406442                   0.002205681
    ARMCDTRT:AVISITVIS2             0.4225282                   0.005382569
    ARMCDTRT:AVISITVIS3             0.5218971                   0.011420575
    ARMCDTRT:AVISITVIS4             0.4489247                  -0.012589283
                                     RACEWhite    SEXFemale     ARMCDTRT
    (Intercept)                   -0.177111308 -0.168415217 -0.567480906
    RACEBlack or African American  0.181466304  0.031537926  0.028374129
    RACEWhite                      0.443035801  0.023364777 -0.042968995
    SEXFemale                      0.023364777  0.282971189  0.001814594
    ARMCDTRT                      -0.042968995  0.001814594  1.153791725
    AVISITVIS2                    -0.003149280  0.006471853  0.419528600
    AVISITVIS3                    -0.002952986  0.006771404  0.517277529
    AVISITVIS4                    -0.008230720  0.004088901  0.440653554
    ARMCDTRT:AVISITVIS2            0.013485683 -0.016801299 -0.845758354
    ARMCDTRT:AVISITVIS3            0.006720617 -0.024696304 -1.044355829
    ARMCDTRT:AVISITVIS4            0.002967665 -0.009038640 -0.877606881
                                    AVISITVIS2   AVISITVIS3   AVISITVIS4
    (Intercept)                   -0.422756455 -0.523122299 -0.440644229
    RACEBlack or African American  0.002972514  0.010825469  0.002205681
    RACEWhite                     -0.003149280 -0.002952986 -0.008230720
    SEXFemale                      0.006471853  0.006771404  0.004088901
    ARMCDTRT                       0.419528600  0.517277529  0.440653554
    AVISITVIS2                     0.642749706  0.399048940  0.368340113
    AVISITVIS3                     0.399048940  0.676823960  0.401800094
    AVISITVIS4                     0.368340113  0.401800094  1.723478787
    ARMCDTRT:AVISITVIS2           -0.643020114 -0.399203255 -0.368624024
    ARMCDTRT:AVISITVIS3           -0.399238901 -0.676484876 -0.401792995
    ARMCDTRT:AVISITVIS4           -0.368506585 -0.402167824 -1.723586879
                                  ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
    (Intercept)                           0.422528163         0.521897062
    RACEBlack or African American         0.005382569         0.011420575
    RACEWhite                             0.013485683         0.006720617
    SEXFemale                            -0.016801299        -0.024696304
    ARMCDTRT                             -0.845758354        -1.044355829
    AVISITVIS2                           -0.643020114        -0.399238901
    AVISITVIS3                           -0.399203255        -0.676484876
    AVISITVIS4                           -0.368624024        -0.401792995
    ARMCDTRT:AVISITVIS2                   1.275359305         0.805849821
    ARMCDTRT:AVISITVIS3                   0.805849821         1.410501907
    ARMCDTRT:AVISITVIS4                   0.728711516         0.796418986
                                  ARMCDTRT:AVISITVIS4
    (Intercept)                           0.448924745
    RACEBlack or African American        -0.012589283
    RACEWhite                             0.002967665
    SEXFemale                            -0.009038640
    ARMCDTRT                             -0.877606881
    AVISITVIS2                           -0.368506585
    AVISITVIS3                           -0.402167824
    AVISITVIS4                           -1.723586879
    ARMCDTRT:AVISITVIS2                   0.728711516
    ARMCDTRT:AVISITVIS3                   0.796418986
    ARMCDTRT:AVISITVIS4                   3.425654435
    
    $varcor
              VIS1      VIS2       VIS3       VIS4
    VIS1 40.553664 14.396045  4.9747288 13.3866534
    VIS2 14.396045 26.571483  2.7854661  7.4744790
    VIS3  4.974729  2.785466 14.8978517  0.9082111
    VIS4 13.386653  7.474479  0.9082111 95.5568420
    
    $formula
    [1] "FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)"
    
    $dataset
    [1] "fev_data"
    
    $reml
    [1] TRUE
    
    $method
    [1] "BFGS"
    
    $convergence
    [1] 0
    
    $evaluations
    function gradient 
          15       15 
    
    $conv_message
    [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
    
    $call
    h_mmrm_tmb(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | 
        USUBJID), data = "fev_data", reml = reml, start = start, 
        control = control)
    

