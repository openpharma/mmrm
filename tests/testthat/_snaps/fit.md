# mmrm works for custom optimizer

    Code
      print(fit)
    Output
      mmrm fit
      
      Formula:     FEV1 ~ ARMCD + ar1(AVISIT | SEX/USUBJID)
      Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
      timepoints)
      Covariance:  auto-regressive order one (4 variance parameters of 2 groups)
      Method:      REML
      Deviance:    4915.743
      
      Coefficients: 
      (Intercept)    ARMCDTRT 
        39.589438    5.106232 
      
      Model Inference Optimization:
      Converged with code 0 and message: this is wrong

# mmrm works for constructed control

    Code
      print(fit)
    Output
      mmrm fit
      
      Formula:     FEV1 ~ ARMCD + ar1(AVISIT | SEX/USUBJID)
      Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
      timepoints)
      Covariance:  auto-regressive order one (4 variance parameters of 2 groups)
      Method:      REML
      Deviance:    3855.007
      
      Coefficients: 
      (Intercept)    ARMCDTRT 
        40.290948    4.227962 
      
      Model Inference Optimization:
      Converged with code 0 and message: 

