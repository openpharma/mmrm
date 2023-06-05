# refit_multiple_optimizers works as expected with default arguments

    Code
      result
    Output
      mmrm fit
      
      Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
      Covariance:  unstructured (10 variance parameters)
      Inference:   REML
      Deviance:    3386.45
      
      Coefficients: 
                        (Intercept) RACEBlack or African American 
                        30.77740654                    1.53059492 
                          RACEWhite                     SEXFemale 
                         5.64356788                    0.32602732 
                           ARMCDTRT                    AVISITVIS2 
                         3.77441404                    4.83960389 
                         AVISITVIS3                    AVISITVIS4 
                        10.34216711                   15.05378632 
                ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
                        -0.04209006                   -0.69380693 
                ARMCDTRT:AVISITVIS4 
                         0.62412276 
      
      Model Inference Optimization:
      Converged with code 0 and message: 

