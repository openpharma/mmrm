# print.mmrm_tmb works as expected

    mmrm fit
    
    Formula:     FEV1 ~ RACE + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method:      REML
    Deviance:    3642.395
    
    Coefficients: 
                      (Intercept) RACEBlack or African American 
                        41.227284                      0.800180 
                        RACEWhite 
                         5.879103 
    
    Model Inference Optimization:
    Converged with code 0 and message: relative convergence (4)

---

    mmrm fit
    
    Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
    Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
    timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method:      REML
    Deviance:    3386.45
    
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
    Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch

# print.mmrm_tmb works as expected for rank deficient fits

    mmrm fit
    
    Formula:     FEV1 ~ SEX + SEX2 + us(AVISIT | USUBJID)
    Data:        .mmrm_tmb_dat_rank_deficient (used 537 observations from 197 
    subjects with maximum 4 timepoints)
    Covariance:  unstructured (10 variance parameters)
    Method:      REML
    Deviance:    3699.803
    
    Coefficients: (1 not defined because of singularities)
    (Intercept)   SEXFemale  SEX2Female 
     42.8057710   0.0451592          NA 
    
    Model Inference Optimization:
    Converged with code 0 and message: relative convergence (4)

