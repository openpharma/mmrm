# h_df_bw_calc works as expected for the vignette example

    Code
      result$coefs_between_within
    Output
                        (Intercept) RACEBlack or African American 
                        "intercept"                     "between" 
                          RACEWhite                     SEXFemale 
                          "between"                     "between" 
                           ARMCDTRT                    AVISITVIS2 
                          "between"                      "within" 
                         AVISITVIS3                    AVISITVIS4 
                           "within"                      "within" 
                ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
                           "within"                      "within" 
                ARMCDTRT:AVISITVIS4 
                           "within" 

