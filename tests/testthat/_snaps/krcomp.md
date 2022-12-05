# h_get_kr_comp works as expected on grouped/ungrouped mmrm

    Code
      h_get_kr_comp(fit$tmb_data, fit$theta_est)
    Output
      $P
                [,1]      [,2]
      [1,] -8.695140 -4.184842
      [2,] -4.184842 -4.184842
      [3,] -3.299361 -1.646053
      [4,] -1.646053 -1.646053
      
      $Q
                [,1]     [,2]
      [1,] 17.390281 8.369683
      [2,]  8.369683 8.369683
      [3,]  6.598723 3.292106
      [4,]  3.292106 3.292106
      [5,]  6.598723 3.292106
      [6,]  3.292106 3.292106
      [7,]  3.134605 1.583003
      [8,]  1.583003 1.583003
      
      $R
                 [,1]      [,2]
      [1,] 17.3902809 8.3696833
      [2,]  8.3696833 8.3696833
      [3,]  6.5987227 3.2921062
      [4,]  3.2921062 3.2921062
      [5,]  6.5987227 3.2921062
      [6,]  3.2921062 3.2921062
      [7,]  0.5879173 0.3052184
      [8,]  0.3052184 0.3052184
      

---

    Code
      h_get_kr_comp(fit$tmb_data, fit$theta_est)
    Output
      $P
                 [,1]       [,2]
      [1,] -4.0627829 -1.9071032
      [2,] -1.9071032 -1.9071032
      [3,] -1.5472040 -0.7314360
      [4,] -0.7314360 -0.7314360
      [5,] -4.6422530 -2.2809325
      [6,] -2.2809325 -2.2809325
      [7,] -1.7581639 -0.9132362
      [8,] -0.9132362 -0.9132362
      
      $Q
                 [,1]      [,2]
       [1,] 8.1255658 3.8142063
       [2,] 3.8142063 3.8142063
       [3,] 3.0944080 1.4628720
       [4,] 1.4628720 1.4628720
       [5,] 3.0944080 1.4628720
       [6,] 1.4628720 1.4628720
       [7,] 1.5023637 0.7058882
       [8,] 0.7058882 0.7058882
       [9,] 9.2845061 4.5618651
      [10,] 4.5618651 4.5618651
      [11,] 3.5163279 1.8264724
      [12,] 1.8264724 1.8264724
      [13,] 3.5163279 1.8264724
      [14,] 1.8264724 1.8264724
      [15,] 1.6434002 0.8743055
      [16,] 0.8743055 0.8743055
      
      $R
                  [,1]       [,2]
       [1,] 8.12556579 3.81420631
       [2,] 3.81420631 3.81420631
       [3,] 3.09440804 1.46287195
       [4,] 1.46287195 1.46287195
       [5,] 3.09440804 1.46287195
       [6,] 1.46287195 1.46287195
       [7,] 0.37412102 0.24936420
       [8,] 0.24936420 0.24936420
       [9,] 9.28450608 4.56186508
      [10,] 4.56186508 4.56186508
      [11,] 3.51632787 1.82647238
      [12,] 1.82647238 1.82647238
      [13,] 3.51632787 1.82647238
      [14,] 1.82647238 1.82647238
      [15,] 0.22145917 0.06159161
      [16,] 0.06159161 0.06159161
      

# h_df_1d_kr works as expected in the standard case

    Code
      h_df_1d_kr(object_mmrm_kr, c(0, 1), TRUE)
    Output
      $est
      [1] 4.225743
      
      $se
      [1] 0.9585828
      
      $df
      [1] 188.4695
      
      $t_stat
      [1] 4.408323
      
      $p_val
      [1] 1.747171e-05
      

---

    Code
      h_df_1d_kr(object_mmrm_kr, c(1, 1), FALSE)
    Output
      $est
      [1] 44.51385
      
      $se
      [1] 0.6903402
      
      $df
      [1] 183.1095
      
      $t_stat
      [1] 64.48104
      
      $p_val
      [1] 8.004282e-128
      

# h_df_md_kr works as expected in the standard case

    Code
      h_df_md_kr(object_mmrm_kr, matrix(c(0, 1, 1, 0), nrow = 2), TRUE)
    Output
      $num_df
      [1] 2
      
      $num_df
      [1] 2
      
      $denom_df
      [1] 188.6484
      
      $f_stat
      [1] 3913.724
      
      $p_val
      [1] 2.576037e-154
      

---

    Code
      h_df_md_kr(object_mmrm_kr, matrix(c(0, -1, 1, 0), nrow = 2), FALSE)
    Output
      $num_df
      [1] 2
      
      $num_df
      [1] 2
      
      $denom_df
      [1] 188.6484
      
      $f_stat
      [1] 3913.724
      
      $p_val
      [1] 2.576037e-154
      

# h_kr_df works as expected in the standard case

    Code
      h_kr_df(v0 = object_mmrm_kr$beta_vcov, l = matrix(c(0, 1), nrow = 1), w = w, p = kr_comp$
        P)
    Output
      $m
      [1] 188.4695
      
      $lambda
      [1] 1
      

# h_var_adj works as expected in the standard case

    Code
      h_var_adj(v = object_mmrm_kr$beta_vcov, w = component(object_mmrm_kr,
        "theta_vcov"), p = object_mmrm_kr$kr_comp$P, q = object_mmrm_kr$kr_comp$Q, r = object_mmrm_kr$
        kr_comp$R, linear = TRUE)
    Output
                  (Intercept)   ARMCDTRT
      (Intercept)   0.4441196 -0.4441196
      ARMCDTRT     -0.4441196  0.9227150

---

    Code
      h_var_adj(v = object_mmrm_kr$beta_vcov, w = component(object_mmrm_kr,
        "theta_vcov"), p = object_mmrm_kr$kr_comp$P, q = object_mmrm_kr$kr_comp$Q, r = object_mmrm_kr$
        kr_comp$R, linear = FALSE)
    Output
                  (Intercept)   ARMCDTRT
      (Intercept)   0.4423115 -0.4423115
      ARMCDTRT     -0.4423115  0.9188811

