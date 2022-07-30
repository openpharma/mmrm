# Homogeneous Ante-dependence covariance structure ----
# Note: this covariance structure is not available in SAS, so use mmrm to produce test results
formula <- FEV1 ~ ad(AVISIT | USUBJID)

## ML ----
result <- mmrm(formula, fev_data, reml = FALSE)

sink(file = "mmrm_ad_ml.txt")
result
summary(result)
cat("result$theta_est \n")
result$theta_est
cat("\n deviance(result) \n")
print(deviance(result), 8)
sink(file = NULL)

## REML ----
result <- mmrm(formula, fev_data, reml = TRUE)

sink(file = "mmrm_ad_reml.txt")
result
summary(result)
cat("result$theta_est \n")
result$theta_est
cat("\n deviance(result) \n")
print(deviance(result), 8)
sink(file = NULL)
