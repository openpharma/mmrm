library(TMB)

compile("autodiff.cpp")
dyn.load(dynlib("autodiff"))

## Data and parameters
data <- list()
## Make a function object
data$n_visits <- 3L
data$cov_type <- "ar1"
parameters <- list(theta = rep(1, 2))

obj <- MakeADFun(data, parameters, DLL = "autodiff", ADreport = TRUE, silent = TRUE)

result <- obj$report()

derivative_ar1 <- function(theta, visits, order = 1) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  sig <- exp(2 * theta1)
  rho <- theta2 / sqrt(1 + theta2 ^ 2)
  rr <- (1 + theta2 ^ 2)
  # r: number of parameters
  # visits: number of total visits
  # sig: sigma^2
  # rho: rho
  # order: order of derivative
  # provided the covariance structure, derive the first order derivatives
  # return a list of matrix
  dis <- abs(outer(seq_len(visits), seq_len(visits), "-"))
  if (order == 0) {
    return(list(rho^dis * sig))
  }
  if (order == 1) {
    psig <- rho ^ dis * sig * 2
    prho <- rho ^ (dis - 1) * dis * sig * rr ^ -1.5
    return(list(sig = psig, rho = prho))
  }
  if (order == 2) {
    sigsig <- rho ^ dis * sig * 4
    sigrho <- rho ^ (dis - 1) * dis * sig * rr ^ -1.5 * 2
    rhosig <- rho ^ (dis - 1) * dis * sig * rr ^ -1.5 * 2
    rhorho <- rho ^ (dis - 2) * dis * (dis - 1) * sig * rr ^ -3 + rho ^ (dis - 1) * dis * sig * -3 * theta2 * rr ^ -2.5
    return(list(sig = list(sig = sigsig, rho = sigrho), rho = list(sig = rhosig, rho = rhorho)))
  }
}

d1 <- derivative_ar1(parameters$theta, data$n_visits, order = 1)

result$sigma_d1

result$ld1[,,2] %*% t(result$l) * 2
