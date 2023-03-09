fit <- mmrm(FEV1 ~ ARMCD + ar1(AVISIT|USUBJID), data = fev_data)

xwxi <- fit$beta_vcov
xi <- fit$tmb_data$x_matrix[1:2, ]
phi_i <- fit$cov[c(2, 4), c(2, 4)]
wi <- solve(phi_i)

bcov <- fit$beta_vcov

mp <- function(x, p = 1) {
  z <- eigen(x)
  z$vectors %*% diag(z$values^p) %*% t(z$vectors)
}

b1 <- function(wi, xi, bcov, p = -1) {
  i <- diag(rep(1, nrow(xi)))
  l <- chol(wi)
  t(l) %*% mp(i - l %*% xi %*% bcov %*% t(xi) %*% t(l), p) %*% l
}


b2 <- function(wi, xi, bcov, p = -1) {
  i <- diag(rep(1, nrow(xi)))
  wi %*% mp(i - xi %*% bcov %*% t(xi) %*% wi, p)
}

b1(wi, xi, bcov) - b2(wi, xi, bcov)

b1(wi, xi, bcov, -1/3) - b2(wi, xi, bcov, -1/3)
