v <- matrix(c(0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0), ncol = 3)

f <- function(theta) {
  sd <- exp(theta[1])
  rho <- plogis(theta[2])
  v <- sd * rho^v
  as.vector(v)
}

f2 <- function(theta) {
  as.vector(numDeriv::jacobian(f, theta))
}

aa <- numDeriv::jacobian(f, c(1, 1))

bb <- numDeriv::jacobian(f2, c(1, 1))
toString(rbind(matrix(aa[, 1], ncol = 3), matrix(aa[, 2], ncol = 3)))


toString(t(rbind(
  matrix(bb[1:9, 1], ncol = 3),
  matrix(bb[10:18, 1], ncol = 3),
  matrix(bb[1:9, 2], ncol = 3),
  matrix(bb[10:18, 2], ncol = 3)
)))
