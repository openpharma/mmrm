# Author: Ben Bolker

library(TMB)
library(lme4)
library(glmmTMB)

## fit basic model
# Daniel: Note REML = FALSE here!
m1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)
glmmtmb1 <- glmmTMB(
  Reaction ~ Days + (Days | Subject),
  sleepstudy,
  REML = FALSE
)

## compile and load TMB DLL
# Daniel: Note that here we could also select the other AD framework
# (TMBad) instead of the default one (CppAD).
compile("TMB_mixed.cpp") ##  "-O0 -g" for debugging
dyn.load(dynlib("TMB_mixed"))

## extract X and Z from lmer fit (lots of other ways to do this;
## model.matrix() is fine for X, we could use lme4::lFormula() for
## Z-construction instead of going all the way through lmer() or glmmTMB()
## if we want to build fancy RE model matrices ourselves we need
## Matrix::fac2sparse() and KhatriRao (see vignette("lmer", package = "lme4")
## for details)

X <- getME(m1, "X")
Z <- getME(m1, "Z")

Xg <- getME(glmmtmb1, "X")
Zg <- getME(glmmtmb1, "Z")

all.equal(X, Xg, check.attributes = FALSE)
all.equal(Z, Zg)
# so
# glmmTMB uses dgTMatrix (stored as (possibly redundant) triplets) while
# lme4 uses dgCMatrix (compressed, sparse, column-oriented format)
all.equal(as.matrix(Z), as.matrix(Zg))

## construct data for TMB
tmbdat <- list(
  X = X,
  Z = Z,
  yobs = sleepstudy$Reaction,
  n_re = 2L # n = 2 random effects here: intercept and slope
)

# construct starting parameter values for TMB
tmbpars <- list(
  beta = rep(0, ncol(X)),
  b = rep(0, ncol(Z)),
  theta = rep(0, 3), ## 2 SD pars + 1 corr par ((n+1)*n/2)
  logsd = 0
)

## build TMB object
obj <- MakeADFun(
  data = tmbdat,
  parameters = tmbpars,
  random = "b", # integrate over the random effects
  DLL = "TMB_mixed", # We need to specify this because glmmTMB loads other DLLs.
  silent = TRUE ## FALSE for debugging etc.
)

## fit
tmbfit1 <- with(
  obj,
  nlminb(
    start = par,
    objective = fn,
    gradient = gr
  )
)

## checking glmmTMB vs lmer
## compare FE vcov
all.equal(as.matrix(vcov(m1)), vcov(glmmtmb1)$cond, tolerance = 1e-4)
## compare RE cov matrix
all.equal(c(VarCorr(m1)$Subject), c(VarCorr(glmmtmb1)$cond$Subject), tol = 1e-4)

## Comparing TMB fit with glmmTMB fit:
all.equal(
  unname(glmmtmb1$fit$par),
  ## reorder parameters, and double logsd
  ##  (glmmTMB fits on the log-variance rather than the log-sd scale)
  unname(c(
    tmbfit1$par[1:2],
    tmbfit1$par["logsd"] * 2,
    tmbfit1$par[3:5]
  )),
  tolerance = 1e-6
)

## try with BFGS
## ?? clearly suboptimal fit (5 log-likelihood units worse than tmbfit1 ...)
## would better starting values help??
## don't know what's going on here.

# Fortunately with L-BFGS-B it works the same as with nlminb:
tmbfit2 <- with(
  obj,
  optim(
    par = par,
    fn = fn,
    gr = gr,
    method = "L-BFGS-B" # Note that despite the name similarity the code is
    # completely different than BFGS, see ?optim.
  )
)
tmbfit2$convergence
tmbfit2$value
tmbfit1$objective
logLik(glmmtmb1)
