# Model-Robust Variance Estimator for G-Computation

## Background

Suppose we have data from a randomized clinical trial comparing
treatment groups. After fitting a particular MMRM, one must then
commonly answer the question: what is the mean value of the outcome
variable in each treatment group?

One method to answer this question is to use G-computation to estimate
the marginal means of each treatment group. In this section, we describe
the G-computation estimator in the context of the MMRM and a
corresponding approach to variance estimation.

## G-Computation Estimator

There are \\J\\ treatment groups, \\m\\ planned timepoints, and \\n\\
total subjects. Define \\ x\_{tji} = X\_{ti}(j) \in \mathbb{R}^{1 \times
p}, \\ as the row of the design matrix corresponding to subject \\i\\,
evaluated at timepoint \\t\\ if the subject had received treatment
\\j\\. In practice, this means that the design matrix columns
corresponding to the treatment variable are set to the appropriate
values for treatment \\j\\ - even when the subject actually received a
different treatment in the trial.

The G-computation estimator of the mean outcome at time \\t\\ under
treatment \\j\\ is \\ \hat{\theta}\_{tj} = \frac{1}{n}\sum\_{i=1}^n
x\_{tji}\\\hat{\beta}, \qquad j = 1,\dotsc,J. \\ Collect these into the
vector \\\hat{\theta}\_t =
(\hat{\theta}\_{t1},\dotsc,\hat{\theta}\_{tJ})^\top \in \mathbb{R}^J\\.
Treatment effects can then be defined by linear contrasts or other
functions of \\\hat{\theta}\_t\\.

## Covariance Estimator of \\\hat{\theta}\_t\\

The covariance matrix of \\\hat{\theta}\_t\\ can be decomposed using the
law of total variance as: \\ \text{Var}(\hat{\theta}\_t) = \text{E}\[
\text{Var}(\hat{\theta}\_t~\|~X) \] +
\text{Var}\[\text{E}(\hat{\theta}\_t~\|~ X)\] \\

Many variance estimators focus only on the first term. To enable a more
general correct variance estimator that is robust to model
misspecification, we need to add back the second term, which is related
to the randomness of baseline covariates used in the design matrix.

To account for the extra source of variation from treating baseline
covariates as random, define the vector of subject-level contributions
to the G-computation estimator at time \\t\\, \\ \hat{\mathbf{v}}\_i =
\begin{pmatrix} x\_{t1i}\\\hat\beta \\ \vdots \\ x\_{tJi}\\\hat\beta
\end{pmatrix} \in \mathbb{R}^J, \qquad i = 1,\ldots,n, \\ with sample
mean \\\bar{\mathbf{v}} = n^{-1}\sum\_{i=1}^n \hat{\mathbf{v}}\_i\\ and
sample covariance \\ \hat{\Sigma}\_v = \frac{1}{n-1}\sum\_{i=1}^n
\left(\hat{\mathbf{v}}\_i - \bar{\mathbf{v}}\right)
\left(\hat{\mathbf{v}}\_i - \bar{\mathbf{v}}\right)^\top. \\

Then if \\\hat{\Omega}\_{\theta}\\ is the covariance estimator derived
from the model while treating baseline variables as fixed (i.e. an
estimate of the first term above), the overall covariance estimator that
is robust to model misspecification is \\ \hat{\Omega}\_c =
\hat{\Omega}\_{\theta} + \frac{1}{n} \hat{\Sigma}\_v. \\

## Implementation

The G-computation estimator and corresponding covariance can be
implemented using the `emmeans` package. To enable correct variance
estimation, there is an argument `emmeans_gcomp_vars` in
[`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md)
that captures which variables can be treated as fixed or non-random. For
example, since every subject is predicted from the model at the same
time point and under each treatment, a model including `TRTP` and
`AVISIT` could include `emmeans_gcomp_vars = c("TRTP", "AVISIT")`.

Using `emmeans` with option `weights = "proportional"` and passing in
the `data` with rows for every subject and timepoint can produce
\\\hat{\theta}\_{tj}\\.

Using

``` r

emmeans(fit, ~TRTP | AVISIT, weights = "proportional", data = data)
```

the `emmeans` function will create a linear combination matrix
\\L^{\text{global}}\\. For example, the row \\k=(t-1)J + j\\ of this
matrix is \\ L^{\text{global}}\_k = \frac{1}{n} \sum\_{i=1}^n
x\_{tji}^{\top} \\ and the G-computation estimate is \\L^{\text{global}}
\hat{\beta}\\. Using the model-trusting variance, `emmeans` calculates
the coefficients covariance estimate \\\hat{V}\\ using
[`vcov()`](https://rdrr.io/r/stats/vcov.html) and subsequently
calculates \\ L^{\text{global}} \hat{V} L^{\text{global}\top} \\ as the
model-trusting covariance of the G-computation estimator.

To work with `emmeans` functionality, we create a model-robust
coefficients covariance estimate \\\hat{V}\_c\\ such that \\
L^{\text{global}} \hat{V}\_c L^{\text{global}\top} = \hat{\Omega}\_c =
L^{\text{global}} \hat{V} L^{\text{global}\top} + S. \\ where \\S\\ is a
block-diagonal matrix corresponding to the relevant empirical covariance
\\\frac{1}{n} \hat{\Sigma}\_v\\ at each time point.

This turns out to involve solving a linear system \\ \hat{V}\_c =
\hat{V} + L^{\text{global}\top} (L^{\text{global}}
L^{\text{global}\top})^{-} S (L^{\text{global}}
L^{\text{global}\top})^{-} L^{\text{global}}, \\ where \\Z^-\\
represents the Moore-Penrose inverse of \\Z\\.
