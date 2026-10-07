# Satterthwaite

Here we describe the details of the Satterthwaite degrees of freedom
calculations.

### Satterthwaite degrees of freedom for asymptotic covariance

In Christensen (2018) the Satterthwaite degrees of freedom approximation
based on normal models is well detailed and the computational approach
for models fitted with the `lme4` package is explained. We follow the
algorithm and explain the implementation in this `mmrm` package. The
model definition is the same as in [Details of the model fitting in
`mmrm`](https://openpharma.github.io/mmrm/articles/algorithm.md).

We are also using the same notation as in the [Details of the
Kenward-Roger
calculations](https://openpharma.github.io/mmrm/articles/kenward.md). In
particular, we assume we have a full row rank contrast matrix \\C \in
\mathbb{R}^{c\times p}\\ with which we want to test the linear
hypothesis \\C\beta = 0\\. Further, \\W(\hat\theta)\\ is the covariance
estimate of \\\hat\theta\\: the inverse observed information, obtained
by inverting the Hessian of the negative log-likelihood evaluated at
\\\hat\theta\\ (the restricted likelihood for REML fits). \\\Phi(\theta)
= \left\\X^\top \Omega(\theta)^{-1} X\right\\ ^{-1}\\ is the asymptotic
covariance matrix of \\\hat\beta\\, and \\k\\ is the number of
covariance parameters in \\\theta\\.

#### One-dimensional contrast

We start with the case of a one-dimensional contrast, i.e. \\c = 1\\.
The Satterthwaite adjusted degrees of freedom for the corresponding
t-test are then defined as: \\ \hat\nu(\hat\theta) =
\frac{2f(\hat\theta)^2}{f{'}(\hat\theta)^\top W(\hat\theta)
f{'}(\hat\theta)} \\ where \\f(\hat\theta) = C \Phi(\hat\theta) C^\top\\
is the scalar in the numerator and we can identify it as the variance
estimate for the estimated scalar contrast \\C\hat\beta\\. The
computational challenge is essentially to evaluate the denominator in
the expression for \\\hat\nu(\hat\theta)\\, which amounts to computing
the \\k\\-dimensional gradient \\f{'}(\hat\theta)\\ of \\f(\theta)\\
(for the given contrast matrix \\C\\) at the estimate \\\hat\theta\\. We
already have the variance-covariance matrix \\W(\hat\theta)\\ of the
variance parameter vector \\\theta\\ from the model fitting.

##### Jacobian approach

However, if we proceeded in a naive way here, we would need to recompute
the denominator again for every chosen \\C\\. This would be slow, e.g.
when changing \\C\\ every time we want to test a single coefficient
within \\\beta\\. It is better to instead evaluate the gradient of the
matrix valued function \\\Phi(\theta)\\, which is therefore the
Jacobian, with regards to \\\theta\\, \\\mathcal{J}(\theta) =
\nabla\_\theta \Phi(\theta)\\. Imagine \\\mathcal{J}(\theta)\\ as the
3-dimensional array with \\k\\ faces of size \\p\times p\\. Left and
right multiplying each face by \\C\\ and \\C^\top\\ respectively leads
to the \\k\\-dimensional gradient \\f'(\theta) = C \mathcal{J}(\theta)
C^\top\\. Therefore for each new contrast \\C\\ we just need to perform
simple matrix multiplications, which is fast (see
[`h_gradient()`](https://openpharma.github.io/mmrm/reference/h_gradient.md)
where this is implemented). Thus, having computed the estimated Jacobian
\\\mathcal{J}(\hat\theta)\\, it is only a matter of putting the
different quantities together to compute the estimate of the denominator
degrees of freedom, \\\hat\nu(\hat\theta)\\.

##### Jacobian calculation

Currently, we evaluate the gradient of \\\Phi(\theta)\\ through function
[`h_jac_list()`](https://openpharma.github.io/mmrm/reference/h_jac_list.md).
It uses automatic differentiation provided in `TMB`.

We first obtain the Jacobian of the inverse of the covariance matrix of
coefficients (\\\Phi(\theta)^{-1}\\), following the [Kenward-Roger
calculations](https://openpharma.github.io/mmrm/articles/kenward.html#special-considerations-for-mmrm-models).
Please note that we only need \\P_h\\ matrices.

Then, to obtain the Jacobian of the covariance matrix of coefficients,
following the
[algorithm](https://openpharma.github.io/mmrm/articles/kenward.html#derivative-of-the-sigma-1),
we use \\\Phi(\theta)\\ estimated in the fit to obtain the Jacobian.

The result is a list (of length \\k\\ where \\k\\ is the dimension of
the variance parameter \\\theta\\) of matrices of \\p \times p\\, where
\\p\\ is the dimension of \\\beta\\.

Because only the \\P_h\\ matrices are needed, the Satterthwaite
implementation initializes a first-derivative-only cache: for
non-spatial covariance structures, it differentiates the Cholesky factor
once and caches the covariance and inverse covariance first derivatives
for each observed-visit pattern. It does not run nested automatic
differentiation or allocate second-derivative caches. Spatial covariance
first derivatives are evaluated analytically on demand. Neither
\\Q\_{hj}\\ nor \\R\_{hj}\\ is constructed for the Satterthwaite
Jacobian; the gradient and degrees-of-freedom formulas remain unchanged.

##### Connection to the scalar Kenward-Roger shortcut

For a REML fit, the [Kenward-Roger
implementation](https://openpharma.github.io/mmrm/articles/kenward.html#one-dimensional-shortcut)
can obtain the same scalar degrees of freedom directly from its cached
\\P_h\\ matrices. Write \\C = l^\top\\, with \\l \in \mathbb{R}^p\\, and
evaluate all quantities at \\\hat\theta\\, omitting this argument below.
The Jacobian identity

\\ \frac{\partial\Phi}{\partial\theta_h} = -\Phi P_h\Phi \\

implies \\f'\_h = -l^\top\Phi P_h\Phi l\\. Define \\a_h = -f'\_h/f\\.
The Satterthwaite formula therefore becomes

\\ \hat\nu = \frac{2}{a^\top Wa}. \\

For one-dimensional KR, \\A_1 = A_2 = a^\top Wa\\ and the F scale is
exactly \\\lambda = 1\\.
[`h_kr_df()`](https://openpharma.github.io/mmrm/reference/h_kr_df.md)
uses this scalar shortcut for both full and linear KR, while
Satterthwaite continues to use its cached Jacobian through
[`h_gradient()`](https://openpharma.github.io/mmrm/reference/h_gradient.md).
The equality uses the **unadjusted** covariance \\\Phi\\ and the same
\\W\\; KR standard errors still use the adjusted covariance \\\Phi_A\\,
so equal scalar degrees of freedom do not imply equal test statistics,
p-values, or confidence intervals. For multiple contrasts, KR uses a
normalized contrast-space contraction of its moment quantities; the
Satterthwaite eigen-decomposition described below remains unchanged.

#### Multi-dimensional contrast

When \\c \> 1\\ we are testing multiple contrasts at once. Here an
F-statistic \\ F = \frac{1}{c} (C\hat\beta)^\top (C \Phi(\hat\theta)
C^\top)^{-1} (C\hat\beta) \\ is calculated, and we are interested in
estimating an appropriate denominator degrees of freedom for \\F\\,
while assuming \\c\\ are the numerator degrees of freedom. Note that
only in special cases, such as orthogonal or balanced designs, the F
distribution will be exact under the null hypothesis. In general, it is
an approximation.

The calculations are described in detail in Christensen (2018), and we
don’t repeat them here in detail. The implementation is in
[`h_df_md_sat()`](https://openpharma.github.io/mmrm/reference/h_df_md_sat.md)
and starts with an eigen-decomposition of the asymptotic
variance-covariance matrix of the contrast estimate, i.e. \\C
\Phi(\hat\theta) C^\top\\. This rewrites \\cF\\ as a sum of squared
standardized contrasts. Under the null hypothesis, each standardized
contrast is approximated by a \\t\_{\nu_a}\\ distribution, and its
square by an \\F\_{1,\nu_a}\\ distribution, where \\\nu_a\\ is
calculated using the one-dimensional Satterthwaite formula, for \\a = 1,
\dotsc, c\\. The eigen-decomposition diagonalizes the estimated contrast
covariance; it does not guarantee independence of the studentized
statistics, whose denominators are estimated from the data. When the
component degrees of freedom exceed two, matching the approximate
expectation of the sum to that of \\cF\_{c,\nu}\\ gives the overall
denominator degrees of freedom. This expectation calculation uses
linearity of expectation and does not require independence. Numerically
equal component degrees of freedom are returned directly. For unequal
components, the implementation returns two denominator degrees of
freedom if any component has at most two.

### Satterthwaite degrees of freedom for empirical covariance

In Bell and McCaffrey (2002) the Satterthwaite degrees of freedom in
combination with a sandwich covariance matrix estimator are described.

#### One-dimensional contrast

For one-dimensional contrast, following the same notation in [Details of
the model fitting in
`mmrm`](https://openpharma.github.io/mmrm/articles/algorithm.md) and
[Details of the Kenward-Roger
calculations](https://openpharma.github.io/mmrm/articles/kenward.md), we
have the following derivation. Let \\C = l^\top\\ be the contrast, with
a column vector \\l \in \mathbb{R}^p\\. First consider ordinary least
squares. Distinguish the model errors \\\epsilon = Y - X\beta\\ from the
fitted residuals \\e = Y - X\hat\beta = (I-H)\epsilon\\, where \\ H =
X(X^\top X)^{-1}X^\top. \\ Write \\e_i\\ for the residuals of subject
\\i\\. The sandwich estimator of the variance of \\l^\top\hat\beta\\ is

\\ v = s l^\top(X^\top X)^{-1}\sum\_{i}{X_i^\top A_i e_i e_i^\top A_i
X_i} (X^\top X)^{-1} l \\

where \\s\\ takes the value of \\\frac{n}{n-1}\\, \\1\\ or
\\\frac{n-1}{n}\\, and \\A_i\\ takes \\I_i\\, \\(I_i -
H\_{ii})^{-\frac{1}{2}}\\, or \\(I_i - H\_{ii})^{-1}\\ respectively (as
in the [empirical covariance with weighted least
squares](https://openpharma.github.io/mmrm/articles/empirical_wls.md)).
Here \\I_i\\ is the \\m_i\times m_i\\ identity and \\H\_{ii}\\ is the
subject’s diagonal block of \\H\\. Under a working normal model for
\\\epsilon\\, with the design and adjustment matrices treated as fixed,
\\v\\ has the distribution of a weighted sum of independent \\\chi_1^2\\
variables. The weights are the eigenvalues of the \\n\times n\\ matrix
\\\Gamma\\ with elements \\ \Gamma\_{ij} = \gamma_i^\top V \gamma_j \\

where

\\ \gamma_i = s^{\frac{1}{2}} (I - H)\_i^\top A_i X_i (X^\top X)^{-1} l
\\

\\(I - H)\_i\\ corresponds to the rows of subject \\i\\, so that
\\\gamma_i \in \mathbb{R}^N\\ with \\N = \sum_i m_i\\ observations in
total. \\V = \operatorname{Var}(\epsilon) \in \mathbb{R}^{N \times N}\\
is the working covariance matrix of the model errors. The fitted
residuals have covariance \\(I-H)V(I-H)^\top\\; their projection is
already included in \\\gamma_i\\. In particular, \\v =
\sum_i(\gamma_i^\top\epsilon)^2\\.

So the degrees of freedom can be represented as \\ \nu =
\frac{(\sum\_{i}\omega_i)^2}{\sum\_{i}{\omega_i^2}} \\

where \\\omega_i, i = 1, \dotsc, n\\ are the eigenvalues of \\\Gamma\\.
Bell and McCaffrey (2002) also suggests that \\V\\ can be chosen as
identity matrix, so \\\Gamma\_{ij} = \gamma_i^\top \gamma_j\\.

For generalized least squares, apply these equations to the transformed
response and design from the [weighted least squares
estimator](https://openpharma.github.io/mmrm/articles/algorithm.html#weighted-least-squares-estimator).
Specifically, factor \\\hat\Omega = L\_\Omega L\_\Omega^\top\\ and use
\\Y^\dagger = L\_\Omega^{-1}Y\\, \\X^\dagger = L\_\Omega^{-1}X\\, and
\\\epsilon^\dagger = L\_\Omega^{-1}\epsilon\\. The hat matrix and fitted
residuals are then computed in these transformed coordinates. With the
fitted covariance as the working model, the transformed errors have
working covariance \\V = I\\; the transformation is treated as fixed for
this approximation. Below, \\X\\ and \\H\\ refer to these transformed
coordinates when applying the formulas to `mmrm`.

To avoid repeated computation of matrix \\A_i\\, \\H\\ etc for different
contrasts, we calculate and cache the following

\\ \Gamma^\ast_i = (I - H)\_i^\top A_i X_i (X^\top X)^{-1} \\ which is
an \\N \times p\\ matrix. With different contrasts, we need only
calculate the following \\ \gamma_i = s^{\frac{1}{2}} \Gamma^\ast_i l \\
to obtain an \\N \times 1\\ matrix, and \\\Gamma\\ can be computed with
the \\\gamma_i\\.

To obtain the degrees of freedom, and to avoid eigen computation on a
large matrix, we can use the following equation

\\ \nu = \frac{(\sum\_{i}\omega_i)^2}{\sum\_{i}{\omega_i^2}} =
\frac{\operatorname{tr}(\Gamma)^2}{\sum\_{i}{\sum\_{j}{\Gamma\_{ij}^2}}}
\\

The common factor \\s\\ cancels from this degrees-of-freedom ratio, so
it need not be included in the calculation. The trace identities used
here are proved in the [appendix](#appendix-trace-identities).

#### Multi-dimensional contrast

For multiple contrasts, we apply the same eigen-decomposition and
expectation-matching technique as for asymptotic covariance, using the
empirical covariance matrix and the empirical scalar degrees of freedom
for each component.

### Appendix: Trace identities

#### Cyclic invariance of trace

We first show \\ \operatorname{tr}(AB) = \operatorname{tr}(BA) \\

Let \\A\\ have dimension \\r\times q\\, \\B\\ have dimension \\q\times
r\\ \\ \operatorname{tr}(AB) = \sum\_{i=1}^{r}{(AB)\_{ii}} =
\sum\_{i=1}^{r}{\sum\_{j=1}^{q}{A\_{ij}B\_{ji}}} \\

\\ \operatorname{tr}(BA) = \sum\_{i=1}^{q}{(BA)\_{ii}} =
\sum\_{i=1}^{q}{\sum\_{j=1}^{r}{B\_{ij}A\_{ji}}} \\

so \\\operatorname{tr}(AB) = \operatorname{tr}(BA)\\

#### Trace and squared eigenvalues

We next show \\ \operatorname{tr}(\Gamma) = \sum\_{i}(\omega_i) \\ and
\\ \sum\_{i}(\omega_i^2) = \sum\_{i}{\sum\_{j}{\Gamma\_{ij}^2}} \\ if
\\\Gamma = \Gamma^\top\\

Following eigen decomposition, we have \\ \Gamma = U
\operatorname{diag}(\omega) U^\top \\ where
\\\operatorname{diag}(\omega)\\ is the diagonal matrix of the
eigenvalues, and \\U\\ is an orthogonal matrix.

Using the previous formula that \\\operatorname{tr}(AB) =
\operatorname{tr}(BA)\\, we have

\\ \operatorname{tr}(\Gamma) = \operatorname{tr}(U
\operatorname{diag}(\omega) U^\top) =
\operatorname{tr}(\operatorname{diag}(\omega) U^\top U) =
\operatorname{tr}(\operatorname{diag}(\omega)) = \sum\_{i}(\omega_i) \\

\\ \operatorname{tr}(\Gamma^\top \Gamma) = \operatorname{tr}(U
\operatorname{diag}(\omega) U^\top U \operatorname{diag}(\omega) U^\top)
= \operatorname{tr}(\operatorname{diag}(\omega)^2 U^\top U) =
\operatorname{tr}(\operatorname{diag}(\omega)^2) = \sum\_{i}(\omega_i^2)
\\

and \\\operatorname{tr}(\Gamma^\top \Gamma)\\ can be further expressed
as

\\ \operatorname{tr}(\Gamma^\top \Gamma) = \sum\_{i}{(\Gamma^\top
\Gamma)\_{ii}} = \sum\_{i}{\sum\_{j}{\Gamma^\top\_{ij}\Gamma\_{ji}}} =
\sum\_{i}{\sum\_{j}{\Gamma\_{ij}^2}} \\

## References

Bell RM, McCaffrey DF (2002). “Bias Reduction in Standard Errors for
Linear Regression with Multi-Stage Samples.” *Survey Methodology*,
**28**(2), 169–182.

Christensen RHB (2018). *Satterthwaite’s Method for Degrees of Freedom
in Linear Mixed Models*. Retrieved from
<https://github.com/runehaubo/lmerTestR/blob/35dc5885205d709cdc395b369b08ca2b7273cb78/pkg_notes/Satterthwaite_for_LMMs.pdf>
