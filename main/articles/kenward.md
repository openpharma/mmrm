# Kenward-Roger

Here we describe the details of the calculations for the Kenward-Roger
degrees of freedom and the adjusted covariance matrix of the
coefficients.

### Model definition

The model definition is the same as what we have in [Details of the
model fitting in
`mmrm`](https://openpharma.github.io/mmrm/articles/algorithm.md). We are
using the same notations.

#### Linear model

For each subject $`i`$ we observe a vector
``` math
Y_i = (y_{i1}, \dotsc, y_{im_i})^\top \in \mathbb{R}^{m_i}
```
and given a design matrix
``` math
X_i \in \mathbb{R}^{m_i \times p}
```
and a corresponding coefficient vector $`\beta \in \mathbb{R}^{p}`$ we
assume that the observations are multivariate normal distributed:
``` math
Y_i \sim N(X_i\beta, \Sigma_i)
```
where the covariance matrix $`\Sigma_i \in \mathbb{R}^{m_i \times m_i}`$
is derived by subsetting the overall covariance matrix
$`\Sigma \in \mathbb{R}^{m \times m}`$ appropriately by
``` math
\Sigma_i = G_i^{-1/2} S_i^\top \Sigma S_i G_i^{-1/2}
```
where the subsetting matrix $`S_i \in \{0, 1\}^{m \times m_i}`$ contains
in each of its $`m_i`$ columns contains a single 1 indicating which
overall time point is matching $`t_{ih}`$.
$`G_i \in \mathbb{R}_{\gt 0}^{m_i \times m_i}`$ is the diagonal weight
matrix.

Conditional on the design matrices $`X_i`$, the coefficient vector
$`\beta`$ and the covariance matrix $`\Sigma`$ we assume that the
observations are independent between the subjects.

We can write the linear model for all subjects together as
``` math
Y = X\beta + \epsilon
```
where $`Y \in \mathbb{R}^N`$ combines all subject specific observations
vectors $`Y_i`$ such that we have in total $`N = \sum_{i = 1}^{n}{m_i}`$
observations, $`X \in \mathbb{R}^{N \times p}`$ combines all subject
specific design matrices and $`\epsilon \in \mathbb{R}^N`$ has a
multivariate normal distribution
``` math
\epsilon \sim N(0, \Omega)
```
where $`\Omega \in \mathbb{R}^{N \times N}`$ is block-diagonal
containing the subject specific $`\Sigma_i`$ covariance matrices on the
diagonal and 0 in the remaining entries.

### Mathematical Details of Kenward-Roger method

The mathematical derivation of the Kenward-Roger method is based on the
Taylor expansion of the obtained covariance matrix of $`\hat\beta`$ to
get a more accurate estimate for it. All these derivations are based on
the restricted maximum likelihood. Following the same
[notation](https://openpharma.github.io/mmrm/articles/algorithm.html#covariance-matrix-model),
the covariance matrix, $`\Omega`$ can be represented as a function of
covariance matrix parameters
$`\theta = (\theta_1, \dotsc, \theta_k)^\top`$, i.e. $`\Omega(\theta)`$.
Here after model fitting with `mmrm`, we obtain the estimate
$`\hat\beta = \Phi(\hat\theta)X^\top\Omega(\hat\theta)^{-1}Y`$, where
$`\Phi(\theta) = \left\{X^\top \Omega(\theta)^{-1} X\right\} ^{-1}`$ is
the asymptotic covariance matrix of $`\hat\beta`$. However, Kackar and
Harville (1984) suggests that although the $`\hat\beta`$ is unbiased for
$`\beta`$, the covariance matrix,
$`\hat\Phi = \left\{X^\top \hat\Omega X\right\}^{-1}`$ can be biased.
They showed that the variability of $`\hat\beta`$ can be partitioned
into two components,

``` math
  \Phi_A = \Phi + \Lambda
```

where $`\Phi`$ is the variance-covariance matrix of the asymptotic
distribution of $`\hat\beta`$ as $`n\rightarrow \infty`$ as defined
above, and $`\Lambda`$ represents the amount to which the asymptotic
variance-covariance matrix underestimates $`\Phi_A`$.

Based on a Taylor series expansion around $`\theta`$, $`\Lambda`$ can be
approximated by

``` math
  \Lambda \simeq \Phi \left\{\sum_{h=1}^k{\sum_{j=1}^k{W_{hj}(Q_{hj} - P_h \Phi P_j)} }\right\} \Phi
```
where
``` math
  P_h = X^\top \frac{\partial{\Omega^{-1}}}{\partial \theta_h} X
```
``` math
  Q_{hj} = X^\top \frac{\partial{\Omega^{-1}}}{\partial \theta_h} \Omega \frac{\partial{\Omega^{-1}}}{\partial \theta_j} X
```

$`W`$ is the inverse of the Hessian matrix of the log-likelihood
function of $`\theta`$ evaluated at the estimate $`\hat\theta`$,
i.e. the observed Fisher Information matrix as a consistent estimator of
the variance-covariance matrix of $`\hat\theta`$.

Again, based on a Taylor series expansion about $`\theta`$, Kenward and
Roger (1997) show that
``` math
  \hat\Phi \simeq \Phi + \sum_{h=1}^k{(\hat\theta_h - \theta_h)\frac{\partial{\Phi}}{\partial{\theta_h}}} + \frac{1}{2} \sum_{h=1}^k{\sum_{j=1}^k{(\hat\theta_h - \theta_h)(\hat\theta_j - \theta_j)\frac{\partial^2{\Phi}}{\partial{\theta_h}\partial{\theta_j}}}}
```
Ignoring the possible bias in $`\hat\theta`$,
``` math
  E(\hat\Phi) \simeq \Phi + \frac{1}{2} \sum_{h=1}^k{\sum_{j=1}^k{W_{hj}\frac{\partial^2{\Phi}}{\partial{\theta_h}\partial{\theta_j}}}}
```
Using previously defined notations, this can be further written as
``` math
  \frac{\partial^2{\Phi}}{\partial{\theta_h}\partial{\theta_j}} = \Phi (P_h \Phi P_j + P_j \Phi P_h - Q_{hj} - Q_{jh} + R_{hj}) \Phi
```
where
``` math
  R_{hj} = X^\top\Omega^{-1}\frac{\partial^2\Omega}{\partial{\theta_h}\partial{\theta_j}} \Omega^{-1} X
```

substituting $`\Phi`$ and $`\Lambda`$ back to the $`\hat\Phi_A`$, we
have

``` math
  \hat\Phi_A = \hat\Phi + 2\hat\Phi \left\{\sum_{h=1}^k{\sum_{j=1}^k{W_{hj}(Q_{hj} - P_h \hat\Phi P_j - \frac{1}{4}R_{hj})} }\right\} \hat\Phi
```

where $`\Omega(\hat\theta)`$ replaces $`\Omega(\theta)`$ in the
right-hand side.

Please note that, if we ignore $`R_{hj}`$, the second-order derivatives,
we will get a different estimate of adjusted covariance matrix, and we
call this the linear Kenward-Roger approximation.

#### Special Considerations for mmrm models

In mmrm models, $`\Omega`$ is a block-diagonal matrix, hence we can
calculate $`P`$, $`Q`$ and $`R`$ for each subject and add them up.

``` math
  P_h = \sum_{i=1}^{N}{P_{ih}} = \sum_{i=1}^{N}{X_i^\top \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_h} X_i}
```

``` math
  Q_{hj} = \sum_{i=1}^{N}{Q_{ihj}} = \sum_{i=1}^{N}{X_i^\top \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_h} \Sigma_i \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_j} X_i}
```

``` math
  R_{hj} = \sum_{i=1}^{N}{R_{ihj}} = \sum_{i=1}^{N}{X_i^\top\Sigma_i^{-1}\frac{\partial^2\Sigma_i}{\partial{\theta_h}\partial{\theta_j}} \Sigma_i^{-1} X_i}
```

#### Derivative of the overall covariance matrix $`\Sigma`$

The derivative of the overall covariance matrix $`\Sigma`$ with respect
to the variance parameters can be calculated through the derivatives of
the Cholesky factor, and hence obtained through automatic
differentiation, following [matrix identities
calculations](https://en.wikipedia.org/wiki/Matrix_calculus#Identities_in_differential_form).
``` math
  \frac{\partial{\Sigma}}{\partial{\theta_h}} = \frac{\partial{LL^\top}}{\partial{\theta_h}} = \frac{\partial{L}}{\partial{\theta_h}}L^\top + L\frac{\partial{L^\top}}{\partial{\theta_h}}
```

``` math
  \frac{\partial^2{\Sigma}}{\partial{\theta_h}\partial{\theta_j}} = \frac{\partial^2{L}}{\partial{\theta_h}\partial{\theta_j}}L^\top + L\frac{\partial^2{L^\top}}{\partial{\theta_h}\partial{\theta_j}} + \frac{\partial{L}}{\partial{\theta_h}}\frac{\partial{L^T}}{\partial{\theta_j}} + \frac{\partial{L}}{\partial{\theta_j}}\frac{\partial{L^\top}}{\partial{\theta_h}}
```

#### Derivative of the $`\Sigma^{-1}`$

The derivatives of $`\Sigma^{-1}`$ can be calculated through

\$\$ \frac{\partial{\Sigma\Sigma^{-1}}}{\partial{\theta_h}}\\ =
\frac{\partial{\Sigma}}{\partial{\theta_h}}\Sigma^{-1} +
\Sigma\frac{\partial{\Sigma^{-1}}}{\partial{\theta_h}} \\ = 0 \$\$
``` math
  \frac{\partial{\Sigma^{-1}}}{\partial{\theta_h}} = - \Sigma^{-1} \frac{\partial{\Sigma}}{\partial{\theta_h}}\Sigma^{-1}
```

#### Subjects with missed visits

If a subject do not have all visits, the corresponding covariance matrix
can be represented as
``` math
  \Sigma_i = S_i^\top \Sigma S_i
```

and the derivatives can be obtained through

``` math
  \frac{\partial{\Sigma_i}}{\partial{\theta_h}} = S_i^\top \frac{\partial{\Sigma}}{\partial{\theta_h}} S_i
```

``` math
  \frac{\partial^2{\Sigma_i}}{\partial{\theta_h}\partial{\theta_j}} = S_i^\top \frac{\partial^2{\Sigma}}{\partial{\theta_h}\partial{\theta_j}} S_i
```

The derivative of the $`\Sigma_i^{-1}`$,
$`\frac{\partial\Sigma_i^{-1}}{\partial{\theta_h}}`$ can be calculated
through $`\Sigma_i^{-1}`$ and
$`\frac{\partial{\Sigma_i}}{\partial{\theta_h}}`$ using the above.

#### Scenario under group specific covariance estimates

When fitting grouped `mmrm` models, the covariance matrix for subject i
of group $`g(i)`$, can be written as \$\$ \Sigma_i = S_i^\top
\Sigma\_{g(i)} S_i\$. \$\$ Assume there are $`B`$ groups, the number of
parameters is increased by $`B`$ times. With the fact that for each
group, the corresponding $`\theta`$ will not affect other parts, we will
have block-diagonal $`P`$, $`Q`$ and $`R`$ matrices, where the blocks
are given by:

``` math
P_h = \begin{pmatrix}
P_{h, 1} & \dots & P_{h, B} \\
\end{pmatrix}
```

``` math
Q_{hj} = \begin{pmatrix}
Q_{hj, 1} & 0 & \dots & \dots & 0 \\
0 & Q_{hj, 2} & 0 & \dots & 0\\
\vdots & & \ddots & & \vdots \\
\vdots & & & \ddots & \vdots \\
0 & \dots & \dots & 0 & Q_{hj, B}
\end{pmatrix}
```

``` math
R_{hj} = \begin{pmatrix}
R_{hj, 1} & 0 & \dots & \dots & 0 \\
0 & R_{hj, 2} & 0 & \dots & 0\\
\vdots & & \ddots & & \vdots \\
\vdots & & & \ddots & \vdots \\
0 & \dots & \dots & 0 & R_{hj, B}
\end{pmatrix}
```

Use $`P_{j, b}`$ to denote the block diagonal part for group $`b`$, we
have
``` math
  P_{h,b} = \sum_{g(i) = b}{P_{ih}} = \sum_{g(i) = b}{X_i^\top \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_h} X_i}
```

``` math
  Q_{hj,b} = \sum_{g(i) = b}{Q_{ihj}} = \sum_{g(i) = b}{X_i^\top \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_h} \Sigma_i \frac{\partial{\Sigma_i^{-1}}}{\partial \theta_j} X_i}
```

``` math
  R_{hj,b} = \sum_{g(i) = b}{R_{ihj}} = \sum_{g(i) = b}{X_i^\top\Sigma_i^{-1}\frac{\partial^2\Sigma_i}{\partial{\theta_h}\partial{\theta_j}} \Sigma_i^{-1} X_i}
```

Similarly for $`R`$.

#### Scenario under weighted mmrm

Under weights mmrm model, the covariance matrix for subject $`i`$, can
be represented as

``` math
  \Sigma_i = G_i^{-1/2} S_i^\top \Sigma S_i G_i^{-1/2}
```

Where $`G_i`$ is a diagonal matrix of the weights. Then, when deriving
$`P`$, $`Q`$ and $`R`$, there are no mathematical differences as they
are constant, and having $`G_i`$ in addition to $`S_i`$ does not change
the algorithms and we can simply multiply the formulas with
$`G_i^{-1/2}`$, similarly as above for the subsetting matrix.

### Inference

Suppose we are testing the linear combination of $`\beta`$, $`C\beta`$
with $`C \in \mathbb{R}^{c\times p}`$, we can use the following
F-statistic
``` math
  F = \frac{1}{c} (\hat\beta - \beta)^\top  C (C^\top \hat\Phi_A C)^{-1} C^\top (\hat\beta - \beta)
```
and
``` math
  F^* = \lambda F
```
follows exact $`F_{c,m}`$ distribution.

When we have only one coefficient to test, then $`C`$ is a column vector
containing a single $`1`$ inside. We can still follow the same
calculations as for the multi-dimensional case. This recovers the
degrees of freedom results of the Satterthwaite method.

$`\lambda`$ and $`m`$ can be calculated through

``` math
  M = C (C^\top \Phi C)^{-1} C^\top
```

``` math
  A_1 = \sum_{h=1}^k{\sum_{j=1}^k{W_{hj} tr(M \Phi P_h \Phi) tr(M \Phi P_j \Phi)}}
```

``` math
  A_2 = \sum_{h=1}^k{\sum_{j=1}^k{W_{hj} tr(M \Phi P_h \Phi M \Phi P_j \Phi)}}
```

``` math
  B = \frac{1}{2c}(A_1 + 6A_2)
```

``` math
  g = \frac{(c+1)A_1 - (c+4)A_2}{(c+2)A_2}
```

``` math
  c_1 = \frac{g}{3c+2(1-g)}
```

``` math
  c_2 = \frac{c-g}{3c+2(1-g)}
```

``` math
  c_3 = \frac{c+2-g}{3c+2(1-g)}
```
``` math
E^*={\left\{1-\frac{A_2}{c}\right\}}^{-1}
```
``` math
V^*=\frac{2}{c}{\left\{\frac{1+c_1 B}{(1-c_2 B)^2(1-c_3 B)}\right\}}
```

``` math
\rho = \frac{V^{*}}{2(E^*)^2}
```

``` math
m = 4 + \frac{c+2}{c\rho - 1}
```
``` math
\lambda = \frac{m}{E^*(m-2)}
```

### Parameterization methods and Kenward-Roger

While the Kenward-Roger adjusted covariance matrix is adopting a Taylor
series to approximate the true value, the choices of parameterization
can change the result. In a simple example of unstructured covariance
structure, in our current approach, where the parameters are elements of
the Cholesky factor of $`\Sigma`$ (see
[parameterization](https://openpharma.github.io/mmrm/articles/covariance.md)),
the second-order derivatives of $`\Sigma`$ over our parameters, are
non-zero matrices. However, if we use the elements of $`\Sigma`$ as our
parameters, then the second-order derivatives are zero matrices.
However, the differences can be small and will not affect the inference.
If you would like to match SAS results for the unstructured covariance
model, you can use the linear Kenward-Roger approximation.

### Implementations in `mmrm`

In package `mmrm`, we have implemented Kenward-Roger calculations based
on the previous sections. Specially, for the first-order and
second-order derivatives, we use automatic differentiation to obtain the
results easily for non-spatial covariance structure. For spatial
covariance structure, we derive the exact results.

#### Spatial Exponential Derivatives

For spatial exponential covariance structure, we have

``` math
\theta = (\theta_1,\theta_2)
```
``` math
\sigma = e^{\theta_1}
```
``` math
\rho = \frac{e^{\theta_2}}{1 + e^{\theta_2}}
```

``` math
\Sigma_{ij} = \sigma \rho^{d_{ij}}
```
where $`d_{ij}`$ is the distance between time point $`i`$ and time point
$`j`$.

So the first-order derivatives can be written as:

\$\$ \frac{\partial{\Sigma\_{ij}}}{\partial\theta_1} =
\frac{\partial\sigma}{\partial\theta_1} \rho^{d\_{ij}}\\ =
e^{\theta_1}\rho^{d\_{ij}} \\ = \Sigma\_{ij} \$\$

\$\$ \frac{\partial{\Sigma\_{ij}}}{\partial\theta_2} =
\sigma\frac{\partial{\rho^{d\_{ij}}}}{\partial\theta_2} \\ =
\sigma\rho^{d\_{ij}-1}{d\_{ij}}\frac{\partial\rho}{\partial\theta_2}\\ =
\sigma\rho^{d\_{ij}-1}{d\_{ij}}\rho(1-\rho) \\ = \sigma \rho^{d\_{ij}}
{d\_{ij}} (1-\rho) \$\$

Second-order derivatives can be written as:

\$\$ \frac{\partial^2{\Sigma\_{ij}}}{\partial\theta_1\partial\theta_1}\\
= \frac{\partial\Sigma\_{ij}}{\partial\theta_1}\\ = \Sigma\_{ij} \$\$

\$\$ \frac{\partial^2{\Sigma\_{ij}}}{\partial\theta_1\partial\theta_2} =
\frac{\partial^2{\Sigma\_{ij}}}{\partial\theta_2\partial\theta_1} \\ =
\frac{\partial\Sigma\_{ij}}{\partial\theta_2}\\ =
\sigma\rho^{d\_{ij}-1}{d\_{ij}}\rho(1-\rho)\\ =
\sigma\rho^{d\_{ij}}{d\_{ij}}(1-\rho) \$\$

\$\$ \frac{\partial^2{\Sigma\_{ij}}}{\partial\theta_2\partial\theta_2}\\
=
\frac{\partial{\sigma\rho^{d\_{ij}}{d\_{ij}}(1-\rho)}}{\partial\theta_2}\\
= \sigma\rho^{d\_{ij}}{d\_{ij}}(1-\rho)(d\_{ij} (1-\rho) - \rho) \$\$

## References

Kackar RN, Harville DA (1984). “Approximations for Standard Errors of
Estimators of Fixed and Random Effects in Mixed Linear Models.” *Journal
of the American Statistical Association*, **79**(388), 853–862.
[https://doi.org/10.1080/01621459.1984.10477102.](https://doi.org/10.1080/01621459.1984.10477102)

Kenward MG, Roger JH (1997). “Small Sample Inference for Fixed Effects
from Restricted Maximum Likelihood.” *Biometrics*, **53**(3), 983–997.
[https://doi.org/10.2307/2533558.](https://doi.org/10.2307/2533558)
