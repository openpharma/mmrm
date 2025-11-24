# Mixed Models for Repeated Measures

## Abstract

Mixed models for repeated measures (MMRMs) are frequently used in the
analysis of data from clinical trials. They are specifically suited to
model continuous variables that were repeatedly measured at discrete
time points (or within defined time-windows). In a clinical trial, these
time points are typically visits according to a schedule that is
pre-defined in the trial protocol. The distinguishing feature of MMRMs,
compared to other implementations of linear mixed models, is that
subject-specific random effects (which are not of direct interest for
estimation and inference) are considered as residual effects, i.e. they
are part of the error correlation matrix. This vignette provides a brief
methodological introduction to MMRMs. MMRMs are described as an
extension to a basic linear mixed-effects model. The aim is to provide a
basic orientation and guide for applied statisticians working in the
field of clinical trials regarding theoretical underpinnings of MMRMs
and practical use in clinical trials. In the descriptions of the models
below, we generally follow Pinheiro and Bates (2000) and C. Mallinckrodt
and Lipkovich (2017).

## The basic linear mixed-effects model

Laird and Ware (1982) introduced the basic linear mixed-effects model
for a single level of grouping to be any model that expresses an
$`n_i`$-dimensional response (column) vector $`y_i`$ for the $`i`$th
subject (or, more generally, group or unit) as \$\$ y_i = X_i \beta +
Z_i b_i + \epsilon_i, \quad i=1,\ldots,M ,\\ b_i \sim
\mathcal{N}(0,\Psi), \quad \epsilon_i \sim \mathcal{N}(0,\sigma^2I),
\$\$ where

- $`\beta`$ is the $`p`$-dimensional vector of fixed effects,
- $`b`$ is the $`q`$-dimensional vector of random patient-specific
  effects,
- $`X_i`$ (of size $`n_i \times p`$) and $`Z_i`$ (of size
  $`n_i \times q`$) are known regressor matrices relating observations
  to the fixed-effects and random-effects, respectively, and
- $`\epsilon_i`$ is the $`n_i`$-dimensional within-subject error.

The random effects $`b_i`$ and the within-group errors $`\epsilon_i`$
are assumed to follow a normal distribution, with means of $`0`$ and
variance-covariance matrices $`\Psi`$ and $`\sigma^2I`$, where $`I`$ is
an identity matrix. They are further assumed to be independent for
different subjects and independent of each other for the same subject.
The random effects $`b_i`$ describe the shift from the mean of the
linear predictor for each subject. As they are defined to have a mean of
$`0`$, any non-zero mean for a term in the random effects must be
expressed as part of the fixed-effects terms. Therefore, the columns of
$`Z_i`$ are usually a (small) subset of the columns of $`X_i`$.

Mixed models are called “mixed” because they consider both fixed and
random effects and thus allow considerable modeling flexibility. This is
the case even for the basic formulation described above. However, it
still restricts within-group errors to be independent, identically
distributed random variables with mean of zero and constant variance.
These assumptions may often be seen as too restrictive (unrealistic) for
applications. For example, in the case of clinical trials with repeated
measurements of subjects over time, observations are not independent and
within-subject correlation needs to be accounted for by the model.

## Extending the basic linear mixed-effects model

The basic linear mixed-effects model can be extended in order to
incorporate within-subject errors that are heteroscedastic (i.e. have
unequal variances) and/ or are correlated. For this purpose, we can
express the within-subject errors as:
``` math
  \epsilon_i \sim \mathcal{N}(0,\Lambda_i), \quad i=1,\ldots,M,
```
where the $`\Lambda_i`$ are positive-definite matrices parameterized by
a fixed, generally small set of parameters $`\lambda`$. As in the basic
model, the within-group errors $`\epsilon_i`$ are assumed to be
independent for different $`i`$ and independent of the random effects
$`b_i`$. The variance-covariance matrix of the response vector $`y_i`$,
``` math
  \text{Var}(y_i) = \Sigma_i = \left( Z_i \Psi Z_{i}^{T} + \Lambda_i \right),  
```
comprises a random-effects component, given by $`Z_i \Psi Z_{i}^{T}`$,
and a within-subject component, given by $`\Lambda_i`$. When fitting
such models, there will generally be a “competition” and trade-off
between the complexities of the two components, and care must be
exercised to prevent nonidentifiability, or near nonidentifiability, of
the parameters. This is one of the reasons why it can be advantageous in
practice to use only one of these components as long as it still allows
to capture all relevant sources of variability. In longitudinal studies
with only one level of grouping, the within-subject component is
particularly important to be considered in order to account for
within-subject correlation, whereas an additional random-effects
component is often not strictly needed.

Of note, in the literature, the random-effects component and the
within-subject component are sometimes also referred to as $`R`$ and
$`G`$, or as $`R`$-side and $`G`$-side random effects (Cnaan, Laird, and
Slasor (1997), Littell, Pendergast, and Natarajan (2000)).

## The MMRM as a special case

In a clinical trial setting, one often chooses to directly model the
variance-covariance structure of the response, i.e. to account for
within-subject dependencies using the within-group component
$`\Lambda_i`$, and can omit the random effects component ($`Z_i b_i`$).
Hence, in this case, $`\text{Var}(y_i)=\Sigma_i=\Lambda_i`$ . This
yields the MMRM with:

``` math
     y_i = X_i\beta + \epsilon_i, \quad \epsilon_i \sim \mathcal{N}(0,\Sigma_i), \quad i=1,\ldots,M.
```

The $`\Sigma_i`$ matrices are obtained by subsetting the overall
variance-covariance matrix $`\Sigma \in \mathbb{R}^{m \times m}`$, where
$`m`$ is the total number of scheduled visits per subject, appropriately
by
``` math
     \Sigma_i = S_i^\top \Sigma S_i ,    
```
where $`S_i \in \{0, 1\}^{m \times m_i}`$ is the subject-specific
‘’subsetting matrix’’ that indicates the visits with available
observations (see also the vignette on the Details of the Model Fitting
in `mmrm`).

When written as a model for all $`n`$ subjects in a trial, the MMRM is
represented by
``` math
Y = X\beta + \epsilon,
```
where $`Y \in \mathbb{R}^N`$ combines all subject-specific observations
$`y_i`$ such that in total there are $`N = \sum_{i = 1}^{n}{m_i}`$
observations, $`X \in \mathbb{R}^{N \times p}`$ combines all
subject-specific design matrices, $`\beta \in \mathbb{R}^p`$ is a vector
of fixed effects, and $`\epsilon \in \mathbb{R}^N`$ has a multivariate
normal distribution,
``` math
\epsilon \sim N(0, \Omega),
```
with $`\Omega \in \mathbb{R}^{N \times N}`$ being a block-diagonal
matrix, containing the subject-specific $`\Sigma_i`$ on the diagonal
(and with all other entries being equal to $`0`$).

When modeling longitudinal responses in a clinical trial with multiple
follow-up visits, the linear predictor $`X\beta`$ typically considers
fixed effects of baseline values, treatment and visit, as well as
interactions between treatment and visit, and possibly between baseline
and visit. Commonly the fixed effects are of most interest and the
correlation structure $`\Sigma`$ can be viewed as a nuisance quantity.
However, it is important to model it carefully, since it affects
validity of the estimated variance of $`\beta`$. The `mmrm` package
supports a wide range of covariance structures (see also the vignette on
Covariance Structures in `mmrm`).

## Missing data

Mixed models can accommodate unbalanced data and use all available
observations and subjects in the analysis. Inferences are valid under
the assumption that missing observations are independent of unobserved
data, but may be dependent on observed data. This assumption that is
often seen as reasonable and is called “missing at random”-assumption.
By contrast, some imputation methods to handle missing data and modeling
alternatives, such as the last-observation carried forward-approach or
models based on generalized estimating equations, require stricter
assumptions on missingness mechanism (C. H. Mallinckrodt, Lane, Schnell,
Peng, and Mancuso (2008), Fitzmaurice (2016)).

## References

Cnaan A, Laird NM, Slasor P (1997). “Using the general linear mixed
model to analyse unbalanced repeated measures and longitudinal data.”
*Statistics in Medicine*, **16**(20), 2349–2380.

Fitzmaurice GM (2016). “Generalized Estimating Equations.” In *Wiley
StatsRef: Statistics Reference Online* 1–4. John Wiley & Sons, Ltd.

Laird NM, Ware JH (1982). “Random-effects models for longitudinal data.”
*Biometrics*, **38**(4), 963–974.

Littell RC, Pendergast J, Natarajan R (2000). “Modelling covariance
structure in the analysis of repeated measures data.” *Statistics in
Medicine*, **19**(13), 1793–1819.

Mallinckrodt CH, Lane PW, Schnell D, Peng Y, Mancuso JP (2008).
“Recommendations for the Primary Analysis of Continuous Endpoints in
Longitudinal Clinical Trials.” *Drug Information Journal*, **42**(4),
303–319.

Mallinckrodt C, Lipkovich I (2017). *Analyzing Longitudinal Clinical
Trial Data. A Practical Guide.* Chapman & Hall/ CRC Biostatistics
Series.

Pinheiro J, Bates DM (2000). *Mixed-Effects Models in S and S-PLUS*.
Springer (Statistics; Computing).
