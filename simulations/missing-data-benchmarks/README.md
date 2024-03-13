# Impact of Missing Data on MMRM Software

## Description

We assess the impact of missing data on various mixed models for repeated
measures (MMRMs) implementations' ability to fit models. The MMRM
implementations considered are the `mmrm`, `nlme` and `glmmTMB` R packages, as
well as SAS's `MIXED` procedure.

The data-generating processes (DGPs) used in this simulation study are inspired
by those seen in late-stage, longitudinal ophthalmology studies. All DGPs have
10 equidistant visits, and observations are approximately evenly split across
two treatment arms. The repeated measures are continuous and possess either
unstructured, heterogeneous compound symmetry or homogeneous Toeplitz covariance
matrices. The DGPs also include a baseline stratification factor and a
continuous baseline covariate. DGPs have one of three levels of treatment
effects: none, small, or moderate. The magnitude of these treatment effects are
similar to those observed in real ophthalmology studies. These DGPs also include
four levels of patient dropout: either 0%, 5-10%, 10-15%, or 75% of observations
are missing by the final visit. Dropout is a function of the baseline covariates
and the visit number. DGPs with the first three levels of missingness are
representative of recent ophthalmology trials, while the DGPs with the highest
dropout rates provide a stress test for the MMRM software compared here. A total
of 36 DGPs are considered.

One hundred replicates of 200, 400 and 600 observations, evenly split across
treatment arms, were drawn from each of these DGPs. The following MMRM estimators
were then applied:
- `mmrm::mmrm()` with heterogeneous compound symmetry, heterogeneous Toeplitz or
  unstructured repeated measures covariance.
- `glmmTMB::glmmTMB()` with heterogeneous compound symmetry, heterogeneous
  Toeplitz or unstructured repeated measures covariance.
- `nlme::gls()` with heterogeneous compound symmetry or unstructured repeated
  measures covariance. The heterogeneous Toeplitz matrix was not considered
  since it is not provided "out-of-the-box" by the `nlme` package.
- `PROC MIXED` with heterogeneous compound symmetry, heterogeneous Toeplitz or
  unstructured repeated measures covariance.

The following empirical metrics and operating characteristics were then
evaluated:
- Bias: The empirical bias of the treatment effect estimators at each visit.
- Variance: The empirical variance of the treatment effect estimators at each
  visit.
- 95% coverage: The empirical 95% coverage of the treatment effect estimators at
  each visit.
- Type 1 error rate: The empirical type 1 error rates of the treatment effect at
  each visit, using the Bonferroni-adjusted p-values. Only the DGPs with a null
  treatment effect are considered.
- Type 2 error rate: The empirical type 2 error rates of the treatment effect at
  each visit, using the Bonferroni-adjusted p-values. Only the DGPs with
  non-null treatment effects are considered.
- Convergence rate: The proportion of replicates on which each estimator
  converged.
- Mean fit time: The average time to fit the MMRM estimator across replicates.

## Results

The empirical bias, variance, coverage, and type 1 and 2 error rates of all
estimators considered are very similar in the 0%, 5-10% and 10-15% dropout
scenarios. `glmmTMB()` using an unstructured covariance matrix is unreliable,
however, as it fails to converge more than 50% of the time. `gls()` with the
unstructured covariance matrix also takes several of orders of magnitude longer
to converge than any other estimator. In these settings, `mmrm()` is therefore
the most reliable and fastest-fitting MMRM R software, and produces results
virtually identical to `PROC MIXED`, the current gold standard.

Similar conclusions are obtained from a review of the DGPs with the highest
levels of missingness: all estimators tend to perform similarly in terms of
empirical metrics and operating characteristics. However, `gls()` with an
unstructured covariance matrix fails to converge in most replicates. Again, this
suggests that `mmrm` is the most reliable software for fitting MMRMs in R.

An in-depth summary of the simulation study's results will follow shortly in the
form of a peer-reviewed publication.

## Navigating the Directory

This directory contains the following folders:

- `R/`: Contains the R scripts for the simulation study.
  - `dgp/`: Code relating to DGP definition.
  - `eval/`: Functions for measuring empirical metrics and operating
     characteristics.
  - `format-replicate-results/`: Functions to post-process existing tibble of fitted model objects to extract the `emmeans` results and fitted covariance matrices.
  - `meal.R`: Main R script to test the simulation study, only runs 2 repetitions.
  - `meals/`: Main R scripts for running the simulation studies. 
     Specify here the number of repetitions via `n_reps` at the end of each script.
  - `method/`: Wrapper functions for MMRM estimators.
  - `plot-results/`: Helps plotting the simulation results for all DGPs.
  - `viz/`: Visualization functions for plotting empirical metrics and operating
    characteristics.
- `results/`: Contains the partial simulation results.
- `scripts/`: Batch job scripts that start the `R/meals/` scripts on the HPC.
- `tests/`: Contains the testing files for the scripts in R. These tests can be
  ran by running `simChef::run_tests()` in your R console from the root of the
  simulation study directory.

The [`simChef`](https://github.com/Yu-Group/simChef) R package was used to
backbone for this simulation study. We recommend reviewing this package's
documentation prior to reviewing the simulation code.

## Reproducing the Simulation Study

All simulation results can be reproduced by running the R scripts in `R/meals/`
with the appropriately set sample sizes. We recommend performing these
simulations in a high-powered computing environment, as they are time consuming,
via using the batch job scripts.
Note too that the `sasr` R package and a SAS Studio account are required to
reproduce these simulations.
