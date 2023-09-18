library(MASS)
library(dplyr)
library(purrr)
library(microbenchmark)
library(stringr)
library(mmrm)
library(lme4)
library(nlme)
library(glmmTMB)
library(sasr)
library(knitr)
library(emmeans)
library(ggplot2)

set.seed(5123)
n_reps <- 10

# transfer large data

s <- get_sas_session()
df2sd_long <- function(data, name, block = 500, n_try = 10) {
  n <- ceiling(nrow(data) / block)
  data_idx <- ceiling(seq_len(nrow(data)) / block)
  data_split <- split(data, data_idx)
  for (i in seq_len(n)) {
    dd <- data_split[[i]]
    row.names(dd) <- NULL
    for (j in n_try) {
      a <- df2sd(dd, sprintf("%s_%i", name, i))
      log <- s$lastlog()
      status <- grepl("error", log, ignore.case = TRUE)
      if (!status) {
        break
      }
    }
    if (status) {
      stop("not successful")
    }
  }
  sas_code <- sprintf("
    data %s;
    set %s;
    run;
  ", name, paste(sprintf("%s_%i", name, rev(seq_len(n))), collapse = " "))
  run_sas(sas_code)
}

# FEV data: convergence time ----
# load the data into SAS
df2sd_long(fev_data, "fev_data")
# fit PROC GLIMMIX 10 times if a sascfg file exists
fit_times <- sapply(
  seq_len(n_reps),
  function(x) {
    sas_code <- "
    PROC GLIMMIX DATA = fev_data;
      CLASS AVISIT(ref = 'VIS1') RACE(ref = 'Asian') ARMCD(ref = 'PBO') USUBJID;
      MODEL FEV1 = RACE ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
      RANDOM AVISIT / subject=USUBJID type=un;
    RUN;
  "

    # generate the SAS output
    sas_result <- run_sas(sas_code)

    # extract the fit time (seconds)
    fit_time <- sas_result$LOG %>%
      str_extract("(?<=real time)\\s*[0-9.]+") %>%
      as.numeric()

    # convert fit times from seconds to milliseconds
    fit_time * 1000
  }
)
# add PROC GLIMMIX results to convergence results table
proc_glimmix_row_fev <- tibble(
  expression = "PROC GLIMMIX",
  median = median(fit_times),
  lower = quantile(fit_times, probs = 0.25),
  upper = quantile(fit_times, probs = 0.75),
  evaluations = n_reps
)

# BCVA data: convergence time ----

# SAS do not support row names
row.names(bcva_data) <- NULL
df2sd_long(bcva_data, "bcva_data", block = 500)

fit_times <- sapply(
  seq_len(n_reps),
  function(x) {
    sas_code <- "
    PROC GLIMMIX DATA = bcva_data;
      CLASS AVISIT(ref = 'VIS01') RACE(ref = 'Asian') ARMCD(ref = 'CTL') USUBJID;
      MODEL BCVA_CHG = BCVA_BL RACE ARMCD AVISIT ARMCD*AVISIT / ddfm=satterthwaite solution chisq;
      RANDOM AVISIT / subject=USUBJID type=un;
    RUN;
  "

    # generate the SAS output
    sas_result <- run_sas(sas_code)

    # extract the fit time (seconds)
    fit_time <- sas_result$LOG %>%
      str_extract("(?<=real time)\\s*[0-9.]+") %>%
      as.numeric()

    # convert fit times from seconds to milliseconds
    fit_time
  }
)

# add PROC GLIMMIX results to convergence results table
proc_glimmix_row_bcva <- tibble(
  expression = "PROC GLIMMIX",
  median = median(fit_times),
  lower = quantile(fit_times, probs = 0.25),
  upper = quantile(fit_times, probs = 0.75),
  evaluations = n_reps
)

# estimations fev ----

sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out;
  PROC GLIMMIX DATA = fev_data;
    CLASS AVISIT(ref = 'VIS1') RACE(ref = 'Asian') ARMCD(ref = 'PBO') USUBJID;
    MODEL FEV1 = ARMCD AVISIT ARMCD*AVISIT RACE / ddfm=satterthwaite solution chisq;
    RANDOM AVISIT / subject=USUBJID type=un;
    LSMEANS ARMCD*AVISIT / pdiff slice=AVISIT cl alpha = 0.05 OBSMARGINS;
  RUN;
"

# generate the SAS output
sas_result <- run_sas(sas_code)

## extract the ATEs across visits
ates_df <- sd2df("diffs_out", "work") %>%
  dplyr::filter(AVISIT == `_AVISIT`) %>%
  dplyr::mutate(
    contrast = paste0(AVISIT, ": ", ARMCD, " - ", `_ARMCD`)
  ) %>%
  dplyr::select(contrast, Estimate, StdErr, DF, tValue, Lower, Upper) %>%
  dplyr::arrange(contrast)

proc_glimmix_ests_fev <- ates_df$Estimate


# estimations bcva ----

sas_code <- "ODS OUTPUT LSMEANS = lsmeans_out DIFFS = diffs_out;
  PROC GLIMMIX DATA = bcva_data;
    CLASS AVISIT(ref = 'VIS01') RACE(ref = 'Asian') ARMCD(ref = 'CTL') USUBJID;
    MODEL BCVA_CHG = BCVA_BL ARMCD AVISIT ARMCD*AVISIT RACE / ddfm=satterthwaite solution chisq;
    RANDOM AVISIT / subject=USUBJID type=un;
    LSMEANS ARMCD*AVISIT / pdiff slice=AVISIT cl alpha = 0.05 OBSMARGINS;
  RUN;
"

# generate the SAS output
sas_result <- run_sas(sas_code)

## extract the ATEs across visits
ates_df <- sd2df("diffs_out", "work") %>%
  dplyr::filter(AVISIT == `_AVISIT`) %>%
  dplyr::mutate(
    contrast = paste0(AVISIT, ": ", ARMCD, " - ", `_ARMCD`)
  ) %>%
  dplyr::select(contrast, Estimate, StdErr, DF, tValue, Lower, Upper) %>%
  dplyr::arrange(contrast)

proc_glimmix_ests_bcva <- ates_df$Estimate

# DGP ----
# helper function for generating covariates
generate_covariates <- function(n_obs, n_visits = 10) {
  # participant ID
  participant <- seq_len(n_obs)

  # baseline best corrected visual acuity score
  base_bcva <- rnorm(n = n_obs, mean = 75, sd = 10)

  # stratification factor
  strata <- as.vector(
    c(1, 2, 3) %*% rmultinom(n = n_obs, 1, prob = c(0.3, 0.3, 0.4))
  )

  # treatment indicator
  trt <- rbinom(n = n_obs, size = 1, prob = 0.5)

  # visit number
  visit_num <- rep(seq_len(n_visits), n_obs)

  # assemble into a covariates data.frame
  data.frame(
    participant = rep(participant, each = n_visits),
    base_bcva = rep(base_bcva, each = n_visits),
    strata = as.factor(rep(strata, each = n_visits)),
    trt = rep(trt, each = n_visits),
    visit_num
  )
}

# helper function for randomly generating unstructured covariance matrix
compute_unstructured_matrix <- function(vars = seq(from = 1, by = 0.5, length.out = 10)) {
  n_visits <- length(vars)
  corr_mat <- abs(cov2cor(
    clusterGeneration::genPositiveDefMat(dim = n_visits)$Sigma
  ))
  sd_mat <- diag(sqrt(vars))
  us_mat <- sd_mat %*% corr_mat %*% sd_mat
  return(us_mat)
}

# helper function for generating BCVA data
generate_outcomes <- function(
    covars_df,
    cov_mat,
    intercept = 5,
    base_bcva_coef = 1,
    strata_2_coef = -1,
    strata_3_coef = 1,
    trt_coef = 1,
    visit_coef = 0.25,
    trt_visit_coef = 0.25) {
  # construct the model matrix
  model_mat <- model.matrix(
    ~ base_bcva + strata + trt * visit_num,
    data = covars_df
  )

  # generate the bcva outcomes
  n_visits <- nrow(cov_mat)
  n_obs <- nrow(covars_df) / n_visits
  effect_coefs <- c(
    intercept, base_bcva_coef, strata_2_coef, strata_3_coef,
    trt_coef, visit_coef, trt_visit_coef
  )
  as.vector(model_mat %*% effect_coefs +
    as.vector(t(MASS::mvrnorm(n_obs, rep(0, n_visits), cov_mat))))
}

# MAR helper function
missing_at_random <- function(covars_df, type) {
  # compute missingness probabilities
  if (type == "none") {
    prob_miss <- 0
  } else if (type == "mild") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.3 * covars_df$visit_num - 0.2 *
          (covars_df$trt == 0))
    )
  } else if (type == "moderate") {
    prob_miss <- plogis(
      -(5 - 0.01 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.4 * covars_df$visit_num - 0.5 *
          (covars_df$trt == 0))
    )
  } else if (type == "high") {
    prob_miss <- plogis(
      -(5 - 0.02 * covars_df$base_bcva + 0.5 * (covars_df$strata == 2) +
        1 * (covars_df$strata == 3) - 0.5 * covars_df$visit_num - 1 *
          (covars_df$trt == 0))
    )
  }

  # generate vector of missingness indicators
  missing_ind <- rbinom(nrow(covars_df), 1, prob_miss)

  # remove indicated visits
  covars_df[missing_ind == 0, ]
}

# BCVA data-generating process
rct_dgp_fun <- function(
    n_obs = 1000,
    outcome_covar_mat = compute_unstructured_matrix(),
    trt_coef = 0.25,
    visit_coef = 0.25,
    trt_visit_coef = 0.25,
    missing_type = "moderate") {
  # generate the covariates
  covars_df <- generate_covariates(
    n_obs = n_obs, n_visits = nrow(outcome_covar_mat)
  )

  # generate the outcomes
  bcva_out <- generate_outcomes(
    covars_df = covars_df,
    cov_mat = outcome_covar_mat,
    trt_coef = trt_coef,
    visit_coef = visit_coef,
    trt_visit_coef = trt_visit_coef
  )

  # compute the change in BCVA
  bcva_change <- bcva_out - covars_df$base_bcva

  # assemble into a data.frame
  df <- data.frame(
    participant = covars_df$participant,
    bcva_change = bcva_change,
    base_bcva = covars_df$base_bcva,
    strata = covars_df$strata,
    trt = covars_df$trt,
    visit_num = covars_df$visit_num
  )

  # delete observations at random
  df <- missing_at_random(df, type = missing_type)

  # format to resemble FEV dataset
  df <- df %>%
    transmute(
      USUBJID = factor(participant),
      VISITN = visit_num,
      AVISIT = paste0("VIS", str_pad(visit_num, width = 2, pad = "0")),
      AVISIT = factor(
        AVISIT,
        levels = paste0("VIS", str_pad(seq_len(10), width = 2, pad = "0"))
      ),
      ARMCD = ifelse(trt == 1, "TRT", "CTL"),
      RACE = ifelse(strata == 1, "Black",
        ifelse(strata == 2, "Asian", "White")
      ),
      BCVA_BL = base_bcva,
      BCVA_CHG = bcva_change
    )

  return(df)
}

# dgp data

dgp_data <- lapply(
  seq_len(n_reps),
  function(idx) {
    list(
      none = rct_dgp_fun(n_obs = 200, missing_type = "none"),
      mild = rct_dgp_fun(n_obs = 200, missing_type = "mild"),
      moderate = rct_dgp_fun(n_obs = 200, missing_type = "moderate"),
      high = rct_dgp_fun(n_obs = 200, missing_type = "high")
    )
  }
)

get_dgp_data <- function(missing_type) {
  lapply(dgp_data, function(x) {
    x[[missing_type]]
  })
}
# convergence rate ----
get_convergence_rates_sas <- function(missingness_level) {
  purrr::map_df(
    get_dgp_data(missing_type = missingness_level),
    function(data) {
      ## SAS
      # compute marginal estimates from PROC GLIMMIX
      # load the data into SAS
      df2sd_long(data, "miss_df")

      sas_code <- "ODS OUTPUT ConvergenceStatus = conv_status;
        PROC GLIMMIX DATA = miss_df;
          CLASS AVISIT(ref = 'VIS01') RACE(ref = 'Asian') ARMCD(ref = 'CTL') USUBJID;
          MODEL BCVA_CHG = BCVA_BL ARMCD AVISIT ARMCD*AVISIT RACE / ddfm=satterthwaite solution chisq;
          RANDOM AVISIT / subject=USUBJID type=un;
        RUN;
      "
      # generate the SAS output
      sas_result <- run_sas(sas_code)
      ## extract the convergence status
      conv_status_df <- sd2df("conv_status", "work")
      sas_converged <- conv_status_df$Status == 0

      ## assemble tibble row
      results <- tibble(
        method = "PROC GLIMMIX",
        converged = sas_converged
      )
      return(results)
    }
  ) %>%
    group_by(method) %>%
    summarize(convergence_rate = mean(converged), .groups = "drop") %>%
    mutate(missingness = missingness_level)
}

## get the convergence rates
cr_no_missing <- get_convergence_rates_sas("none")
cr_mild_missing <- get_convergence_rates_sas("mild")
cr_moderate_missing <- get_convergence_rates_sas("moderate")
cr_high_missing <- get_convergence_rates_sas("high")
cr_all_sas <- bind_rows(
  cr_no_missing, cr_mild_missing, cr_moderate_missing, cr_high_missing
)

# FEV data: convergence time ----
mb <- microbenchmark(
  times = n_reps,
  gls = gls(
    FEV1 ~ RACE + ARMCD * AVISIT,
    data = fev_data,
    na.action = na.omit,
    correlation = nlme::corSymm(form = ~ VISITN | USUBJID),
    weights = nlme::varIdent(form = ~ 1 | VISITN)
  ),
  mmrm = mmrm(
    formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  ),
  lmer = lmer(
    FEV1 ~ RACE + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
    data = fev_data,
    control = lmerControl(check.nobs.vs.nRE = "ignore"),
    na.action = na.omit
  ),
  glmmTMB = glmmTMB(
    FEV1 ~ RACE + ARMCD * AVISIT + us(0 + AVISIT | USUBJID),
    data = fev_data,
    dispformula = ~0,
    REML = TRUE
  )
)

# construct the partial results table based on R-based MMRM procedures
partial_conv_time_tbl_fev <- mb %>%
  summary() %>%
  dplyr::arrange(median) %>%
  dplyr::select(expression = expr, median, lower = lq, upper = uq, evaluations = neval)

# BCVA data: convergence time ----
mb <- microbenchmark(
  times = n_reps,
  gls = gls(
    BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT,
    data = bcva_data,
    na.action = na.omit,
    correlation = nlme::corSymm(form = ~ VISITN | USUBJID),
    weights = nlme::varIdent(form = ~ 1 | VISITN)
  ),
  mmrm = mmrm(
    formula = BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = bcva_data
  ),
  lmer = lmer(
    BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
    data = bcva_data,
    control = lmerControl(check.nobs.vs.nRE = "ignore"),
    na.action = na.omit
  ),
  glmmTMB = glmmTMB(
    BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(0 + AVISIT | USUBJID),
    data = bcva_data,
    dispformula = ~0,
    REML = TRUE
  )
)

# construct the partial results table based on R-based MMRM procedures
partial_conv_time_tbl_bcva <- mb %>%
  summary() %>%
  dplyr::arrange(median) %>%
  dplyr::select(expression = expr, median, lower = lq, upper = uq, evaluations = neval)


# Estimations FEV ----

# compute marginal estimates for mmrm
mmrm_fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
mmrm_ests <- as.data.frame(
  emmeans(
    mmrm_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    weights = "proportional"
  )$contrasts
)$estimate

# compute marginal estimates for lmer
lmer_fit <- lmer(
  FEV1 ~ RACE + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
  data = fev_data[complete.cases(fev_data), ],
  control = lmerControl(check.nobs.vs.nRE = "ignore")
)
lmer_ests <- as.data.frame(
  emmeans(
    lmer_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    data = fev_data[complete.cases(fev_data), ],
    weights = "proportional"
  )$contrasts
)$estimate

# compute marginal estimates for gls
gls_fit <- gls(
  FEV1 ~ RACE + ARMCD * AVISIT,
  data = fev_data[complete.cases(fev_data), ],
  correlation = nlme::corSymm(form = ~ VISITN | USUBJID),
  weights = nlme::varIdent(form = ~ 1 | VISITN)
)
gls_ests <- as.data.frame(
  emmeans(
    gls_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    data = fev_data[complete.cases(fev_data), ],
    weights = "proportional",
    mode = "df.error"
  )$contrasts
)$estimate

# compute marginal estimates for glmmTMB
glmmtmb_fit <- glmmTMB(
  FEV1 ~ RACE + ARMCD * AVISIT + us(0 + AVISIT | USUBJID),
  data = fev_data,
  dispformula = ~0
)
glmmtmb_ests <- as.data.frame(
  emmeans(
    glmmtmb_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    weights = "proportional"
  )$contrasts
)$estimate


# compute the relative differences
rel_diff <- function(ests, ref) (ests - ref) / ref

# construct table for plotting
# NOTE: that convergence is assessed manually here, not all methods provide
# methods for reporting convergence
rel_diff_ests_tbl_fev <- tibble(
  parameter = factor(rep(c("Visit 1", "Visit 2", "Visit 3", "Visit 4"), 4)),
  estimator = rep(c("mmrm", "lmer", "gls", "glmmTMB"), each = 4),
  converged = rep(c(TRUE, TRUE, TRUE, TRUE), each = 4),
  rel_diff = c(
    rel_diff(mmrm_ests, proc_glimmix_ests_fev),
    rel_diff(lmer_ests, proc_glimmix_ests_fev),
    rel_diff(gls_ests, proc_glimmix_ests_fev),
    rel_diff(glmmtmb_ests, proc_glimmix_ests_fev)
  )
) %>%
  mutate(converged = factor(converged, levels = c(TRUE, FALSE)))

# Estimations BCVA ----

# compute marginal estimates for mmrm
mmrm_fit <- mmrm(
  formula = BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = bcva_data
)
mmrm_ests <- as.data.frame(
  emmeans(
    mmrm_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    weights = "proportional"
  )$contrasts
)$estimate

# compute marginal estimates for lmer
lmer_fit <- lmer(
  BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
  data = bcva_data[complete.cases(bcva_data), ],
  control = lmerControl(check.nobs.vs.nRE = "ignore")
)
lmer_ests <- as.data.frame(
  emmeans(
    lmer_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    data = bcva_data[complete.cases(bcva_data), ],
    weights = "proportional"
  )$contrasts
)$estimate

# compute marginal estimates for gls
gls_fit <- gls(
  BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT,
  data = bcva_data[complete.cases(bcva_data), ],
  correlation = nlme::corSymm(form = ~ VISITN | USUBJID),
  weights = nlme::varIdent(form = ~ 1 | VISITN)
)
gls_ests <- as.data.frame(
  emmeans(
    gls_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    data = bcva_data[complete.cases(bcva_data), ],
    weights = "proportional",
    mode = "df.error"
  )$contrasts
)$estimate

# compute marginal estimates for glmmTMB
glmmtmb_fit <- glmmTMB(
  BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(0 + AVISIT | USUBJID),
  data = bcva_data,
  dispformula = ~0
)
glmmtmb_ests <- as.data.frame(
  emmeans(
    glmmtmb_fit,
    spec = trt.vs.ctrl ~ ARMCD | AVISIT,
    weights = "proportional"
  )$contrasts
)$estimate

# construct table for plotting
# NOTE: that convergence is assessed manually here, not all methods provide
# methods for reporting convergence
rel_diff_ests_tbl_bcva <- tibble(
  parameter = factor(rep(c(paste("Visit", seq_len(10))), 4)),
  estimator = rep(c("mmrm", "lmer", "gls", "glmmTMB"), each = 10),
  converged = rep(c(TRUE, FALSE, TRUE, FALSE), each = 10),
  rel_diff = c(
    rel_diff(mmrm_ests, proc_glimmix_ests_bcva),
    rel_diff(lmer_ests, proc_glimmix_ests_bcva),
    rel_diff(gls_ests, proc_glimmix_ests_bcva),
    rel_diff(glmmtmb_ests, proc_glimmix_ests_bcva)
  )
) %>%
  mutate(converged = factor(converged, levels = c(TRUE, FALSE)))


# extract the number of patients at each visit
df_missingness <- lapply(
  dgp_data,
  function(x) {
    sapply(x, function(y) {
      table(y$AVISIT)
    })
  }
)
df_missingness <- Reduce(`+`, df_missingness) / n_reps


# Convergence rate ----
get_convergence_rates <- function(missingness_level) {
  purrr::map_df(
    get_dgp_data(missing_type = missingness_level),
    function(miss_df) {
      ## fit MMRM methods and extract convergence

      ## mmrm
      safe_mmrm <- safely(mmrm)
      mmrm_fit <- safe_mmrm(
        formula = BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
        data = miss_df
      )
      mmrm_converged <- is.null(mmrm_fit$error)
      mmrm_converged

      ## lme4
      lmer_fit <- lmer(
        BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + (0 + AVISIT | USUBJID),
        data = miss_df[complete.cases(miss_df), ],
        control = lmerControl(check.nobs.vs.nRE = "ignore")
      )
      lmer_converged <- all(!str_detect(
        lmer_fit@optinfo$conv$lme4$messages,
        "Model failed to converge"
      ))

      ## nlme
      safe_gls <- safely(gls)
      gls_fit <- safe_gls(
        BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT,
        data = miss_df[complete.cases(miss_df), ],
        correlation = nlme::corSymm(form = ~ VISITN | USUBJID),
        weights = nlme::varIdent(form = ~ 1 | VISITN)
      )
      gls_converged <- is.null(gls_fit$error)

      ## glmmTMB
      glmmtmb_fit <- glmmTMB(
        BCVA_CHG ~ BCVA_BL + RACE + ARMCD * AVISIT + us(0 + AVISIT | USUBJID),
        data = miss_df,
        dispformula = ~0
      )
      glmmtmb_converged <- glmmtmb_fit$fit$convergence == 0

      results <- tibble(
        method = c("mmrm", "lmer", "gls", "glmmTMB"),
        converged = c(
          mmrm_converged, lmer_converged, gls_converged,
          glmmtmb_converged
        )
      )

      return(results)
    }
  ) %>%
    group_by(method) %>%
    summarize(convergence_rate = mean(converged), .groups = "drop") %>%
    mutate(missingness = missingness_level)
}

## get the convergence rates
cr_no_missing <- get_convergence_rates("none")
cr_mild_missing <- get_convergence_rates("mild")
cr_moderate_missing <- get_convergence_rates("moderate")
cr_high_missing <- get_convergence_rates("high")
cr_all <- bind_rows(
  cr_no_missing, cr_mild_missing, cr_moderate_missing, cr_high_missing
)

cached_mmrm_results <- list(
  conv_time_fev = rbind(proc_glimmix_row_fev, partial_conv_time_tbl_fev),
  conv_time_bcva = rbind(proc_glimmix_row_bcva, partial_conv_time_tbl_bcva),
  rel_diff_ests_tbl_fev = rel_diff_ests_tbl_fev,
  rel_diff_ests_tbl_bcva = rel_diff_ests_tbl_bcva,
  conv_rate = rbind(cr_all_sas, cr_all),
  df_missingness = df_missingness
)

usethis::use_data(cached_mmrm_results, overwrite = TRUE)
