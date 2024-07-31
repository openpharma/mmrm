#' mmrm wrapper function
#'
#' @description This function conduct analysis on simulated data using mmrm.
#'
#' @param df Input dataset.
#' @param covar_rsp response covariance structure.
#' @param covar_type covariance structure used in model.
#'
#' @return A formated results.
mmrm_wrapper_fun <- function(df, covar_rsp, covar_type, reml = TRUE) {
  # NOTE: nlme produces an error when the model fails to converge. This function
  # safely returns an error message, and allows us to check if the model
  # converged.
  safe_mmrm <- purrr::safely(mmrm::mmrm)
  rspvar <- paste0("chg", covar_rsp)
  formula <- as.formula(sprintf("%s ~ bcva_bl + strata + trt * visit + %s(visit | id)", rspvar, covar_type))
  fit_time <- microbenchmark::microbenchmark(
    # NOTE: Errors when using L-BFGS-B first
    fit <- safe_mmrm(formula = formula, data = df, optimizer = c("BFGS", "L-BFGS-B"), reml = reml),
    times = 1L
  )
  format_fit_results(fit$result, df, fit_time$time / 1e9, df$group[1])
}

#' glmmTMB wrapper function
#'
#' @description This function conduct analysis on simulated data using glmmTMB.
#'
#' @param df Input dataset.
#' @param covar_rsp response covariance structure.
#' @param covar_type covariance structure used in model.
#'
#' @return A formated results.
glmmtmb_wrapper_fun <- function(df, covar_rsp, covar_type, reml = TRUE) {
  control <- glmmTMB::glmmTMBControl(parallel = 1)
  safe_glmm <- purrr::safely(glmmTMB::glmmTMB)
  rspvar <- paste0("chg", covar_rsp)
  covvar <- switch(covar_type,
    us = "us",
    csh = "cs",
    toeph = "toep"
  )
  formula <- as.formula(sprintf("%s ~ bcva_bl + strata + trt * visit + %s(visit | id)", rspvar, covvar))
  fit_time <- microbenchmark::microbenchmark(
    fit <- safe_glmm(
      formula = formula,
      dispformula = ~0,
      data = df,
      REML = reml,
      control = control
    ),
    times = 1L
  )
  format_fit_results(fit$result, df, fit_time$time / 1e9, df$group[1])
}

#' nlme wrapper function
#'
#' @description This function conduct analysis on simulated data using nlme
#'
#' @param df Input dataset.
#' @param covar_rsp response covariance structure.
#' @param covar_type covariance structure used in model.
#'
#' @return A formated results.
nlme_wrapper_fun <- function(df, covar_rsp, covar_type, reml = TRUE) {
  # NOTE: nlme produces an error when the model fails to converge. This function
  # safely returns an error message, and allows us to check if the model
  # converged.
  safe_gls <- purrr::safely(nlme::gls)
  formula <- as.formula(sprintf("chg%s ~ bcva_bl + strata + trt * visit", covar_rsp))
  if (covar_type == "us") {
    correlation <- nlme::corSymm(form = ~ visit2 | id)
  } else if (covar_type == "csh") {
    correlation <- nlme::corCompSymm(form = ~ visit2 | id)
  } else if (covar_type == "toeph") {
    correlation <- nlme::corARMA(form = ~ visit2 | id, p = nlevels(df$visit) - 1, q = 0)
  } else {
    stop("This covariance matrix is not supported by this wrapper function.")
  }
  df$visit2 <- as.numeric(df$visit)
  fit_time <- microbenchmark::microbenchmark(
    fit <- safe_gls(
      formula,
      correlation = correlation,
      weights = nlme::varIdent(form = ~ 1 | visit),
      data = df,
      method = if (reml) "REML" else "ML",
      control = list(returnObject = FALSE)
    ),
    times = 1L
  )
  format_fit_results(fit$result, df, fit_time$time / 1e9, df$group[1])
}

analyze_data <- function(df, method, covar_rsp, covar_type, reml) {
  if (method == "mmrm") {
    ana_fun <- mmrm_wrapper_fun
  } else if (method == "glmmtmb") {
    ana_fun <- glmmtmb_wrapper_fun
  } else if (method == "nlme") {
    ana_fun <- nlme_wrapper_fun
  }
  results <- lapply(
    split(df, df$group),
    function(x) {
      x$id <- as.factor(x$id)
      cat(sprintf("Analyzing group %s\n", x$group[1]))
      ret <- ana_fun(x, covar_rsp, covar_type, reml)
      ret$method_covar <- covar_type
      ret$method <- method
      ret
    }
  )
  dplyr::bind_rows(results)
}

get_convergence <- function(obj, ...) {
  UseMethod("get_convergence")
}
get_convergence.mmrm <- function(obj, ...) {
  obj$opt_details$convergence == 0
}
get_convergence.glmmTMB <- function(obj, ...) {
  obj$fit$convergence == 0
}
get_convergence.gls <- function(obj, ...) {
  TRUE
}

get_convergence.NULL <- function(obj, ...) {
  FALSE
}

get_emmeans_output <- function(obj, ...) {
  UseMethod("get_emmeans_output")
}
get_emmeans_output.default <- function(obj, data, ...) {
  # extract emmeans output
  tryCatch(
    {
      marginal_means <- emmeans::emmeans(
        obj,
        spec = trt.vs.ctrl ~ trt | visit,
        weights = "proportional",
        data = data,
        mode = "df.error"
      )
      emmeans_df <- as.data.frame(marginal_means$contrasts)
      # compute lower and upper 95% CI
      emmeans_df <- get_95_ci(emmeans_df)
      # format to resemble SAS output
      format_emmeans_df(emmeans_df)
    },
    error = function(e) {
      NA
    }
  )
}

get_emmeans_output.NULL <- function(obj, ...) {
  NA
}

get_95_ci <- function(emmeans_df) {
  emmeans_df |>
    dplyr::mutate(
      lower = estimate - qnorm(0.975) * SE,
      upper = estimate + qnorm(0.025) * SE,
    )
}

format_emmeans_df <- function(emmeans_df) {
  emmeans_df |>
    dplyr::transmute(
      visit = visit,
      estimate = estimate,
      stderr = SE,
      df = df,
      tvalue = t.ratio,
      pvalue = p.value,
      lower = lower,
      upper = upper
    )
}

get_cov_mat_estimate <- function(obj, ...) {
  UseMethod("get_cov_mat_estimate")
}
get_cov_mat_estimate.mmrm <- function(obj, ...) {
  mmrm::VarCorr(obj)
}
get_cov_mat_estimate.NULL <- function(obj, ...) {
  NA_real_
}
get_cov_mat_estimate.glmmTMB <- function(obj, id = "id", ...) {
  mat_with_attrs <- glmmTMB::VarCorr(obj)[[c("cond", id)]]
  dim <- nrow(mat_with_attrs)
  ind <- seq_len(dim)
  mat_with_attrs[ind, ind]
}
get_cov_mat_estimate.gls <- function(obj) {
  gns <- attr(obj$modelStruct$varStruct, "groupNames")
  vw <- nlme::varWeights(obj$modelStruct$varStruct)[gns]
  vars <- (obj$sigma / vw)^2
  S <- nlme::corMatrix(obj$modelStruct$corStruct, covariate = seq(0, length(gns) - 1))
  result <- t(S * sqrt(vars)) * sqrt(vars)
  colnames(result) <- gns
  row.names(result) <- gns
  order <- match(sort(as.numeric(gns)), gns)
  result[order, order]
}

format_fit_results <- function(fit, data, fit_time, group) {
  tibble::tibble(
    converged = get_convergence(fit),
    fit_time = fit_time,
    emmeans_output = list(get_emmeans_output(fit, data = data)),
    covmat_estimates = list(get_cov_mat_estimate(fit)),
    rep = group
  )
}
