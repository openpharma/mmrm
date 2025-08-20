#' Methods for `mmrm` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param object (`mmrm`)\cr the fitted MMRM including Jacobian and call etc.
#' @param ... not used.
#' @return Depends on the method, see Details and Functions.
#'
#' @details
#' While printing the summary of (`mmrm`)\cr object, the following will be displayed:
#' 1. Formula. The formula used in the model.
#' 2. Data. The data used for analysis, including number of subjects, number of valid observations.
#' 3. Covariance. The covariance structure and number of variance parameters.
#' 4. Method. Restricted maximum likelihood(REML) or maximum likelihood(ML).
#' 5. Model selection criteria. AIC, BIC, log likelihood and deviance.
#' 6. Coefficients. Coefficients of the covariates.
#' 7. Covariance estimate. The covariance estimate(for each group).
#'    1. If the covariance structure is non-spatial, the covariance matrix of all categorical time points available
#'       in data will be displayed.
#'    2. If the covariance structure is spatial, the covariance matrix of two time points with unit distance
#'       will be displayed.
#'
#' `confint` is used to obtain the confidence intervals for the coefficients.
#' Please note that this is different from the confidence interval of difference
#' of least square means from `emmeans`.
#'
#' @name mmrm_methods
#'
#' @seealso [`mmrm_tmb_methods`], [`mmrm_tidiers`] for additional methods.
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' object <- mmrm(formula, fev_data)
NULL

#' Coefficients Table for MMRM Fit
#'
#' This is used by [summary.mmrm()] to obtain the coefficients table.
#'
#' @param object (`mmrm`)\cr model fit.
#'
#' @return Matrix with one row per coefficient and columns
#'   `Estimate`, `Std. Error`, `df`, `t value` and `Pr(>|t|)`.
#'
#' @keywords internal
h_coef_table <- function(object) {
  assert_class(object, "mmrm")

  coef_est <- component(object, "beta_est")
  coef_contrasts <- diag(x = rep(1, length(coef_est)))
  rownames(coef_contrasts) <- names(coef_est)
  coef_table <- t(apply(
    coef_contrasts,
    MARGIN = 1L,
    FUN = function(contrast) unlist(df_1d(object, contrast))
  ))
  assert_names(
    colnames(coef_table),
    identical.to = c("est", "se", "df", "t_stat", "p_val")
  )
  colnames(coef_table) <- c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")

  coef_aliased <- component(object, "beta_aliased")
  if (any(coef_aliased)) {
    names_coef_na <- names(which(coef_aliased))
    coef_na_table <- matrix(
      data = NA,
      nrow = length(names_coef_na),
      ncol = ncol(coef_table),
      dimnames = list(names_coef_na, colnames(coef_table))
    )
    coef_table <- rbind(coef_table, coef_na_table)[names(coef_aliased), ]
  }

  coef_table
}

#' @describeIn mmrm_methods summarizes the MMRM fit results.
#' @exportS3Method
#' @examples
#' # Summary:
#' summary(object)
summary.mmrm <- function(object, ...) {
  aic_list <- list(
    AIC = AIC(object),
    BIC = BIC(object),
    logLik = logLik(object),
    deviance = deviance(object)
  )
  coefficients <- h_coef_table(object)
  call <- stats::getCall(object)
  components <- component(object, c(
    "cov_type", "reml", "n_groups", "n_theta",
    "n_subjects", "n_timepoints", "n_obs",
    "beta_vcov", "varcor"
  ))
  components$method <- object$method
  components$vcov <- object$vcov
  structure(
    c(
      components,
      list(
        coefficients = coefficients,
        n_singular_coefs = sum(component(object, "beta_aliased")),
        aic_list = aic_list,
        call = call
      )
    ),
    class = "summary.mmrm"
  )
}

#' Printing MMRM Function Call
#'
#' This is used in [print.summary.mmrm()].
#'
#' @param call (`call`)\cr original [mmrm()] function call.
#' @param n_obs (`int`)\cr number of observations.
#' @param n_subjects (`int`)\cr number of subjects.
#' @param n_timepoints (`int`)\cr number of timepoints.
#'
#' @keywords internal
h_print_call <- function(call, n_obs, n_subjects, n_timepoints) {
  pass <- 0
  if (!is.null(tmp <- call$formula)) {
    cat("Formula:    ", deparse(tmp), fill = TRUE)
    rhs <- tmp[[2]]
    pass <- nchar(deparse(rhs))
  }
  if (!is.null(call$data)) {
    cat(
      "Data:       ", deparse(call$data), "(used", n_obs, "observations from",
      n_subjects, "subjects with maximum", n_timepoints, "timepoints)",
      fill = TRUE
    )
  }
  # Display the expression of weights
  if (!is.null(call$weights)) {
    cat("Weights:    ", deparse(call$weights), fill = TRUE)
  }
}

#' Printing MMRM Covariance Type
#'
#' This is used in [print.summary.mmrm()].
#'
#' @param cov_type (`string`)\cr covariance structure abbreviation.
#' @param n_theta (`count`)\cr number of variance parameters.
#' @param n_groups (`count`)\cr number of groups.
#' @keywords internal
h_print_cov <- function(cov_type, n_theta, n_groups) {
  assert_string(cov_type)
  assert_count(n_theta, positive = TRUE)
  assert_count(n_groups, positive = TRUE)
  cov_definition <- switch(cov_type,
    us = "unstructured",
    toep = "Toeplitz",
    toeph = "heterogeneous Toeplitz",
    ar1 = "auto-regressive order one",
    ar1h = "heterogeneous auto-regressive order one",
    ad = "ante-dependence",
    adh = "heterogeneous ante-dependence",
    cs = "compound symmetry",
    csh = "heterogeneous compound symmetry",
    sp_exp = "spatial exponential"
  )

  catstr <- sprintf(
    "Covariance:  %s (%d variance parameters%s)\n",
    cov_definition,
    n_theta,
    ifelse(n_groups == 1L, "", sprintf(" of %d groups", n_groups))
  )
  cat(catstr)
}

#' Printing AIC and other Model Fit Criteria
#'
#' This is used in [print.summary.mmrm()].
#'
#' @param aic_list (`list`)\cr list as part of from [summary.mmrm()].
#' @param digits (`number`)\cr number of decimal places used with [round()].
#'
#' @keywords internal
h_print_aic_list <- function(aic_list,
                             digits = 1) {
  diag_vals <- round(unlist(aic_list), digits)
  diag_vals <- format(diag_vals)
  print(diag_vals, quote = FALSE)
}

#' @describeIn mmrm_methods prints the MMRM fit summary.
#' @exportS3Method
#' @keywords internal
print.summary.mmrm <- function(x,
                               digits = max(3, getOption("digits") - 3),
                               signif.stars = getOption("show.signif.stars"), # nolint
                               ...) {
  cat("mmrm fit\n\n")
  h_print_call(x$call, x$n_obs, x$n_subjects, x$n_timepoints)
  h_print_cov(x$cov_type, x$n_theta, x$n_groups)
  cat("Method:      ", x$method, "\n", sep = "")
  cat("Vcov Method: ", x$vcov, "\n", sep = "")
  cat("Inference:   ")
  cat(ifelse(x$reml, "REML", "ML"))
  cat("\n\n")
  cat("Model selection criteria:\n")
  h_print_aic_list(x$aic_list)
  cat("\n")
  cat("Coefficients: ")
  if (x$n_singular_coefs > 0) {
    cat("(", x$n_singular_coefs, " not defined because of singularities)", sep = "")
  }
  cat("\n")
  stats::printCoefmat(
    x$coefficients,
    zap.ind = 3,
    digits = digits,
    signif.stars = signif.stars
  )
  cat("\n")
  cat("Covariance estimate:\n")
  if (is.list(x$varcor)) {
    for (v in names(x$varcor)) {
      cat(sprintf("Group: %s\n", v))
      print(round(x$varcor[[v]], digits = digits))
    }
  } else {
    print(round(x$varcor, digits = digits))
  }
  cat("\n")
  invisible(x)
}


#' @describeIn mmrm_methods obtain the confidence intervals for the coefficients.
#' @exportS3Method
#' @examples
#' # Confidence Interval:
#' confint(object)
confint.mmrm <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) {
    parm <- pnames
  }
  assert(
    check_subset(parm, pnames),
    check_integerish(parm, lower = 1L, upper = length(cf))
  )
  if (is.numeric(parm)) parm <- pnames[parm]
  assert_number(level, lower = 0, upper = 1)
  a <- (1 - level) / 2
  pct <- paste(format(100 * c(a, 1 - a), trim = TRUE, scientific = FALSE, digits = 3), "%")
  coef_table <- h_coef_table(object)
  df <- coef_table[parm, "df"]
  ses <- coef_table[parm, "Std. Error"]
  fac <- stats::qt(a, df = df)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  sefac <- ses * fac
  ci[] <- cf[parm] + c(sefac, -sefac)
  ci
}



#' Analysis of Variance for `mmrm` Fits
#'
#' If supplied only one model fit, the function will calculate and return the
#' significance of the model terms. If supplied more than one model fit,
#' standard diagnostics will be returned for each model, optionally including
#' likelihood ratio test (LRT) results for adjacent models.
#'
#' @param object (`mmrm`)\cr an `mmrm` model fit.
#' @param ... (`mmrm`)\cr optional `mmrm` model fits. If left empty, the
#'   significance of each term in `object` will be calculated.
#' @param test (`flag`)\cr indicating whether the output should include
#'   likelihood ratio test (LRT) results comparing the model fits to one
#'   another. Defaults to `TRUE`. Ignored if `...` is empty.
#' @param refit (`flag`)\cr indicating whether the models should be refitted
#'   with the dataset consisting of their shared set of observations before
#'   performing diagnostics and testing. This is ignored if the models already
#'   share the same dataset. If `refit = FALSE` and the models have different
#'   underlying data sets, an error will be thrown. Defaults to `FALSE`. Ignored
#'   if `...` is empty.
#'
#' @details When `test = FALSE` (or, when only one model is supplied), this
#'   function will process any `mmrm` fits, related or unrelated.
#'
#'   When supplying multiple models and `test = TRUE`, adjacent pairs of models
#'   are tested sequentially. In other words, the order of the supplied models
#'   matters. Furthermore, there are are multiple requirements for successful
#'   LRT. See the section "Requirements for LRT" below.
#'
#'   # Requirements for LRT
#'
#'   1. Each supplied model fit must have more degrees of freedom than
#'   the preceding model fit.
#'
#'   1. If all supplied models were estimated using maximum likelihood (ML), the
#'   models must have nested covariates in order to undergo LRT. In other words,
#'   the set of covariates for each model must be a subset of the covariates of
#'   the next model. However, if any of the supplied models were estimated using
#'   restricted maximum likelihood (REML), all models must have the same
#'   covariates.
#'
#'   1. The covariance structure of each model must be either (a) the same as
#'   that of the next model or (b) a special case of that of the next model. See
#'   the section "Covariance structure nesting hierarchy" below.
#'
#'   1. All supplied model fits must either already use the same data or be
#'   refitted using `refit = TRUE`, which refits all models to the dataset of
#'   common observations between all models' respective data sets.
#'
#'   # Covariance structure nesting hierarchy
#'
#'   ## Structured nests within unstructured
#'
#'   Tautologically, all covariance structures are special cases of an
#'   unstructured covariance, and a model *with* a covariance structure can be
#'   considered "nested" within an model *without* a covariance structure
#'   (assuming that the covariates are also nested).
#'
#'   ## Homogeneous nests within analogous heterogeneous
#'
#'   All homogeneous covariance structures are nested within their corresponding
#'   heterogeneous counterparts. For instance, the homogeneous Toeplitz
#'   covariance structure is nested within the heterogeneous Toeplitz covariance
#'   structure.
#'
#'   ## Other nested structures
#'
#'   Some different covariance structure types are also nested:
#'
#'   - First-order auto-regressive (`ar1` / `ar1h`) is nested within:
#'     - ante-dependence (`ad` / `adh`)
#'     - Toeplitz (`toep` / `toeph`)
#'   - Compound symmetry (`cs` / `csh`) is nested within Toeplitz (`toep` / `toeph`)
#'
#' @returns A data frame with a row for each supplied model. If `...` is empty,
#'   this will be the the returned value of [h_anova_single_mmrm_model()] with
#'   `object` supplied as its argument. Otherwise, the resulting data frame will
#'   have the following columns:
#'
#'   - `Model`: the sequence number of the model according to the order in which
#'   the models were supplied to this function.
#'
#'   - `refit`: logical, indicating whether or not the model was refitted. If
#'   the `refit` argument was `FALSE`, all values will be `FALSE`.
#'
#'   - `REML`: logical, indicating whether or not the model was fitted using
#'   restricted maximum likelihood (REML) estimation. If `FALSE`, the model was
#'   fitted using maximum likelihood (ML) estimation.
#'
#'   - `n_param`: the number of variance parameters in the model fit, obtained
#'   via [logLik.mmrm_tmb()].
#'
#'   - `n_coef`: the number of estimated coefficients in the model fit, obtained
#'   via [logLik.mmrm_tmb()].
#'
#'   - `df`: degrees of freedom of the model fit, obtained via
#'   [logLik.mmrm_tmb()].
#'
#'   - `AIC`: Akaike's "An Information Criterion" of the model fit, obtained via
#'   [stats::AIC()].
#'
#'   - `BIC`: the Bayesian Information Criterion of the model fit, obtained via
#'   [stats::BIC()].
#'
#'   - `logLik`: the log likelihood of the model fit, obtained via
#'   [logLik.mmrm_tmb()].
#'
#'   - `call`: the [call] that created the model fit, obtained via [component()]
#'   with `name = "call"`, which is passed to [deparse1()]. If the model was
#'   refitted (i.e., if its `refit` entry in this table is `TRUE`), this `call`
#'   will be different from the `call` component of the pre-refitted model fit.
#'
#'   The data frame will have these additional columns inserted before `call` if
#'   `test = TRUE`. Note that since each of these columns describe the results
#'   of a likelihood ratio test (LRT) between the previous row's and current
#'   row's model fits, the first element of each of these columns will be `NA`.
#'
#' - `test`: character, indicating which two model fits were compared. Values
#'  are of the form `{Model - 1} vs {Model}`.
#'
#' - `log_likelihood_ratio`: the logarithm of the likelihood ratio between the two
#'  models being compared.
#'
#' - `p_value`: the p-value of the `log_likelihood_ratio`.
#'
#' @seealso For details on the single model operation of this function, see
#'   [h_anova_single_mmrm_model()]. For details on the generic, see
#'   [stats::anova()].
#'
#' @export
#'
#' @name stats_anova
#'
#' @examples
#' # Create a few model fits, only adding terms from one to the next.
#' # Notice also that each covariance structure is a special case of the one
#' # that follows.
#' fit_sex_ar1 <-
#'   mmrm(FEV1 ~ FEV1_BL + SEX + ARMCD + ar1(AVISIT | USUBJID),
#'     data = fev_data,
#'     reml = FALSE
#'   )
#' fit_sex_race_toeph <-
#'   mmrm(
#'     FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID),
#'     data = fev_data,
#'     reml = FALSE
#'   )
#' fit_interaction_us <-
#'   mmrm(
#'     FEV1 ~ FEV1_BL + SEX * RACE + ARMCD + us(AVISIT | USUBJID),
#'     data = fev_data,
#'     reml = FALSE
#'   )
#'
#' # Single model fit, showing significance of model terms:
#' anova(fit_interaction_us)
#'
#' # Multiple model fits, with diagnostics for each fit and likelihood ratio
#' # testing (LRT) for each adjacent pair. LRT is possible because when the fits
#' # are in this order, their covariates and covariance structures are nested.
#' anova(fit_sex_ar1, fit_sex_race_toeph, fit_interaction_us)
#'
#' # We can only change the order if we forego LRT using test = FALSE.
#' anova(fit_sex_race_toeph, fit_interaction_us, fit_sex_ar1, test = FALSE)
#'
#' # Create a subset of fev_data set with the 4th visit removed.
#' fev_subset <- droplevels(fev_data[fev_data$VISITN < 4, ])
#'
#' # Recreate fit_sex_race_toeph but this time based off fev_subset:
#' fit_sex_race_toeph_sub <-
#'   mmrm(
#'     FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID),
#'     data = fev_subset,
#'     reml = FALSE
#'   )
#'
#' # If a model was created with a different data set, refit = TRUE is needed.
#' anova(fit_sex_ar1, fit_sex_race_toeph_sub, fit_interaction_us, refit = TRUE)
anova.mmrm <- function(object, ..., test = TRUE, refit = FALSE) {

  assert_class(object, "mmrm")

  fits <- list(object, ...)

  if (length(fits) == 1L) {
    out <- h_anova_single_mmrm_model(object)

  } else {

    # Ensure all objects in ... are mmrm fits.
    lapply(fits[-1L], assert_class, classes = "mmrm", .var.name = "...")

    assert_flag(test)
    assert_flag(refit)

    # If the data are not all the same and refit = TRUE, refit all with the
    # largest common data set.
    if (refit && !h_check_fits_all_data_same(fits)) {
      common_data <- h_fits_common_data(fits)

      # A logical vector for each model fit, indicating whether the model needs
      # to be refit. A model needs to be refit if its columns are not nested
      # within the columns of common_data.
      needs_refit <-
        !vapply(
          lapply(fits, h_get_minimal_fit_data),
          h_check_columns_nested,
          FUN.VALUE = logical(1L),
          data_augmented = common_data
        )

      fits[needs_refit] <-
        lapply(fits[needs_refit], h_refit_mmrm, data = common_data)
    } else {
      needs_refit <- rep_len(FALSE, length(fits))
    }

    log_likelihood_vec <- lapply(fits, logLik)

    standard_diagnostics <-
      data.frame(
        Model = seq_along(fits),
        refit = needs_refit,
        REML = vapply(fits, component, logical(1L), "reml"),
        n_param = vapply(log_likelihood_vec, attr, numeric(1L), "n_param"),
        n_coef = vapply(log_likelihood_vec, attr, numeric(1L), "n_coef"),
        df = vapply(log_likelihood_vec, attr, numeric(1L), "df"),
        AIC = vapply(fits, AIC, numeric(1L)),
        BIC = vapply(fits, BIC, numeric(1L)),
        logLik = as.numeric(log_likelihood_vec)
      )

    if (test) {

      h_assert_lrt_suitability(fits, refit, dfs = out$df, is_reml = out$REML)

      # Create labels for the pair being compared (e.g., "3 vs 4"). Force the
      # first element to be NA because a pair of models is the previous row's
      # model plus the current row's model.
      out$test <- c(NA, paste(out$Model[-length(fits)], "vs", out$Model[-1L]))

      out$log_likelihood_ratio <- c(NA, diff(out$logLik))

      out$p_value <-
        stats::pchisq(
          2 * abs(out$log_likelihood_ratio),
          df = abs(diff(out$df)),
          lower.tail = FALSE
        )
    }

    out$call <- vapply(lapply(fits, component, "call"), deparse1, character(1L))

  }

  class(out) <- union("anova.mmrm", class(out))

  out
}
