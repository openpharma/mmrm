#' Tidying Methods for `mmrm` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These methods tidy the estimates from an `mmrm` object into a
#' summary.
#'
#' @param x (`mmrm`)\cr fitted model.
#' @param conf.int (`flag`)\cr if `TRUE` columns for the lower (`conf.low`) and upper bounds
#'   (`conf.high`) of coefficient estimates are included.
#' @param conf.level (`number`)\cr defines the range of the optional confidence internal.
#' @param newdata (`data.frame` or `NULL`)\cr optional new data frame.
#' @param se_fit (`flag`)\cr whether to return standard errors of fit.
#' @param interval (`string`)\cr type of interval calculation.
#' @param type.residuals (`string`)\cr passed on to [residuals.mmrm_tmb()].
#' @param ... only used by `augment()` to pass arguments to the [predict.mmrm_tmb()] method.
#'
#' @name mmrm_tidiers
#' @aliases mmrm_tidiers
#'
#' @seealso [`mmrm_methods`], [`mmrm_tmb_methods`] for additional methods.
#'
#' @examples
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
NULL

#' @describeIn mmrm_tidiers derives tidy `tibble` from an `mmrm` object.
#' @exportS3Method
#' @examples
#' # Applying tidy method to return summary table of covariate estimates.
#' fit |> tidy()
#' fit |> tidy(conf.int = TRUE, conf.level = 0.9)
tidy.mmrm <- function(x, # nolint
                      conf.int = FALSE, # nolint
                      conf.level = 0.95, # nolint
                      ...) {
  assert_flag(conf.int)
  assert_number(conf.level, lower = 0, upper = 1)
  tbl <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(tbl) <- c("term", "estimate", "std.error", "df", "statistic", "p.value")
  coefs <- coef(x)
  if (length(coefs) != nrow(tbl)) {
    coefs <- tibble::enframe(coefs, name = "term", value = "estimate")
    tbl <- merge(coefs, tbl, by = c("term", "estimate"))
  }
  if (conf.int) {
    ci <- h_tbl_confint_terms(x, level = conf.level)
    tbl <- tibble::as_tibble(merge(tbl, ci, by = "term"))
  }
  tbl
}

#' @describeIn mmrm_tidiers derives `glance` `tibble` from an `mmrm` object.
#' @exportS3Method
#' @examples
#' # Applying glance method to return summary table of goodness of fit statistics.
#' fit |> glance()
glance.mmrm <- function(x, ...) { # nolint
  tibble::as_tibble(summary(x)$aic_list)
}

#' @describeIn mmrm_tidiers derives `augment` `tibble` from an `mmrm` object.
#' @exportS3Method
#' @examples
#' # Applying augment method to return merged `tibble` of model data, fitted and residuals.
#' fit |> augment()
#' fit |> augment(interval = "confidence")
#' fit |> augment(type.residuals = "pearson")
augment.mmrm <- function(x, # nolint
                         newdata = NULL,
                         interval = c("none", "confidence", "prediction"),
                         se_fit = (interval != "none"),
                         type.residuals = c("response", "pearson", "normalized"), # nolint
                         ...) {
  type.residuals <- match.arg(type.residuals) # nolint
  resid_df <- NULL
  if (is.null(newdata)) {
    newdata <- stats::get_all_vars(x, data = stats::na.omit(x$data))
    resid_df <- data.frame(
      .rownames = rownames(newdata),
      .resid = unname(residuals(x, type = type.residuals))
    )
  }
  interval <- match.arg(interval)

  tbl <- h_newdata_add_pred(
    x,
    newdata = newdata,
    se_fit = se_fit,
    interval = interval,
    ...
  )
  if (!is.null(resid_df)) {
    tbl <- merge(tbl, resid_df, by = ".rownames")
    tbl$.rownames <- as.numeric(tbl$.rownames)
    tbl <- tbl[order(tbl$.rownames), , drop = FALSE]
  }
  tibble::as_tibble(tbl)
}

#' Extract `tibble` with Confidence Intervals and Term Names
#'
#' This is used in [tidy.mmrm()].
#'
#' @param x (`mmrm`)\cr fit object.
#' @param ... passed to [stats::confint()], hence not used at the moment.
#'
#' @return A `tibble` with `term`, `conf.low`, `conf.high` columns.
#'
#' @keywords internal
h_tbl_confint_terms <- function(x, ...) {
  df <- stats::confint(x, ...)
  tbl <- tibble::as_tibble(df, rownames = "term", .name_repair = "minimal")
  names(tbl) <- c("term", "conf.low", "conf.high")
  tbl
}

#' Add Prediction Results to New Data
#'
#' This is used in [augment.mmrm()].
#'
#' @param x (`mmrm`)\cr fit.
#' @param newdata (`data.frame`)\cr data to predict.
#' @param se_fit (`flag`)\cr whether to return standard error of prediction,
#'   can only be used when `interval` is not "none".
#' @param interval (`string`)\cr type of interval.
#' @param ... passed to [predict.mmrm_tmb()].
#'
#' @return The `newdata` as a `tibble` with additional columns `.fitted`,
#'   `.lower`, `.upper` (if interval is not `none`) and `.se.fit` (if `se_fit`
#'   requested).
#'
#' @keywords internal
h_newdata_add_pred <- function(x,
                               newdata,
                               se_fit,
                               interval,
                               ...) {
  assert_class(x, "mmrm")
  assert_data_frame(newdata)
  assert_flag(se_fit)
  assert_string(interval)
  if (interval == "none") {
    assert_false(se_fit)
  }

  tbl <- h_df_to_tibble(newdata)
  pred_results <- predict(
    x,
    newdata = newdata,
    na.action = stats::na.pass,
    se.fit = se_fit,
    interval = interval,
    ...
  )
  if (interval == "none") {
    assert_numeric(pred_results)
    tbl$.fitted <- unname(pred_results)
  } else {
    assert_matrix(pred_results)
    tbl$.fitted <- unname(pred_results[, "fit"])
    tbl$.lower <- unname(pred_results[, "lwr"])
    tbl$.upper <- unname(pred_results[, "upr"])
  }
  if (se_fit) {
    tbl$.se.fit <- unname(pred_results[, "se"])
  }
  tbl
}

#' Coerce a Data Frame to a `tibble`
#'
#' This is used in [h_newdata_add_pred()].
#'
#' @details This is only a thin wrapper around [tibble::as_tibble()], except
#' giving a useful error message and it checks for `rownames` and adds them
#' as a new column `.rownames` if they are not just a numeric sequence as
#' per the [tibble::has_rownames()] decision.
#'
#' @param data (`data.frame`)\cr what to coerce.
#'
#' @return The `data` as a `tibble`, potentially with a `.rownames` column.
#'
#' @keywords internal
h_df_to_tibble <- function(data) {
  tryCatch(tbl <- tibble::as_tibble(data), error = function(cnd) {
    stop("Could not coerce data to `tibble`. Try explicitly passing a",
      "dataset to either the `data` or `newdata` argument.",
      call. = FALSE
    )
  })
  if (tibble::has_rownames(data)) {
    tbl <- tibble::add_column(tbl, .rownames = rownames(data), .before = TRUE)
  }
  tbl
}
