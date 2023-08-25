#' @export
generics::tidy
#' @export
generics::glance
#' @export
generics::augment

#' Tidying methods for an `mmrm` objects
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
#' @param newdata (`NULL` or `data.frame`)\cr optional new data frame.
#' @param se_fit (`flag`)\cr whether to return standard errors of fit.
#' @param interval (`string`)\cr type of interval calculation.
#' @param type.residuals (`string`)\cr passed on to [residuals.mmrm_tmb()].
#' @param \dots arguments passed on to the [predict.mmrm_tmb()] method.
#'
#' @name mmrm_tidiers
#' @aliases mmrm_tidiers
#'
#' @examples
#' fit <- mmrm(
#'    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'    data = fev_data
#'    )
#'
#'   # Applying tidy method to return summary table of covariate estimates.
#'     fit |> tidy()
#'     fit |> tidy(conf.int = TRUE)
#'     fit |> tidy(conf.int = TRUE, conf.level = 0.9)
#'
#'   # Applying glance method to return summary table of goodness of fit statistics.
#'     fit |> glance()
#'
#'   # Applying augment method to return merged tibble of model data, fitted and residuals.
#'     fit |> augment()
#'     fit |> augment(interval = 'confidence')
#'     fit |> augment(interval = 'prediction')
#'
#'     fit |> augment(type.residuals='pearson')
#'     fit |> augment(type.residuals='normalized')
NULL

#' @describeIn mmrm_tidiers Method to derive tidy tibble from an `mmrm` object
#' @export
tidy.mmrm <- function (x, conf.int = FALSE, conf.level = 0.95) {
  assert_logical(conf.int)
  assert_numeric(conf.level)
  ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "df", "statistic", "p.value")
  coefs <- coef(x)
  if (length(coefs) != nrow(ret)) {
    coefs <- tibble::enframe(coefs, name = "term", value = "estimate")
    ret <- merge(coefs, ret, by = c("term", "estimate"))
  }
  if (conf.int) {
    ci <- h_mmrm_confint_terms(x, level = conf.level)
    ret <- tibble::as_tibble(merge(ret, ci, by = "term"))
  }
  ret
}

#' @describeIn mmrm_tidiers Method to derive glance tibble from an `mmrm` object
#' @export
glance.mmrm <- function(x) {
  tibble::as_tibble(summary(x)$aic_list)
}

#' @describeIn mmrm_tidiers Method to derive augment tibble from an `mmrm` object
#' @export
augment.mmrm <- function(x, newdata = NULL,
                         interval = c("none", "confidence", "prediction"),
                         se_fit = interval!='none',
                         type.residuals = c("response", "pearson", "normalized"), ...) {
  resid <- NULL
  typeresiduals <- match.arg(type.residuals, choices = c("response", "pearson", "normalized"))
  if (missing(newdata)) {
    newdata <- stats::get_all_vars(x, data = stats::na.omit(x$data))
    resid <- data.frame(.rownames = rownames(newdata), resid = unname(residuals(x, type = type.residuals)))
  }
  interval <- match.arg(interval, choices = c("none", "confidence", "prediction"))

  ret <- h_mmrm_augment_newdata(x, newdata = newdata, se_fit = se_fit, interval = interval, ...)

  if (!is.null(resid)) {
    ret <- merge(ret, resid, by = '.rownames')
    ret$.rownames <- as.numeric(ret$.rownames)
    ret <- ret[order(ret$.rownames),]
  }

  tibble::as_tibble(ret)
}


h_mmrm_confint_terms <- function(x, ...){
  ci <- suppressMessages(stats::confint(x, ...))
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(coef(x))[1]
  }
  ci <- tibble::as_tibble(ci, rownames = "term", .name_repair = "minimal")
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}

h_mmrm_augment_newdata <- function(x, newdata, se_fit, interval = NULL, ...) {

  if (is.null(interval)){
    interval <- 'none'
  }

  if (interval == 'none'){
    se_fit <- FALSE
  }

  df <- h_augment_tibble(newdata)
  pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = se_fit, interval = interval, ...)

  if(interval=='none'){
    df$.fitted <- unname(pred_obj)
  }else{
    df$.fitted <- unname(pred_obj[, "fit"])
    df$.lower  <- unname(pred_obj[, "lwr"])
    df$.upper  <- unname(pred_obj[, "upr"])
  }

  if (se_fit) {
    se_idx <- which(colnames(pred_obj) %in% c("se.fit", "se"))
    df$.se.fit <- as.numeric(pred_obj[,se_idx])
  }

  df
}

h_augment_tibble <- function(data) {
  if (inherits(data, "matrix") & is.null(colnames(data))) {
    stop("The supplied `data`/`newdata` argument was an unnamed matrix. ",
         "Please supply a matrix or dataframe with column names.")
  }
  tryCatch(df <- tibble::as_tibble(data), error = function(cnd) {
    stop("Could not coerce data to `tibble`. Try explicitly passing a",
         "dataset to either the `data` or `newdata` argument.",
         call. = FALSE)
  })
  if (tibble::has_rownames(data)) {
    df <- tibble::add_column(df, .rownames = rownames(data),.before = TRUE)
  }
  df
}
