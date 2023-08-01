#' @importFrom generics tidy glance augment
#' @export
generics::tidy
#' @export
generics::glance
#' @export
generics::augment

#' @title Tidying methods for an mmrm model
#' @description These methods tidy the estimates from an mmrm_tmb object into a
#' summary
#' @param x Fitted model object from the mmrm package
#' @param conf.int If TRUE columns for the lower (conf.low) and upper bounds
#'   (conf.high) of coefficient estimates are included., Default: FALSE
#' @param conf.level Defines the range of the confidence internal.
#'   Only used if conf.int = TRUE, Default: 0.95
#' @param newdata data.frame, new data frame, Default: NULL
#' @param se_fit logical, return standard errors of fit,
#'   Default: interval!='none'
#' @param interval type of interval calculation. Can be abbreviated,
#'   Default: c("none", "confidence", "prediction")
#' @param type.residuals Passed on to residuals.mmrm_tmb().
#'   Default: c("response", "pearson", "normalized")
#' @param \dots arguments passed on to predict method
#' @return All tidying methods return a tibble.
#'   The structure depends on the method chosen.
#' @examples
#' if(interactive()){
#'
#'   # fit a model
#'     fit <- mmrm(FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID), data = fev_data)
#'
#'   # tidy method
#'     fit |> tidy()
#'     fit |> tidy(conf.int = TRUE)
#'     fit |> tidy(conf.int = TRUE, conf.level = 0.9)
#'
#'   # glance method
#'     fit |> glance()
#'
#'   # augment method
#'     fit |> augment()
#'     fit |> augment(interval = 'confidence')
#'     fit |> augment(interval = 'prediction')
#'
#'     fit |> augment(type.residuals='pearson')
#'     fit |> augment(type.residuals='normalized')
#'  }
#' @seealso
#'  \code{\link[tibble]{as_tibble}}, \code{\link[tibble]{enframe}}
#' @rdname tidiers
#' @export
#' @importFrom tibble as_tibble enframe
tidy.mmrm <- function (x, conf.int = FALSE, conf.level = 0.95) {
    ret <- summary(x)$coefficients  |> tibble::as_tibble(rownames = "term")
    colnames(ret) <- c("term", "estimate", "std.error", "df", "statistic", "p.value")
    coefs <- coef(x)
        if (length(coefs) != nrow(ret)) {
            coefs <- coefs |> tibble::enframe(name = "term", value = "estimate")
            ret <- merge(coefs, ret, by = c("term", "estimate"))
        }
        if (conf.int) {
            ci <- mmrm_confint_terms(x, level = conf.level)
            ret <- merge(ret, ci, by = "term") |> tibble::as_tibble()
        }
        ret
}

#' @rdname tidiers
#' @export
#' @importFrom tibble as_tibble enframe
glance.mmrm <- function (x) {
  tibble::as_tibble(summary(x)$aic_list)
}

#' @rdname tidiers
#' @export
#' @importFrom tibble as_tibble
augment.mmrm <- function(x, newdata = NULL,
                         interval = c("none", "confidence", "prediction"),
                         se_fit = interval!='none',
                         type.residuals = c("response", "pearson", "normalized"), ...) {
  .resid <- NULL
  type.residuals <- match.arg(type.residuals, choices = c("response", "pearson", "normalized"))
  if (missing(newdata)) {
    newdata <- mmrm_getData(x)
    .resid <- data.frame(.rownames = rownames(newdata), .resid = residuals(x, type = type.residuals) |> unname())
  }
  interval <- match.arg(interval, choices = c("none", "confidence", "prediction"))

  ret <- mmrm_augment_newdata(x, newdata = newdata, .se_fit = se_fit, interval = interval)

  if (!is.null(.resid)) {
    ret <- merge(ret, .resid, by = '.rownames')
    ret$.rownames <- as.numeric(ret$.rownames)
    ret <- ret[order(ret$.rownames),]
  }

  ret |> tibble::as_tibble()
}


#' @importFrom tibble as_tibble
mmrm_confint_terms <- function (x, ...)
{
    ci <- suppressMessages(confint(x, ...))
    if (is.null(dim(ci))) {
        ci <- matrix(ci, nrow = 1)
        rownames(ci) <- names(coef(x))[1]
    }
    ci <- tibble::as_tibble(ci, rownames = "term", .name_repair = "minimal")
    names(ci) <- c("term", "conf.low", "conf.high")
    ci
}


mmrm_getData <- function(x){
  get_all_vars(x, data = na.omit(x$data))
}

#' @importFrom tibble is_tibble
has_rownames <- function (df) {
  if (tibble::is_tibble(df)) {
    return(FALSE)
  }
  any(rownames(df) != as.character(1:nrow(df)))
}

#' @importFrom tibble add_column
as_augment_tibble <- function (data) {
  if (inherits(data, "matrix") & is.null(colnames(data))) {
    stop("The supplied `data`/`newdata` argument was an unnamed matrix. ",
         "Please supply a matrix or dataframe with column names.")
  }
  tryCatch(df <- as_tibble(data), error = function(cnd) {
    stop("Could not coerce data to `tibble`. Try explicitly passing a",
         "dataset to either the `data` or `newdata` argument.",
         call. = FALSE)
  })
  if (has_rownames(data)) {
    df <- tibble::add_column(df, .rownames = rownames(data),
                             .before = TRUE)
  }
  df
}

#' @importFrom rlang as_label f_lhs
mmrm_augment_newdata <- function (x, data, newdata, .se_fit, interval = NULL, ...) {

  passed_newdata <- !is.null(newdata)
  df <- if (passed_newdata)
    newdata
  else data
  df <- as_augment_tibble(df)
  x$terms <- terms(x$formula_parts$formula)

  if (!is.null(x$terms) & inherits(x$terms, "formula")) {
    has_response <- rlang::as_label(rlang::f_lhs(x$terms)) %in%
      names(df) || all.vars(x$terms)[1] %in% names(df)
  }
  else {
    has_response <- FALSE
  }
  if (.se_fit) {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass,
                        se.fit = .se_fit, interval = interval, ...)
    if (is.null(interval) || interval == "none") {
      df$.fitted <- pred_obj$fit |> unname()
    }
    else {
      df$.fitted <- pred_obj[, "fit"]
      df$.lower <- pred_obj[, "lwr"]
      df$.upper <- pred_obj[, "upr"]
    }
    se_idx <- which(colnames(pred_obj) %in% c("se.fit", "se"))
    df$.se.fit <- pred_obj[[se_idx]]
  }
  else if (!is.null(interval) && interval != "none") {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = .se_fit, interval = interval, ...)
    df$.fitted <- pred_obj[, "fit"]
    df$.lower <- pred_obj[, "lwr"]
    df$.upper <- pred_obj[, "upr"]
  }
  else if (passed_newdata) {
    if (is.null(interval) || interval == "none") {

      df$.fitted <- predict(x, newdata = newdata, na.action = na.pass, se.fit = FALSE, ...) |> unname()
    }
    else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, se.fit = .se_fit, ...)
      df$.fitted <- pred_obj[, "fit"]
      df$.lower <- pred_obj[, "lwr"]
      df$.upper <- pred_obj[, "upr"]
    }
  }
  else {
    if (is.null(interval) || interval == "none") {
      df$.fitted <- predict(x, na.action = na.pass, ...) |> unname()
    }
    else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, se.fit = .se_fit, ...)
      df$.fitted <- pred_obj[, "fit"]
      df$.lower <- pred_obj[, "lwr"]
      df$.upper <- pred_obj[, "upr"]
    }
  }

  df
}
