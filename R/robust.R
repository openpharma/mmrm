#' Dynamic Registration of `vcovCR` Methods
#'
.onLoad <- function(libname, pkgname) { # nolint
  requireNamespace("sandwich", quietly = TRUE)
  requireNamespace("clubSandwich", quietly = TRUE)
}

#' @name bread
#' @exportS3Method
#' @noRd
bread.mmrm_tmb <- function(object) {
  n_obs <- nrow(object$tmb_data$full_frame)
  sigma2_term <- as.numeric(stats::var(residuals(object, "response")) / stats::var(residuals(object, "pearson")))
  object$beta_vcov * n_obs / sigma2_term
}


#' @name sub_f
#' @keywords internal
#' @noRd
sub_f <- function (x, fac, dim) {
  function(f) switch(dim,
                     row = x[fac == f, , drop = FALSE],
                     col = x[, fac == f, drop = FALSE],
                     both = x[fac == f, fac == f, drop = FALSE])
}


#' @name matrix_list
#' @keywords internal
#' @noRd
matrix_list <- function (x, fac, dim) {
  if (is.vector(x)) {
    if (dim != "both")
      stop(paste0("Object must be a matrix in order to subset by ", dim, "."))
    x_list <- split(x, fac)
    lapply(x_list, function(x) diag(x, nrow = length(x)))
  } else {
    lapply(levels(fac), sub_f(x, fac, dim))
  }
}


#' @name matrix_power
#' @keywords internal
#' @noRd
matrix_power <- function (x, p, symmetric = TRUE, tol = -12) {
  eig <- eigen(x, symmetric = symmetric)
  val_p <- with(eig, ifelse(values > 10^tol, values^p, 0))
  with(eig, vectors %*% (val_p * t(vectors)))
}


#' @name adjust_est_mats
#' @keywords internal
#' @noRd
adjust_est_mats <- function (type, est_mats, adjustments) {
  switch(type,
         CR0 = est_mats,
         CR1 = lapply(est_mats, function(e) e * adjustments),
         CR1p = lapply(est_mats, function(e) e * adjustments),
         CR1S = lapply(est_mats, function(e) e * adjustments),
         CR2 = Map(function(e, a) e %*% a, e = est_mats, a = adjustments),
         CR3 = Map(function(e, a) e %*% a, e = est_mats, a = adjustments),
         CR4 = Map(function(e, a) a %*% e, e = est_mats, a = adjustments))
}


#' @name handle_vectors
#' @keywords internal
#' @noRd
handle_vectors <- function (x, obj) {
  if (inherits(stats::na.action(obj), "omit")) {
    x <- x[-stats::na.action(obj)]
  }
  if (!is.null(wts <- stats::weights(obj))) {
    pos_wts <- wts > 0
    if (!all(pos_wts)) x <- x[pos_wts]
  }
  return(x)
}


#' @name vcov_CR
#' @keywords internal
#' @export
#' @noRd
vcov_CR.mmrm_tmb <- function (object, cluster, type, target = NULL, inverse_var = FALSE,
                              form = "sandwich", ignore_FE = FALSE) {
  cluster <- droplevels(as.factor(cluster))
  alias <- is.na(coef(object))
  X <- object$tmb_data$x_matrix
  ngrps <- object$tmb_data$n_groups
  if (any(alias)) {
    X <- X[, !alias, drop = FALSE]
  }
  p <- NCOL(X)
  N <- NROW(X)
  cluster_length <- length(cluster)
  if (cluster_length != N) {
    cluster <- droplevels(handle_vectors(cluster, object))
    if (length(cluster) != N) {
      stop("Clustering variable must have length equal to the number of rows in the data used to fit object.")
    }
  }
  if (any(is.na(cluster)))
    stop("Clustering variable cannot have missing values.")
  J <- nlevels(cluster)
  if (J < 2)
    stop("Cluster-robust variance estimation will not work when the data only includes a single cluster.")
  X_list <- matrix_list(X, cluster, "row")

  if (ngrps == 1) {
    corr_mat <- as.matrix(object$cov / as.numeric(stats::var(residuals(object, "response")) / stats::var(residuals(object, "pearson"))))
  } else if (ngrps > 1) {
    corr_mat <- lapply(object$cov, function(x) {
      as.matrix(x / as.numeric(stats::var(residuals(object, "response")) / stats::var(residuals(object, "pearson"))))
    })
  }

  times_per_subj <- object$tmb_data$full_frame[c(object$formula_parts$subject_var, object$formula_parts$visit_var)]
  times_per_subj[[object$formula_parts$visit_var]] <- as.numeric(times_per_subj[[object$formula_parts$visit_var]])
  times_list <- split(times_per_subj[[object$formula_parts$visit_var]], times_per_subj[[object$formula_parts$subject_var]])

  if (ngrps == 1) {
    W_list <- lapply(times_list, function(x) {
      chol2inv(chol(corr_mat[x, x]))
    })} else if (ngrps > 1) {
      W_list <- lapply(seq_along(times_list), function(x) {
        grp_name <- object$tmb_data$subject_groups[x]
        chol2inv(chol(corr_mat[[grp_name]][times_list[[x]], times_list[[x]]]))
      })
    }
  XW_list <- Map(function(x, w) as.matrix(t(x) %*% w), x = X_list,
                 w = W_list)
  if (is.null(target)) {
    if (inverse_var) {
      Theta_list <- lapply(W_list, function(w) chol2inv(chol(w)))
    } else {
      if (ngrps == 1) {
        Theta_list <- lapply(times_list, function(x) {corr_mat[x, x]})
      } else if (ngrps > 1) {
        Theta_list <- lapply(seq_along(times_list), function(x) {
          grp_name <- object$tmb_data$subject_groups[x]
          corr_mat[[grp_name]][times_list[[x]], times_list[[x]]]
        })
      }
    }
  } else {
    if (!is.list(target)) {
      if (length(target) != N) {
        target <- handle_vectors(target, object)
      }
      Theta_list <- matrix_list(target, cluster, "both")
    }
    else {
      Theta_list <- target
    }
  }
  if (type %in% c("CR2", "CR4")) {
    UWU_list <- Map(function(uw, u) uw %*% u, uw = XW_list, u = X_list)
    M_U <- matrix_power(Reduce("+", UWU_list), p = -1)
  }

  adjustments <- do.call(type, args = mget(names(formals(fun = type, envir = environment(vcovCR)))), envir = environment(vcovCR))
  E_list <- adjust_est_mats(type = type, est_mats = XW_list, adjustments = adjustments)
  resid <- residuals(object)
  res_list <- split(resid, cluster)
  components <- do.call(cbind, Map(function(e, r) e %*% r,
                                   e = E_list, r = res_list))
  v_scale <- nobs(object)
  w_scale <- attr(W_list, "w_scale")
  if (is.null(w_scale))
    w_scale <- 1L
  if (form == "estfun") {
    bread <- bread(object)
    estfun <- bread %*% components
    return(estfun * (w_scale/v_scale))
  }
  meat <- tcrossprod(components) * w_scale^2/v_scale
  if (form == "sandwich") {
    bread <- bread(object)
  }
  else if (form == "meat") {
    bread <- NULL
  }
  else if (is.matrix(form)) {
    bread <- form
    form <- "sandwich"
  }
  vcov <- switch(form, sandwich = bread %*% meat %*% bread/v_scale,
                 meat = meat)
  rownames(vcov) <- colnames(vcov) <- colnames(X)
  attr(vcov, "type") <- type
  attr(vcov, "cluster") <- cluster
  attr(vcov, "bread") <- bread
  attr(vcov, "v_scale") <- v_scale
  attr(vcov, "est_mats") <- XW_list
  attr(vcov, "adjustments") <- adjustments
  attr(vcov, "target") <- Theta_list
  attr(vcov, "inverse_var") <- inverse_var
  attr(vcov, "ignore_FE") <- ignore_FE
  class(vcov) <- c("vcovCR", "clubSandwich")
  return(vcov)
}


#' @name vcovCR
#' @exportS3Method
#' @noRd
vcovCR.mmrm_tmb <- function (object, cluster, type, target, inverse_var, form = "sandwich", ...) {
  if (missing(cluster))
    cluster <- object$tmb_data$full_frame[[object$formula_parts$subject_var]]
  if (missing(target))
    target <- NULL
  if (missing(inverse_var))
    inverse_var <- is.null(target)
  vcov_CR.mmrm_tmb(object, cluster = cluster, type = type, target = target,
                   inverse_var = inverse_var, form = form)
}
