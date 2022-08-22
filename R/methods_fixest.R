# .fixest -----------------------

#' @export
standard_error.fixest <- function(model, vcov = NULL, vcov_args = NULL, ...) {
  params <- insight::get_parameters(model)

  if (!is.null(vcov)) {
    # we don't want to wrap this in a tryCatch because the `fixest` error is
    # informative when `vcov` is wrong.
    V <- insight::get_varcov(model, vcov = vcov, vcov_args = vcov_args)
    SE <- sqrt(diag(V))

  } else {
    stats <- summary(model)
    SE <- as.vector(stats$se)
  }

  .data_frame(
    Parameter = params$Parameter,
    SE = SE
  )
}


#' @export
degrees_of_freedom.fixest <- function(model, method = "wald", ...) {
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(tolower(method), choices = c("analytical", "any", "fit", "wald", "residual", "normal"))

  if (method %in% c("wald", "residual", "fit")) {
    s <- summary(model)
    vcov_scaled <- s$cov.scaled
    if (is.null(vcov_scaled)) {
      s$nobs - s$nparams
    } else {
      max(s$nobs - attr(vcov_scaled, "dof.K"), 1)
    }
  } else {
    degrees_of_freedom.default(model, method = method, ...)
  }
}




# .feglm -----------------------

#' @export
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. error"])
  )
}

## TODO add ci_method later?

#' @export
p_value.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, 4])
  )
}
