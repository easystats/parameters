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
  # fixest degrees of freedom can be tricky. best to use the function by the
  # package.
  insight::check_if_installed("fixest")
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(
    tolower(method),
    choices = c("wald", "residual")
  )
  method <- switch(method,
    "wald" = "t",
    "residual" = "resid"
  )
  fixest::degrees_freedom(model, type = method)
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
