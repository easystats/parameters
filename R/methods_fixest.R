# .fixest -----------------------

#' @export
standard_error.fixest <- function(model, ...) {
  stats <- summary(model)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats$se)
  )
}

## TODO add ci_method later?

#' @export
p_value.fixest <- function(model, ...) {
  stats <- summary(model)$coeftable
  params <- insight::get_parameters(model)
  stat_col <- which(colnames(stats) %in% c("Pr(>|t|)", "Pr(>|z|)"))

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, stat_col])
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
