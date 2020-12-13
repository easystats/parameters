# classes: .tobit, .gamlss

# The `AER::ivreg` is being spun off to a separate package. The methods in
# `methods_ivreg.R` should work for objects produce by `AER`.


#################### .tobit ------

#' @include methods_gamlss.R
#' @export
ci.tobit <- ci.gamlss


#' @export
p_value.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  p <- p_value.default(model, ...)
  p[p$Parameter %in% params$Parameter, ]
}


#' @include simulate_model.R
#' @export
simulate_model.tobit <- simulate_model.default


#' @export
standard_error.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  std.error <- standard_error.default(model, ...)
  std.error[std.error$Parameter %in% params$Parameter, ]
}




#################### .gamlss ------

#' @export
standard_error.gamlss <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  .data_frame(
    Parameter = parms$Parameter,
    SE = as.vector(cs[, 2]),
    Component = parms$Component
  )
}


#' @export
p_value.gamlss <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))
  .data_frame(
    Parameter = parms$Parameter,
    p = as.vector(cs[, 4]),
    Component = parms$Component
  )
}


#' @export
model_parameters.gamlss <- model_parameters.gam
