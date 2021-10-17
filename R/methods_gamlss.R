#################### .gamlss ------

#' @export
model_parameters.gamlss <- model_parameters.gam


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
