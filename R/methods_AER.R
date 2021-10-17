# classes: .tobit

# The `AER::ivreg` is being spun off to a separate package. The methods in
# `methods_ivreg.R` should work for objects produce by `AER`.


#################### .tobit ------


#' @export
p_value.tobit <- function(model, method = NULL, ...) {
  params <- insight::get_parameters(model)
  p <- p_value.default(model, method = method, ...)
  p[p$Parameter %in% params$Parameter, ]
}


#' @export
simulate_model.tobit <- simulate_model.default


#' @export
standard_error.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  std.error <- standard_error.default(model, ...)
  std.error[std.error$Parameter %in% params$Parameter, ]
}
