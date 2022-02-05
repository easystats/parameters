#' @export
standard_error.bayesx <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, component = "conditional", flatten = TRUE),
    SE = model$fixed.effects[, 2]
  )
}


#' @export
ci.bayesx <- function(x, ci = .95, ...) {
  .ci_generic(model = x, ci = ci, dof = Inf, component = "conditional", ...)
}


#' @export
p_value.bayesx <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, component = "conditional", flatten = TRUE),
    p = model$fixed.effects[, 4]
  )
}
