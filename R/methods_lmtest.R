#' @export
degrees_of_freedom.coeftest <- function(model, ...) {
  attributes(model)$df
}

#' @export
ci.coeftest <- ci.default

#' @export
p_value.coeftest <- function(model, ...) {
  .data_frame(
    Parameter = .remove_backticks_from_string(row.names(model)),
    p = model[, 4]
  )
}

#' @export
standard_error.coeftest <- function(model, ...) {
  .data_frame(
    Parameter = .remove_backticks_from_string(row.names(model)),
    SE = model[, "Std. Error"]
  )
}

#' @export
model_parameters.coeftest <- model_parameters.ivFixed
