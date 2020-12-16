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
model_parameters.coeftest <- function(model,
                                      ci = .95,
                                      verbose = TRUE,
                                      ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    merge_by = "Parameter",
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff=500)
  out
}
