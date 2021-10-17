
#' @export
ci.ivprobit <- ci.default



#' @export
degrees_of_freedom.ivprobit <- degrees_of_freedom.ivFixed


#' @export
standard_error.ivprobit <- function(model, ...) {
  .data_frame(
    Parameter = model$names,
    SE = as.vector(model$se)
  )
}


#' @export
p_value.ivprobit <- p_value.default


#' @export
model_parameters.ivprobit <- model_parameters.ivFixed
