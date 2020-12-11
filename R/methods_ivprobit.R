
#' @export
ci.ivprobit <- ci.lm


#' @export
standard_error.ivprobit <- function(model, ...) {
  .data_frame(
    Parameter = model$names,
    SE = as.vector(model$se)
  )
}


#' @export
p_value.ivprobit <- function(model, ...) {
  .data_frame(
    Parameter = model$names,
    p = as.vector(model$pval)
  )
}


#' @export
model_parameters.ivprobit <- model_parameters.coeftest
