
#' @export
ci.logistf <- ci.glm


#' @export
standard_error.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))
  se <- sqrt(diag(s$var))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(s$coefficients)),
    SE = as.vector(se)
  )
}


#' @export
p_value.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(s$prob)),
    p = as.vector(s$prob)
  )
}


#' @export
model_parameters.logistf <- model_parameters.glm
