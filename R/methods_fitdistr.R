#' @export
model_parameters.fitdistr <- function(model,
                                      exponentiate = FALSE,
                                      verbose = TRUE,
                                      ...) {
  out <- data.frame(
    Parameter = names(model$estimate),
    Coefficient = as.vector(model$estimate),
    SE = as.vector(model$sd),
    stringsAsFactors = FALSE
  )

  # exponentiate coefficients and SE/CI, if requested
  out <- .exponentiate_parameters(out, model, exponentiate)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.fitdistr <- function(model, ...) {
  data.frame(
    Parameter = names(model$estimate),
    SE = as.vector(model$sd),
    stringsAsFactors = FALSE
  )
}
