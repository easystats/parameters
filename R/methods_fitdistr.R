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

  if (exponentiate) out <- .exponentiate_parameters(out, model)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
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
