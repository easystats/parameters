#' @export
model_parameters.ridgelm <- function(model, verbose = TRUE, ...) {
  parameters <- insight::get_parameters(model)
  parameters$Scale <- as.vector(model$scales)

  # remove all complete-missing cases
  parameters <- parameters[apply(parameters, 1, function(i) !all(is.na(i))), ]

  rownames(parameters) <- NULL

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  parameters
}
