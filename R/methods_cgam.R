
#' @export
standard_error.cgam <- function(model, ...) {
  sc <- summary(model)
  se <- as.vector(sc$coefficients[, "StdErr"])

  params <- insight::get_parameters(model, component = "all")

  if (!is.null(sc$coefficients2)) se <- c(se, rep(NA, nrow(sc$coefficients2)))

  .data_frame(
    Parameter = params$Parameter,
    SE = se,
    Component = params$Component
  )
}


#' @rdname model_parameters.gam
#' @export
model_parameters.cgam <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  component = c("conditional", "smooth_terms", "all"),
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  verbose = TRUE,
                                  ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  out <-
    .model_parameters_generic(
      model = model,
      ci = ci,
      component = component,
      bootstrap = bootstrap,
      iterations = iterations,
      merge_by = merge_by,
      standardize = standardize,
      exponentiate = exponentiate,
      ...
    )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
