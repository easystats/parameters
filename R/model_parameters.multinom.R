#' Parameters from multinomial or cumulative link models
#'
#' Parameters from multinomial or cumulative link models
#'
#' @param model A model with multinomial or categorical response value.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @details Multinomial or cumulative link models, i.e. models where the
#'   response value (dependent variable) is categorical and has more than two
#'   levels, usually return coefficients for each response level. Hence, the
#'   output from \code{model_parameters()} will split the coefficient tables
#'   by the different levels of the model's response.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("brglm2")) {
#'   data("stemcell")
#'   model <- bracl(
#'     research ~ as.numeric(religion) + gender,
#'     weights = frequency,
#'     data = stemcell,
#'     type = "ML"
#'   )
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.mlm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @rdname model_parameters.mlm
#' @export
model_parameters.multinom <- model_parameters.mlm


#' @export
model_parameters.brmultinom <- model_parameters.mlm


#' @rdname model_parameters.mlm
#' @export
model_parameters.bracl <- model_parameters.mlm



#' @rdname model_parameters.mlm
#' @export
model_parameters.DirichletRegModel <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "precision"), standardize = NULL, exponentiate = FALSE, ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component", "Response")
  } else {
    merge_by <- c("Parameter", "Response")
  }

  ## TODO check merge by

  junk <- utils::capture.output(out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    ...
  ))

  out$Response[is.na(out$Response)] <- ""
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
