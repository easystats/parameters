# classes: .averaging

#################### .averaging


#' Parameters from special models
#'
#' Parameters from special regression models not listed under one of the previous categories yet.
#'
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("brglm2", quietly = TRUE)) {
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
#' @export
model_parameters.averaging <- function(model,
                                       ci = .95,
                                       component = c("conditional", "full"),
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       verbose = TRUE,
                                       ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    merge_by = "Parameter",
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @rdname standard_error
#' @export
standard_error.averaging <- function(model, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = as.vector(s[, 3])
  )
}


#' @importFrom insight get_parameters
#' @rdname p_value.DirichletRegModel
#' @export
p_value.averaging <- function(model, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = as.vector(s[, 5])
  )
}


#' @export
ci.averaging <- function(x, ci = .95, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}
