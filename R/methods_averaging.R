# classes: .averaging

#################### .averaging


#' Parameters from special models
#'
#' Parameters from special regression models not listed under one of the previous categories yet.
#'
#' @param component Model component for which parameters should be shown. May be
#'   one of `"conditional"`, `"precision"` (**betareg**),
#'   `"scale"` (**ordinal**), `"extra"` (**glmx**),
#'   `"marginal"` (**mfx**), `"conditional"` or `"full"` (for
#'   `MuMIn::model.avg()`) or `"all"`.
#' @param include_studies Logical, if `TRUE` (default), includes parameters
#'   for all studies. Else, only parameters for overall-effects are shown.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.stanreg
#' @inheritParams simulate_model
#'
#' @seealso [insight::standardize_names()] to rename
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
                                       ci = 0.95,
                                       component = c("conditional", "full"),
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       summary = getOption("parameters_summary", FALSE),
                                       keep = NULL,
                                       drop = NULL,
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
    keep_parameters = keep,
    drop_parameters = drop,
    summary = summary,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.averaging <- function(model, component = "conditional", ...) {
  component <- match.arg(component, choices = c("conditional", "full"))
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
ci.averaging <- function(x, ci = 0.95, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  .ci_generic(model = x, ci = ci, dof = Inf, component = component)
}
