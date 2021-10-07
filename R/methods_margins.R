#' @export
model_parameters.margins <- function(model, ci = .95, exponentiate = FALSE, p_adjust = NULL, verbose = TRUE, ...) {
  # Parameters, Estimate and CI
  params <- insight::get_parameters(model)
  params <- .data_frame(
    params,
    SE = summary(model)$SE
  )

  # CI
  params <- merge(params, ci(model, ci = ci), by = "Parameter", sort = FALSE)

  # Statistic
  statistic <- insight::get_statistic(model)
  params <- merge(params, statistic, by = "Parameter", sort = FALSE)

  # p-value
  params <- .data_frame(params, p = summary(model)$p)

  # ==== Renaming

  if ("Statistic" %in% names(params)) {
    names(params) <- gsub("Statistic", gsub("(-|\\s)statistic", "", attr(statistic, "statistic", exact = TRUE)), names(params))
    names(params) <- gsub("chi-squared", "Chi2", names(params))
  }
  names(params) <- gsub("(c|C)hisq", "Chi2", names(params))
  names(params) <- gsub("Estimate", "Coefficient", names(params))

  # ==== adjust p-values?

  if (!is.null(p_adjust)) {
    params <- .p_adjust(params, p_adjust, model, verbose)
  }

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
ci.margins <- function(x, ci = .95, ...) {
  .ci_generic(model = x, ci = ci, dof = Inf, ...)
}


#' @export
standard_error.margins <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    SE = summary(model)$SE
  )
}


#' @export
p_value.margins <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = summary(model)$p
  )
}


#' @export
format_parameters.margins <- function(model, ...) {
  NULL
}
