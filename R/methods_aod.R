# classes: .glimML


## TODO add ci_method later?


#################### .glimML ------

#' @title Parameters from special models
#' @name model_parameters.glimML
#'
#' @description
#' Parameters from special regression models not listed under one of the
#' previous categories yet.
#'
#' @param component Model component for which parameters should be shown. May be
#' one of `"conditional"`, `"precision"` (e.g. **betareg**), `"scale"` (e.g.
#' **ordinal**), `"extra"` (e.g. **glmx**), `"marginal"` (e.g. **mfx**),
#' `"conditional"` or `"full"` (for `MuMIn::model.avg()`) or `"all"`. See section
#' _Model components_ for an overview of possible options for `component`.
#' @param include_studies Logical, if `TRUE` (default), includes parameters
#'   for all studies. Else, only parameters for overall-effects are shown.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.stanreg
#' @inheritParams simulate_model
#'
#' @seealso [insight::standardize_names()] to rename columns into a consistent,
#' standardized naming scheme.
#'
#' @inheritSection model_parameters.zcpglm Model components
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
model_parameters.glimML <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = "conditional",
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    summary = getOption("parameters_summary", FALSE),
                                    include_info = getOption("parameters_info", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- insight::validate_argument(
    component,
    c("conditional", "random", "dispersion", "all")
  )
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  # dispersion is just an alias...
  if (component == "dispersion") {
    component <- "random"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  se <- s[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    SE = as.vector(se)
  )
}


#' @export
p_value.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  p <- s[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    p = as.vector(p)
  )
}
