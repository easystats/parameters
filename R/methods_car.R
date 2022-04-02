#' @export
model_parameters.deltaMethod <- function(model, p_adjust = NULL, verbose = TRUE, ...) {
  # tweak column names
  params <- insight::standardize_names(datawizard::rownames_as_column(model, "Parameter"))

  # find CIs
  ci_cols <- grepl("%$", colnames(params))
  cis <- as.numeric(gsub("%", "", colnames(params)[ci_cols], fixed = TRUE)) / 100
  ci <- diff(cis)

  # rename CI columns
  colnames(params)[ci_cols] <- c("CI_low", "CI_high")

  if (!is.null(p_adjust)) {
    params <- .p_adjust(params, p_adjust, model, verbose)
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci = ci,
    exponentiate = FALSE,
    bootstrap = FALSE,
    iterations = NULL,
    ci_method = "residual",
    p_adjust = p_adjust,
    summary = FALSE,
    verbose = verbose,
    ...
  )

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(params, "no_caption") <- TRUE
  params
}


#' @export
ci.deltaMethod <- function(x, ...) {
  params <- model_parameters(x, ...)
  ci <- attributes(params)$ci
  params$CI <- ci
  as.data.frame(params[c("Parameter", "CI", "CI_low", "CI_high")])
}


#' @export
standard_error.deltaMethod <- function(model, ...) {
  params <- model_parameters(model, ...)
  as.data.frame(params[c("Parameter", "SE")])
}


#' @export
p_value.deltaMethod <- function(model, ...) {
  params <- model_parameters(model, ...)
  if (is.null(params[["p"]])) {
    return(NULL)
  }
  as.data.frame(params[c("Parameter", "p")])
}
