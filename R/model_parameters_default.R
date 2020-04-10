#' Parameters from (General) Linear Models
#'
#' Extract and compute indices and measures to describe parameters of (general) linear models (GLMs).
#'
#' @param model Model object.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param bootstrap Should estimates be based on bootstrapped model? If \code{TRUE}, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply (see also \code{\link[=bootstrap_parameters]{bootstrap_parameters()}}).
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootstrapped frequentist models.
#' @param standardize The method used for standardizing the parameters. Can be \code{"refit"}, \code{"posthoc"}, \code{"smart"}, \code{"basic"} or \code{NULL} (default) for no standardization. See 'Details' in \code{\link[effectsize]{standardize_parameters}}. Note that robust estimation (i.e. \code{robust=TRUE}) of standardized parameters only works when \code{standardize="refit"}.
#' @param exponentiate Logical, indicating whether or not to exponentiate the the coefficients (and related confidence intervals). This is typical for, say, logistic regressions, or more generally speaking: for models with log or logit link. \strong{Note:} standard errors are also transformed (by multiplying the standard errors with the exponentiated coefficients), to mimic behaviour of other software packages, such as Stata.
#' @param robust Logical, if \code{TRUE}, robust standard errors are calculated (if possible), and confidence intervals and p-values are based on these robust standard errors. Additional arguments like \code{vcov_estimation} or \code{vcov_type} are passed down to other methods, see \code{\link[=standard_error_robust]{standard_error_robust()}} for details.
#' @param component Model component for which parameters should be shown. May be one of \code{"conditional"}, \code{"precision"} (\pkg{betareg}), \code{"scale"} (\pkg{ordinal}), \code{"extra"} (\pkg{glmx}) or \code{"all"}.
#' @param p_adjust Character vector, if not \code{NULL}, indicates the method to adjust p-values. See \code{\link[stats]{p.adjust}} for details.
#' @param ... Arguments passed to or from other methods. For instance, when \code{bootstrap = TRUE}, arguments like \code{ci_method} are passed down to \code{\link[bayestestR]{describe_posterior}}.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model)
#'
#' # bootstrapped parameters
#' model_parameters(model, bootstrap = TRUE)
#'
#' # standardized parameters
#' model_parameters(model, standardize = "refit")
#'
#' # different p-value style in output
#' model_parameters(model, p_digits = 5)
#' model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")
#'
#' # logistic regression model
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#'
#' # show odds ratio / exponentiated coefficients
#' model_parameters(model, exponentiate = TRUE)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.default <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, robust = FALSE, p_adjust = NULL, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    effects = "fixed",
    robust = robust,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



.model_parameters_generic <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, merge_by = "Parameter", standardize = NULL, exponentiate = FALSE, effects = "fixed", robust = FALSE, df_method = NULL, p_adjust = NULL, ...) {
  # to avoid "match multiple argument error", check if "component" was
  # already used as argument and passed via "...".
  mc <- match.call()
  comp_argument <- parse(text = .safe_deparse(mc))[[1]]$component

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- if (is.null(comp_argument)) {
      .extract_parameters_generic(model, ci = ci, component = "conditional", merge_by = merge_by, standardize = standardize, effects = effects, robust = robust, df_method = df_method, p_adjust = p_adjust, ...)
    } else {
      .extract_parameters_generic(model, ci = ci, merge_by = merge_by, standardize = standardize, effects = effects, robust = robust, df_method = df_method, p_adjust = p_adjust, ...)
    }
  }

  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}




# other special cases ------------------------------------------------


#' @export
model_parameters.averaging <- function(model, ci = .95, component = c("conditional", "full"), exponentiate = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    merge_by = "Parameter",
    exponentiate = exponentiate,
    component = component,
    effects = "fixed",
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



#' @rdname model_parameters.default
#' @export
model_parameters.betareg <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("conditional", "precision", "all"), standardize = NULL, exponentiate = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



#' @rdname model_parameters.default
#' @export
model_parameters.clm2 <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "scale"), standardize = NULL, exponentiate = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
model_parameters.clmm2 <- model_parameters.clm2


#' @rdname model_parameters.default
#' @export
model_parameters.glmx <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "extra"), standardize = NULL, exponentiate = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
