#' Parameters from Bayesian Models
#'
#' Parameters from Bayesian models.
#'
#' @param model Bayesian model (including SEM from \pkg{blavaan}. May also be
#'   a data frame with posterior samples.
#' @param ci Credible Interval (CI) level. Default to 0.89 (89\%). See
#'   \code{\link[bayestestR]{ci}} for further details.
#' @param group_level Logical, for multilevel models (i.e. models with random
#'   effects) and when \code{effects = "all"} or \code{effects = "random"},
#'   include the parameters for each group level from random effects. If
#'   \code{group_level = FALSE} (the default), only information on SD and COR
#'   are shown.
#' @param parameters Character containing a regular expression pattern
#'   that describes the parameters that should be returned from the data frame, or
#'   a named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If \code{parameters} is a character vector, every
#'   parameter in the \emph{"Parameters"} column that matches the regular expression
#'   in \code{parameters} will be selected from the returned data frame. Furthermore,
#'   if \code{parameters} has more than one element, these will be merged with an
#'   \code{OR} operator into a regular expression pattern like this: \code{"(one|two|three)"}.
#'   If \code{parameters} is a named list of regular expression patterns, the names
#'   of the list-element should equal the column name where selection should be
#'   applied. This is useful for model objects where \code{model_parameters()}
#'   returns multiple columns with parameter components, like in
#'   \code{\link{model_parameters.lavaan}}. For \strong{Bayesian models} from
#'   packages like \pkg{brms} or \code{rstanarm}, please note that the regular
#'   expression pattern should match the parameter names as they are stored in
#'   the returned data frame, which probably is different from how the parameter
#'   names are actually printed! To find out the exact parameter names, look at
#'   the \code{$Parameter} column from object returned by \code{model_parameters()}.
#' @inheritParams model_parameters.default
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams insight::get_parameters
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note When \code{standardize = "refit"}, columns \code{diagnostic},
#'   \code{bf_prior} and \code{priors} refer to the \emph{original}
#'   \code{model}. If \code{model} is a data frame, arguments \code{diagnostic},
#'   \code{bf_prior} and \code{priors} are ignored. \cr \cr There is also a
#'   \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method}
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' \dontrun{
#' library(parameters)
#' if (require("rstanarm")) {
#'   model <- stan_glm(
#'     Sepal.Length ~ Petal.Length * Species,
#'     data = iris, iter = 500, refresh = 0
#'   )
#'   model_parameters(model)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.stanreg <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = .89,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 1.0,
                                     bf_prior = NULL,
                                     diagnostic = c("ESS", "Rhat"),
                                     priors = TRUE,
                                     effects = "fixed",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
                                     parameters = NULL,
                                     verbose = TRUE,
                                     ...) {


  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    diagnostic = diagnostic,
    priors = priors,
    effects = effects,
    standardize = standardize,
    filter_parameters = parameters,
    verbose = verbose,
    ...
  )

  if (effects != "fixed") {
    random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!Sigma\\[)(.*)", params$Parameter, perl = TRUE))
    if (length(random_effect_levels) && isFALSE(group_level)) params <- params[-random_effect_levels, ]
  }

  params <- .add_pretty_names(params, model)
  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    ci_method = ci_method,
    verbose = verbose,
    ...
  )

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_stan", "parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
model_parameters.stanmvreg <- function(model,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = .89,
                                       ci_method = "hdi",
                                       test = "pd",
                                       rope_range = "default",
                                       rope_ci = 1.0,
                                       bf_prior = NULL,
                                       diagnostic = c("ESS", "Rhat"),
                                       priors = TRUE,
                                       effects = "fixed",
                                       standardize = NULL,
                                       parameters = NULL,
                                       verbose = TRUE,
                                       ...) {


  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    diagnostic = diagnostic,
    priors = priors,
    effects = effects,
    standardize = standardize,
    filter_parameters = parameters,
    verbose = verbose,
    ...
  )

  params$Parameter <- gsub("^(.*)\\|(.*)", "\\2", params$Parameter)
  params <- .add_pretty_names(params, model)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.stanreg <- standard_error.brmsfit


#' @export
standard_error.mvstanreg <- standard_error.brmsfit


#' @export
p_value.stanreg <- p_value.BFBayesFactor
