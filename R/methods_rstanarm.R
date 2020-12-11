#' @export
standard_error.stanreg <- function(model,
                                   effects = c("fixed", "random"),
                                   component = c("all", "conditional", "zi", "zero_inflated"),
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  params <- insight::get_parameters(model, effects = effects, component = component, ...)

  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}

#' @export
standard_error.mvstanreg <- standard_error.stanreg


#' p-values for Bayesian Models
#'
#' This function attempts to return, or compute, p-values of Bayesian models.
#'
#' @param model A statistical model.
#' @inheritParams p_value
#'
#' @details For Bayesian models, the p-values corresponds to the \emph{probability of direction} (\code{\link[bayestestR]{p_direction}}), which is converted to a p-value using \code{bayestestR::convert_pd_to_p()}.
#'
#' @return The p-values.
#'
#' @examples
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_value(model)
#' @importFrom bayestestR p_direction convert_pd_to_p
#' @export
p_value.stanreg <- function(model, ...) {
  p <- bayestestR::p_direction(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}


#' Parameters from Bayesian Models
#'
#' Parameters of Bayesian models.
#'
#' @param model Bayesian model. May also be a data frame with posterior samples.
#' @param ci Credible Interval (CI) level. Default to 0.89 (89\%). See \code{\link[bayestestR]{ci}} for further details.
#' @param group_level Logical, for multilevel models (i.e. models with random effects) and when \code{effects = "all"} or \code{effects = "random"}, include the parameters for each group level from random effects. If \code{group_level = FALSE} (the default), only information on SD and COR are shown.
#' @inheritParams model_parameters.default
#' @inheritParams bayestestR::describe_posterior
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @note When \code{standardize = "refit"}, columns \code{diagnostic}, \code{bf_prior} and \code{priors} refer to the \emph{original} \code{model}.
#' If \code{model} is a data frame, arguments \code{diagnostic}, \code{bf_prior} and \code{priors} are ignored.
#' \cr \cr
#' There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details Currently supported models are \code{brmsfit}, \code{stanreg}, \code{stanmvreg}, \code{MCMCglmm}, \code{mcmc} and \code{bcplm}.
#'
#' @examples
#' \donttest{
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
#' @importFrom insight get_priors
#' @inheritParams insight::get_parameters
#' @export
model_parameters.stanreg <-
  function(model,
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
           verbose = TRUE,
           ...) {


    # Processing
    params <-
      .extract_parameters_bayesian(
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
        ...
      )

    if (effects != "fixed") {
      random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!Sigma\\[)(.*)", params$Parameter, perl = TRUE))
      if (length(random_effect_levels) && !isTRUE(group_level)) params <- params[-random_effect_levels, ]
    }

    params <- .add_pretty_names(params, model)
    if (exponentiate) params <- .exponentiate_parameters(params)
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
                                       verbose = TRUE,
                                       ...) {


  # Processing
  params <-
    .extract_parameters_bayesian(
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
      ...
    )

  params$Parameter <- gsub("^(.*)\\|(.*)", "\\2", params$Parameter)
  params <- .add_pretty_names(params, model)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}

