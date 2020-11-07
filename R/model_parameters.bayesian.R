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
model_parameters.stanreg <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, effects = "fixed", exponentiate = FALSE, standardize = NULL, group_level = FALSE, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, effects = effects, standardize = standardize, ...)

  if (effects == "fixed") {
    attr(params, "pretty_names") <- format_parameters(model)
  } else {
    random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!Sigma\\[)(.*)", params$Parameter, perl = TRUE))
    if (length(random_effect_levels) && !isTRUE(group_level)) params <- params[-random_effect_levels, ]
    params <- .add_pretty_names(params, model, effects = effects, component = NULL)
  }

  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ci_method = ci_method, ...)

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_stan", "parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
model_parameters.stanmvreg <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = "pd", rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, effects = "fixed", standardize = NULL, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, effects = effects, standardize = standardize, ...)
  params$Parameter <- gsub("^(.*)\\|(.*)", "\\2", params$Parameter)

  if (effects == "fixed") {
    attr(params, "pretty_names") <- format_parameters(model)
  } else {
    params <- .add_pretty_names(params, model, effects = effects, component = NULL)
  }

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}



#' @importFrom insight clean_parameters find_random get_response model_info is_multivariate
#' @rdname model_parameters.stanreg
#' @inheritParams insight::get_parameters
#' @export
model_parameters.brmsfit <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, effects = "fixed", component = "all", exponentiate = FALSE, standardize = NULL, group_level = FALSE, ...) {
  modelinfo <- insight::model_info(model)

  # Bayesian meta analysis

  if (!insight::is_multivariate(model) && isTRUE(modelinfo$is_meta)) {
    params <- .model_parameters_brms_meta(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, diagnostic = diagnostic, priors = priors, exponentiate = exponentiate, standardize = standardize, ...)
  } else {

    # Processing
    params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, effects = effects, component = component, standardize = standardize, ...)

    if (effects == "fixed" && component == "conditional") {
      attr(params, "pretty_names") <- format_parameters(model)
    } else {
      random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!sd_|cor_)(.*)", params$Parameter, perl = TRUE))
      if (length(random_effect_levels) && !isTRUE(group_level)) params <- params[-random_effect_levels, ]
      params <- .add_pretty_names(params, model, effects = effects, component = component)
    }

    if (exponentiate) params <- .exponentiate_parameters(params)
    params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ci_method = ci_method, ...)

    attr(params, "parameter_info") <- insight::clean_parameters(model)
    attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
    class(params) <- unique(c("parameters_stan", "see_parameters_model", "parameters_model", class(params)))
  }

  params
}


# brms meta analysis -------

#' @importFrom insight get_parameters
.model_parameters_brms_meta <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, diagnostic = c("ESS", "Rhat"), priors = TRUE, exponentiate = FALSE, standardize = NULL, ...) {

  # parameters
  smd <- insight::get_parameters(model, effects = "fixed")
  studies <- insight::get_parameters(model, effects = "random", parameters = "^(?!sd_)")
  studies[] <- lapply(studies, function(i) i + smd[[1]])
  tau <- insight::get_parameters(model, effects = "random", parameters = "^sd_")

  params <- bayestestR::describe_posterior(
    cbind(studies, smd),
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    ...
  )

  params_diagnostics <- bayestestR::diagnostic_posterior(
    model,
    effects = "all",
    diagnostic = diagnostic,
    ...
  )

  params_tau <- bayestestR::describe_posterior(
    tau,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    ...
  )

  # add weights
  params$Weight <- 1 / c(insight::get_response(model)[[2]], NA)

  # merge description with diagnostis
  params <- merge(params, params_diagnostics, by = "Parameter", all.x = TRUE, sort = FALSE)

  # Renaming
  re_name <- insight::find_random(model, flatten = TRUE)

  study_names <- gsub(sprintf("r_%s\\.(.*)", re_name[1]), "\\1", colnames(studies))
  # replace dots by white space
  study_names <- gsub(".", " ", study_names, fixed = TRUE)
  # remove "Intercept"
  study_names <- trimws(gsub("Intercept", "", study_names, fixed = TRUE))

  cleaned_parameters <- c(study_names, "Overall", "tau")

  # components
  params$Component <- "Studies"
  params_tau$Component <- "tau"

  # merge with tau
  params <- merge(params, params_tau, all = TRUE, sort = FALSE)

  # reorder columns
  ci_column <- which(colnames(params) == "CI_high")
  weight_column <- which(colnames(params) == "Weight")
  first_cols <- c(1:ci_column, weight_column)
  params <- params[, c(first_cols, seq_len(ncol(params))[-first_cols])]


  # add attributes
  attr(params, "tau") <- params_tau
  attr(params, "pretty_names") <- cleaned_parameters
  attr(params, "cleaned_parameters") <- cleaned_parameters
  attr(params, "ci") <- ci
  attr(params, "bayes_ci_method") <- ci_method
  attr(params, "exponentiate") <- exponentiate
  attr(params, "model_class") <- class(model)
  attr(params, "is_bayes_meta") <- TRUE
  attr(params, "study_weights") <- params$Weight
  attr(params, "data") <- cbind(studies, smd, tau)

  class(params) <- unique(c("parameters_brms_meta", "see_parameters_brms_meta", class(params)))
  params
}



#' @export
model_parameters.MCMCglmm <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

  attr(params, "pretty_names") <- format_parameters(model)
  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}



#' @export
model_parameters.mcmc <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = NULL, diagnostic = NULL, priors = FALSE, ...)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}

#' @export
model_parameters.data.frame <- model_parameters.mcmc


#' @export
model_parameters.bcplm <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}

#' @export
model_parameters.bayesQR <- model_parameters.bcplm


#' @export
model_parameters.stanfit <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, diagnostic = c("ESS", "Rhat"), effects = "fixed", exponentiate = FALSE, standardize = NULL, group_level = FALSE, ...) {

  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = NULL, diagnostic = diagnostic, priors = FALSE, effects = effects, standardize = standardize, ...)

  if (effects != "fixed") {
    random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!Sigma\\[)(.*)", params$Parameter, perl = TRUE))
    if (length(random_effect_levels) && !isTRUE(group_level)) params <- params[-random_effect_levels, ]
  }

  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ci_method = ci_method, ...)

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_stan", "parameters_model", "see_parameters_model", class(params))

  params
}
