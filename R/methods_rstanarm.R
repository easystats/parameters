#' @export
model_parameters.stanreg <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = "pd",
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     bf_prior = NULL,
                                     diagnostic = c("ESS", "Rhat"),
                                     priors = TRUE,
                                     effects = "fixed",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # for coef(), we don't need all the attributes and just stop here
  if (effects %in% c("total", "random_total")) {
    params <- .group_level_total(model)
    params$Effects <- "total"
    class(params) <- c("parameters_coef", "see_parameters_coef", class(params))
    return(params)
  }

  if (utils::packageVersion("insight") > "1.2.0" && effects == "random" && group_level) {
    effects <- "grouplevel"
  }

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
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  ## TODO: remove this once insight > 1.2.0 on CRAN
  if (effects != "fixed") {
    random_effect_levels <- which(
      params$Effects == "random" & !startsWith(params$Parameter, "Sigma[")
    )
    if (length(random_effect_levels) && isFALSE(group_level)) {
      params <- params[-random_effect_levels, , drop = FALSE]
    }
  }

  ## TODO: can we use the regular pretty-name-formatting?
  params <- .add_pretty_names(params, model)

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    ci_method = ci_method,
    group_level = group_level,
    verbose = verbose,
    ...
  )

  attr(params, "parameter_info") <- insight::clean_parameters(model)
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
model_parameters.stanmvreg <- function(model,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = "pd",
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       bf_prior = NULL,
                                       diagnostic = c("ESS", "Rhat"),
                                       priors = TRUE,
                                       effects = "fixed",
                                       standardize = NULL,
                                       keep = NULL,
                                       drop = NULL,
                                       verbose = TRUE,
                                       ...) {
  if (utils::packageVersion("insight") > "1.2.0" && effects == "random" && group_level) {
    effects <- "grouplevel"
  }

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
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  params$Parameter <- gsub("^(.*)\\|(.*)", "\\2", params$Parameter)
  params <- .add_pretty_names(params, model)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.stanreg <- standard_error.brmsfit


#' @export
standard_error.mvstanreg <- standard_error.brmsfit


#' @export
p_value.stanreg <- p_value.BFBayesFactor
