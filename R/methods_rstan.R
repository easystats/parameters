#' @export
model_parameters.stanfit <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = "pd",
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     diagnostic = c("ESS", "Rhat"),
                                     effects = "fixed",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
                                     keep = NULL,
                                     drop = NULL,
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
    bf_prior = NULL,
    diagnostic = diagnostic,
    priors = FALSE,
    effects = effects,
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  if (effects != "fixed") {
    random_effect_levels <- which(
      params$Effects %in% "random" & !startsWith(params$Parameter, "Sigma[")
    )
    if (length(random_effect_levels) && isFALSE(group_level)) {
      params <- params[-random_effect_levels, ]
    }
  }

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

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
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
