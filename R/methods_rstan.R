#' @export
model_parameters.stanfit <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = .89,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 1.0,
                                     diagnostic = c("ESS", "Rhat"),
                                     effects = "fixed",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
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
    ...
  )

  if (effects != "fixed") {
    random_effect_levels <- which(params$Effects %in% "random" & grepl("^(?!Sigma\\[)(.*)", params$Parameter, perl = TRUE))
    if (length(random_effect_levels) && !isTRUE(group_level)) params <- params[-random_effect_levels, ]
  }

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
