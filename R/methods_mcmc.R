#' @export
model_parameters.mcmc <- function(model,
                                  centrality = "median",
                                  dispersion = FALSE,
                                  ci = .89,
                                  ci_method = "hdi",
                                  test = c("pd", "rope"),
                                  rope_range = "default",
                                  rope_ci = 1.0,
                                  verbose = TRUE,
                                  ...) {


  # Processing
  params <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = NULL, diagnostic = NULL, priors = FALSE, ...)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}

