#' @export
model_parameters.bayesQR <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = .95,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     bf_prior = NULL,
                                     diagnostic = c("ESS", "Rhat"),
                                     priors = TRUE,
                                     keep = NULL,
                                     drop = NULL,
                                     parameters = keep,
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
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
p_value.bayesQR <- p_value.BFBayesFactor
