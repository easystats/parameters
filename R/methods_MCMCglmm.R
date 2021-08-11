#' @export
standard_error.MCMCglmm <- function(model, ...) {
  nF <- model$Fixed$nfl
  parms <- as.data.frame(model$Sol[, 1:nF, drop = FALSE])

  .data_frame(
    Parameter = .remove_backticks_from_string(colnames(parms)),
    SE = unname(sapply(parms, stats::sd))
  )
}


#' @export
p_value.MCMCglmm <- function(model, ...) {
  nF <- model$Fixed$nfl
  p <- 1 - colSums(model$Sol[, 1:nF, drop = FALSE] > 0) / dim(model$Sol)[1]
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", flatten = TRUE),
    p = p
  )
}


#' @export
model_parameters.MCMCglmm <- function(model,
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
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      ...
    )

  attr(params, "pretty_names") <- format_parameters(model)
  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
