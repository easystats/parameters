#' @export
model_parameters.draws <- function(model,
                                   centrality = "median",
                                   dispersion = FALSE,
                                   ci = 0.95,
                                   ci_method = "eti",
                                   test = "pd",
                                   rope_range = "default",
                                   rope_ci = 0.95,
                                   exponentiate = FALSE,
                                   keep = NULL,
                                   drop = NULL,
                                   verbose = TRUE,
                                   ...) {
  out <- .posterior_draws_to_df(model)

  # Processing
  params <- .extract_parameters_bayesian(
    out,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = NULL,
    diagnostic = NULL,
    priors = FALSE,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, exponentiate = exponentiate)

  attr(params, "ci") <- ci
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


# Standard Errors ---------------------------------------------

#' @export
standard_error.draws <- function(model, verbose = TRUE, ...) {
  params <- .posterior_draws_to_df(model)
  .data_frame(
    Parameter = colnames(params),
    SE = unname(vapply(params, stats::sd, 1, na.rm = TRUE))
  )
}


# p-Values ---------------------------------------------

#' @export
p_value.draws <- function(model, ...) {
  params <- .posterior_draws_to_df(model)
  p <- bayestestR::p_direction(params)
  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = vapply(p$pd, bayestestR::convert_pd_to_p, 1)
  )
}


# helper ------------------------------

.posterior_draws_to_df <- function(x) {
  UseMethod(".posterior_draws_to_df")
}

.posterior_draws_to_df.default <- function(x) {
  insight::format_error(sprintf("Objects of class `%s` are not yet supported.", class(x)[1]))
}

.posterior_draws_to_df.data.frame <- function(x) {
  x
}

.posterior_draws_to_df.draws_df <- function(x) {
  insight::check_if_installed("posterior")
  datawizard::data_remove(as.data.frame(posterior::as_draws_df(x)), c(".chain", ".iteration", ".draw"))
}

.posterior_draws_to_df.draws_matrix <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_array <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_list <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_rvars <- .posterior_draws_to_df.draws_df
