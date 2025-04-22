#' @export
model_parameters.merMod <- function(model,
                                    ci = 0.95,
                                    ci_method = NULL,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "fixed",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    include_info = getOption("parameters_info", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  # which component to return?
  effects <- insight::validate_argument(
    effects,
    c("fixed", "total", "random_total", "all")
  )

  if (effects == "fixed") {
    return(model_parameters.default(
      model,
      ci = ci,
      ci_method = ci_method,
      bootstrap = bootstrap,
      iterations = iterations,
      standardize = standardize,
      exponentiate = exponentiate,
      p_adjust = p_adjust,
      vcov = vcov,
      vcov_args = vcov_args,
      include_info = include_info,
      keep = keep,
      drop = drop,
      verbose = verbose,
      ...
    ))
  }

  # for coef(), we don't need all the attributes and just stop here
  if (effects %in% c("total", "random_total")) {
    params <- .group_level_total(model)
    params$Effects <- "total"
    class(params) <- c("parameters_coef", "see_parameters_coef", class(params))
    return(params)
  }

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  params
}


#' @export
standard_error.coxme <- function(model, ...) {
  beta_coef <- model$coefficients

  if (length(beta_coef) > 0) {
    .data_frame(
      Parameter = .remove_backticks_from_string(names(beta_coef)),
      SE = sqrt(diag(stats::vcov(model)))
    )
  }
}

## TODO add ci_method later?

#' @export
p_value.coxme <- function(model, ...) {
  stat <- insight::get_statistic(model)

  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(1 - stats::pchisq(stat$Statistic^2, df = 1))
    )
  }
}
