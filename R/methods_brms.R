#' @rdname model_parameters.stanreg
#' @inheritParams insight::get_parameters
#' @export
model_parameters.brmsfit <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = .95,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     bf_prior = NULL,
                                     diagnostic = c("ESS", "Rhat"),
                                     priors = FALSE,
                                     effects = "fixed",
                                     component = "all",
                                     exponentiate = FALSE,
                                     standardize = NULL,
                                     group_level = FALSE,
                                     keep = NULL,
                                     drop = NULL,
                                     parameters = keep,
                                     verbose = TRUE,
                                     ...) {
  modelinfo <- insight::model_info(model, verbose = FALSE)

  # Bayesian meta analysis

  if (!insight::is_multivariate(model) && isTRUE(modelinfo$is_meta)) {
    params <- .model_parameters_brms_meta(
      model,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      diagnostic = diagnostic,
      priors = priors,
      exponentiate = exponentiate,
      standardize = standardize,
      keep_parameters = keep,
      drop_parameters = drop,
      ...
    )
  } else {

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
      component = component,
      standardize = standardize,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      ...
    )

    if (!(effects == "fixed" && component == "conditional")) {
      random_effect_levels <- which(params$Effects == "random" & grepl("^(?!sd_|cor_)(.*)", params$Parameter, perl = TRUE) & !(params$Parameter %in% c("car", "sdcar")))
      if (length(random_effect_levels) && isFALSE(group_level)) params <- params[-random_effect_levels, ]
    }

    params <- .add_pretty_names(params, model, group_level)

    if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
      params <- .exponentiate_parameters(params, model, exponentiate)
    }

    params <- .add_model_parameters_attributes(params,
      model,
      ci,
      exponentiate,
      ci_method = ci_method,
      verbose = verbose,
      ...
    )

    attr(params, "parameter_info") <- insight::clean_parameters(model)
    attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
    class(params) <- unique(c("parameters_stan", "see_parameters_model", "parameters_model", class(params)))
  }

  params
}


# brms meta analysis -------

.model_parameters_brms_meta <- function(model,
                                        centrality = "median",
                                        dispersion = FALSE,
                                        ci = .95,
                                        ci_method = "hdi",
                                        test = c("pd", "rope"),
                                        rope_range = "default",
                                        rope_ci = 0.95,
                                        diagnostic = c("ESS", "Rhat"),
                                        priors = FALSE,
                                        exponentiate = FALSE,
                                        standardize = NULL,
                                        keep_parameters = NULL,
                                        drop_parameters = NULL,
                                        verbose = TRUE,
                                        ...) {


  # parameters
  smd <- insight::get_parameters(model, effects = "fixed", component = "conditional")
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

  # merge description with diagnostic
  params <- merge(params, params_diagnostics, by = "Parameter", all.x = TRUE, sort = FALSE)

  # Renaming
  re_name <- insight::find_random(model, flatten = TRUE)

  study_names <- gsub(sprintf("r_%s\\[(.*)\\]", re_name[1]), "\\1", colnames(studies))
  # replace dots by white space
  study_names <- gsub(".", " ", study_names, fixed = TRUE)
  # remove "Intercept"
  study_names <- insight::trim_ws(gsub(",Intercept", "", study_names, fixed = TRUE))

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

  # filter parameters, if requested
  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    params <- .filter_parameters(params,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }

  # add attributes
  attr(params, "tau") <- params_tau
  attr(params, "pretty_names") <- cleaned_parameters
  attr(params, "cleaned_parameters") <- cleaned_parameters
  attr(params, "ci") <- ci
  attr(params, "ci_method") <- ci_method
  attr(params, "exponentiate") <- exponentiate
  attr(params, "model_class") <- class(model)
  attr(params, "is_bayes_meta") <- TRUE
  attr(params, "study_weights") <- params$Weight
  attr(params, "data") <- cbind(studies, smd, tau)

  class(params) <- unique(c("parameters_brms_meta", "see_parameters_brms_meta", class(params)))
  params
}


#' @export
standard_error.brmsfit <- function(model,
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
p_value.brmsfit <- p_value.BFBayesFactor
