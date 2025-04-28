#' @title Parameters from Bayesian Models
#' @name model_parameters.brmsfit
#'
#' @description
#' Model parameters from Bayesian models. This function internally calls
#' [`bayestestR::describe_posterior()`] to get the relevant information for
#' the output.
#'
#' @param model Bayesian model (including SEM from **blavaan**. May also be
#' a data frame with posterior samples, however, `as_draws` must be set to
#' `TRUE` (else, for data frames `NULL` is returned).
#' @param ci Credible Interval (CI) level. Default to `0.95` (`95%`). See
#' [bayestestR::ci()] for further details.
#' @param group_level Logical, for multilevel models (i.e. models with random
#' effects) and when `effects = "all"` or `effects = "random"`,
#' include the parameters for each group level from random effects. If
#' `group_level = FALSE` (the default), only information on SD and COR
#' are shown.
#' @param component Which type of parameters to return, such as parameters for the
#' conditional model, the zero-inflation part of the model, the dispersion
#' term, or other auxiliary parameters be returned? Applies to models with
#' zero-inflation and/or dispersion formula, or if parameters such as `sigma`
#' should be included. May be abbreviated. Note that the *conditional*
#' component is also called *count* or *mean* component, depending on the
#' model. There are three convenient shortcuts: `component = "all"` returns
#' all possible parameters. If `component = "location"`, location parameters
#' such as `conditional`, `zero_inflated`, or `smooth_terms`, are returned
#' (everything that are fixed or random effects - depending on the `effects`
#' argument - but no auxiliary parameters). For `component = "distributional"`
#' (or `"auxiliary"`), components like `sigma`, `dispersion`, or `beta`
#' (and other auxiliary parameters) are returned.
#' @param as_draws Logical, if `TRUE` and `model` is of class `data.frame`,
#' the data frame is treated as posterior samples and handled similar to
#' Bayesian models. All arguments in `...` are passed to
#' `model_parameters.draws()`.
#' @inheritParams model_parameters.default
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams insight::get_parameters
#'
#' @seealso [insight::standardize_names()] to rename columns into a consistent,
#' standardized naming scheme.
#'
#' @note When `standardize = "refit"`, columns `diagnostic`, `bf_prior` and
#' `priors` refer to the *original* `model`. If `model` is a data frame,
#' arguments `diagnostic`, `bf_prior` and `priors` are ignored.
#'
#' There is also a
#' [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#' implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @inheritSection model_parameters.zcpglm Model components
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' library(parameters)
#' model <- suppressWarnings(stan_glm(
#'   Sepal.Length ~ Petal.Length * Species,
#'   data = iris, iter = 500, refresh = 0
#' ))
#' model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.brmsfit <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = "pd",
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
  } else if (effects %in% c("total", "random_total")) {
    # group level total effects (coef())
    params <- .group_level_total(
      model,
      centrality,
      dispersion,
      ci,
      ci_method,
      test,
      rope_range,
      rope_ci,
      ...
    )
    params$Effects <- "total"
    class(params) <- c("parameters_coef", "see_parameters_coef", class(params))
    return(params)
  } else {

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
      component = component,
      standardize = standardize,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      ...
    )

    ## TODO: remove this once insight > 1.2.0 on CRAN

    # if random effects are included, check if group-level estimates
    # should be returned or not. If not, remove them.
    if (effects != "fixed") {
      random_effect_levels <- which(
        params$Effects == "random" &
          grepl("^(?!sd_|cor_)(.*)", params$Parameter, perl = TRUE) &
          !(params$Parameter %in% c("car", "sdcar"))
      )
      if (length(random_effect_levels) && isFALSE(group_level)) {
        params <- params[-random_effect_levels, ]
      }
    }

    # add prettified names as attribute. Furthermore, group column is added
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
    attr(params, "dpars") <- insight::find_auxiliary(model, verbose = FALSE)
    class(params) <- unique(c("parameters_model", "see_parameters_model", class(params)))
  }

  params
}


# brms meta analysis -------

.model_parameters_brms_meta <- function(model,
                                        centrality = "median",
                                        dispersion = FALSE,
                                        ci = 0.95,
                                        ci_method = "eti",
                                        test = "pd",
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
                                   effects = "fixed",
                                   component = "all",
                                   ...) {

  ## TODO: remove validation of effects and component once insight > 1.2.0 is on CRAN

  effects <- insight::validate_argument(
    effects,
    c("fixed", "random")
  )
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated")
  )

  params <- insight::get_parameters(model, effects = effects, component = component, ...)

  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}


#' @export
p_value.brmsfit <- p_value.BFBayesFactor
