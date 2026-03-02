#' @export
model_parameters.coxme <- function(
  model,
  ci = 0.95,
  ci_method = NULL,
  ci_random = NULL,
  bootstrap = FALSE,
  iterations = 1000,
  standardize = NULL,
  effects = "all",
  group_level = FALSE,
  exponentiate = FALSE,
  p_adjust = NULL,
  vcov = NULL,
  vcov_args = NULL,
  wb_component = FALSE,
  include_info = getOption("parameters_mixed_info", FALSE),
  include_sigma = FALSE,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  insight::check_if_installed("lme4")
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else {
      ci_method <- switch(
        insight::find_statistic(model),
        `t-statistic` = "residual",
        "wald"
      )
    }
  }

  # p-values, CI and se might be based of wald, or KR
  ci_method <- tolower(ci_method)

  if (isTRUE(bootstrap)) {
    ci_method <- insight::validate_argument(
      ci_method,
      c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai")
    )
  } else {
    ci_method <- insight::validate_argument(
      ci_method,
      c(
        "wald",
        "normal",
        "residual",
        "ml1",
        "betwithin",
        "satterthwaite",
        "kenward",
        "kr",
        "boot",
        "profile",
        "uniroot"
      )
    )
  }

  # which component to return?
  effects <- insight::validate_argument(
    effects,
    c("fixed", "random", "grouplevel", "total", "random_total", "all")
  )
  params <- NULL

  # group level estimates =================================================
  # =======================================================================

  # for coef(), we don't need all the attributes and just stop here
  if (effects %in% c("total", "random_total")) {
    params <- .group_level_total(model)
    params$Effects <- "total"
    class(params) <- c("parameters_coef", "see_parameters_coef", class(params))
    return(params)
  }

  # group grouplevel estimates (BLUPs), handle alias
  if (effects == "grouplevel") {
    effects <- "random"
    group_level <- TRUE
  }

  # post hoc standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_alert(
        "Standardizing coefficients only works for fixed effects of the mixed model."
      )
    }
    effects <- "fixed"
  }

  # for refit, we completely refit the model, than extract parameters,
  # ci etc. as usual - therefor, we set "standardize" to NULL
  if (isTRUE(standardize == "refit")) {
    model <- datawizard::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  # fixed effects =================================================
  # ===============================================================

  if (effects %in% c("fixed", "all")) {
    # Processing
    if (bootstrap) {
      params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
      if (effects != "fixed") {
        effects <- "fixed"
        if (verbose) {
          insight::format_alert(
            "Bootstrapping only returns fixed effects of the mixed model."
          )
        }
      }
    } else {
      fun_args <- list(
        model,
        ci = ci,
        ci_method = ci_method,
        standardize = standardize,
        p_adjust = p_adjust,
        wb_component = wb_component,
        keep_parameters = keep,
        drop_parameters = drop,
        verbose = verbose,
        include_sigma = include_sigma,
        include_info = include_info,
        vcov = vcov,
        vcov_args = vcov_args
      )
      fun_args <- c(fun_args, dots)
      params <- do.call(".extract_parameters_mixed", fun_args)
    }

    params$Effects <- "fixed"

    # exponentiate coefficients and SE/CI, if requested
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  att <- attributes(params)

  # add random effects, either group level or re variances
  # ======================================================

  params <- .add_random_effects_lme4(
    model,
    params,
    ci,
    ci_method,
    ci_random,
    effects,
    group_level,
    verbose
  )

  # clean-up
  # ======================================================

  # remove empty column
  if (!is.null(params$Level) && all(is.na(params$Level))) {
    params$Level <- NULL
  }

  # due to rbind(), we lose attributes from "extract_parameters()",
  # so we add those attributes back here...
  if (!is.null(att)) {
    attributes(params) <- utils::modifyList(att, attributes(params))
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci = ci,
    exponentiate,
    bootstrap,
    iterations,
    ci_method = ci_method,
    p_adjust = p_adjust,
    verbose = verbose,
    include_info = include_info,
    group_level = group_level,
    wb_component = wb_component,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

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
