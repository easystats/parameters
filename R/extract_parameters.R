# generic function ------------------------------------------------------


#' @keywords internal
.extract_parameters_generic <- function(model,
                                        ci,
                                        component,
                                        merge_by = c("Parameter", "Component"),
                                        standardize = NULL,
                                        effects = "fixed",
                                        ci_method = NULL,
                                        p_adjust = NULL,
                                        wb_component = FALSE,
                                        verbose = TRUE,
                                        keep_component_column = FALSE,
                                        keep_parameters = NULL,
                                        drop_parameters = NULL,
                                        include_sigma = TRUE,
                                        summary = FALSE,
                                        vcov = NULL,
                                        vcov_args = NULL,
                                        ...) {
  dots <- list(...)

  # ==== check if standardization is required and package available

  if (isTRUE(standardize)) {
    if (verbose) {
      insight::format_alert(
        "`standardize` must be on of \"refit\", \"posthoc\", \"basic\", \"smart\" or \"pseudo\"."
      )
    }
    standardize <- NULL
  }

  # ==== model exceptions

  if (inherits(model, c("crq", "crqs"))) {
    merge_by <- c("Parameter", "Component")
  }


  # ==== for refit, we completely refit the model, than extract parameters, ci etc. as usual

  if (isTRUE(standardize == "refit")) {
    fun_args <- c(list(model, verbose = FALSE), dots)
    # argument name conflict with deprecated `robust`
    fun_args[["robust"]] <- NULL
    fun <- datawizard::standardize
    model <- do.call(fun, fun_args)
  }

  parameters <- insight::get_parameters(model,
    effects = effects,
    component = component,
    verbose = FALSE
  )
  statistic <- insight::get_statistic(model, component = component)

  # check if all estimates are non-NA
  parameters <- .check_rank_deficiency(parameters)


  # ==== check if we really have a component column

  if (!("Component" %in% names(parameters)) && "Component" %in% merge_by) {
    merge_by <- setdiff(merge_by, "Component")
  }


  # ==== check Degrees of freedom

  if (!.dof_method_ok(model, ci_method, type = "ci_method")) {
    ci_method <- NULL
  }


  # ==== for ordinal models, first, clean parameter names and then indicate
  #      intercepts (alpha-coefficients) in the component column

  if (inherits(model, "polr")) {
    intercept_groups <- grep("Intercept:", parameters$Parameter, fixed = TRUE)
    parameters$Parameter <- gsub("Intercept: ", "", parameters$Parameter, fixed = TRUE)
  } else if (inherits(model, "clm") && !is.null(model$alpha)) {
    intercept_groups <- rep(
      c("intercept", "location", "scale"),
      lengths(model[c("alpha", "beta", "zeta")])
    )
  } else if (inherits(model, "clm2") && !is.null(model$Alpha)) {
    intercept_groups <- rep(
      c("intercept", "location", "scale"),
      lengths(model[c("Alpha", "beta", "zeta")])
    )
  } else {
    intercept_groups <- NULL
  }

  original_order <- parameters$.id <- seq_len(nrow(parameters))

  # column name for coefficients, non-standardized
  coef_col <- "Coefficient"


  # ==== CI - only if we don't already have CI for std. parameters

  if (is.null(ci)) {
    ci_cols <- NULL
  } else {
    fun_args <- list(model,
      ci = ci,
      component = component,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose
    )
    fun_args <- c(fun_args, dots)
    if (!is.null(ci_method)) {
      fun_args[["method"]] <- ci_method
    }
    ci_df <- suppressMessages(do.call("ci", fun_args))

    if (is.null(ci_df)) {
      ci_cols <- NULL
    } else {
      # for multiple CI columns, reshape CI-dataframe to match parameters df
      if (length(ci) > 1) {
        ci_df <- datawizard::reshape_ci(ci_df)
      }
      # remember names of CI columns, used for later sorting of columns
      ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", merge_by)]
      parameters <- merge(parameters, ci_df, by = merge_by, sort = FALSE)
    }
  }


  # ==== p value

  fun_args <- list(model,
    method = ci_method,
    effects = effects,
    verbose = verbose,
    component = component,
    vcov = vcov,
    vcov_args = vcov_args
  )
  fun_args <- c(fun_args, dots)
  pval <- do.call("p_value", fun_args)

  if (!is.null(pval)) {
    parameters <- merge(parameters, pval, by = merge_by, sort = FALSE)
  }


  # ==== standard error - only if we don't already have SE for std. parameters

  std_err <- NULL
  fun_args <- list(model,
    effects = effects,
    component = component,
    verbose = verbose,
    vcov = vcov,
    vcov_args = vcov_args
  )
  fun_args <- c(fun_args, dots)
  if (!is.null(ci_method)) {
    fun_args[["method"]] <- ci_method
  }
  std_err <- do.call("standard_error", fun_args)

  if (!is.null(std_err)) {
    parameters <- merge(parameters, std_err, by = merge_by, sort = FALSE)
  }


  # ==== test statistic - fix values for robust vcov


  # deprecated argument `robust = TRUE`
  if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else if (!is.null(statistic)) {
    parameters <- merge(parameters, statistic, by = merge_by, sort = FALSE)
  }


  # ==== degrees of freedom

  if (is.null(ci_method)) {
    df_error <- degrees_of_freedom(model, method = "any", verbose = FALSE)
  } else {
    df_error <- degrees_of_freedom(model, method = ci_method, verbose = FALSE)
  }
  if (!is.null(df_error) && (length(df_error) == 1 || length(df_error) == nrow(parameters))) {
    if (length(df_error) == 1) {
      parameters$df_error <- df_error
    } else {
      # order may have changed due to merging, so make sure
      # df are in correct order.
      parameters$df_error <- df_error[order(parameters$.id)]
    }
  }


  # ==== Rematch order after merging

  parameters <- parameters[match(original_order, parameters$.id), ]


  # ==== Renaming

  if ("Statistic" %in% names(parameters)) {
    stat_type <- attr(statistic, "statistic", exact = TRUE)
    if (!is.null(stat_type)) {
      names(parameters) <- gsub("Statistic", gsub("(-|\\s)statistic", "", stat_type), names(parameters), fixed = TRUE)
      names(parameters) <- gsub("chi-squared", "Chi2", names(parameters), fixed = TRUE)
    }
  }
  names(parameters) <- gsub("(c|C)hisq", "Chi2", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters), fixed = TRUE)


  # ==== add intercept groups for ordinal models

  if (inherits(model, "polr") && !is.null(intercept_groups)) {
    parameters$Component <- "beta"
    parameters$Component[intercept_groups] <- "alpha"
  } else if (inherits(model, c("clm", "clm2")) && !is.null(intercept_groups)) {
    parameters$Component <- intercept_groups
  }


  # ==== remove Component column if not needed

  if (!is.null(parameters$Component) && insight::n_unique(parameters$Component) == 1 && !keep_component_column) parameters$Component <- NULL # nolint
  if ((!is.null(parameters$Effects) && insight::n_unique(parameters$Effects) == 1) || effects == "fixed") parameters$Effects <- NULL # nolint


  # ==== filter parameters, if requested

  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    parameters <- .filter_parameters(parameters,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }


  # ==== adjust p-values?

  if (!is.null(p_adjust)) {
    parameters <- .p_adjust(parameters, p_adjust, model, verbose)
  }


  # ==== remove all complete-missing cases

  parameters <- parameters[apply(parameters, 1, function(i) !all(is.na(i))), ]


  # ==== add within/between attributes

  if (inherits(model, c("glmmTMB", "MixMod")) && isTRUE(wb_component)) {
    parameters <- .add_within_between_effects(model, parameters)
  }


  # ==== Std Coefficients for other methods than "refit"

  if (!is.null(standardize) && !isTRUE(standardize == "refit")) {
    # give minimal attributes required for standardization
    temp_pars <- parameters
    class(temp_pars) <- c("parameters_model", class(temp_pars))
    attr(temp_pars, "ci") <- ci
    attr(temp_pars, "object_name") <- model # pass the model as is (this is a cheat - teehee!)

    std_parms <- standardize_parameters(temp_pars, method = standardize)
    parameters$Std_Coefficient <- std_parms$Std_Coefficient
    parameters$SE <- attr(std_parms, "standard_error")

    if (!is.null(ci)) {
      parameters$CI_low <- std_parms$CI_low
      parameters$CI_high <- std_parms$CI_high
    }

    coef_col <- "Std_Coefficient"
  }


  # ==== Reorder

  col_order <- c(
    "Parameter", coef_col, "SE", ci_cols, "t", "z", "t / F", "t/F",
    "z / Chisq", "z/Chisq", "z / Chi2", "z/Chi2", "F", "Chi2",
    "chisq", "chi-squared", "Statistic", "df", "df_error", "p",
    "Component", "Response", "Effects"
  )
  parameters <- parameters[col_order[col_order %in% names(parameters)]]


  # ==== add sigma and residual df

  if (isTRUE(include_sigma) || isTRUE(summary)) {
    parameters <- .add_sigma_residual_df(parameters, model)
  }


  rownames(parameters) <- NULL
  parameters
}


# helper ----------------


.add_sigma_residual_df <- function(params, model) {
  if (is.null(params$Component) || !"sigma" %in% params$Component) {
    sig <- .safe(suppressWarnings(insight::get_sigma(model, ci = NULL, verbose = FALSE)))
    attr(params, "sigma") <- as.numeric(sig)

    resdf <- .safe(suppressWarnings(insight::get_df(model, type = "residual")))
    attr(params, "residual_df") <- as.numeric(resdf)
  }
  params
}



.filter_parameters <- function(params, keep = NULL, drop = NULL, verbose = TRUE) {
  if (!is.null(keep) && is.list(keep)) {
    for (i in names(keep)) {
      params <- .filter_parameters_vector(params,
        keep[[i]],
        drop = NULL,
        column = i,
        verbose = verbose
      )
    }
  } else {
    params <- .filter_parameters_vector(params,
      keep,
      drop,
      column = NULL,
      verbose = verbose
    )
  }
  params
}


.filter_parameters_vector <- function(params,
                                      keep = NULL,
                                      drop = NULL,
                                      column = NULL,
                                      verbose = TRUE) {
  # check pattern
  if (!is.null(keep) && length(keep) > 1) {
    keep <- paste0("(", paste0(keep, collapse = "|"), ")")
    if (verbose) {
      insight::format_alert(
        sprintf("The `keep` argument has more than 1 element. Merging into following regular expression: `%s`.", keep)
      )
    }
  }

  # check pattern
  if (!is.null(drop) && length(drop) > 1) {
    drop <- paste0("(", paste0(drop, collapse = "|"), ")")
    if (verbose) {
      insight::format_alert(
        sprintf("The `drop` argument has more than 1 element. Merging into following regular expression: `%s`.", drop)
      )
    }
  }

  if (is.null(column) || !column %in% colnames(params)) {
    if ("Parameter" %in% colnames(params)) {
      column <- "Parameter"
    } else {
      column <- 1
    }
  }

  # row to keep and drop
  if (is.null(keep)) {
    rows_to_keep <- rep_len(TRUE, nrow(params))
  } else {
    rows_to_keep <- grepl(keep, params[[column]], perl = TRUE)
  }

  if (is.null(drop)) {
    rows_to_drop <- rep_len(TRUE, nrow(params))
  } else {
    rows_to_drop <- !grepl(drop, params[[column]], perl = TRUE)
  }


  out <- params[rows_to_keep & rows_to_drop, ]

  if (nrow(out) == 0) {
    if (verbose) {
      insight::format_alert(
        "The pattern defined in the `keep` (and `drop`) arguments would remove all parameters from the output. Thus, selecting specific parameters will be ignored." # nolint
      )
    }
    return(params)
  }

  out
}


# mixed models function ------------------------------------------------------


#' @keywords internal
.extract_parameters_mixed <- function(model,
                                      ci = 0.95,
                                      ci_method = "wald",
                                      standardize = NULL,
                                      p_adjust = NULL,
                                      wb_component = FALSE,
                                      keep_parameters = NULL,
                                      drop_parameters = NULL,
                                      include_sigma = FALSE,
                                      summary = FALSE,
                                      vcov = NULL,
                                      vcov_args = NULL,
                                      verbose = TRUE,
                                      ...) {
  dots <- list(...)

  special_ci_methods <- c("betwithin", "satterthwaite", "ml1", "kenward", "kr")

  # get parameters and statistic
  parameters <- insight::get_parameters(model, effects = "fixed", component = "all", verbose = FALSE)
  statistic <- insight::get_statistic(model, component = "all")

  # check if all estimates are non-NA
  parameters <- .check_rank_deficiency(parameters)

  # sometimes, due to merge(), row-order messes up, so we save this here
  original_order <- parameters$.id <- seq_len(nrow(parameters))

  # remove SE column
  parameters <- datawizard::data_remove(parameters, c("SE", "Std. Error"), verbose = FALSE)

  # column name for coefficients, non-standardized
  coef_col <- "Coefficient"


  # Degrees of freedom
  if (.dof_method_ok(model, ci_method)) {
    dof <- degrees_of_freedom(model, method = ci_method, verbose = FALSE)
  } else {
    dof <- Inf
  }

  df_error <- data.frame(
    Parameter = parameters$Parameter,
    df_error = as.vector(dof),
    stringsAsFactors = FALSE
  )
  # for KR-dof, we have the SE as well, to save computation time
  df_error$SE <- attr(dof, "se", exact = TRUE)


  # CI - only if we don't already have CI for std. parameters

  if (is.null(ci)) {
    ci_cols <- NULL
  } else {
    # robust (current or deprecated)
    if (!is.null(vcov) || isTRUE(list(...)[["robust"]])) {
      fun_args <- list(model,
        ci = ci,
        vcov = vcov,
        vcov_args = vcov_args,
        verbose = verbose
      )
      fun_args <- c(fun_args, dots)
      ci_df <- suppressMessages(do.call("ci", fun_args))
    } else if (ci_method %in% c("kenward", "kr")) {
      # special handling for KR-CIs, where we already have computed SE
      ci_df <- .ci_kenward_dof(model, ci = ci, df_kr = df_error)
    } else {
      ci_df <- ci(model, ci = ci, method = ci_method, effects = "fixed")
    }
    if (length(ci) > 1) ci_df <- datawizard::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]
    parameters <- merge(parameters, ci_df, by = "Parameter", sort = FALSE)
  }


  # standard error - only if we don't already have SE for std. parameters
  if (!"SE" %in% colnames(parameters)) {
    if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
      fun_args <- list(model,
        vcov = vcov,
        vcov_args = vcov_args,
        verbose = verbose
      )
      fun_args <- c(fun_args, dots)
      parameters <- merge(parameters, do.call("standard_error", fun_args), by = "Parameter", sort = FALSE)
      # special handling for KR-SEs, which we already have computed from dof
    } else if ("SE" %in% colnames(df_error)) {
      se_kr <- df_error
      se_kr$df_error <- NULL
      parameters <- merge(parameters, se_kr, by = "Parameter", sort = FALSE)
    } else {
      parameters <- merge(
        parameters,
        standard_error(model, method = ci_method, effects = "fixed"),
        by = "Parameter",
        sort = FALSE
      )
    }
  }


  # p value
  if (!is.null(vcov) || isTRUE(list(...)[["robust"]])) {
    fun_args <- list(model,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose
    )
    fun_args <- c(fun_args, dots)
    parameters <- merge(parameters, do.call("p_value", fun_args), by = "Parameter", sort = FALSE)
  } else {
    if ("Pr(>|z|)" %in% names(parameters)) {
      names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
    } else if (ci_method %in% special_ci_methods) {
      # special handling for KR-p, which we already have computed from dof
      # parameters <- merge(parameters, .p_value_dof_kr(model, params = parameters, dof = df_error), by = "Parameter")
      parameters <- merge(
        parameters,
        .p_value_dof(model, dof = df_error$df_error, method = ci_method, se = df_error$SE),
        by = "Parameter",
        sort = FALSE
      )
    } else {
      parameters <- merge(
        parameters,
        p_value(model, dof = dof, effects = "fixed"),
        by = "Parameter",
        sort = FALSE
      )
    }
  }


  # adjust standard errors and test-statistic as well
  if ((!is.null(vcov) || ci_method %in% special_ci_methods) ||
    # deprecated argument
    isTRUE(list(...)[["robust"]])) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else {
    parameters <- merge(parameters, statistic, by = "Parameter", sort = FALSE)
  }


  # dof
  if (!"df" %in% names(parameters)) {
    if (!ci_method %in% special_ci_methods) {
      df_error <- data.frame(
        Parameter = parameters$Parameter,
        df_error = degrees_of_freedom(model, method = "any"),
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(df_error) && nrow(df_error) == nrow(parameters)) {
      if ("SE" %in% colnames(df_error)) {
        df_error$SE <- NULL
      }
      parameters <- merge(parameters, df_error, by = "Parameter", sort = FALSE)
    }
  }


  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub(
    "Statistic",
    gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE),
    names(parameters),
    fixed = TRUE
  )
  names(parameters) <- gsub("Std. Error", "SE", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("t value", "t", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("z value", "z", names(parameters), fixed = TRUE)

  # filter parameters, if requested
  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    parameters <- .filter_parameters(parameters,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }

  # adjust p-values?
  if (!is.null(p_adjust)) {
    parameters <- .p_adjust(parameters, p_adjust, model, verbose)
  }

  # if we have within/between effects (from demean()), we can add a component
  # column for nicer printing...
  if (isTRUE(wb_component)) {
    parameters <- .add_within_between_effects(model, parameters)
  }

  # Std Coefficients for other methods than "refit"
  if (!is.null(standardize)) {
    temp_pars <- parameters
    class(temp_pars) <- c("parameters_model", class(temp_pars))
    attr(temp_pars, "ci") <- ci
    attr(temp_pars, "object_name") <- model # pass the model as is (this is a cheat - teehee!)

    std_parms <- standardize_parameters(temp_pars, method = standardize)
    parameters$Std_Coefficient <- std_parms$Std_Coefficient
    parameters$SE <- attr(std_parms, "standard_error")

    if (!is.null(ci)) {
      parameters$CI_low <- std_parms$CI_low
      parameters$CI_high <- std_parms$CI_high
    }

    coef_col <- "Std_Coefficient"
  }

  # Reorder
  col_order <- c("Parameter", coef_col, "SE", ci_cols, "t", "z", "df", "df_error", "p", "Component")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]


  # add sigma
  if (isTRUE(include_sigma) || isTRUE(summary)) {
    parameters <- .add_sigma_residual_df(parameters, model)
  }


  rownames(parameters) <- NULL
  parameters
}



.add_within_between_effects <- function(model, parameters) {
  # This function checks whether the model contains predictors that were
  # "demeaned" using the "demean()" function. If so, these columns have an
  # attribute indicating the within or between effect, and in such cases,
  # this effect is used as "Component" value. by this, we get a nicer print
  # for model parameters...

  # extract attributes that indicate within and between effects
  within_effects <- .find_within_between(model, "within-effect")
  between_effects <- .find_within_between(model, "between-effect")

  # if there are no attributes, return
  if (is.null(within_effects) && is.null(between_effects)) {
    return(parameters)
  }

  if (is.null(parameters$Component)) {
    parameters$Component <- "rewb-contextual"
  }

  if (!is.null(within_effects)) {
    index <- unique(unlist(sapply(
      within_effects,
      grep,
      x = parameters$Parameter,
      fixed = TRUE
    ), use.names = FALSE))
    parameters$Component[index] <- "within"
  }

  if (!is.null(between_effects)) {
    index <- unique(unlist(sapply(
      between_effects,
      grep,
      x = parameters$Parameter,
      fixed = TRUE
    ), use.names = FALSE))
    parameters$Component[index] <- "between"
  }

  interactions <- grep(":", parameters$Parameter, fixed = TRUE)
  if (length(interactions)) {
    parameters$Component[interactions] <- "interactions"
  }

  if (((!all(c("within", "between") %in% parameters$Component)) && inherits(model, "merMod")) ||
    all(parameters$Component == "rewb-contextual")) {
    parameters$Component <- NULL
  }

  parameters
}



.find_within_between <- function(model, which_effect) {
  mf <- stats::model.frame(model)
  unlist(sapply(names(mf), function(i) {
    if (!is.null(attr(mf[[i]], which_effect, exact = TRUE))) {
      i
    }
  }), use.names = FALSE)
}






# Bayes function ------------------------------------------------------


#' @keywords internal
.extract_parameters_bayesian <- function(model,
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
                                         standardize = NULL,
                                         keep_parameters = NULL,
                                         drop_parameters = NULL,
                                         verbose = TRUE,
                                         ...) {
  # no ROPE for multi-response models
  if (insight::is_multivariate(model) && any(c("rope", "p_rope") %in% test)) {
    test <- setdiff(test, c("rope", "p_rope"))
    if (verbose) {
      insight::format_alert(
        "Multivariate response models are not yet supported for tests `rope` and `p_rope`."
      )
    }
  }

  # MCMCglmm need special handling
  if (inherits(model, "MCMCglmm")) {
    parameters <- bayestestR::describe_posterior(
      model,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      diagnostic = "ESS",
      verbose = verbose,
      ...
    )
  } else if (!is.null(standardize)) {
    parameters <- bayestestR::describe_posterior(
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
      verbose = verbose,
      ...
    )

    # Don't test BF on standardized params
    test_no_BF <- test[!test %in% c("bf", "bayesfactor", "bayes_factor")]
    if (length(test_no_BF) == 0) test_no_BF <- NULL
    std_post <- standardize_posteriors(model, method = standardize)
    std_parameters <- bayestestR::describe_posterior(
      std_post,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test_no_BF,
      rope_range = rope_range,
      rope_ci = rope_ci,
      verbose = verbose,
      ...
    )

    parameters <- merge(
      std_parameters,
      parameters[c("Parameter", setdiff(colnames(parameters), colnames(std_parameters)))],
      sort = FALSE
    )
  } else {
    parameters <- bayestestR::describe_posterior(
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
      verbose = verbose,
      ...
    )
  }

  if (length(ci) > 1) {
    parameters <- datawizard::reshape_ci(parameters)
  }

  # Remove unnecessary columns
  if ("CI" %in% names(parameters) && insight::n_unique(parameters$CI) == 1) {
    parameters$CI <- NULL
  }
  if ("ROPE_CI" %in% names(parameters) && insight::n_unique(parameters$ROPE_CI) == 1) {
    parameters$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(parameters) && "ROPE_high" %in% names(parameters)) {
    parameters$ROPE_low <- NULL
    parameters$ROPE_high <- NULL
  }

  # filter parameters, if requested
  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    parameters <- .filter_parameters(parameters,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }

  rownames(parameters) <- NULL
  parameters
}





# SEM function ------------------------------------------------------


#' @keywords internal
.extract_parameters_lavaan <- function(model,
                                       ci = 0.95,
                                       standardize = FALSE,
                                       keep_parameters = NULL,
                                       drop_parameters = NULL,
                                       verbose = TRUE,
                                       ...) {
  insight::check_if_installed("lavaan")


  # lavaan::parameterEstimates does not accept NULL `level`, but a lot of our
  # other methods do. It is often useful to pass `NULL` to speed things up,
  # but it doesn't work here.
  if (is.null(ci)) {
    ci <- 0.95
  }

  # set proper default
  if (is.null(standardize)) {
    standardize <- FALSE
  }

  # check for valid parameters
  valid_std_options <- c("all", "std.all", "latent", "std.lv", "no_exogenous", "std.nox")
  if (!is.logical(standardize) && !(standardize %in% valid_std_options)) {
    if (verbose) {
      insight::format_alert(
        "`standardize` should be one of `TRUE`, \"all\", \"std.all\", \"latent\", \"std.lv\", \"no_exogenous\" or \"std.nox\".",
        "Returning unstandardized solution."
      )
    }
    standardize <- FALSE
  }

  # CI
  if (length(ci) > 1L) {
    ci <- ci[1]
    if (verbose) {
      insight::format_alert(
        paste0("lavaan models only accept one level of CI. Keeping the first one: `ci = ", ci, "`.")
      )
    }
  }

  # collect dots
  dot_args <- list(...)

  # list all argument names from the `lavaan` function
  dot_args <- dot_args[names(dot_args) %in% c(
    "zstat",
    "pvalue",
    "standardized",
    "fmi",
    "level",
    "boot.ci.type",
    "cov.std",
    "fmi.options",
    "rsquare",
    "remove.system.eq",
    "remove.eq",
    "remove.ineq",
    "remove.def",
    "remove.nonfree",
    "add.attributes",
    "output",
    "header"
  )]

  # Get estimates
  data <- do.call(
    lavaan::parameterEstimates,
    c(
      list(object = model, se = TRUE, ci = TRUE, level = ci),
      dot_args
    )
  )

  label <- data$label

  # check if standardized estimates are requested, and if so, which type
  if (isTRUE(standardize) || !is.logical(standardize)) {
    if (is.logical(standardize)) {
      standardize <- "all"
    }

    type <- switch(standardize,
      all = ,
      std.all = "std.all",
      latent = ,
      std.lv = "std.lv",
      no_exogenous = ,
      std.nox = "std.nox",
      "std.all"
    )

    # this function errors on unknown arguments
    valid <- names(formals(lavaan::standardizedsolution))
    dots <- list(...)
    dots <- dots[names(dots) %in% valid]
    fun_args <- c(list(model, se = TRUE, level = ci, type = type), dots)
    f <- utils::getFromNamespace("standardizedsolution", "lavaan")
    data <- do.call("f", fun_args)
    names(data)[names(data) == "est.std"] <- "est"
  }


  params <- data.frame(
    To = data$lhs,
    Operator = data$op,
    From = data$rhs,
    Coefficient = data$est,
    SE = data$se,
    CI_low = data$ci.lower,
    CI_high = data$ci.upper,
    z = data$z,
    p = data$pvalue,
    stringsAsFactors = FALSE
  )

  if (!is.null(label)) {
    params$Label <- label
  }

  params$Component <- NA_character_
  params$Component[params$Operator == "=~"] <- "Loading"
  params$Component[params$Operator == "~"] <- "Regression"
  params$Component[params$Operator == "~~"] <- "Correlation"
  params$Component[params$Operator == ":="] <- "Defined"
  params$Component[params$Operator == "~1"] <- "Mean"

  params$Component[as.character(params$From) == as.character(params$To)] <- "Variance"

  if ("p" %in% colnames(params)) {
    params$p[is.na(params$p)] <- 0
  }

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }

  # filter parameters, if requested
  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    params <- .filter_parameters(params,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }

  params
}



# tools -------------------------


.check_rank_deficiency <- function(p, verbose = TRUE) {
  if (anyNA(p$Estimate)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        sprintf(
          "Model matrix is rank deficient. Parameters `%s` were not estimable.",
          toString(p$Parameter[is.na(p$Estimate)])
        )
      )
    }
    p <- p[!is.na(p$Estimate), ]
  }
  p
}
