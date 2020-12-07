# generic function ------------------------------------------------------


#' @importFrom insight get_statistic get_parameters get_sigma
#' @importFrom stats confint p.adjust.methods p.adjust
#' @keywords internal
.extract_parameters_generic <- function(model,
                                        ci,
                                        component,
                                        merge_by = c("Parameter", "Component"),
                                        standardize = NULL,
                                        effects = "fixed",
                                        robust = FALSE,
                                        df_method = NULL,
                                        p_adjust = NULL,
                                        wb_component = FALSE,
                                        verbose = TRUE,
                                        ...) {

  # ==== check if standardization is required and package available

  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # ==== model exceptions

  if (inherits(model, c("crq", "crqs"))) {
    merge_by <- c("Parameter", "Component")
  }


  # ==== for refit, we completely refit the model, than extract parameters, ci etc. as usual

  if (!is.null(standardize) && standardize == "refit") {
    model <- effectsize::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  parameters <- insight::get_parameters(model, effects = effects, component = component, verbose = verbose)
  statistic <- insight::get_statistic(model, component = component)

  # check if all estimates are non-NA
  parameters <- .check_rank_deficiency(parameters)

  # ==== check if we really have a component column

  if (!("Component" %in% names(parameters)) && "Component" %in% merge_by) {
    merge_by <- setdiff(merge_by, "Component")
  }


  # ==== check Degrees of freedom

  if (!.dof_method_ok(model, df_method)) {
    df_method <- NULL
  }


  # ==== for ordinal models, first, clean parameter names and then indicate
  #      intercepts (alpha-coefficients) in the component column

  if (inherits(model, "polr")) {
    intercept_groups <- which(grepl("^Intercept:", parameters$Parameter))
    parameters$Parameter <- gsub("Intercept: ", "", parameters$Parameter, fixed = TRUE)
  } else if (inherits(model, "clm") && !is.null(model$alpha)) {
    intercept_groups <- match(names(model$alpha), parameters$Parameter)
  } else {
    intercept_groups <- NULL
  }

  original_order <- parameters$.id <- 1:nrow(parameters)

  # column name for coefficients, non-standardized
  coef_col <- "Coefficient"


  # ==== CI - only if we don't already have CI for std. parameters


  if (!is.null(ci)) {
    if (isTRUE(robust)) {
      ci_df <- suppressMessages(ci_robust(model, ci = ci, verbose = verbose, ...))
    } else if (!is.null(df_method)) {
      ci_df <- suppressMessages(
        ci(
          model,
          ci = ci,
          effects = effects,
          component = component,
          method = df_method,
          verbose = verbose
        )
      )
    } else {
      ci_df <- suppressMessages(ci(
        model,
        ci = ci,
        effects = effects,
        component = component,
        verbose = verbose
      ))
    }
    if (!is.null(ci_df)) {
      if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
      ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", merge_by)]
      parameters <- merge(parameters, ci_df, by = merge_by)
    } else {
      ci_cols <- c()
    }
  } else {
    ci_cols <- c()
  }


  # ==== p value

  if (isTRUE(robust)) {
    pval <- p_value_robust(model, ...)
  } else if (!is.null(df_method)) {
    pval <- p_value(
      model,
      effects = effects,
      component = component,
      method = df_method,
      verbose = verbose
    )
  } else {
    pval <- p_value(model,
      effects = effects,
      component = component,
      verbose = verbose
    )
  }

  if (!is.null(pval)) {
    parameters <- merge(parameters, pval, by = merge_by)
  }


  # ==== standard error - only if we don't already have SE for std. parameters

  std_err <- NULL

  if (isTRUE(robust)) {
    std_err <- standard_error_robust(model, ...)
  } else if (!is.null(df_method)) {
    std_err <- standard_error(
      model,
      effects = effects,
      component = component,
      method = df_method,
      verbose = verbose
    )
  } else {
    std_err <- standard_error(model,
      effects = effects,
      component = component,
      verbose = verbose
    )
  }

  if (!is.null(std_err)) {
    parameters <- merge(parameters, std_err, by = merge_by)
  }


  # ==== test statistic - fix values for robust estimation

  if (isTRUE(robust)) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else if (!is.null(statistic)) {
    parameters <- merge(parameters, statistic, by = merge_by)
  }


  # ==== degrees of freedom
  if (!is.null(df_method)) {
    df_error <- degrees_of_freedom(model, method = df_method)
  } else {
    df_error <- degrees_of_freedom(model, method = "any")
  }
  if (!is.null(df_error) && (length(df_error) == 1 || length(df_error) == nrow(parameters))) {
    parameters$df_error <- df_error
  }


  # ==== Rematch order after merging

  parameters <- parameters[match(original_order, parameters$.id), ]


  # ==== Renaming

  if ("Statistic" %in% names(parameters)) {
    stat_type <- attr(statistic, "statistic", exact = TRUE)
    if (!is.null(stat_type)) {
      names(parameters) <- gsub("Statistic", gsub("(-|\\s)statistic", "", stat_type), names(parameters))
      names(parameters) <- gsub("chi-squared", "Chi2", names(parameters))
    }
  }
  names(parameters) <- gsub("(c|C)hisq", "Chi2", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))


  # ==== add intercept groups for ordinal models

  if (inherits(model, c("polr", "clm")) && !is.null(intercept_groups)) {
    parameters$Component <- "beta"
    parameters$Component[intercept_groups] <- "alpha"
  } else if (inherits(model, "clm2") && !is.null(model$Alpha)) {
    intercept_groups <- match(names(model$Alpha), parameters$Parameter)
    parameters$Component[parameters$Component == "conditional"] <- "beta"
    parameters$Component[intercept_groups] <- "alpha"
  }


  # ==== remove Component column if not needed

  if (.n_unique(parameters$Component) == 1) parameters$Component <- NULL
  if (.n_unique(parameters$Effects) == 1 || effects == "fixed") parameters$Effects <- NULL


  # ==== adjust p-values?

  if (!is.null(p_adjust) && tolower(p_adjust) %in% stats::p.adjust.methods && "p" %in% colnames(parameters)) {
    parameters$p <- stats::p.adjust(parameters$p, method = p_adjust)
  }


  # ==== remove all complete-missing cases

  parameters <- parameters[apply(parameters, 1, function(i) !all(is.na(i))), ]


  # ==== add within/between attributes

  if (inherits(model, c("glmmTMB", "MixMod")) && isTRUE(wb_component)) {
    parameters <- .add_within_between_effects(model, parameters)
  }


  # ==== Std Coefficients for other methods than "refit"

  if (!is.null(standardize)) {
    # give minimal attributes required for standardization
    temp_pars <- parameters
    class(temp_pars) <- c("parameters_model", class(temp_pars))
    attr(temp_pars, "ci") <- ci
    attr(temp_pars, "object_name") <- model # pass the model as is (this is a cheat - teehee!)

    std_parms <- effectsize::standardize_parameters(temp_pars, method = standardize)
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


  # ==== add sigma

  if (is.null(parameters$Component) || !"sigma" %in% parameters$Component) {
    sig <- tryCatch(
      {
        suppressWarnings(insight::get_sigma(model))
      },
      error = function(e) {
        NULL
      }
    )
    attr(parameters, "sigma") <- as.numeric(sig)
  }


  rownames(parameters) <- NULL
  parameters
}




# mixed models function ------------------------------------------------------


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model,
                                      ci = .95,
                                      df_method = "wald",
                                      standardize = NULL,
                                      robust = FALSE,
                                      p_adjust = NULL,
                                      wb_component = FALSE,
                                      ...) {
  # check if standardization is required and package available
  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # for refit, we completely refit the model, than extract parameters,
  # ci etc. as usual - therefor, we set "standardize" to NULL
  if (!is.null(standardize) && standardize == "refit") {
    model <- effectsize::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  special_df_methods <- c("betwithin", "satterthwaite", "ml1", "kenward", "kr")

  # get parameters and statistic
  parameters <- insight::get_parameters(model, effects = "fixed", component = "all")
  statistic <- insight::get_statistic(model, component = "all")

  # check if all estimates are non-NA
  parameters <- .check_rank_deficiency(parameters)

  # sometimes, due to merge(), row-order messes up, so we save this here
  original_order <- parameters$.id <- 1:nrow(parameters)

  # remove SE column
  parameters <- .remove_columns(parameters, c("SE", "Std. Error"))

  # column name for coefficients, non-standardized
  coef_col <- "Coefficient"


  # Degrees of freedom
  if (.dof_method_ok(model, df_method)) {
    df <- degrees_of_freedom(model, df_method)
  } else {
    df <- Inf
  }

  df_error <- data.frame(
    Parameter = parameters$Parameter,
    df_error = as.vector(df),
    stringsAsFactors = FALSE
  )
  # for KR-dof, we have the SE as well, to save computation time
  df_error$SE <- attr(df, "se", exact = TRUE)





  # CI - only if we don't already have CI for std. parameters

  if (!is.null(ci)) {
    if (isTRUE(robust)) {
      ci_df <- suppressMessages(ci_robust(model, ci = ci, ...))
    } else if (df_method %in% c("kenward", "kr")) {
      # special handling for KR-CIs, where we already have computed SE
      ci_df <- .ci_kenward_dof(model, ci = ci, df_kr = df_error)
    } else {
      ci_df <- ci(model, ci = ci, method = df_method, effects = "fixed")
    }
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]
    parameters <- merge(parameters, ci_df, by = "Parameter")
  } else {
    ci_cols <- c()
  }


  # standard error - only if we don't already have SE for std. parameters
  if (!("SE" %in% colnames(parameters))) {
    if (isTRUE(robust)) {
      parameters <- merge(parameters, standard_error_robust(model, ...), by = "Parameter")
      # special handling for KR-SEs, which we already have computed from dof
    } else if ("SE" %in% colnames(df_error)) {
      se_kr <- df_error
      se_kr$df_error <- NULL
      parameters <- merge(parameters, se_kr, by = "Parameter")
    } else {
      parameters <- merge(parameters, standard_error(model, method = df_method, effects = "fixed"), by = "Parameter")
    }
  }


  # p value
  if (isTRUE(robust)) {
    parameters <- merge(parameters, p_value_robust(model, ...), by = "Parameter")
  } else {
    if ("Pr(>|z|)" %in% names(parameters)) {
      names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
    } else if (df_method %in% special_df_methods) {
      # special handling for KR-p, which we already have computed from dof
      parameters <- merge(parameters, .p_value_dof_kr(model, params = parameters, dof = df_error), by = "Parameter")
    } else {
      parameters <- merge(parameters, p_value(model, dof = df, effects = "fixed"), by = "Parameter")
    }
  }


  # adjust standard errors and test-statistic as well
  if (!isTRUE(robust) && df_method %in% special_df_methods) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else {
    parameters <- merge(parameters, statistic, by = "Parameter")
  }


  # dof
  if (!"df" %in% names(parameters)) {
    if (!df_method %in% special_df_methods) {
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
      parameters <- merge(parameters, df_error, by = "Parameter")
    }
  }


  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub("Statistic", gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE), names(parameters))
  names(parameters) <- gsub("Std. Error", "SE", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))
  names(parameters) <- gsub("t value", "t", names(parameters))
  names(parameters) <- gsub("z value", "z", names(parameters))

  # adjust p-values?
  if (!is.null(p_adjust) && tolower(p_adjust) %in% stats::p.adjust.methods && "p" %in% colnames(parameters)) {
    parameters$p <- stats::p.adjust(parameters$p, method = p_adjust)
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

    std_parms <- effectsize::standardize_parameters(temp_pars, method = standardize)
    parameters$Std_Coefficient <- std_parms$Std_Coefficient
    parameters$SE <- attr(std_parms, "standard_error")

    if (!is.null(ci)) {
      parameters$CI_low <- std_parms$CI_low
      parameters$CI_high <- std_parms$CI_high
    }

    coef_col <- "Std_Coefficient"
  }


  # Reorder
  order <- c("Parameter", coef_col, "SE", ci_cols, "t", "z", "df", "df_error", "p", "Component")
  parameters <- parameters[order[order %in% names(parameters)]]


  # add sigma
  if (is.null(parameters$Component) || !"sigma" %in% parameters$Component) {
    sig <- tryCatch(
      {
        suppressWarnings(insight::get_sigma(model))
      },
      error = function(e) {
        NULL
      }
    )
    attr(parameters, "sigma") <- as.numeric(sig)
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
    index <- unique(unlist(sapply(within_effects, function(i) {
      grep(i, parameters$Parameter, fixed = TRUE)
    })))
    parameters$Component[index] <- "within"
  }

  if (!is.null(between_effects)) {
    index <- unique(unlist(sapply(between_effects, function(i) {
      grep(i, parameters$Parameter, fixed = TRUE)
    })))
    parameters$Component[index] <- "between"
  }

  interactions <- grep(":", parameters$Parameter, fixed = TRUE)
  if (length(interactions)) {
    parameters$Component[interactions] <- "interactions"
  }

  if (((!("within" %in% parameters$Component) || !("between" %in% parameters$Component)) && inherits(model, "merMod")) || all(parameters$Component == "rewb-contextual")) {
    parameters$Component <- NULL
  }

  parameters
}



#' @importFrom stats model.frame
.find_within_between <- function(model, which_effect) {
  mf <- stats::model.frame(model)
  unlist(sapply(names(mf), function(i) {
    if (!is.null(attr(mf[[i]], which_effect, exact = TRUE))) {
      i
    }
  }))
}






# Bayes function ------------------------------------------------------


#' @importFrom bayestestR describe_posterior reshape_ci
#' @importFrom insight is_multivariate
#' @importFrom stats sd setNames na.omit
#' @keywords internal
.extract_parameters_bayesian <- function(model,
                                         centrality = "median",
                                         dispersion = FALSE,
                                         ci = .89,
                                         ci_method = "hdi",
                                         test = c("pd", "rope"),
                                         rope_range = "default",
                                         rope_ci = 1.0,
                                         bf_prior = NULL,
                                         diagnostic = c("ESS", "Rhat"),
                                         priors = TRUE,
                                         standardize = NULL,
                                         ...) {
  # check if standardization is required and package available
  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # no ROPE for multi-response models
  if (insight::is_multivariate(model)) {
    test <- setdiff(test, c("rope", "p_rope"))
    warning("Multivariate response models are not yet supported for tests 'rope' and 'p_rope'.", call. = FALSE)
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
      ...
    )

    # Don't test BF on standardized params
    test_no_BF <- test[!test %in% c("bf", "bayesfactor", "bayes_factor")]
    if (length(test_no_BF) == 0) test_no_BF <- NULL
    std_post <- effectsize::standardize_posteriors(model, method = standardize)
    std_parameters <- bayestestR::describe_posterior(
      std_post,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test_no_BF,
      rope_range = rope_range,
      rope_ci = rope_ci,
      ...
    )

    parameters <- merge(std_parameters, parameters[c("Parameter", setdiff(colnames(parameters), colnames(std_parameters)))], sort = FALSE)
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
      ...
    )
  }

  if (length(ci) > 1) {
    parameters <- bayestestR::reshape_ci(parameters)
  }

  # Remove unnecessary columns
  if ("CI" %in% names(parameters) && .n_unique(parameters$CI) == 1) {
    parameters$CI <- NULL
  }
  if ("ROPE_CI" %in% names(parameters) && .n_unique(parameters$ROPE_CI) == 1) {
    parameters$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(parameters) & "ROPE_high" %in% names(parameters)) {
    parameters$ROPE_low <- NULL
    parameters$ROPE_high <- NULL
  }

  parameters
}





# SEM function ------------------------------------------------------


#' @keywords internal
.extract_parameters_lavaan <- function(model, ci = 0.95, standardize = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }

  # check for valid parameters
  if (!is.logical(standardize)) {
    if (!(standardize %in% c("all", "std.all", "latent", "std.lv", "no_exogenous", "std.nox"))) {
      warning("'standardize' should be one of TRUE, 'all', 'std.all', 'latent', 'std.lv', 'no_exogenous' or 'std.nox'. Returning unstandardized solution.", call. = FALSE)
      standardize <- FALSE
    }
  }

  # CI
  if (length(ci) > 1) {
    ci <- ci[1]
    warning(paste0("lavaan models only accept one level of CI :( Keeping the first one: `ci = ", ci, "`."))
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
  data <- do.call(lavaan::parameterEstimates, c(
    list(object = model, se = TRUE, ci = TRUE, level = ci),
    dot_args
  ))
  label <- data$label

  # check if standardized estimates are requested, and if so, which type
  if (isTRUE(standardize) || !is.logical(standardize)) {
    if (is.logical(standardize)) {
      standardize <- "all"
    }
    type <- switch(
      standardize,
      "all" = ,
      "std.all" = "std.all",
      "latent" = ,
      "std.lv" = "std.lv",
      "no_exogenous" = ,
      "std.nox" = "std.nox",
      "std.all"
    )
    data <- lavaan::standardizedsolution(model, se = TRUE, level = ci, type = type, ...)
    names(data)[names(data) == "est.std"] <- "est"
  }


  if (inherits(model, "blavaan")) {
    params <- data.frame(
      To = data$lhs,
      Operator = data$op,
      From = data$rhs,
      Coefficient = data$est,
      SE = data$se,
      CI_low = data$ci.lower,
      CI_high = data$ci.upper,
      stringsAsFactors = FALSE
    )
  } else {
    params <- data.frame(
      To = data$lhs,
      Operator = data$op,
      From = data$rhs,
      Coefficient = data$est,
      SE = data$se,
      CI_low = data$ci.lower,
      CI_high = data$ci.upper,
      p = data$pvalue,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(label)) {
    params$Label <- label
  }

  params$Type <- ifelse(params$Operator == "=~", "Loading",
    ifelse(params$Operator == "~", "Regression",
      ifelse(params$Operator == "~~", "Correlation",
        ifelse(params$Operator == ":=", "Defined",
          ifelse(params$Operator == "~1", "Mean", NA)
        )
      )
    )
  )
  params$Type <- ifelse(as.character(params$From) == as.character(params$To), "Variance", params$Type)

  if ("p" %in% colnames(params)) {
    params$p <- ifelse(is.na(params$p), 0, params$p)
  }

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }

  params
}






# tools -------------------------


.check_rank_deficiency <- function(p, verbose = TRUE) {
  if (anyNA(p$Estimate)) {
    if (isTRUE(verbose)) warning(sprintf("Model matrix is rank deficient. Parameters %s were not estimable.", paste(p$Parameter[is.na(p$Estimate)], collapse = ", ")), call. = FALSE)
    p <- p[!is.na(p$Estimate), ]
  }
  p
}
