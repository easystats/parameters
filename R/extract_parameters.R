# generic function ------------------------------------------------------


#' @importFrom insight get_statistic get_parameters
#' @importFrom stats confint p.adjust.methods p.adjust
#' @keywords internal
.extract_parameters_generic <- function(model, ci, component, merge_by = c("Parameter", "Component"), standardize = NULL, effects = "fixed", robust = FALSE, df_method = NULL, p_adjust = NULL, ...) {

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

  parameters <- insight::get_parameters(model, effects = effects, component = component)
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


  # ==== Std Coefficients for other methods than "refit"

  if (!is.null(standardize)) {
    # standardize model parameters and calculate related CI and SE
    std_coef <- effectsize::standardize_parameters(model, method = standardize, ci = NULL)
    parameters <- merge(parameters, std_coef, by = merge_by)
    coef_col <- "Std_Coefficient"
    # merge all data, including CI and SE for std. parameters
    if (inherits(std_coef, "effectsize_std_params")) {
      parameters <- merge(parameters, ci(std_coef, ci = ci), by = merge_by)
      parameters <- merge(parameters, standard_error(std_coef), by = merge_by)
    }
    # if we have CIs, remember columns names to select later
    if (!is.null(ci)) {
      ci_cols <- c("CI_low", "CI_high")
    } else {
      ci_cols <- c()
    }
  }


  # ==== CI - only if we don't already have CI for std. parameters

  if (is.null(standardize)) {
    if (!is.null(ci)) {
      if (isTRUE(robust)) {
        ci_df <- suppressMessages(ci_robust(model, ci = ci, ...))
      } else if (!is.null(df_method)) {
        ci_df <- suppressMessages(ci(model, ci = ci, effects = effects, component = component, method = df_method))
      } else {
        ci_df <- suppressMessages(ci(model, ci = ci, effects = effects, component = component))
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
  }


  # ==== p value

  if (isTRUE(robust)) {
    pval <- p_value_robust(model, ...)
  } else if (!is.null(df_method)) {
    pval <- p_value(model, effects = effects, component = component, method = df_method)
  } else {
    pval <- p_value(model, effects = effects, component = component)
  }

  if (!is.null(pval)) {
    parameters <- merge(parameters, pval, by = merge_by)
  }


  # ==== standard error - only if we don't already have SE for std. parameters

  std_err <- NULL
  if (is.null(standardize)) {
    if (isTRUE(robust)) {
      std_err <- standard_error_robust(model, ...)
    } else if (!is.null(df_method)) {
      std_err <- standard_error(model, effects = effects, component = component, method = df_method)
    } else {
      std_err <- standard_error(model, effects = effects, component = component)
    }
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
    names(parameters) <- gsub("Statistic", gsub("(-|\\s)statistic", "", attr(statistic, "statistic", exact = TRUE)), names(parameters))
    names(parameters) <- gsub("chi-squared", "chisq", names(parameters))
  }
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))


  # ==== Reorder

  col_order <- c("Parameter", coef_col, "SE", ci_cols, "t", "z", "t / F", "z / Chisq", "F", "chisq", "chi-squared", "df", "df_error", "p", "Component", "Response", "Effects")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]


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


  rownames(parameters) <- NULL
  parameters
}




# mixed models function ------------------------------------------------------


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model, ci = .95, df_method = "wald", standardize = NULL, robust = FALSE, p_adjust = NULL, ...) {
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


  # Std Coefficients for other methods than "refit"
  if (!is.null(standardize)) {
    # standardize model parameters and calculate related CI and SE
    std_coef <- effectsize::standardize_parameters(model, method = standardize, ci = NULL)
    parameters <- merge(parameters, std_coef, by = "Parameter")
    coef_col <- "Std_Coefficient"
    # merge all data, including CI and SE for std. parameters
    if (inherits(std_coef, "effectsize_std_params")) {
      parameters <- merge(parameters, ci(std_coef, ci = ci), by = "Parameter")
      parameters <- merge(parameters, standard_error(std_coef), by = "Parameter")
    }
    # if we have CIs, remember columns names to select later
    if (!is.null(ci)) {
      ci_cols <- c("CI_low", "CI_high")
    } else {
      ci_cols <- c()
    }
  }


  # CI - only if we don't already have CI for std. parameters
  if (is.null(standardize)) {
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
  }


  # standard error - only if we don't already have SE for std. parameters
  if (is.null(standardize) || !("SE" %in% colnames(parameters))) {
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
  if (!isTRUE(robust) && is.null(standardize) && df_method %in% special_df_methods) {
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

  # Reorder
  order <- c("Parameter", coef_col, "SE", ci_cols, "t", "z", "df", "df_error", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  # adjust p-values?
  if (!is.null(p_adjust) && tolower(p_adjust) %in% stats::p.adjust.methods && "p" %in% colnames(parameters)) {
    parameters$p <- stats::p.adjust(parameters$p, method = p_adjust)
  }

  # if we have within/between effects (from demean()), we can add a component
  # column for nicer printing...
  parameters <- .add_within_between_effects(model, parameters)

  rownames(parameters) <- NULL
  parameters
}



.add_within_between_effects <- function(model, parameters) {

  # This function checks whether the model contains predictors that were
  # "demeaned" using the "demean()" function. If so, these columns have an
  # attribute indicating the within or between effect, and in such cases,
  # this effect is used as "Component" value. by this, we get a nicer print
  # for model parameters...

  parameters$Component <- "rewb-contextual"

  within_effects <- .find_within_between(model, "within-effect")
  if (!is.null(within_effects)) {
    index <- unique(unlist(sapply(within_effects, function(i) {
      grep(i, parameters$Parameter, fixed = TRUE)
    })))
    parameters$Component[index] <- "within"
  }

  between_effects <- .find_within_between(model, "between-effect")
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

  if (!("within" %in% parameters$Component) || !("between" %in% parameters$Component)) {
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


#' @importFrom stats sd setNames na.omit
#' @keywords internal
.extract_parameters_bayesian <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, standardize = NULL, ...) {
  # check if standardization is required and package available
  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # MCMCglmm need special handling
  if (inherits(model, "MCMCglmm")) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, diagnostic = "ESS", ...)
  } else if (!is.null(standardize)) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

    # Don't test BF on standardized params
    test_no_BF <- test[!test %in% c("bf", "bayesfactor", "bayes_factor")]
    if (length(test_no_BF) == 0) test_no_BF <- NULL
    std_post <- effectsize::standardize_posteriors(model, method = standardize)
    std_parameters <- bayestestR::describe_posterior(std_post, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test_no_BF, rope_range = rope_range, rope_ci = rope_ci, ...)

    parameters <- merge(std_parameters, parameters[c("Parameter", setdiff(colnames(parameters), colnames(std_parameters)))], sort = FALSE)
  } else {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)
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

  # CI
  if (length(ci) > 1) {
    ci <- ci[1]
    warning(paste0("lavaan models only accept one level of CI :( Keeping the first one: `ci = ", ci, "`."))
  }

  # Get estimates
  if (standardize == FALSE) {
    data <- lavaan::parameterEstimates(model, se = TRUE, level = ci, ...)
  } else {
    data <- lavaan::standardizedsolution(model, se = TRUE, level = ci, ...)
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
    p = data$pvalue
  )

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
  params$p <- ifelse(is.na(params$p), 0, params$p)

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }

  params
}




#' @keywords internal
.extract_parameters_blavaan <- function(model, ci = 0.95, standardize = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }

  # CI
  if (length(ci) > 1) {
    ci <- ci[1]
    warning(paste0("blavaan models only accept one level of CI :( Keeping the first one: `ci = ", ci, "`."))
  }

  # Get estimates
  if (standardize == FALSE) {
    data <- lavaan::parameterEstimates(model, se = TRUE, level = ci, ...)
  } else {
    data <- lavaan::standardizedsolution(model, se = TRUE, level = ci, ...)
    names(data)[names(data) == "est.std"] <- "est"
  }



  params <- data.frame(
    To = data$lhs,
    Operator = data$op,
    From = data$rhs,
    Coefficient = data$est,
    SE = data$se,
    CI_low = data$ci.lower,
    CI_high = data$ci.upper
  )

  params$Type <- ifelse(params$Operator == "=~", "Loading",
    ifelse(params$Operator == "~", "Regression",
      ifelse(params$Operator == "~~", "Correlation",
        ifelse(params$Operator == "~1", "Mean", NA)
      )
    )
  )
  params$Type <- ifelse(as.character(params$From) == as.character(params$To), "Variance", params$Type)

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }

  params
}







# Lame models ------------------------------------------------------


#' @keywords internal
.extract_parameters_anova <- function(model) {

  # Processing
  if ("manova" %in% class(model)) {
    parameters <- as.data.frame(summary(model)$stats)
    parameters$Parameter <- trimws(row.names(parameters))
    parameters[["den Df"]] <- NULL
    parameters[["num Df"]] <- NULL
  } else if ("aov" %in% class(model)) {
    parameters <- as.data.frame(summary(model)[[1]])
    parameters$Parameter <- trimws(row.names(parameters))
  } else if ("anova" %in% class(model)) {
    parameters <- as.data.frame(model)
    parameters$Parameter <- trimws(row.names(parameters))
    # Deal with anovas of models
    if (length(attributes(model)$heading) == 2) {
      info <- attributes(model)$heading[[2]]
      if (grepl("Model", info)) {
        parameters$Parameter <- unlist(strsplit(info, "\n", fixed = TRUE))
      }
    } else if (length(attributes(model)$heading) > 2) {
      parameters$Parameter <- attributes(model)$heading[-1:-2]
    }

    # If mixed models...
    sumsq <- names(parameters)[names(parameters) %in% c("Sum Sq", "Sum of Sq")]
    df_num <- names(parameters)[names(parameters) %in% c("npar", "Df", "NumDF")]
    mean_sq <- names(parameters)[names(parameters) %in% c("Mean Sq")]

    if (length(sumsq) != 0 && length(df_num) != 0) {
      parameters$Mean_Square <- parameters[[sumsq]] / parameters[[df_num]]
    } else if (length(mean_sq) != 0) {
      parameters$Mean_Square <- parameters[[mean_sq]]
    }

    if (length(df_num) == 0 && length(sumsq) != 0 && "Mean_Square" %in% colnames(parameters) && !("Df" %in% colnames(parameters))) {
      parameters$Df <- round(parameters[[sumsq]] / parameters$Mean_Square)
    }
  } else if ("aovlist" %in% class(model)) {
    if (names(model)[1L] == "(Intercept)") {
      model <- model[-1L]
    }
    parameters <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), lapply(names(model), function(i) {
      aov_summary <- summary(model[[i]])
      if (inherits(aov_summary, "summary.manova")) {
        temp <- as.data.frame(aov_summary$stats)
      } else {
        temp <- as.data.frame(aov_summary[[1]])
      }
      temp$Parameter <- trimws(row.names(temp))
      temp$Group <- i
      temp
    }))
    # parameters <- parameters[order(parameters$Group), ]
  } else if ("anova.rms" %in% class(model)) {
    parameters <- data.frame(model)
    parameters$Parameter <- rownames(parameters)
    parameters$Parameter[parameters$Parameter == "ERROR"] <- "Residuals"
    parameters$Parameter[parameters$Parameter == "TOTAL"] <- "Total"
  }

  # Rename
  names(parameters) <- gsub("(Pr|P)\\(>.*\\)", "p", names(parameters))
  names(parameters) <- gsub("npar", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("NumDF", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("d.f.", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.Df", "Chisq_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi DoF", "Chisq_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Partial.SS", "Sum_Squares_Partial", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum of Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Mean Sq", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("MS", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("approx F", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("F value", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.Df", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.DoF", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chisq.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chi.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.sq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi-Square", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR.Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("p.value", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("logLik", "Log_Likelihood", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("deviance", "Deviance", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("^P$", "p", names(parameters))
  names(parameters) <- gsub("Df", "df", names(parameters), fixed = TRUE)

  # Reorder
  row.names(parameters) <- NULL
  order <- c("Group", "Parameter", "Pillai", "AIC", "BIC", "Log_Likelihood", "Deviance", "Chisq", "Chisq_df", "RSS", "Sum_Squares", "Sum_Squares_Partial", "df", "df_residual", "Mean_Square", "F", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  .remove_backticks_from_parameter_names(parameters)
}


#' @keywords internal
.extract_parameters_htest <- function(model) {
  if (insight::model_info(model)$is_correlation) {
    names <- unlist(strsplit(model$data.name, " and "))
    out <- data.frame(
      "Parameter1" = names[1],
      "Parameter2" = names[2]
    )

    if (model$method == "Pearson's Chi-squared test") {
      out$Chisq <- model$statistic
      out$df <- model$parameter
      out$p <- model$p.value
      out$Method <- "Pearson"
    } else if (grepl("Pearson", model$method)) {
      out$r <- model$estimate
      out$t <- model$statistic
      out$df <- model$parameter
      out$p <- model$p.value
      out$CI_low <- model$conf.int[1]
      out$CI_high <- model$conf.int[2]
      out$Method <- "Pearson"
    } else if (grepl("Spearman", model$method)) {
      out$rho <- model$estimate
      out$S <- model$statistic
      out$df <- model$parameter
      out$p <- model$p.value
      out$Method <- "Spearman"
    } else {
      out$tau <- model$estimate
      out$z <- model$statistic
      out$df <- model$parameter
      out$p <- model$p.value
      out$Method <- "Kendall"
    }
  } else if (insight::model_info(model)$is_ttest) {
    if (grepl(" and ", model$data.name)) {
      names <- unlist(strsplit(model$data.name, " and "))
      out <- data.frame(
        "Parameter1" = names[1],
        "Parameter2" = names[2],
        "Mean_Parameter1" = model$estimate[1],
        "Mean_Parameter2" = model$estimate[2],
        "Difference" = model$estimate[1] - model$estimate[2],
        "t" = model$statistic,
        "df" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "Method" = model$method
      )
    } else if (grepl(" by ", model$data.name)) {
      names <- unlist(strsplit(model$data.name, " by "))
      out <- data.frame(
        "Parameter" = names[1],
        "Group" = names[2],
        "Mean_Group1" = model$estimate[1],
        "Mean_Group2" = model$estimate[2],
        "Difference" = model$estimate[2] - model$estimate[1],
        "t" = model$statistic,
        "df" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "Method" = model$method
      )
    } else {
      out <- data.frame(
        "Parameter" = model$data.name,
        "Mean" = model$estimate,
        "mu" = model$null.value,
        "Difference" = model$estimate - model$null.value,
        "t" = model$statistic,
        "df" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "Method" = model$method
      )
    }
  } else {
    stop("model_parameters not implemented for such h-tests yet.")
  }

  row.names(out) <- NULL
  out
}



.check_rank_deficiency <- function(p, verbose = TRUE) {
  if (anyNA(p$Estimate)) {
    if (isTRUE(verbose)) warning(sprintf("Model matrix is rank deficient. Parameters %s were not estimable.", paste(p$Parameter[is.na(p$Estimate)], collapse = ", ")), call. = FALSE)
    p <- p[!is.na(p$Estimate), ]
  }
  p
}
