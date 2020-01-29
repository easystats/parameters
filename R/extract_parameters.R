# generic function ------------------------------------------------------


#' @importFrom insight get_statistic get_parameters
#' @importFrom stats confint
#' @keywords internal
.extract_parameters_generic <- function(model, ci, component, merge_by = c("Parameter", "Component"), standardize = NULL, effects = "fixed", robust = FALSE, df_method = NULL, ...) {
  # check if standardization is required and package available
  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # for refit, we completely refit the model, than extract parameters, ci etc. as usual
  if (!is.null(standardize) && standardize == "refit") {
    model <- effectsize::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  parameters <- insight::get_parameters(model, effects = effects, component = component)
  statistic <- insight::get_statistic(model, component = component)

  # check if we really have a component column
  if (!("Component" %in% names(parameters)) && "Component" %in% merge_by) {
    merge_by <- setdiff(merge_by, "Component")
  }

  # check Degrees of freedom
  if (!.dof_method_ok(model, df_method)) {
    df_method <- NULL
  }


  # clean parameter names

  if (inherits(model, "polr")) {
    parameters$Parameter <- gsub("Intercept: ", "", parameters$Parameter, fixed = TRUE)
  }

  original_order <- parameters$.id <- 1:nrow(parameters)

  # column name for coefficients, non-standardized
  coef_col <- "Coefficient"

  # Std Coefficients for other methods than "refit"
  if (!is.null(standardize)) {
    # standardize model parameters and calculate related CI and SE
    std_coef <- effectsize::standardize_parameters(model, method = standardize)
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


  # CI - only if we don't already have CI for std. parameters
  if (is.null(standardize)) {
    if (!is.null(ci)) {
      if (isTRUE(robust)) {
        ci_df <- suppressMessages(ci_robust(model, ci = ci, ...))
      } else if (!is.null(df_method)) {
        ci_df <- suppressMessages(ci(model, ci = ci, effects = effects, component = component, method = df_method))
      } else {
        ci_df <- suppressMessages(ci(model, ci = ci, effects = effects, component = component))
      }
      if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
      ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", merge_by)]
      parameters <- merge(parameters, ci_df, by = merge_by)
    } else {
      ci_cols <- c()
    }
  }


  # p value
  if (isTRUE(robust)) {
    parameters <- merge(parameters, p_value_robust(model, ...), by = merge_by)
  } else if (!is.null(df_method)) {
    parameters <- merge(parameters, p_value(model, effects = effects, component = component, method = df_method), by = merge_by)
  } else {
    parameters <- merge(parameters, p_value(model, effects = effects, component = component), by = merge_by)
  }


  # standard error - only if we don't already have SE for std. parameters
  if (is.null(standardize)) {
    if (isTRUE(robust)) {
      parameters <- merge(parameters, standard_error_robust(model, ...), by = merge_by)
    } else if (!is.null(df_method)) {
      parameters <- merge(parameters, standard_error(model, effects = effects, component = component, method = df_method), by = merge_by)
    } else {
      parameters <- merge(parameters, standard_error(model, effects = effects, component = component), by = merge_by)
    }
  }


  # test statistic - fix values for robust estimation
  if (isTRUE(robust)) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else {
    parameters <- merge(parameters, statistic, by = merge_by)
  }


  # dof
  if (!is.null(df_method)) {
    df_error <- degrees_of_freedom(model, method = df_method)
  } else {
    df_error <- degrees_of_freedom(model, method = "any")
  }
  if (!is.null(df_error) && (length(df_error) == 1 || length(df_error) == nrow(parameters))) {
    parameters$df_error <- df_error
  }


  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub("Statistic", gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE), names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))

  # Reorder
  col_order <- c("Parameter", coef_col, "SE", ci_cols, "t", "z", "t / F", "z / Chisq", "F", "chisq", "df", "df_error", "p", "Component", "Response", "Effects")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]

  # remove Component column if not needed
  if (length(unique(parameters$Component)) == 1) parameters$Component <- NULL
  if (length(unique(parameters$Effects)) == 1 || effects == "fixed") parameters$Effects <- NULL

  rownames(parameters) <- NULL
  parameters
}




# mixed models function ------------------------------------------------------


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model, ci = .95, df_method = "wald", standardize = NULL, robust = FALSE, ...) {
  # check if standardization is required and package available
  if (!is.null(standardize) && !requireNamespace("effectsize", quietly = TRUE)) {
    insight::print_color("Package 'effectsize' required to calculate standardized coefficients. Please install it.\n", "red")
    standardize <- NULL
  }

  # for refit, we completely refit the model, than extract parameters, ci etc. as usual
  if (!is.null(standardize) && standardize == "refit") {
    model <- effectsize::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  parameters <- insight::get_parameters(model, effects = "fixed", component = "all")
  statistic <- insight::get_statistic(model, component = "all")

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


  # Std Coefficients for other methods than "refit"
  if (!is.null(standardize)) {
    # standardize model parameters and calculate related CI and SE
    std_coef <- effectsize::standardize_parameters(model, method = standardize)
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
      } else {
        ci_df <- ci(model, ci = ci, method = df_method)
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
    } else {
      parameters <- merge(parameters, standard_error(model, method = df_method), by = "Parameter")
    }
  }


  # p value
  if (isTRUE(robust)) {
    parameters <- merge(parameters, p_value_robust(model, ...), by = "Parameter")
  } else {
    if ("Pr(>|z|)" %in% names(parameters)) {
      names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
    } else {
      parameters <- merge(parameters, p_value(model, dof = df), by = "Parameter")
    }
  }


  # adjust standard errors and test-statistic as well
  if (!isTRUE(robust) && is.null(standardize) && df_method %in% c("betwithin", "ml1", "kenward", "kr")) {
    parameters$Statistic <- parameters$Estimate / parameters$SE
  } else {
    parameters <- merge(parameters, statistic, by = "Parameter")
  }


  # dof
  if (!"df" %in% names(parameters)) {
    if (df_method %in% c("betwithin", "ml1", "satterthwaite", "kenward", "kr"))
      df_error <- df
    else
      df_error <- degrees_of_freedom(model, method = "any")
    if (!is.null(df_error) && (length(df_error) == 1 || length(df_error) == nrow(parameters))) {
      parameters$df_error <- df_error
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

  rownames(parameters) <- NULL
  parameters
}





# Bayes function ------------------------------------------------------


#' @importFrom stats sd setNames
#' @keywords internal
.extract_parameters_bayesian <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, ...) {

  # MCMCglmm need special handling
  if (inherits(model, "MCMCglmm")) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, diagnostic = "ESS", ...)

    # Bayesian Models
  } else if ((insight::is_multivariate(model) && insight::model_info(model)[[1]]$is_bayesian) || insight::model_info(model)$is_bayesian) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

    # Bootstrapped Models
  } else {
    data <- bootstrap_model(model, iterations = iterations)
    parameters <- bayestestR::describe_posterior(data, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, ...)
  }

  if (length(ci) > 1) {
    parameters <- bayestestR::reshape_ci(parameters)
  }

  # Remove unecessary columns
  if ("CI" %in% names(parameters) && length(unique(parameters$CI)) == 1) {
    parameters$CI <- NULL
  }
  if ("ROPE_CI" %in% names(parameters) && length(unique(parameters$ROPE_CI)) == 1) {
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
        ifelse(params$Operator == "~1", "Mean", NA)
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
  if ("aov" %in% class(model)) {
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
    if (length(sumsq) != 0) {
      parameters$Mean_Square <- parameters[[sumsq]] / parameters[["Df"]]
    }
  } else if ("aovlist" %in% class(model)) {
    if (names(model)[1L] == "(Intercept)") {
      model <- model[-1L]
    }
    parameters <- data.frame()
    rowmax <- 0
    for (i in names(model)) {
      temp <- as.data.frame(summary(model[[i]])[[1]])
      temp$Parameter <- trimws(row.names(temp))
      temp$Group <- i
      row.names(temp) <- 1:nrow(temp) + rowmax
      rowmax <- nrow(temp)
      if (nrow(parameters) == 0) {
        parameters <- temp
      } else {
        parameters <- merge(parameters, temp, all = TRUE)
      }
    }
    parameters <- parameters[order(parameters$Group), ]
  }

  # Rename
  names(parameters) <- gsub("Pr(>F)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Df", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.Df", "Chisq_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi DoF", "Chisq_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Mean Sq", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("F value", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.Df", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.DoF", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum of Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr(>Chi_Square)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr(>ChiSquare)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr(>Chisq)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("P(>|Chi|)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr(>Chi)", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chisq.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chi.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.sq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR.Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR Chisq", "Chisq", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("p.value", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("logLik", "Log_Likelihood", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("deviance", "Deviance", names(parameters), fixed = TRUE)

  # Reorder
  row.names(parameters) <- NULL
  order <- c("Group", "Parameter", "AIC", "BIC", "Log_Likelihood", "Deviance", "Chisq", "Chisq_df", "RSS", "Sum_Squares", "df", "df_residual", "Mean_Square", "F", "p")
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

    if (grepl("Pearson", model$method)) {
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
        "Difference" = model$estimate[2] - model$estimate[1],
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
