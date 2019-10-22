# generic function ------------------------------------------------------


#' @importFrom insight get_statistic get_parameters
#' @importFrom stats confint
#' @keywords internal
.extract_parameters_generic <- function(model, ci, component, merge_by = c("Parameter", "Component"), ...) {
  parameters <- insight::get_parameters(model, effects = "fixed", component = component)
  .statistic <- insight::get_statistic(model, component = component)

  # clean parameter names

  if (inherits(model, "polr")) {
    ## TODO replace with "$Parameter" once insight update is on CRAN
    parameters[[1]] <- gsub("Intercept: ", "", parameters[[1]], fixed = TRUE)
  }

  ## TODO remove, once fixed in insight
  colnames(parameters) <- .capitalize(colnames(parameters))
  original_order <- parameters$.id <- 1:nrow(parameters)

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci, component = component)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", merge_by)]
    parameters <- merge(parameters, ci_df, by = merge_by)
  } else {
    ci_cols <- c()
  }


  # p value
  parameters <- merge(parameters, p_value(model, component = component), by = merge_by)

  # standard error
  parameters <- merge(parameters, standard_error(model, component = component), by = merge_by)

  # test statistic
  parameters <- merge(parameters, .statistic, by = merge_by)

  # dof
  df_residual <- degrees_of_freedom(model, method = "any")
  if (!is.null(df_residual) && (length(df_residual) == 1 || length(df_residual) == nrow(parameters))) {
    parameters$df_residual <- df_residual
  }

  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub("Statistic", gsub("-statistic", "", attr(.statistic, "statistic", exact = TRUE), fixed = TRUE), names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))

  # Reorder
  col_order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "t / F", "z / Chisq", "F", "chisq", "df", "df_residual", "p", "Component", "Response")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]

  # remove Component column if not needed
  if (length(unique(parameters$Component)) == 1) parameters$Component <- NULL

  rownames(parameters) <- NULL
  parameters
}




# glm function ------------------------------------------------------


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95, linear = FALSE) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)

  if (linear) {
    names(parameters) <- c("Coefficient", "SE", "t", "p")
  } else {
    names(parameters) <- c("Coefficient", "SE", "z", "p")
  }


  parameters$df_residual <- degrees_of_freedom(model, method = "any")
  parameters$Parameter <- row.names(parameters)

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]

    col_order <- parameters$Parameter
    parameters <- merge(parameters, ci_df, by = "Parameter")
    parameters <- parameters[match(col_order, parameters$Parameter), ]
  } else {
    ci_cols <- c()
  }


  # Reorder
  order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  rownames(parameters) <- NULL
  parameters
}






# mixed models function ------------------------------------------------------


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model, ci = .95, p_method = "wald", ci_method = "wald", ...) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  parameters$Parameter <- row.names(parameters)
  original_order <- parameters$Parameter

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci, method = ci_method)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]

    col_order <- parameters$Parameter
    parameters <- merge(parameters, ci_df, by = "Parameter")
    parameters <- parameters[match(col_order, parameters$Parameter), ]
  } else {
    ci_cols <- c()
  }


  # p value
  if ("Pr(>|z|)" %in% names(parameters)) {
    names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
  } else {
    if (insight::model_info(model)$is_linear) {
      if (p_method == "kenward") {
        parameters$df <- dof_kenward(model)
        parameters <- merge(parameters, p_value(model, method = "kenward", dof = parameters$DoF), by = "Parameter")
      } else {
        parameters <- merge(parameters, p_value(model, method = p_method), by = "Parameter")
      }
    } else {
      parameters <- merge(parameters, p_value(model), by = "Parameter")
    }
  }


  # adjust standard errors as well
  if (p_method == "kenward" || ci_method == "kenward") {
    parameters[["Std. Error"]] <- se_kenward(model)
  }


  # dof
  if (p_method != "kenward" && !"df" %in% names(parameters)) {
    df_residual <- degrees_of_freedom(model, method = "any")
    if (!is.null(df_residual) && (length(df_residual) == 1 || length(df_residual) == nrow(parameters))) {
      parameters$df_residual <- df_residual
    }
  }


  # Rematch order after merging
  parameters <- parameters[match(parameters$Parameter, original_order), ]
  row.names(parameters) <- NULL

  # Renaming
  names(parameters) <- gsub("Std. Error", "SE", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))
  names(parameters) <- gsub("t value", "t", names(parameters))
  names(parameters) <- gsub("z value", "z", names(parameters))

  # Reorder
  order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df", "df_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  rownames(parameters) <- NULL
  parameters
}





# Bayes function ------------------------------------------------------


#' @importFrom stats sd setNames
#' @keywords internal
.extract_parameters_bayesian <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, ...) {

  # Bayesian Models
  if (insight::model_info(model)$is_bayesian) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

    # MCMCglmm need special handling
  } else if (inherits(model, "MCMCglmm")) {
    parameters <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, diagnostic = "ESS", ...)

    # Bootstrapped Models
  } else {
    data <- model_bootstrap(model, iterations = iterations)
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

  parameters
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
