.effectsizes_for_anova <- function(model, parameters, omega_squared, eta_squared, epsilon_squared, df_error, ci = NULL) {
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' required for this function to work. Please install it.")
  }

  # user actually does not want to compute effect sizes
  if (is.null(omega_squared) && is.null(eta_squared) && is.null(epsilon_squared)) {
    return(parameters)
  }

  # set defaults
  if (isTRUE(omega_squared)) {
    omega_squared <- "partial"
  }
  if (isTRUE(eta_squared)) {
    eta_squared <- "partial"
  }
  if (isTRUE(epsilon_squared)) {
    epsilon_squared <- "partial"
  }

  # check if we have any information on denominator df
  if (is.null(df_error) && !("DenDF" %in% colnames(model))) {
    if ("Residuals" %in% parameters$Parameter) {
      df_col <- colnames(parameters)[colnames(parameters) %in% c("df", "df_error", "Df", "NumDF")]
      df_error <- parameters[parameters$Parameter == "Residuals", df_col]
    } else {
      warning("Cannot compute effect size without denominator degrees of freedom. Please specify 'df_error', or use package 'lmerTest' to fit your mixed model.", call. = FALSE)
      return(parameters)
    }
  }

  # denominator DF
  if (is.null(df_error)) {
    df_error <- model$DenDF
  } else if (length(df_error) > 1) {
    # term names
    rn <- rownames(model)

    # find predictors in degrees of freedom
    predictor_assignment <- sapply(rn, function(i) {
      grep(paste0("^", i), names(df_error))
    }, simplify = FALSE)

    # use average df or categorical
    df_error <- unlist(lapply(predictor_assignment, function(i) {
      round(mean(df_error[i]), 2)
    }))
  }

  # numerator DF
  df_num <- NULL
  df_num_cols <- colnames(model)[colnames(model) %in% c("npar", "Df", "NumDF")]

  if (length(df_num_cols) != 0) {
    df_num <- model[[df_num_cols[1]]]
  } else if ("df" %in% colnames(parameters)) {
    df_num <- parameters[["df"]]
  }

  # check if we have any information on denominator df
  if (is.null(df_num)) {
    warning("Cannot compute effect size without numerator degrees of freedom.", call. = FALSE)
    return(parameters)
  }

  # F value
  f_value <- model[["F value"]]

  # Omega squared
  if (!is.null(omega_squared)) {
    fx <- effectsize::F_to_omega2(f = f_value, df = df_num, df_error = df_error, ci = ci)
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "adjusted") {
      fx <- effectsize::F_to_eta2_adj(f = f_value, df = df_num, df_error = df_error, ci = ci)
    } else {
      fx <- effectsize::F_to_eta2(f = f_value, df = df_num, df_error = df_error, ci = ci)
    }
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    fx <- effectsize::F_to_epsilon2(f = f_value, df = df_num, df_error = df_error, ci = ci)
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  parameters
}
