#' Parameters from ANOVAs
#'
#' Parameters from ANOVAs.
#'
#' @param model Object of class \link{aov}, \link{anova} or \code{aovlist}.
#' @param omega_squared Compute omega squared as index of effect size. Can be \code{"partial"} (adjusted for effect size) or \code{"raw"}.
#' @param eta_squared Compute eta squared as index of effect size. Can be \code{"partial"} (adjusted for effect size), \code{"raw"}  or \code{"adjusted"} (the latter option only for anova-tables from mixed models).
#' @param epsilon_squared Compute epsilon squared as index of effect size. Can be \code{"partial"} (adjusted for effect size) or \code{"raw"}.
#' @param df_error Denominator degrees of freedom (or degrees of freedom of the error estimate, i.e., the residuals). This is used to compute effect sizes for anova tables from mixed models. See 'Examples'.
#' @param type Numeric, type of sums of squares. May be 1, 2 or 3. If 2 or 3, anova-tables using \code{car::Anova()} will be returned.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @note For anova-tables from mixed models (i.e. \code{anova(lmer())}), only partial or adjusted effect sizes can be computed.
#'
#' @examples
#' if (requireNamespace("effectsize", quietly = TRUE) &&
#'     packageVersion("effectsize") >= "0.2.9") {
#'   df <- iris
#'   df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#'   model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#'   model_parameters(
#'     model,
#'     omega_squared = "partial",
#'     eta_squared = "partial",
#'     epsilon_squared = "partial"
#'   )
#'
#'   model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
#'   model_parameters(model)
#'   model_parameters(
#'     model,
#'     omega_squared = "partial",
#'     eta_squared = "partial",
#'     epsilon_squared = "partial"
#'   )
#'
#'   model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#'   model_parameters(model)
#'
#'   if (require("lme4")) {
#'     mm <- lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species),
#'                data = df)
#'     model <- anova(mm)
#'
#'     # simple parameters table
#'     model_parameters(model)
#'
#'     # parameters table including effect sizes
#'     model_parameters(
#'       model,
#'       eta_squared = "partial",
#'       df_error = dof_satterthwaite(mm)
#'     )
#'   }
#' }
#' @export
model_parameters.aov <- function(model, omega_squared = NULL, eta_squared = NULL, epsilon_squared = NULL, df_error = NULL, type = NULL, ...) {
  if (inherits(model, "aov") && !is.null(type) && type > 1) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning("Package 'car' required for type-2 or type-3 anova. Defaulting to type-1.", call. = FALSE)
    } else {
      model <- car::Anova(model, type = type)
    }
  }

  parameters <- .extract_parameters_anova(model)

  if (inherits(model, "anova")) {
    parameters <- .effectsizes_for_anova(model, parameters, omega_squared, eta_squared, epsilon_squared, df_error)
  } else {
    parameters <- .effectsizes_for_aov(model, parameters, omega_squared, eta_squared, epsilon_squared)
  }

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}



#' @export
model_parameters.anova <- model_parameters.aov

#' @export
model_parameters.aovlist <- model_parameters.aov









# helper ------------------------------



.effectsizes_for_aov <- function(model, parameters, omega_squared, eta_squared, epsilon_squared) {
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' required for this function to work. Please install it.")
  }

  # Sanity checks
  if (!is.null(omega_squared) | !is.null(eta_squared) | !is.null(epsilon_squared)) {
    if (!"Residuals" %in% parameters$Parameter) {
      warning("No residuals data found. Effect size cannot be computed.", call. = FALSE)
      return(parameters)
    }
  }


  # Omega squared
  if (!is.null(omega_squared)) {
    if (omega_squared == "partial") {
      fx <- effectsize::omega_squared(model, partial = TRUE)$Omega_Sq_partial
      parameters$Omega_Sq_partial <- .fix_effectsize_rows(fx, parameters)
    } else {
      fx <- effectsize::omega_squared(model, partial = FALSE)$Omega_Sq
      parameters$Omega_Sq <- .fix_effectsize_rows(fx, parameters)
    }
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "partial") {
      fx <- effectsize::eta_squared(model, partial = TRUE)$Eta_Sq_partial
      parameters$Eta_Sq_partial <- .fix_effectsize_rows(fx, parameters)
    } else {
      fx <- effectsize::eta_squared(model, partial = FALSE)$Eta_Sq
      parameters$Eta_Sq <- .fix_effectsize_rows(fx, parameters)
    }
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    if (epsilon_squared == "partial") {
      fx <- effectsize::epsilon_squared(model, partial = TRUE)$Epsilon_Sq_partial
      parameters$Epsilon_Sq_partial <- .fix_effectsize_rows(fx, parameters)
    } else {
      fx <- effectsize::epsilon_squared(model, partial = FALSE)$Epsilon_Sq
      parameters$Epsilon_Sq <- .fix_effectsize_rows(fx, parameters)
    }
  }

  parameters
}





.effectsizes_for_anova <- function(model, parameters, omega_squared, eta_squared, epsilon_squared, df_error) {
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' required for this function to work. Please install it.")
  }

  # user actually does not want to compute effect sizes
  if (is.null(omega_squared) && is.null(eta_squared) && is.null(epsilon_squared)) {
    return(parameters)
  }

  # check if we have any information on denominator df
  if (is.null(df_error) && !("DenDF" %in% colnames(model))) {
    warning("Cannot compute effect size without denominator degrees of freedom. Please specify 'df_error', or use package 'lmerTest' to fit your mixed model.", call. = FALSE)
    return(parameters)
  }

  # denominator DF
  if (is.null(df_error)) {
    df_error <- model$DenDF
  } else {
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
  df_num_cols <- colnames(model)[colnames(model) %in% c("Df", "NumDF")]

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
    fx <- effectsize::F_to_omega2(f = f_value, df = df_num, df_error = df_error, ci = NA)[[1]]
    parameters$Omega_Sq_partial <- .fix_effectsize_rows(fx, parameters)
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "adjusted") {
      fx <- effectsize::F_to_eta2_adj(f = f_value, df = df_num, df_error = df_error, ci = NA)[[1]]
      parameters$Eta_Sq_partial <- .fix_effectsize_rows(fx, parameters)
    } else {
      fx <- effectsize::F_to_eta2(f = f_value, df = df_num, df_error = df_error, ci = NA)[[1]]
      parameters$Eta_Sq_partial <- .fix_effectsize_rows(fx, parameters)
    }
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    fx <- effectsize::F_to_epsilon2(f = f_value, df = df_num, df_error = df_error, ci = NA)[[1]]
    parameters$Epsilon_sq <- .fix_effectsize_rows(fx, parameters)
  }

  parameters
}



.fix_effectsize_rows <- function(fx, parameters) {
  stat_column <- colnames(parameters)[colnames(parameters) %in% c("F", "t", "z", "statistic")]
  if (nrow(parameters) > length(fx)) {
    es <- rep_len(NA, length.out = nrow(parameters))
    es[!is.na(parameters[[stat_column]])] <- fx
    fx <- es
  }
  fx
}
