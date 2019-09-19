#' ANOVAs Parameters
#'
#' Parameters of ANOVAs.
#'
#' @param model Object of class \link{aov}, \link{anova} or \code{aovlist}.
#' @param omega_squared Compute \link[parameters:eta_squared]{omega squared} as index of effect size. Can be "partial" (adjusted for effect size) or "raw".
#' @param eta_squared Compute \link[parameters:eta_squared]{eta squared} as index of effect size. Can be "partial" (adjusted for effect size) or "raw".
#' @param epsilon_squared Compute \link[parameters:eta_squared]{epsilon squared} as index of effect size.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
#' model_parameters(model)
#' model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)
#'
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' model_parameters(model)
#' \donttest{
#' library(lme4)
#'
#' model <- anova(lmer(Sepal.Length ~ Sepal.Big + (1 | Species), data = df))
#' model_parameters(model)
#' }
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters.aov <- function(model, omega_squared = NULL, eta_squared = NULL, epsilon_squared = NULL, ...) {
  parameters <- .extract_parameters_anova(model)

  # Sanity checks
  if (!is.null(omega_squared) | !is.null(eta_squared) | !is.null(epsilon_squared)) {
    if (!"Residuals" %in% parameters$Parameter) {
      warning("No residuals data found. Omega squared can only be computed for simple `aov` models.")
      omega_squared <- NULL
    }

    if ("Group" %in% names(parameters) && ("Within" %in% parameters$Group)) {
      warning("Omega squared not implemented yet for repeated-measures ANOVAs.")
      omega_squared <- NULL
    }
  }


  # Effect sizes ------------------------------------------------------------
  # Omega squared
  if (!is.null(omega_squared)) {
    if (omega_squared == "partial") {
      parameters$Omega_Sq_partial <- omega_squared(model, partial = TRUE)$Omega_Sq_partial
    } else {
      parameters$Omega_Sq <- omega_squared(model, partial = FALSE)$Omega_Sq
    }
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "partial") {
      parameters$Eta_Sq_partial <- eta_squared(model, partial = TRUE)$Eta_Sq_partial
    } else {
      parameters$Eta_Sq <- eta_squared(model, partial = FALSE)$Eta_Sq
    }
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    parameters$Epsilon_sq <- epsilon_squared(model)$Epsilon_sq
  }

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}



#' @rdname model_parameters.aov
#' @export
model_parameters.anova <- model_parameters.aov

#' @rdname model_parameters.aov
#' @export
model_parameters.aovlist <- model_parameters.aov












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
