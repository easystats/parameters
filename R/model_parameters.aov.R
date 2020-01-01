#' ANOVAs Parameters
#'
#' Parameters of ANOVAs.
#'
#' @param model Object of class \link{aov}, \link{anova} or \code{aovlist}.
#' @param omega_squared Compute omega squared as index of effect size. Can be \code{"partial"} (adjusted for effect size) or \code{"raw"}.
#' @param eta_squared Compute eta squared as index of effect size. Can be \code{"partial"} (adjusted for effect size) or \code{"raw"}.
#' @param epsilon_squared Compute epsilon squared as index of effect size.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame of indices related to the model's parameters.
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
#'
#' library(lme4)
#' model <- anova(lmer(Sepal.Length ~ Sepal.Big + (1 | Species), data = df))
#' model_parameters(model)
#' @export
model_parameters.aov <- function(model, omega_squared = NULL, eta_squared = NULL, epsilon_squared = NULL, ...) {
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' required for this function to work. Please install it.")
  }

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
      parameters$Omega_Sq_partial <- effectsize::omega_squared(model, partial = TRUE)$Omega_Sq_partial
    } else {
      parameters$Omega_Sq <- effectsize::omega_squared(model, partial = FALSE)$Omega_Sq
    }
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "partial") {
      parameters$Eta_Sq_partial <- effectsize::eta_squared(model, partial = TRUE)$Eta_Sq_partial
    } else {
      parameters$Eta_Sq <- effectsize::eta_squared(model, partial = FALSE)$Eta_Sq
    }
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    parameters$Epsilon_sq <- effectsize::epsilon_squared(model)$Epsilon_sq
  }

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}



#' @export
model_parameters.anova <- model_parameters.aov

#' @export
model_parameters.aovlist <- model_parameters.aov
