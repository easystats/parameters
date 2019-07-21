#' (Partial) Omega Squared.
#'
#' Computation of (Partial) Omega Squared for ANOVAs.
#'
#' @param model an ANOVA object.
#' @param partial Return partial omega squared.
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' omega_squared(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' omega_squared(model)
#'
#' # model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' # omega_squared(model)
#' @export
omega_squared <- function(model, partial = TRUE) {
  UseMethod("omega_squared")
}



#' @export
omega_squared.aov <- function(model, partial = TRUE) {
  .omega_squared(model, partial = partial)
}

#' @export
omega_squared.anova <- omega_squared.aov





#' @keywords internal
.omega_squared <- function(model, partial = TRUE) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  }


  if (partial == FALSE) {
    params$Omega_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_residuals + values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
  } else {
    params$Omega_Sq_partial <- (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq_partial"] <- NA
  }

  params[names(params)[names(params) %in% c("Group", "Parameter", "Omega_Sq", "Omega_Sq_partial")]]
}
