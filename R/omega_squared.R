#' (Partial) Omega Squared.
#'
#' Computation of (Partial) Omega Squared for ANOVAs.
#'
#' @param model an ANOVA object.
#' @param partial Return partial omega squared.
#'
#' @examples
#' \dontrun{
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#' 
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' omega_squared(model)
#' 
#' model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
#' omega_squared(model)
#' 
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' omega_squared(model)
#' 
#' model <- anova(lme4::lmer(Sepal.Length ~ Sepal.Big + (1 | Species), data = df))
#' omega_squared(model)
#' }
#' 
#' @export
omega_squared <- function(model, partial = TRUE) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (partial == FALSE) {
    out <- (params$Sum_Squares - params$DoF * values$Mean_Square_residuals) / (values$Sum_Squares_residuals + values$Mean_Square_residuals)
  } else {
    out <- (params$DoF * (params$Mean_Square - values$Mean_Square_residuals)) / (params$DoF * params$Mean_Square + (values$n - params$DoF) * values$Mean_Square_residuals)
  }

  out[params$Parameter == "Residuals"] <- NA

  return(out)
}














#' @keywords internal
.values_aov <- function(params) {

  # number of observations
  N <- sum(params$DoF) + 1


  if ("Group" %in% names(params)) {
    if ("Within" %in% params$Parameter) {
      params[params$Parameter == "Within"]
    }
  }

  # get mean squared of residuals
  Mean_Square_residuals <- params[params$Parameter == "Residuals", ]$Mean_Square
  # get sum of squares of residuals
  Sum_Squares_residuals <- params[params$Parameter == "Residuals", ]$Sum_Squares
  # get total sum of squares
  Sum_Squares_total <- sum(params$Sum_Squares)
  # number of terms in model
  N_terms <- nrow(params) - 1



  return(list(
    "Mean_Square_residuals" = Mean_Square_residuals,
    "Sum_Squares_residuals" = Sum_Squares_residuals,
    "Sum_Squares_total" = Sum_Squares_total,
    "n_terms" = N_terms,
    "n" = N
  ))
}
