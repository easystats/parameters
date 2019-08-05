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
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' omega_squared(model)
#' }
#'
#' @return Omega squared values.
#'
#' @export
omega_squared <- function(model, partial = TRUE) {
  UseMethod("omega_squared")
}



#' @export
omega_squared.aov <- function(model, partial = TRUE) {
  m <- .omega_squared(model, partial = partial)
  class(m) <- c(ifelse(isTRUE(partial), "partial_omega_squared", "omega_squared"), class(m))
  m
}

#' @export
omega_squared.anova <- omega_squared.aov


#' @export
omega_squared.aovlist <- function(model, partial = TRUE) {
  stop("Omega squared not implemented yet for repeated-measures ANOVAs.")

  # params <- .extract_parameters_anova(model)
  # values <- .values_aov(params)
  #
  # if (!"Residuals" %in% params$Parameter) {
  #   stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  # }
  #
  # mapply(function(p, v) {
  #   .extract_omega_squared(p, v, partial)
  # }, split(params, params$Group), values, SIMPLIFY = FALSE)
}



#' @keywords internal
.omega_squared <- function(model, partial = TRUE) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  }

  eff_size <- .extract_omega_squared(params, values, partial)

  # required for CI
  attr(eff_size, "F_statistic") <- params[["F"]]
  eff_size
}


.extract_omega_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params$Omega_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_total + values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
  } else {
    params$Omega_Sq_partial <- (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Omega_Sq", "Omega_Sq_partial"), names(params)), drop = FALSE]
}