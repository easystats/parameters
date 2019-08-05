#' (Partial) Omega Squared.
#'
#' Computation of (Partial) Eta Squared for ANOVAs.
#'
#' @param model an ANOVA object.
#' @param partial Return partial eta squared.
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' eta_squared(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' eta_squared(model)
#'
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' eta_squared(model)
#' }
#'
#' @return Eta squared values.
#'
#' @export
eta_squared <- function(model, partial = TRUE) {
  UseMethod("eta_squared")
}



#' @export
eta_squared.aov <- function(model, partial = TRUE) {
  .eta_squared(model, partial = partial)
}

#' @export
eta_squared.anova <- eta_squared.aov


#' @export
eta_squared.aovlist <- function(model, partial = TRUE) {
  stop("Eta squared not implemented yet for repeated-measures ANOVAs.")

  # params <- .extract_parameters_anova(model)
  # values <- .values_aov(params)
  #
  # if (!"Residuals" %in% params$Parameter) {
  #   stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  # }
  #
  # mapply(function(p, v) {
  #   .extract_eta_squared(p, v, partial)
  # }, split(params, params$Group), values, SIMPLIFY = FALSE)
}



#' @keywords internal
.eta_squared <- function(model, partial = TRUE) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  .extract_eta_squared(params, values, partial)
}


.extract_eta_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params[params$Parameter == "Residuals", "Eta_Sq"] <- NA
    params$Eta_Sq <- params$Sum_Squares / values$Sum_Squares_total
  } else {
    params$Eta_Sq_partial <- params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Eta_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Eta_Sq", "Eta_Sq_partial"), names(params)), drop = FALSE]
}