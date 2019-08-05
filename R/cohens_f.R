#' Cohen's f
#'
#' Computation of Cohen's f for ANOVAs.
#'
#' @inheritParams eta_squared
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' cohens_f(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' cohens_f(model)
#'
#' @return Cohen's f values.
#'
#' @export
cohens_f <- function(model) {
  UseMethod("cohens_f")
}



#' @export
cohens_f.aov <- function(model) {
  m <- .cohens_f(model)
  class(m) <- c("cohens_f", class(m))
  m
}

#' @export
cohens_f.anova <- cohens_f.aov


#' @export
cohens_f.aovlist <- function(model) {
  stop("Cohen's f not implemented yet for repeated-measures ANOVAs.")
}



#' @keywords internal
.cohens_f <- function(model) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  .extract_cohens_f(params, values)
}



.extract_cohens_f <- function(params, values) {
  params$Cohens_f <- params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
  params$Cohens_f <- sqrt(params$Cohens_f / (1 - params$Cohens_f))
  params[params$Parameter == "Residuals", "Cohens_f"] <- NA

  params[, intersect(c("Group", "Parameter", "Cohens_f"), names(params)), drop = FALSE]
}
