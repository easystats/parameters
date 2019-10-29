#' Adjust data for the effect of other variable(s)
#'
#' This function can be used to adjust the data for the effect of other variables present in the dataset. It is based on an underlying fitting of regressions models, allowing for quite some flexibility, such as including factors as random effects in mixed models and/or fitting the models under a Bayesian framework. The values returned by this function are the residuals of the regression models. Note that a regular correlation between two "adjusted" variables is equivalent to the partial correlation between them.
#'
#' @param data A dataframe.
#' @param effect Character vector of column names to be adjusted for (regressed out). If \code{NULL} (the default), all variables will be selected.
#' @param select Character vector of column names. If \code{NULL} (the default), all variables will be selected.
#' @param exclude Character vector of column names to be excluded from selection.
#' @param multilevel If \code{TRUE}, the factors are included as random factors. Else, if \code{FALSE} (default), they are included as fixed effects in the simple regression model.
#' @param bayesian If \code{TRUE}, the models are fitted under the Bayesian framework using \code{rstanarm}.
#'
#' @examples
#' adjust(iris, effect = "Species", select = "Sepal.Length")
#' adjust(iris, effect = "Species", select = "Sepal.Length", multilevel = TRUE)
#' adjust(iris, effect = "Species", select = "Sepal.Length", bayesian = TRUE)
#'
#' adjust(iris)
#'
#' @export
adjust <- function(data, effect = NULL, select = NULL, exclude = NULL, multilevel = FALSE, bayesian = FALSE){

  # Find predictors
  if(is.null(effect)){
    effect <- names(data)
  }

  # Factors
  formula_random <- ""
  facs <- names(data[effect][!sapply(data[effect], is.numeric)])
  if (length(facs) >= 1){
    if(multilevel){
      formula_random <- paste("+", paste(paste0("(1|", facs, ")"), collapse = " + "))
      effect <- effect[!effect %in% facs]
    }
  }

  nums <- sapply(data, is.numeric)
  # Find outcomes
  if(is.null(select)){
    select <- names(data[nums])
  }
  if(is.null(exclude)){
    select <- select[!select %in% c(exclude)]
  }

  # Fit models
  out <- data.frame(.ID = 1:nrow(data))
  for(var in select){
    formula_predictors <- paste(c("1", effect[effect != var]), collapse = " + ")
    formula <- paste(var, "~", formula_predictors, formula_random)

    x <- .model_adjust_for(data, formula, multilevel = multilevel, bayesian = bayesian)
    out[var] <- x
  }
  out[names(data)[!names(data) %in% names(out)]] <- data[names(data)[!names(data) %in% names(out)]]
  out[names(data)]
}




#' @importFrom stats lm residuals
#' @keywords internal
.model_adjust_for <- function(data, formula, multilevel = FALSE, bayesian = FALSE) {
  if (multilevel == FALSE) {
    if (bayesian == FALSE) {
      out <- lm(formula, data = data)$residuals
    } else {
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      out <- rstanarm::stan_glm(formula, data = data, refresh = 0)$residuals
    }
  } else {
    if (bayesian == FALSE) {
      if (!requireNamespace("lme4")) {
        stop("This function needs `lme4` to be installed. Please install by running `install.packages('lme4')`.")
      }
      out <- residuals(lme4::lmer(formula, data = data))
    } else {
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      out <- rstanarm::stan_lmer(formula, data = data, refresh = 0)$residuals
    }
  }
  as.vector(out)
}
