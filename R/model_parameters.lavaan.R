#' Format CFA/SEM from the lavaan package
#'
#' Format CFA/SEM objects from the lavaan package (Rosseel, 2012).
#'
#' @param model CFA or SEM created by the \code{lavaan::cfa} or \code{lavaan::sem} functions.
#' @inheritParams model_parameters.lm
#' @param type What type of links to return. Can be \code{"all"} or some of \code{c("regression", "correlation", "loading", "variance", "mean")}.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(parameters)
#' \dontrun{
#' # lavaan -------------------------------------
#' library(lavaan)
#'
#' # Confirmatory Factor Analysis (CFA) ---------
#'
#' structure <- " visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 "
#' model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#' model_parameters(model)
#' model_parameters(model, standardize = TRUE)
#'
#' # Structural Equation Model (SEM) ------------
#'
#' structure <- "
#'   # latent variable definitions
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'     dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' "
#' model <- lavaan::sem(structure, data = PoliticalDemocracy)
#' model_parameters(model)
#' model_parameters(model, standardize = TRUE)
#'
#'
#' # blavaan ------------------------------------
#' # library(blavaan)
#'
#' # model <- blavaan::bsem(structure, data=PoliticalDemocracy)
#' # model_parameters(model)
#' # model_parameters(model, standardize = TRUE)
#' }
#'
#' @return A data.frame of indices related to the model's parameters.
#'
#' @references \itemize{
#'   \item Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
#' }
#' @export
model_parameters.lavaan <- function(model, ci = 0.95, standardize = FALSE, type = c("regression", "correlation", "loading"), ...) {
  params <- .extract_parameters_lavaan(model, ci = ci, standardize = standardize, ...)

  # Filter
  if (all(type == "all")) {
    type <- c("regression", "correlation", "loading", "variance", "mean")
  }
  params <- params[tolower(params$Type) %in% type, ]

  # add class-attribute for printing
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))
  attr(params, "ci") <- ci
  params
}


#' @keywords internal
.extract_parameters_lavaan <- function(model, ci = 0.95, standardize = FALSE, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }

  if (standardize == FALSE) {
    data <- lavaan::parameterEstimates(model, se = TRUE, level = ci, ...)
  } else {
    data <- lavaan::standardizedsolution(model, se = TRUE, level = ci, ...)
    names(data)[names(data) == "est.std"] <- "est"
  }



  params <- data.frame(
    To = data$lhs,
    Operator = data$op,
    From = data$rhs,
    Coefficient = data$est,
    SE = data$se,
    CI_low = data$ci.lower,
    CI_high = data$ci.upper,
    p = data$pvalue
  )

  params$Type <- ifelse(params$Operator == "=~", "Loading",
    ifelse(params$Operator == "~", "Regression",
      ifelse(params$Operator == "~~", "Correlation",
        ifelse(params$Operator == "~1", "Mean", NA)
      )
    )
  )
  params$Type <- ifelse(as.character(params$From) == as.character(params$To), "Variance", params$Type)
  params$p <- ifelse(is.na(params$p), 0, params$p)

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }

  params
}


#' @export
n_parameters.lavaan <- function(x, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }
  lavaan::fitmeasures(x)$npar
}


#' @export
print.parameters_sem <- function(x, ...){
  formatted_table <- parameters_table(x)
  cat(format_table(formatted_table))
}