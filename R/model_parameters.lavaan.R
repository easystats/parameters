#' Format CFA/SEM from the lavaan package
#'
#' Format CFA/SEM objects from the lavaan package (Rosseel, 2012).
#'
#' @param model CFA or SEM created by the \code{lavaan::cfa} or \code{lavaan::sem} functions.
#' @inheritParams model_parameters.lm
#' @param type What type of links to return. Can be some of \code{c("regression", "correlation", "loading", "variance", "mean")}.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(parameters)
#' \dontrun{
#' library(lavaan)
#'
#' # Confirmatory Factor Analysis (CFA) ---------
#'
#' structure <- ' visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 '
#' model <- lavaan::cfa(structure, data=HolzingerSwineford1939)
#' model_parameters(model)
#'
#' }
#'
#' @references \itemize{
#'   \item Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
#' }
#' @export
model_parameters.lavaan <- function(model, ci = 0.95, standardize = FALSE, type = c("regression", "correlation", "loading"), ...) {

  params <- .extract_parameters_lavaan(model, ci = ci, standardize = standardize, ...)

  # Filter
  params <- params[tolower(params$Type) %in% type, ]

  # add class-attribute for printing
  class(params) <- c("parameters_sem", class(params))

  params

}


#' @keywords internal
.extract_parameters_lavaan <- function(model, ci = 0.95, standardize = FALSE, ...){

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package `lavaan` required. Please install it by running `install.packages(lavaan)`.", call. = FALSE)
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
    p = data$pvalue,
    CI_low = data$ci.lower,
    CI_high = data$ci.upper
  )

  params$Type <- ifelse(params$Operator == "=~", "Loading",
                        ifelse(params$Operator == "~", "Regression",
                               ifelse(params$Operator == "~~", "Correlation",
                                      ifelse(params$Operator == "~1", "Mean", NA))))
  params$Type <- ifelse(as.character(params$From) == as.character(params$To), "Variance", params$Type)
  params$p <- ifelse(is.na(params$p), 0, params$p)

  if ("group" %in% names(data)) {
    params$Group <- data$group
  }
  params
}


#' @export
n_parameters.lavaan <- function(x, ...){
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package `lavaan` required. Please install it by running `install.packages(lavaan)`.", call. = FALSE)
  }
  lavaan::fitmeasures(x)$npar
}



