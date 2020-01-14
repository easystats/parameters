#' Parameters from CFA/SEM models
#'
#' Format CFA/SEM objects from the (b)lavaan package (Rosseel, 2012; Merkle and Rosseel 2018).
#'
#' @param model CFA or SEM created by the \code{lavaan::cfa} or \code{lavaan::sem} functions.
#' @param standardize Return standardized parameters (standardized coefficients). See \code{lavaan::standardizedsolution}.
#' @inheritParams model_parameters.default
#' @param type What type of links to return. Can be \code{"all"} or some of \code{c("regression", "correlation", "loading", "variance", "mean")}.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(parameters)
#'
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
#' \donttest{
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
#'   \item Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
#'   \item Merkle EC , Rosseel Y (2018). blavaan: Bayesian Structural Equation Models via Parameter Expansion. Journal of Statistical Software, 85(4), 1-30. http://www.jstatsoft.org/v85/i04/
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
  attr(params, "model") <- model
  params
}



#' @export
model_parameters.blavaan <- function(model, ci = 0.95, standardize = FALSE, type = c("regression", "correlation", "loading"), ...) {
  params <- .extract_parameters_blavaan(model, ci = ci, standardize = standardize, ...)

  # Filter
  if (all(type == "all")) {
    type <- c("regression", "correlation", "loading", "variance", "mean")
  }
  params <- params[tolower(params$Type) %in% type, ]

  # add class-attribute for printing
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))
  attr(params, "ci") <- ci
  attr(params, "model") <- model
  params
}



#' @export
n_parameters.lavaan <- function(x, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }
  lavaan::fitmeasures(x)$npar
}


#' @importFrom insight format_table
#' @export
print.parameters_sem <- function(x, ...) {
  .print_model_parms_components(x, pretty_names = TRUE, split_column = "Type")
}


#' @export
predict.parameters_sem <- function(object, newdata = NULL, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required for this function to work. Please install it by running `install.packages('lavaan')`.")
  }

  as.data.frame(lavaan::lavPredict(attributes(object)$model, newdata = newdata, method = "EBM", ...))
}
