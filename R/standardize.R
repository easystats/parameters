#' Standardization
#'
#' Performs a standardization of data or parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=standardize.data.frame]{Dataframes}}
#'  \item{\link[=standardize.lm]{Regression models}}
#'  }
#'
#'
#' @param x Object.
#' @param robust If TRUE, the standardization will be based on \link{median} and \link{mad} instead of \link{mean} and \link{sd} (default).
#' @param ... Arguments passed to or from other methods.
#'
#' @export
standardize <- function(x, robust = FALSE, ...) {
  UseMethod("standardize")
}

