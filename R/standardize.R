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
#' @param method The method of standardization. See 'Details'.
#' @param ... Arguments passed to or from other methods.
#'
#' @details Methods:
#' \itemize{
#'  \item \strong{mean}: \code{mean} and \code{sd} (default).
#'  \item \strong{median}: \code{median} and \code{mad}.
#'  \item \strong{2sd}: \code{mean} and two times the \code{sd}.
#' }
#'
#' @export
standardize <- function(x, method = "mean", ...) {
  UseMethod("standardize")
}
