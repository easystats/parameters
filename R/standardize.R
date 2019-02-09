#' Standardization
#'
#' Performs a standardization of data or parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=standardize.data.frame]{data.frame}}
#'  \item{\link[=standardize.lm]{lm}}
#'  }
#'
#'
#' @param x Object.
#' @param robust If TRUE, the standardization will be based on \link{median} and \link{mad} instead of \link{mean} and \link{sd} (default).
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
standardize <- function(x, robust = FALSE, ...) {
  UseMethod("standardize")
}
