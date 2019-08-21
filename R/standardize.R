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
#' @param robust Logical, if \code{TRUE}, centering is done by substracting the
#'   median from the variables and divide it by the median absolute deviation
#'   (MAD). If \code{FALSE}, variables are standardized by substracting the
#'   mean and divide it by the standard deviation (SD).
#'   the
#' @param method The method of standardization. For data.frames, can be \code{"default"} (variables are divided by SD or MAD depending on \code{robust}) or \code{"2sd"} (divided by two times the deviation).
#' @param verbose Toggle warnings on or off.
#' @param force Logical, if \code{TRUE}, forces standardization of factors as
#'   well. Factors are converted to numerical values, with the lowest level
#'   being the value \code{1} (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param ... Arguments passed to or from other methods.
#'
#' @return The standardized object.
#'
#' @export
standardize <- function(x, ...) {
  UseMethod("standardize")
}
