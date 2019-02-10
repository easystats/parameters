#' Model bootstrapping
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_bootstrap.lm]{lm}}
#'  }
#'
#'
#' @param model Statistical model.
#' @param n The number of bootstrap replicates.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
model_bootstrap <- function(model, n = 1000, ...) {
  UseMethod("model_bootstrap")
}
