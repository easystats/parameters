#' Model bootstrapping
#'
#' @inheritParams model_bootstrap
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' @importFrom stats coef
#' @importFrom insight get_data find_parameters
#' @importFrom boot boot
#' @export
model_bootstrap.lm <- function(model, n = 1000, ...) {
  data <- insight::get_data(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample
    fit <- update(model, data = d)
    # TODO: replace with insight::get_parameters
    return(coef(fit))
  }

  results <- boot::boot(data = data, statistic = boot_function, R = n, model = model)

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}





#' Coerce to a Data Frame
#'
#'
#' @param x Any R object.
#' @param row.names Not used.
#' @param optional Not used.
#' @param n The number of bootstrap replicates.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method as.data.frame lm
#' @export
as.data.frame.lm <- function(x, row.names = NULL, optional = FALSE, n = 1000, ...) {
  return(model_bootstrap(x, n = n))
}
