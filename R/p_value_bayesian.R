#' p-values for Bayesian Models
#'
#' This function attempts to return, or compute, p-values of Bayesian models.
#'
#' @param model A statistical model.
#'
#' @details For Bayesian models, the p-values corresponds to the \emph{probability of direction} (\code{\link[bayestestR]{p_direction}}), which is converted to a p-value using \code{bayestestR::convert_pd_to_p()}.
#'
#' @return The p-values.
#'
#'  @examples
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_value(model)
#' @importFrom bayestestR p_direction convert_pd_to_p
#' @export
p_value.brmsfit <- function(model, ...) {
  p <- bayestestR::p_direction(model)

  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}


#' @importFrom insight find_parameters
#' @export
p_value.MCMCglmm <- function(model, ...) {
  nF <- model$Fixed$nfl
  p <- 1 - colSums(model$Sol[, 1:nF, drop = FALSE] > 0) / dim(model$Sol)[1]

  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", flatten = TRUE),
    p = p
  )
}


#' @export
p_value.stanreg <- p_value.brmsfit


#' @export
p_value.BFBayesFactor <- p_value.brmsfit


#' @export
p_value.bcplm <- p_value.brmsfit
