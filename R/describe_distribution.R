#' Describe a Distribution
#'
#'
#' This function describes a distribution.
#'
#' @param x A numeric vector.
#' @inheritParams bayestestR::point_estimate
#'
#' @examples
#' describe_distribution(rnorm(100))
#' describe_distribution(rpois(100, lambda = 4))
#' describe_distribution(runif(100))
#' @importFrom bayestestR map_estimate
#' @importFrom stats IQR density predict
#' @export
describe_distribution <- function(x, estimate = "mean", dispersion = TRUE, range = TRUE, ...) {
  UseMethod("describe_distribution")

}


#' @export
describe_distribution.numeric <- function(x, estimate = "mean", dispersion = TRUE, range = TRUE, ...) {

  # Missing
  n_missing <- sum(is.na(x))
  x <- na.omit(x)

  # Distribution
  type <- as.data.frame(t(find_distribution(x, probabilities = TRUE)))
  type$Type <- row.names(type)

  out <- data.frame(Type = type[which.max(type[, 1]), "Type"],
                     Type_Confidence = type[which.max(type[, 1]), 1] * 100)

  # Point estimates
  out <- cbind(out,
               bayestestR::point_estimate(x, estimate = estimate, dispersion = dispersion, ...))

  # Range
  if (range){
    out <- cbind(out,
                 data.frame(Min = min(x, na.rm = TRUE),
                            Max = max(x, na.rm = TRUE)))
  }

  # Skewness
  out <- cbind(out,
               data.frame(Skewness = skewness(x),
                          Kurtosis = kurtosis(x)))

  out$n_Obs <- length(x)
  out$n_Missing <- n_missing

  out
}
