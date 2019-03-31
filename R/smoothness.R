#' Quantify the smoothness of a vector
#'
#' @param x Numeric vector (similar to a time series).
#' @param method Can be "diff" (the standard deviation of the standardized differences) or "cor" (default, lag-one autocorrelation).
#' @param lag An integer indicating which lag to use. If less than 1, will be interpreted as expressed in percentage of the length of the vector.
#'
#' @examples
#' x <- (-10:10)^3 + rnorm(21, 0, 100)
#' plot(x)
#' smoothness(x, method = "cor")
#' smoothness(x, method = "diff")
#' @references https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
#' @importFrom stats cor
#' @importFrom utils head tail
#' @export
smoothness <- function(x, method = "cor", lag = 1) {
  if (lag < 1) {
    lag <- round(lag * length(x))
  }
  if (lag <= 0) {
    stop("lag cannot be that small.")
  }

  if (method == "cor") {
    smooth <- cor(head(x, length(x) - lag), tail(x, length(x) - lag))
  } else {
    smooth <- sd(diff(x, lag = lag)) / abs(mean(diff(x, lag = lag)))
  }
  return(smooth)
}
