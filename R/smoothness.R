#' Quantify the smoothness of a vector
#'
#' @param x Numeric vector (similar to a time series).
#' @param method Can be "diff" (the standard deviation of the standardized differences) or "cor" (default, lag-one autocorrelation).
#'
#' @examples
#' smoothness(exp(-10:10), method="cor")
#' smoothness(exp(-10:10), method="diff")
#'
#' @references https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
#' @importFrom stats cor
#' @export
smoothness <- function(x, method="cor"){
  if(method == "cor"){
    return(cor(x[-length(x)],x[-1]))
  } else{
    return(sd(diff(x))/abs(mean(diff(x))))
  }
}
