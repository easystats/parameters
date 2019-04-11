#' Convert (log)odds to standardized difference.
#'
#' Convert (log)odds to standardized difference using Cohen's (1988) formula \eqn{\frac{\log(odds)\times\sqrt{3}}{\pi}}.
#'
#' @param odds Odds values in vector or dataframe.
#' @param log Are these Log odds (such as in logistic models)?
#'
#' @examples
#' odds_to_d(0.2)
#' odds_to_d(-1.45, log=TRUE)
#' @export
odds_to_d <- function(odds, log = FALSE) {
  if(log == FALSE){
    log_odds <- log(odds)
  } else{
    log_odds <- odds
  }

  log_odds * (sqrt(3) / pi)
}