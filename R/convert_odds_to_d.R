#' Conversion between (log)odds and standardized difference
#'
#' Enables a conversion between (log)odds and standardized difference using Cohen's (1988) formula \eqn{\frac{\log(odds)\times\sqrt{3}}{\pi}}.
#'
#' @param odds Odds values in vector or dataframe.
#' @param log Are these Log odds (such as in logistic models)?
#' @param d standardized difference values in vector or dataframe.
#'
#' @examples
#' odds_to_d(0.2)
#' odds_to_d(-1.45, log = TRUE)
#' @references \itemize{
#'   \item Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological methods, 8(4), 448.
#'   \item Borenstein, Michael, et al. "Converting among effect sizes." Introduction to meta-analysis (2009): 45-49.
#' }
#' @export
odds_to_d <- function(odds, log = FALSE) {
  if (log == FALSE) {
    log_odds <- log(odds)
  } else {
    log_odds <- odds
  }

  log_odds * (sqrt(3) / pi)
}

#' @rdname odds_to_d
#' @export
convert_odds_to_d <- odds_to_d




#' @rdname odds_to_d
#' @export
d_to_odds <- function(d, log = FALSE) {
  if (log == TRUE) {
    d * pi / sqrt(3)
  } else {
    exp(d * pi / sqrt(3))
  }
}

#' @rdname odds_to_d
#' @export
convert_d_to_odds <- d_to_odds
