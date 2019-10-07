#' Effect Size Conversion
#'
#' Enables a conversion between different indices of effect size, such as standardized difference (Cohen's d), correlation r or (log) odds ratios.
#'
#' @param d Standardized difference value (Cohen's d).
#' @param r Correlation coefficient r.
#' @param odds Odds values in vector or dataframe.
#' @param log Take in or output log odds (such as in logistic models).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' r_to_d(0.5)
#' d_to_odds(d = 1.154701)
#' odds_to_r(odds = 8.120534)
#'
#' d_to_r(d = 1)
#' r_to_odds(0.4472136, log = TRUE)
#' odds_to_d(1.813799, log = TRUE)
#'
#' @return Converted index.
#'
#' @details \itemize{
#'   \item \emph{d to r}: \code{d = 2 * r / sqrt(1 - r^2)}
#'   \item \emph{r to d}: \code{r = d / (sqrt(d^2 + 4))}
#'   \item \emph{odds to d}: \eqn{d = \frac{\log(odds)\times\sqrt{3}}{\pi}}
#'   \item \emph{d to odds}: \eqn{log(odds) = d * \frac{\pi}{\sqrt(3)}}
#' }
#'
#'
#' @references \itemize{
#'   \item Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological methods, 8(4), 448.
#'   \item Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Converting among effect sizes. Introduction to meta-analysis, 45-49.
#' }
#' @export
d_to_r <- function(d, ...) {
  d / (sqrt(d^2 + 4))
}


#' @rdname d_to_r
#' @export
r_to_d <- function(r, ...) {
  2 * r / sqrt(1 - r^2)
}



#' @rdname d_to_r
#' @export
convert_d_to_r <- d_to_r

#' @rdname d_to_r
#' @export
convert_r_to_d <- r_to_d
