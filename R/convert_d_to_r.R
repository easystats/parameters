#' Conversion between standardized difference d and correlation r
#'
#' Enables a conversion between standardized difference (Cohen's d) and correlation r.
#'
#' @param d A standardized difference value (Cohen's d).
#' @param r A correlation coefficient r.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' d_to_r(d = 1.1547)
#' @references \itemize{
#'   \item Borenstein, Michael, et al. "Converting among effect sizes." Introduction to meta-analysis (2009): 45-49.
#' }
#' @export
d_to_r <- function(d, ...) {
  # if(is.null(n1) & is.null(n2)){
  #   n1 <- n2 <- 1
  # }
  # if(is.null(n1) & !is.null(n2)){
  #   n1 <- n2
  # }
  # if(!is.null(n1) & is.null(n2)){
  #   n2 <- n1
  # }
  # a <- (n1+n2)^2/(n1*n2)

  a <- 4
  d / (sqrt(d^2 + a))
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
