#' Convert standardized difference d to correlation r.
#'
#' Convert standardized difference (Cohen's d) to correlation r.
#'
#' @param d A standardized difference value (Cohen's d).
#'
#' @examples
#' d_to_r(d=1.1547)
#'
#' @references \itemize{
#'   \item Borenstein, Michael, et al. "Converting among effect sizes." Introduction to meta-analysis (2009): 45-49.
#' }
#' @export
d_to_r <- function(d) {
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
  r <- d / (sqrt(d^2 + a))

  r
}

#' @rdname d_to_r
#' @export
convert_d_to_r <- d_to_r