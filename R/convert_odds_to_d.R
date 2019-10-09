#' @rdname d_to_r
#' @export
odds_to_d <- function(odds, log = FALSE) {
  if (log == FALSE) {
    log_odds <- log(odds)
  } else {
    log_odds <- odds
  }

  log_odds * (sqrt(3) / pi)
}

#' @rdname d_to_r
#' @export
convert_odds_to_d <- odds_to_d




#' @rdname d_to_r
#' @export
d_to_odds <- function(d, log = FALSE) {
  if (log == TRUE) {
    d * pi / sqrt(3)
  } else {
    exp(d * pi / sqrt(3))
  }
}

#' @rdname d_to_r
#' @export
convert_d_to_odds <- d_to_odds



#' @rdname d_to_r
#' @export
r_to_odds <- function(r, log = FALSE) {
  convert_d_to_odds(convert_r_to_d(r), log = log)
}

#' @rdname d_to_r
#' @export
convert_r_to_odds <- r_to_odds



#' @rdname d_to_r
#' @export
odds_to_r <- function(odds, log = FALSE) {
  convert_d_to_r(convert_odds_to_d(odds, log = log))
}

#' @rdname d_to_r
#' @export
convert_odds_to_r <- odds_to_r