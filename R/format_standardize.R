#' Transform a standardized vector into character
#'
#' Transform a standardized vector into character, e.g., \code{c("-1 SD", "Mean", "+1 SD")}.
#'
#' @param x A standardized numeric vector.
#' @param reference The reference vector from which to compute the mean and SD.
#' @inheritParams standardize
#' @inheritParams format_value
#'
#' @examples
#' format_standardize(c(-1, 0, 1))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE)
#'
#'
#' @export
format_standardize <- function(x, reference = x, robust = FALSE, digits = NULL, ...){

  if(robust){
    central <- median(reference, na.rm = TRUE)
    central_name <- "Median"
    deviation <- mad(reference, na.rm = TRUE)
    deviation_name <- "MAD"
  } else{
    central <- mean(reference, na.rm = TRUE)
    central_name <- "Mean"
    deviation <- sd(reference, na.rm = TRUE)
    deviation_name <- "SD"
  }

  # Express in deviations
  x <- (x - central) / deviation

  # Round
  if(is.null(digits)){
    x <- parameters::format_value(x, round(1/diff(range(x, na.rm = TRUE))), protect_integers = TRUE)
  } else{
    x <- parameters::format_value(x, digits = digits, protect_integers = TRUE)
  }

  # Complete
  x[!grepl("-", x)] <- paste0("+", x[!grepl("-", x)])
  x[x != "+0"] <- paste(x[x != "+0"], deviation_name)
  x[x == "+0"] <- central_name

  factor(x, levels = unique(x))
}