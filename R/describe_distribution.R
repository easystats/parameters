#' Describe a distribution
#'
#' This function describes a distribution by a set of indices (e.g., measures of centrality, dispersion, range, skewness, kurtosis).
#'
#' @param x A numeric vector.
#' @param range Return the range (min and max).
#' @param include_factors Logical, if \code{TRUE}, factors are included in the output, however, only columns for range (first and last factor levels) as well as n and missing will contain information.
#' @inheritParams bayestestR::point_estimate
#'
#' @return A data frame with columns that describe the properties of the variables.
#' @examples
#' describe_distribution(rnorm(100))
#'
#' data(iris)
#' describe_distribution(iris)
#' describe_distribution(iris, include_factors = TRUE)
#' @export
describe_distribution <- function(x, centrality = "mean", dispersion = TRUE, range = TRUE, ...) {
  UseMethod("describe_distribution")
}


#' @importFrom stats na.omit
#' @export
describe_distribution.numeric <- function(x, centrality = "mean", dispersion = TRUE, range = TRUE, ...) {
  out <- data.frame(.temp = 0)

  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)


  # Point estimates
  out <- cbind(
    out,
    bayestestR::point_estimate(x, centrality = centrality, dispersion = dispersion, ...)
  )

  # Range
  if (range) {
    out <- cbind(
      out,
      data.frame(
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    )
  }

  # Skewness
  out <- cbind(
    out,
    data.frame(
      Skewness = skewness(x),
      Kurtosis = kurtosis(x)
    )
  )

  out$n <- length(x)
  out$n_Missing <- n_missing
  out$`.temp` <- NULL

  class(out) <- unique(c("parameters_distribution", class(out)))
  out
}





#' @export
describe_distribution.factor <- function(x, centrality = "mean", dispersion = TRUE, range = TRUE, ...) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    Min = levels(x)[1],
    Max = levels(x)[nlevels(x)],
    Skewness = NA,
    Kurtosis = NA,
    n = length(x),
    n_Missing = n_missing,
    stringsAsFactors = FALSE
  )

  if (!dispersion) {
    out$SD <- NULL
  }

  if (!range) {
    out$Min <- NULL
    out$Max <- NULL
  }

  out
}



#' @rdname describe_distribution
#' @export
describe_distribution.data.frame <- function(x, centrality = "mean", dispersion = TRUE, range = TRUE, include_factors = FALSE, ...) {
  out <- do.call(rbind, lapply(x, function(i) {
    if ((include_factors && is.factor(i)) || (!is.character(i) && !is.factor(i))) {
      describe_distribution(i, centrality = centrality, dispersion = dispersion, range = range)
    }
  }))

  out$Variable <- row.names(out)

  row.names(out) <- NULL
  class(out) <- unique(c("parameters_distribution", class(out)))

  out[c("Variable", setdiff(colnames(out), "Variable"))]
}








#' @export
print.parameters_distribution <- function(x, ...) {
  cat(insight::format_table(x))
}
