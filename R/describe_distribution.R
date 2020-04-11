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
describe_distribution <- function(x, ...) {
  UseMethod("describe_distribution")
}


#' @rdname describe_distribution
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

  # Standard Error
  out <- cbind(out, data.frame(SE = standard_error(x)))

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
      Skewness = as.numeric(skewness(x)),
      Kurtosis = as.numeric(kurtosis(x))
    )
  )

  out$n <- length(x)
  out$n_Missing <- n_missing
  out$`.temp` <- NULL

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
  out
}





#' @rdname describe_distribution
#' @export
describe_distribution.factor <- function(x, dispersion = TRUE, range = TRUE, ...) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    SE = NA,
    Min = levels(x)[1],
    Max = levels(x)[nlevels(x)],
    Skewness = as.numeric(skewness(x)),
    Kurtosis = as.numeric(kurtosis(x)),
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

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
  out
}



#' @export
describe_distribution.character <- function(x, dispersion = TRUE, range = TRUE, ...) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)
  values <- unique(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    SE = NA,
    Min = values[1],
    Max = values[length(values)],
    Skewness = as.numeric(skewness(x)),
    Kurtosis = as.numeric(kurtosis(x)),
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

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
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
  out <- out[c("Variable", setdiff(colnames(out), "Variable"))]

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}








#' @export
print.parameters_distribution <- function(x, digits = 2, ...) {
  if (all(c("Min", "Max") %in% names(x))) {
    x$Min <- insight::format_ci(x$Min, x$Max, ci = NULL, digits = digits, width = "auto", brackets = TRUE)
    x$Max <- NULL
    colnames(x)[which(colnames(x) == "Min")] <- "Range"
  }
  cat(insight::format_table(x))
}
