#' Factor Analysis (FA)
#'
#' This function performs a Factor Analysis (FA).
#'
#' @inheritParams principal_components
#' @inheritParams n_factors
#'
#' @inherit principal_components details
#'
#' @note There is a \code{summary()}-method that prints the Eigenvalues and (explained) variance for each extracted component.
#'
#' @examples
#' library(parameters)
#' if (require("psych")) {
#'   factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2)
#'   factor_analysis(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
#'   factor_analysis(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#'
#'   efa <- factor_analysis(mtcars[, 1:5], n = 2)
#'   summary(efa)
#'   predict(efa)
#' \donttest{
#'   # Automated number of components
#'   factor_analysis(mtcars[, 1:4], n = "auto")
#' }
#' }
#' @return A data frame of loadings.
#' @references \itemize{
#'   \item Hofmann, R. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. Multivariate Behavioral Research, 13:2, 247-250, \doi{10.1207/s15327906mbr1302_9}
#'   \item Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420, \doi{10.1016/j.jrp.2010.03.002}
#' }
#' @export
factor_analysis <- function(x,
                            n = "auto",
                            rotation = "none",
                            sort = FALSE,
                            threshold = NULL,
                            standardize = TRUE,
                            cor = NULL,
                            ...) {
  UseMethod("factor_analysis")
}



#' @export
factor_analysis.data.frame <- function(x,
                                       n = "auto",
                                       rotation = "none",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = TRUE,
                                       cor = NULL,
                                       ...) {

  # Standardize
  if (standardize && is.null(cor)) {
    x <- as.data.frame(scale(x))
  }

  # N factors
  n <- .get_n_factors(x, n = n, type = "FA", rotation = rotation, cor = cor)

  .FA_rotate(x, n, rotation = rotation, sort = sort, threshold = threshold, cor = cor, ...)
}






#' @keywords internal
.FA_rotate <- function(x, n, rotation, sort = FALSE, threshold = NULL, cor = NULL, ...) {
  if (!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none"))) {
    stop("`rotation` must be one of \"varimax\", \"quartimax\", \"promax\", \"oblimin\", \"simplimax\", \"cluster\" or \"none\".")
  }

  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  # rotate loadings
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = FALSE)
  }

  # Pass cor if available
  if (!is.null(cor)) {
    out <- model_parameters(
      psych::fa(
        cor,
        nfactors = n,
        rotate = rotation,
        n.obs = nrow(x),
        ...
      ),
      sort = sort,
      threshold = threshold
    )
  } else {
    out <- model_parameters(
      psych::fa(x, nfactors = n, rotate = rotation, ...),
      sort = sort,
      threshold = threshold
    )
  }

  attr(out, "data_set") <- x
  out
}
