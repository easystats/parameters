#' Factor Analysis (FA)
#'
#' This function performs a Factor Analysis (FA).
#'
#' @inheritParams principal_components
#'
#' @details
#'  \subsection{Complexity}{
#'    Complexity represents the number of latent components needed to account
#'    for the observed variables. Whereas a perfect simple structure solution
#'    has a complexity of 1 in that each item would only load on one factor,
#'    a solution with evenly distributed items has a complexity greater than 1
#'    (\cite{Hofman, 1978; Pettersson and Turkheimer, 2010}) .
#'  }
#'  \subsection{FA or PCA?}{
#'  There is a simplified rule of thumb that may help do decide whether to run
#'  a principal component analysis or a factor analysis:
#'  \itemize{
#'    \item Run principal component analysis if you assume or wish to test a theoretical model of latent factors causing observed variables.
#'    \item Run factor analysis If you want to simply reduce your correlated observed variables to a smaller set of important independent composite variables.
#'  }
#'  (Source: \href{https://stats.stackexchange.com/q/1576/54740}{CrossValidated})
#'  }
#'
#' @note There is a \code{summary()}-method that prints the Eigenvalues and (explained) variance for each extracted component.
#'
#' @examples
#' library(parameters)
#'
#' factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2)
#' factor_analysis(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
#' factor_analysis(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#'
#' FA <- factor_analysis(mtcars[, 1:5], n = 2)
#' summary(FA)
#' predict(FA)
#' \donttest{
#' # Automated number of components
#' factor_analysis(mtcars[, 1:4], n = "auto")
#' }
#'
#' @return A data.frame of loadings.
#' @references \itemize{
#'   \item Hofmann, R. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. Multivariate Behavioral Research, 13:2, 247-250, \doi{10.1207/s15327906mbr1302_9}
#'   \item Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420, \doi{10.1016/j.jrp.2010.03.002}
#' }
#' @importFrom stats prcomp
#' @export
factor_analysis <- function(x, n = "auto", rotation = "none", threshold = NULL, standardize = TRUE, ...) {
  UseMethod("factor_analysis")
}



#' @importFrom stats prcomp na.omit
#' @export
factor_analysis.data.frame <- function(x, n = "auto", rotation = "none", threshold = NULL, standardize = TRUE, ...) {

  # Standardize
  if (standardize) {
    x <- as.data.frame(scale(x))
  }

  # N factors
  n <- .get_n_factors(x, n = n, type = "FA", rotation = rotation)

  .FA_rotate(x, n, rotation = rotation, sort = sort, threshold = threshold, ...)
}


#' @keywords internal
.FA_rotate <- function(x, n, rotation, sort = FALSE, threshold = NULL, ...) {
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

  model_parameters(psych::fa(x, nfactors = n, rotate = rotation, ...), sort = sort, threshold = threshold)
}
