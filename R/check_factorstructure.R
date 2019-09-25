#' Check suitability of data for Factor Analysis (FA)
#'
#' This checks whether the data is appropriate for Factor Analysis (FA) by running the \link[=check_sphericity]{Bartlett's Test of Sphericity} and the \link[=check_kmo]{Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy (MSA)}.
#'
#' @inheritParams check_sphericity
#' @examples
#' library(parameters)
#'
#' check_factorstructure(mtcars)
#' @return A list of lists of indices related to sphericity and KMO.
#' @seealso check_kmo check_sphericity
#' @export
check_factorstructure <- function(x, silent = FALSE, ...) {

  # TODO: Avoid double computation of indices
  # TODO: detect (and remove?) factors
  # TODO: This could be improved using the correlation package to use different correlation methods

  if (!silent) {
    text_kmo <- capture.output(check_kmo(x, silent = FALSE, ...))
    text_sphericity <- capture.output(check_sphericity(x, silent = FALSE, ...))

    text <- paste0("  - KMO, ", text_kmo, "\n  - Sphericity, ", text_sphericity)
    if (grepl("Warning:", text)) {
      insight::print_color(text, "red")
    } else {
      insight::print_color(text, "green")
    }
  }

  invisible(list(
    KMO = check_kmo(x, silent = TRUE, ...),
    sphericity = check_sphericity(x, silent = TRUE, ...)
  ))
}






#' Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy (MSA) for Factor Analysis
#'
#' Kaiser (1970) introduced a Measure of Sampling Adequacy (MSA), later modified by Kaiser and Rice (1974). The Kaiser-Meyer-Olkin (KMO) statistic, which can vary from 0 to 1, indicates the degree to which each variable in a set is predicted without error by the other variables.
#'
#' A value of 0 indicates that the sum of partial correlations is large relative to the sum correlations, indicating factor analysis is likely to be inappropriate. A KMO value close to 1 indicates that the sum of partial correlations is not large relative to the sum of correlations and so factor analysis should yield distinct and reliable factors.
#'
#' Kaiser (1975) suggested that KMO > .9 were marvelous, in the .80s, meritourious, in the .70s, middling, in the .60s, mediocre, in the 50s, miserable, and less than .5, unacceptable. Hair et al. (2006) suggest accepting a value > 0.5. Values between 0.5 and 0.7 are mediocre, and values between 0.7 and 0.8 are good.
#'
#'
#' @inheritParams check_sphericity
#'
#' @examples
#' library(parameters)
#'
#' check_kmo(mtcars)
#' @return A list of indices related to KMO.
#'
#' @details This function is strongly inspired by the \code{KMO} function in the \code{psych} package (Revelle, 2016). All credits go to its author.
#'
#' @references \itemize{
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis and data reduction.
#'   \item Kaiser, H. F. (1970). A second generation little jiffy. Psychometrika, 35(4), 401-415.
#'   \item Kaiser, H. F., \& Rice, J. (1974). Little jiffy, mark IV. Educational and psychological measurement, 34(1), 111-117.
#'   \item Kaiser, H. F. (1974). An index of factorial simplicity. Psychometrika, 39(1), 31-36.
#' }
#' @importFrom stats cor cov2cor
#' @export
check_kmo <- function(x, silent = FALSE, ...) {
  cormatrix <- stats::cor(x, use = "pairwise.complete.obs", ...)
  Q <- solve(cormatrix)

  Q <- stats::cov2cor(Q)
  diag(Q) <- 0
  diag(cormatrix) <- 0

  sumQ2 <- sum(Q^2)
  sumr2 <- sum(cormatrix^2)
  MSA <- sumr2 / (sumr2 + sumQ2)
  MSA_variable <- colSums(cormatrix^2) / (colSums(cormatrix^2) + colSums(Q^2))
  results <- list(MSA = MSA, MSA_variable = MSA_variable)

  if (!silent) {
    if (MSA < 0.5) {
      insight::print_color(sprintf("Warning: The Kaiser, Meyer, Olkin (KMO) measure of sampling adequacy suggests that factor analysis is likely to be inappropriate (KMO = %.2f).", MSA), "red")
    } else {
      insight::print_color(sprintf("OK: The Kaiser, Meyer, Olkin (KMO) measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = %.2f).", MSA), "green")
    }
  }

  invisible(results)
}












#' Bartlett's Test of Sphericity
#'
#' Bartlett (1951) introduced the test of sphericity, which tests whether a matrix is significantly different from an identity matrix. This statistical test for the presence of correlations among variables, providing the statistical probability that the correlation matrix has significant correlations among at least some of variables. As for factor analysis to work, some relationships between variables are needed, thus, a significant Bartlett’s test of sphericity is required, say p < .001.
#'
#'
#' @param x A dataframe.
#' @param silent Hide results printing.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' check_sphericity(mtcars)
#' @details This function is strongly inspired by the \code{cortest.bartlett} function in the \code{psych} package (Revelle, 2016). All credits go to its author.
#'
#' @return A list of indices related to sphericity.
#'
#' @references \itemize{
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis and data reduction.
#'   \item Bartlett, M. S. (1951). The effect of standardization on a Chi-square approximation in factor analysis. Biometrika, 38(3/4), 337-344.
#' }
#'
#' @importFrom stats pchisq cor
#'
#' @export
check_sphericity <- function(x, silent = FALSE, ...) {

  # This could be improved using the correlation package to use different correlation methods
  cormatrix <- stats::cor(x, use = "pairwise.complete.obs", ...)

  n <- nrow(x)
  p <- dim(cormatrix)[2]

  detR <- det(cormatrix)
  statistic <- -log(detR) * (n - 1 - (2 * p + 5) / 6)
  df <- p * (p - 1) / 2
  pval <- stats::pchisq(statistic, df, lower.tail = FALSE)

  results <- list(chisq = statistic, p = pval, dof = df)

  if (!silent) {
    if (pval < 0.001) {
      insight::print_color(sprintf("OK: Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analaysis (Chisq(%i) = %.2f, %s).", df, statistic, format_p(pval)), "green")
    } else {
      insight::print_color(sprintf("Warning: Bartlett's test of sphericity suggests that there is not enough significant correlation in the data for factor analaysis (Chisq(%i) = %.2f, %s).", df, statistic, format_p(pval)), "red")
    }
  }

  invisible(results)
}
