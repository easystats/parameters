#' Check suitability of data for Factor Analysis (FA)
#'
#' This checks whether the data is appropriate for Factor Analysis (FA) by
#' running the \link[=check_sphericity]{Bartlett's Test of Sphericity} and the
#' \link[=check_kmo]{Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy
#' (MSA)}.
#'
#' @inheritParams check_sphericity.data.frame
#' @examples
#' library(parameters)
#' check_factorstructure(mtcars)
#' @return A list of lists of indices related to sphericity and KMO.
#' @seealso \code{\link{check_kmo}}, \code{\link{check_sphericity}} and \code{\link{check_clusterstructure}}.
#' @export
check_factorstructure <- function(x, ...) {

  # TODO: detect (and remove?) factors
  # TODO: This could be improved using the correlation package to use different correlation methods

  kmo <- check_kmo(x, ...)
  sphericity <- check_sphericity(x, ...)

  text <- paste0("  - KMO: ", attributes(kmo)$text, "\n  - Sphericity: ", attributes(sphericity)$text)

  if (attributes(kmo)$color == "red" | attributes(sphericity)$color == "red") {
    color <- "red"
  } else {
    color <- "green"
  }

  out <- list(KMO = kmo, sphericity = sphericity)

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Is the data suitable for Factor Analysis?"
  class(out) <- c("easystats_check", class(out))

  out
}



#' Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy (MSA) for Factor Analysis
#'
#' Kaiser (1970) introduced a Measure of Sampling Adequacy (MSA), later modified
#' by Kaiser and Rice (1974). The Kaiser-Meyer-Olkin (KMO) statistic, which can
#' vary from 0 to 1, indicates the degree to which each variable in a set is
#' predicted without error by the other variables.
#'
#' A value of 0 indicates that the sum of partial correlations is large relative
#' to the sum correlations, indicating factor analysis is likely to be
#' inappropriate. A KMO value close to 1 indicates that the sum of partial
#' correlations is not large relative to the sum of correlations and so factor
#' analysis should yield distinct and reliable factors.
#'
#' Kaiser (1975) suggested that KMO > .9 were marvelous, in the .80s,
#' meritorious, in the .70s, middling, in the .60s, mediocre, in the .50s,
#' miserable, and less than .5, unacceptable. Hair et al. (2006) suggest
#' accepting a value > 0.5. Values between 0.5 and 0.7 are mediocre, and values
#' between 0.7 and 0.8 are good.
#'
#' @inheritParams check_sphericity.data.frame
#'
#' @examples
#' library(parameters)
#' check_kmo(mtcars)
#' @return A list of indices related to KMO.
#'
#' @details This function is strongly inspired by the \code{KMO} function in the
#'   \code{psych} package (Revelle, 2016). All credit goes to its author.
#'
#' @references \itemize{
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis
#'   and data reduction.
#'
#'   \item Kaiser, H. F. (1970). A second generation little jiffy.
#'   Psychometrika, 35(4), 401-415.
#'
#'   \item Kaiser, H. F., & Rice, J. (1974). Little jiffy, mark IV. Educational
#'   and psychological measurement, 34(1), 111-117.
#'
#'   \item Kaiser, H. F. (1974). An index of factorial simplicity.
#'   Psychometrika, 39(1), 31-36.
#' }
#' @export
check_kmo <- function(x, ...) {
  cormatrix <- stats::cor(x, use = "pairwise.complete.obs", ...)
  Q <- solve(cormatrix)

  Q <- stats::cov2cor(Q)
  diag(Q) <- 0
  diag(cormatrix) <- 0

  sumQ2 <- sum(Q^2)
  sumr2 <- sum(cormatrix^2)
  MSA <- sumr2 / (sumr2 + sumQ2)
  MSA_variable <- colSums(cormatrix^2) / (colSums(cormatrix^2) + colSums(Q^2))
  out <- list(MSA = MSA, MSA_variable = MSA_variable)

  if (MSA < 0.5) {
    text <- sprintf("The Kaiser, Meyer, Olkin (KMO) measure of sampling adequacy suggests that factor analysis is likely to be inappropriate (KMO = %.2f).", MSA)
    color <- "red"
  } else {
    text <- sprintf("The Kaiser, Meyer, Olkin (KMO) measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = %.2f).", MSA)
    color <- "green"
  }

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "KMO Measure of Sampling Adequacy"
  class(out) <- c("easystats_check", class(out))

  out
}




#' Bartlett's Test of Sphericity
#'
#' Bartlett's (1951) test of sphericity tests whether a matrix (of correlations)
#' is significantly different from an identity matrix. The test provides
#' probability that the correlation matrix has significant correlations among at
#' least some of the variables in a dataset, a prerequisite for factor analysis
#' to work. In other words, before starting with factor analysis, one needs to
#' check whether Bartlett’s test of sphericity is significant.
#'
#' @param x A dataframe.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#' check_sphericity(mtcars)
#' @details This function is strongly inspired by the \code{cortest.bartlett}
#'   function in the \pkg{psych} package (Revelle, 2016). All credit goes to its
#'   author.
#'
#' @return A list of indices related to sphericity.
#'
#' @references \itemize{
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis
#'   and data reduction.
#'
#'   \item Bartlett, M. S. (1951). The effect of standardization on a Chi-square
#'   approximation in factor analysis. Biometrika, 38(3/4), 337-344.
#' }
#' @export
check_sphericity.data.frame <- function(x, ...) {

  # This could be improved using the correlation package to use different correlation methods
  cormatrix <- stats::cor(x, use = "pairwise.complete.obs", ...)

  n <- nrow(x)
  p <- dim(cormatrix)[2]

  detR <- det(cormatrix)
  statistic <- -log(detR) * (n - 1 - (2 * p + 5) / 6)
  df <- p * (p - 1) / 2
  pval <- stats::pchisq(statistic, df, lower.tail = FALSE)

  out <- list(chisq = statistic, p = pval, dof = df)

  if (pval < 0.001) {
    text <- sprintf("Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analysis (Chisq(%i) = %.2f, %s).", df, statistic, insight::format_p(pval))
    color <- "green"
  } else {
    text <- sprintf("Bartlett's test of sphericity suggests that there is not enough significant correlation in the data for factor analysis (Chisq(%i) = %.2f, %s).", df, statistic, insight::format_p(pval))
    color <- "red"
  }

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Test of Sphericity"
  class(out) <- c("easystats_check", class(out))

  out
}

#' @export
#' @importFrom performance check_sphericity
performance::check_sphericity

