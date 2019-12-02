#' p-values using the "m-l-1" heuristic
#'
#' Approximation of degrees of freedom based on a "m-l-1" heuristic as suggested by Elff et al. (2019).
#'
#' @param model A mixed model.
#' @param dof Degrees of Freedom.
#'
#' @details Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statitics. The \emph{m-l-1} heuristic is such an approach
#' that uses a t-distribution with fewer degrees of freedom (\code{dof_ml1}) to
#' calculate p-values (\code{p_value_ml1}), standard errors (\code{se_ml1})
#' and confidence intervals (\code{ci(method = "ml1")}).
#' \strong{Note} that the "m-l-1" heuristic is not applicable for complex
#' multilevel designs, e.g. with cross-classified clusters. In such cases,
#' more accurate approaches like the Kenward-Roger approximation (\code{dof_kenward()})
#' is recommended. However, the "m-l-1" heuristic also applies to generalized
#' mixed models, while approaches like Kenward-Roger or Satterthwaite are limited
#' to linear mixed models only.
#' @seealso \code{dof_ml1()} and \code{se_ml1()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors of model
#' parameters, based on the "m-l-1" heuristic.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_ml1(model)
#' }
#'
#' @return The p-values.
#' @references Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019): Multilevel Analysis with Few Clusters: Improving Likelihood-based Methods to Provide Unbiased Estimates and Accurate Inference, British Journal of Political Science.
#' @importFrom stats pt coef
#' @export
p_value_ml1 <- function(model, dof = NULL) {
  UseMethod("p_value_ml1")
}



#' @export
p_value_ml1.merMod <- function(model, dof = NULL) {
  if (is.null(dof)) {
    dof <- dof_ml1(model)
  }

  statistic <- insight::get_statistic(model)
  p <- 2 * stats::pt(abs(statistic$Statistic), df = dof, lower.tail = FALSE)

  data.frame(
    Parameter = statistic$Parameter,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}
