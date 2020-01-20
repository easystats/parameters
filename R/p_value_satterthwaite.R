#' Satterthwaite approximation for SEs, CIs and p-values
#'
#' An approximate F-test based on the Satterthwaite (1946) approach.
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#' @inheritParams ci.merMod
#'
#' @details Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statitics. Unlike simpler approximation heuristics
#' like the "m-l-1" rule (\code{dof_ml1}), the Satterthwaite approximation is
#' also applicable in more complex multilevel designs. However, the "m-l-1"
#' heuristic also applies to generalized mixed models, while approaches like
#' Kenward-Roger or Satterthwaite are limited to linear mixed models only.
#'
#' @seealso \code{dof_satterthwaite()} and \code{se_satterthwaite()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors for model
#' parameters, based on the Satterthwaite (1946) approach.
#' \cr \cr
#' \code{\link[=dof_kenward]{dof_kenward()}} and \code{\link[=dof_ml1]{dof_ml1()}}
#' approximate degrees of freedom based on Kenward-Roger's method or the "m-l-1" rule.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_satterthwaite(model)
#' }
#' @return A data frame.
#' @references Satterthwaite FE (1946) An approximate distribution of estimates of variance components. Biometrics Bulletin 2 (6):110–4.
#' @importFrom stats pt coef
#' @export
p_value_satterthwaite <- function(model, dof = NULL) {
  UseMethod("p_value_satterthwaite")
}


#' @export
p_value_satterthwaite.lmerMod <- function(model, dof = NULL) {
  if (is.null(dof)) {
    dof <- dof_satterthwaite(model)
  }
  .p_value_dof(model, dof)
}
