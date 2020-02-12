#' Kenward-Roger approximation for SEs, CIs and p-values
#'
#' An approximate F-test based on the Kenward-Roger (1997) approach.
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#' @inheritParams ci.merMod
#'
#' @details Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statistics. Unlike simpler approximation heuristics
#' like the "m-l-1" rule (\code{dof_ml1}), the Kenward-Roger approximation is
#' also applicable in more complex multilevel designs, e.g. with cross-classified
#' clusters. However, the "m-l-1" heuristic also applies to generalized
#' mixed models, while approaches like Kenward-Roger or Satterthwaite are limited
#' to linear mixed models only.
#'
#' @seealso \code{dof_kenward()} and \code{se_kenward()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors for model
#' parameters, based on the Kenward-Roger (1997) approach.
#' \cr \cr
#' \code{\link[=dof_satterthwaite]{dof_satterthwaite()}} and
#' \code{\link[=dof_ml1]{dof_ml1}} approximate degrees
#' of freedom based on Satterthwaite's method or the "m-l-1" rule.
#'
#' @examples
#' \donttest{
#' if (require("lme4")) {
#'   model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   p_value_kenward(model)
#' }
#' }
#' @return A data frame.
#' @references Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed effects from restricted maximum likelihood. Biometrics, 983-997.
#' @importFrom stats pt coef
#' @export
p_value_kenward <- function(model, dof = NULL) {
  UseMethod("p_value_kenward")
}


#' @export
p_value_kenward.lmerMod <- function(model, dof = NULL) {
  if (is.null(dof)) {
    dof <- dof_kenward(model)
  }
  .p_value_dof(model, dof)
}





# helper ------------------------------

.p_value_dof <- function(model, dof) {
  params <- insight::get_parameters(model)
  se <- se_kenward(model)
  p <- 2 * stats::pt(abs(params$Coefficient / se$SE), df = dof, lower.tail = FALSE)

  data.frame(
    Parameter = statistic$Parameter,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}




.p_value_dof2 <- function(model, params, dof) {
  params <- merge(params, dof, by = "Parameter")
  p <- 2 * stats::pt(abs(params$Estimate / params$SE), df = params$df_error, lower.tail = FALSE)

  data.frame(
    Parameter = params$Parameter,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}
