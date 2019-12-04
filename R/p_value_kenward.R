#' p-values using Kenward-Roger approximation
#'
#' An approximate F-test based on the Kenward-Roger (1997) approach.
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#'
#' @details Inferential statistics (like p-values, confidence intervals and
#' standard errors) may be biased in mixed models when the number of clusters
#' is small (even if the sample size of level-1 units is high). In such cases
#' it is recommended to approximate a more accurate number of degrees of freedom
#' for such inferential statitics. Unlike simpler approximation heuristics
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
#' \code{\link{dof_satterthwaite}} and \code{\link{dof_ml1}} approximate degrees
#' of freedom bases on Satterthwaite's method or the "m-l-1" rule.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_kenward(model)
#' }
#'
#' @return The p-values.
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

  params <- as.data.frame(stats::coef(summary(model)))

  if ("t value" %in% names(params)) {
    p <- 2 * stats::pt(abs(params[, "t value"]), df = dof, lower.tail = FALSE)
  } else {
    stop("Couldn't find any suitable statistic (t value) for Kenward-Roger approximation.")
  }

  if (is.null(names(p))) {
    coef_names <- rownames(params)
  } else {
    coef_names <- names(p)
  }

  data.frame(
    Parameter = coef_names,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}

