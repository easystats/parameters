#' p-values using Satterthwaite approximation
#'
#' An approximate F-test based on a Satterthwaite approximation as suggested by Giesbrecht \& Burns (1985).
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#'
#' @details \code{dof_satterthwaite()} and \code{se_satterthwaite()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors for model
#' parameters, based on the Giesbrecht \& Burns approach.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_satterthwaite(model)
#' }
#'
#' @return The p-values.
#' @references Giesbrecht, F.G. and Burns, J.C. (1985), "Two-Stage Analysis Based on a Mixed Model: Large-sample Asymptotic Theory and Small-Sample Simulation Results", Biometrics 41: 853-862.
#' @importFrom stats pt coef
#' @export
p_value_satterthwaite <- function(model, dof = NULL) {
  UseMethod("p_value_satterthwaite")
}



#' @export
p_value_satterthwaite.merMod <- function(model, dof = NULL) {
  if (is.null(dof)) {
    dof <- dof_satterthwaite(model)
  }

  statistic <- insight::get_statistic(model)
  p <- 2 * stats::pt(abs(statistic$Statistic), df = dof, lower.tail = FALSE)

  data.frame(
    Parameter = statistic$Parameter,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}
