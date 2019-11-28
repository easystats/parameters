#' p-values using m-l-1 heuristic
#'
#' An approximate F-test based on a m-l-1 as suggested by Giesbrecht and Burns (1985).
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#'
#' @details \code{dof_ml1()} and \code{se_ml1()} are small helper-functions
#' to calculate approximated degrees of freedom and standard errors of model
#' parameters, based on the m-l-1 heuristic as suggested by Giesbrecht and
#' Burns.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_ml1(model)
#' }
#'
#' @return The p-values.
#' @references Giesbrecht, F.G. and Burns, J.C. (1985), "Two-Stage Analysis Based on a Mixed Model: Large-sample Asymptotic Theory and Small-Sample Simulation Results", Biometrics 41: 853-862.
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
