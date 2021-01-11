#' @title Kenward-Roger approximation for SEs, CIs and p-values
#' @name p_value_kenward
#'
#' @description An approximate F-test based on the Kenward-Roger (1997) approach.
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
#' \code{\link[=dof_ml1]{dof_ml1()}} approximate degrees
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
  .p_value_dof(model, dof, method = "kenward")
}





# helper ------------------------------

.p_value_dof <- function(model, dof, method, statistic = NULL, se = NULL) {
  params <- insight::get_parameters(model)
  if (is.null(statistic)) {
    statistic <- insight::get_statistic(model)$Statistic
  }

  if (method == "kenward") {
    if (is.null(se)) {
      se <- se_kenward(model)$SE
    }
    estimate <- if ("Coefficient" %in% colnames(params)) {
      params$Coefficient
    } else {
      params$Estimate
    }
    statistic <- estimate / se
  }

  p <- 2 * stats::pt(abs(statistic), df = dof, lower.tail = FALSE)
  .data_frame(
    Parameter = params$Parameter,
    p = unname(p)
  )
}




.p_value_dof_kr <- function(model, params, dof) {
  if ("SE" %in% colnames(params) && "SE" %in% colnames(dof)) {
    params$SE <- NULL
  }
  params <- merge(params, dof, by = "Parameter")
  p <- 2 * stats::pt(abs(params$Estimate / params$SE), df = params$df_error, lower.tail = FALSE)

  .data_frame(
    Parameter = params$Parameter,
    p = unname(p)
  )
}





# helper -------------------------

.check_REML_fit <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.")
  }
  if (!(lme4::getME(model, "is_REML"))) {
    warning("Model was not fitted by REML. Re-fitting model now, but p-values, df, etc. still might be unreliable.", call. = FALSE)
  }
}
