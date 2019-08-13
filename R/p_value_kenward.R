#' p-values using Kenward-Roger approximation
#'
#' An approximate F-test based on the Kenward-Roger (1997) approach.
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
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
    p <- 2 * stats::pt(abs(params[, "t value"]), dof, lower.tail = FALSE)
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





#' @rdname p_value_kenward
#' @export
dof_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  pbkrtest::get_ddf_Lb(model, insight::get_parameters(model, effects = "fixed")$estimate)
}
