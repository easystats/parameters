#' Compute p values using Kenward-Roger approximation
#'
#' The Wald-test approximation treats t-values as Wald z.
#'
#' @param model A statistical model.
#' @param dof Degrees of Freedom.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_kenward(model)
#' model <- circus::merMod_1
#' p_value_kenward(model)
#' }
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
    if (length(coef_names) == length(p)) names(p) <- coef_names
  }

  p
}





#' @rdname p_value_kenward
#' @export
dof_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  pbkrtest::get_ddf_Lb(model, insight::get_parameters(model, effects = "fixed")$estimate)
}
