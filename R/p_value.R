#' Compute p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): TO BE IMPROVED.
#' }
#'
#' @param model A statistical model.
#' @param method For mixed models, can be \link[=p_value_wald]{"wald"} (default) or \link[=p_value_kenward]{"kenward"}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1|Species), data=iris)
#' p_value(model)
#' }
#' @export
p_value <- function(model, ...){
  UseMethod("p_value")
}


#' @export
p_value.lmerMod <- function(model, method = c("wald", "kr"), ...){
  method <- match.arg(method)
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "kr") {
    p_value_kenward(model, ...)
  }
}

#' @export
p_value.merMod <- function(model, ...){
  p_value_wald(model)
}