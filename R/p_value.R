#' Compute p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): TO BE IMPROVED.
#' }
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value(model)
#' }
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}




#' @rdname p_value
#' @export
p_value.htest <- function(model, ...) {
  model$p.value
}


#' @rdname p_value
#' @export
p_value.aov <- function(model, ...) {
  params <- model_parameters(model)
  if(nrow(params) == 0){
    return(NA)
  }
  if("Group" %in% names(params)){
    params <- params[params$Group == "Within",]
  }
  if("Residuals" %in% params$Parameter){
    params <- params[params$Parameter != "Residuals", ]
  }
  if(!"p" %in% names(params)){
    return(NA)
  }
  p <- params$p
  names(p) <- params$Parameter
  p
}
#' @export
p_value.anova <- p_value.aov
#' @export
p_value.aovlist <- p_value.aov



#' @rdname p_value
#' @param method For mixed models, can be \link[=p_value_wald]{"wald"} (default) or \link[=p_value_kenward]{"kenward"}.
#' @export
p_value.lmerMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "kr" | method == "kenward") {
    p_value_kenward(model, ...)
  }
}



#' @rdname p_value
#' @export
p_value.merMod <- function(model, ...) {
  p_value_wald(model, ...)
}
