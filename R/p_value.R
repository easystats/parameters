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
p_value <- function(model, method = "wald", ...){
  UseMethod("p_value")
}


#' @export
p_value.lmerMod <- function(model, method = "wald", ...){
  if(method=="wald" | method=="w"){
    out <- p_value_wald(model)
  } else if(method=="kenward" | method=="kr"){
    out <- p_value_kenward(model)
  } else{
    stop("P-value method should be 'wald' or 'kenward'.")
  }
  return(out)
}

#' @export
p_value.merMod <- function(model, ...){
  return(p_value_wald(model))
}