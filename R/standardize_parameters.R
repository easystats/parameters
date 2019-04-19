#' Get Standardized Model Parameters
#'
#' Compute standardized model parameters (coefs).
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be "refit" (default).
#' @inheritParams standardize
#'
#' @details Methods:
#' \itemize{
#'  \item \emph{refit}: This method is based on a complete model re-fit using the standardized data. It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  }
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' standardize_parameters(model, method="refit")
#' standardize_parameters(model, method="refit", robust=TRUE)
#' standardize_parameters(model, method="full", robust=FALSE)
#' standardize_parameters(model, method="full", robust=TRUE)
#'
#'
#' @export
standardize_parameters <- function(model, method="refit", robust=FALSE, ...){

  if(method == "refit"){
    std_model <- standardize(model, robust=robust, ...)
    std_params <- insight::get_parameters(std_model)
  } else if(method == "full"){
    std_params <- .standardize_parameters_full(model, robust=robust)
  } else if(method == "2SD"){
    stop("method='2SD' not implemented yet :(")
  } else if(method == "partial"){
    stop("method='partial' not implemented yet :(")
  } else{
    stop("method should be 'refit', 'full', '2SD' or 'partial'.")
  }


  std_params
}


#' @keywords internal
.standardize_parameters_full <- function(model, robust=FALSE){
  info <- insight::model_info(model)

  # Linear models
  if(info$is_linear){
    params <- insight::get_parameters(model)
    data <- insight::get_data(model)

    if(robust == FALSE){
      sd_y <- sd(insight::get_response(model))
    } else{
      sd_y <- mad(insight::get_response(model))
    }

    std_params <- data.frame()
    for(name in params$parameter){
      coef <- params[params$parameter == name, ]$estimate
      param_type <- .find_parameter_type(name, data)
      if("interaction" %in% param_type){
        if("numeric" %in% .find_parameter_type(param_type[[2]], data)){
          if(robust == FALSE){
            std_coef <- coef * sd(data[[param_type[[2]]]]) / sd_y
          } else{
            std_coef <- coef * mad(data[[param_type[[2]]]]) / sd_y
          }
        } else{
          std_coef <- coef / sd_y
        }
      } else if("numeric" %in% param_type){
        if(robust == FALSE){
          std_coef <- coef * sd(data[[name]]) / sd_y
        } else{
          std_coef <- coef * mad(data[[name]]) / sd_y
        }
      } else if("intercept" %in% param_type){
        std_coef <- 0
      } else if("factor" %in% param_type){
        std_coef <- coef / sd_y
      } else {
        std_coef <- coef / sd_y
      }
      std_params <- rbind(std_params,
                          data.frame("parameter"=name,
                                     "estimate"=std_coef))
    }

  # Binomial models
  } else if(info$is_binomial){
    stop("method='full' not applicable for binomial models yet. Please use method=`refit`.")
  } else{
    stop("method='full' not applicable to standardize this type of model. Please use method=`refit`.")
  }

  return(std_params)

}




#' @keywords internal
.find_parameter_type <- function(name, data){

  if(grepl(":", name)){
    var <- tail(unlist(strsplit(name, ":", fixed=TRUE)), 1)
    return(c("interaction", var))
  } else if(name == "(Intercept)"){
    return(c("intercept"))
  } else if(name %in% names(data)){
    return(c("numeric"))
  } else{
    facs <- data[sapply(data, is.factor)]
    facs_names <- c()
    for(fac in names(facs)){
      facs_names <- c(facs_names,
                      paste0(fac, unique(data[[fac]])))
    }

    if(name %in% facs_names){
      return(c("factor"))
    } else{
      return(c("unknown"))
    }
  }
}


# TESTING BED ------------------------------------------------------------------

# library(parameters)
# library(insight)
# library(dplyr)
# library(sjstats)
# library(MuMIn)
# library(lm.beta)
#
# comparison <- function(model){
#   out <- standardize_parameters(model, method="refit", robust=FALSE)
#   robust_true <- standardize_parameters(model, method="refit", robust=FALSE)$estimate
#
#   out$full <- out$estimate - standardize_parameters(model, method="full", robust=FALSE)$estimate
#   out$full_robust <- robust_true - standardize_parameters(model, method="full", robust=FALSE)$estimate
#
#
#   # out$MuMin <- out$estimate - MuMIn::std.coef(model, partial.sd=FALSE)[, 1]
#   out$sjstats <- out$estimate - c(NA, sjstats::std_beta(model)[, 2])
#   out$lm.beta <- out$estimate - lm.beta::lm.beta(model)$standardized.coefficients
#   out <- dplyr::select(out, -estimate)
#   return(out)
# }
#
#
# data <- dplyr::mutate(iris, Group_Sepal.Width = as.factor(ifelse(Sepal.Width > 3, "High", "Low")))
#
#
# # Simple models with numerics
# model <- lm(Sepal.Length ~ Petal.Width * Sepal.Width, data=data)
# knitr::kable(comparison(model), digits=2)
#
# # Simple models with numerics
# model <- lm(Sepal.Length ~ Species * Group_Sepal.Width, data=data)
# knitr::kable(comparison(model), digits=2)
#
# # Simple models with numerics
# model <- lm(Sepal.Length ~ Species * Petal.Width, data=data)
# knitr::kable(comparison(model), digits=2)
#
# model <- lm(Sepal.Length ~ Species * Petal.Width * Group_Sepal.Width * Sepal.Width, data=data)
# knitr::kable(comparison(model), digits=2)
#
# # Simple models with numerics
# model <- lm(Sepal.Length ~ Species * poly(Petal.Width, 2), data=data)
# knitr::kable(comparison(model), digits=2)
#
# # COmplex models
# model <- lm(Sepal.Length ~ Species * Petal.Width * Group_Sepal.Width * poly(Petal.Length, 2), data=data)
# knitr::kable(comparison(model), digits=2)

