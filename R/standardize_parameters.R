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
#'  \item \strong{refit}: This method is based on a complete model re-fit using the standardized data (based on \code{mean} and \code{sd}). It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  \item \strong{refit_robust}: This method is based on a complete model re-fit using the standardized data (based on \code{median} and \code{mad}). It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  \item \strong{refit_2sd}: This method is based on a complete model re-fit using the standardized data (based on \code{mean} and two times the \code{sd}). It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  \item \strong{full}: Post-hoc standardization of the model parmaters, based on \code{sd} of parameters.
#'  \item \strong{full_robust}: Post-hoc standardization of the model parmaters, based on \code{mad} of parameters.
#' }
#'
#' @examples
#' data(iris)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "refit_robust")
#' standardize_parameters(model, method = "refit_2sd")
#' standardize_parameters(model, method = "full")
#' standardize_parameters(model, method = "full_robust")
#'
#' iris$binary <- ifelse(iris$Sepal.Width > 3, 1, 0)
#' model <- glm(binary ~ Species * Sepal.Length, data = iris, family = "binomial")
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "refit_robust")
#' standardize_parameters(model, method = "full")
#' standardize_parameters(model, method = "full_robust")
#'
#' @importFrom stats mad sd predict cor
#' @importFrom insight get_parameters model_info get_data get_response
#' @importFrom utils tail
#' @export
standardize_parameters <- function(model, method = "refit", ...) {

  method <- match.arg(method, choices = c("refit", "refit_robust", "refit_2sd", "full", "full_robust", "partial"))

  if (method %in% c("refit", "refit_robust")) {
    method <- switch(method, refit = "mean", refit_robust = "median")
    std_model <- standardize(model, method = method, ...)
    std_params <- insight::get_parameters(std_model)
  } else if (method %in% c("mean", "full", "full_robust")) {
    std_params <- .standardize_parameters_full(model, method)
  } else if (method == "refit_2sd") {
    std_model <- standardize(model, method = "2sd", ...)
    std_params <- insight::get_parameters(std_model)
  } else if (method == "partial") {
    stop("method='partial' not implemented yet :(")
  }

  names(std_params) <- c("Parameter", "Std_Estimate")
  std_params
}


#' @keywords internal
.standardize_parameters_full <- function(model, method) {
  info <- insight::model_info(model)
  params <- insight::get_parameters(model)
  data <- insight::get_data(model)

  # Linear models
  if (info$is_linear) {
    if (method %in% c("mean", "full")) {
      sd_y <- stats::sd(insight::get_response(model))
    } else {
      sd_y <- stats::mad(insight::get_response(model))
    }

    std_params <- data.frame()

    for (name in params$parameter) {
      coef <- params[params$parameter == name, ]$estimate
      std_coef <- .standardize_parameter_full(name, coef, data, sd_y, method)
      std_params <-
        rbind(std_params, data.frame("parameter" = name, "estimate" = std_coef))
    }

  # Binomial models
  } else if (info$is_logit) {
    logit_y <- stats::predict(model)
    r <- stats::cor(insight::get_response(model), odds_to_probs(logit_y, log = TRUE))
    if (method %in% c("mean", "full")) {
      sd_y <- stats::sd(logit_y)
    } else{
      sd_y <- stats::mad(logit_y)
    }

    std_params <- data.frame()
    for (name in params$parameter) {
      coef <- params[params$parameter == name, ]$estimate
      std_coef <- .standardize_parameter_full(name, coef, data, sd_y, method)
      std_coef <- std_coef * r
      std_params <-
        rbind(std_params, data.frame("parameter" = name, "estimate" = std_coef))
    }
  } else {
    stop("method='full' not applicable to standardize this type of model. Please use method='refit'.")
  }

  std_params
}


#' @keywords internal
.standardize_parameter_full <- function(name, coef, data, sd_y, method) {
  param_type <- .find_parameter_type(name, data)

  if ("interaction" %in% param_type) {
    predictor <- param_type[[2]]
    if ("numeric" %in% .find_parameter_type(predictor, data)) {
      if (method %in% c("mean", "full")) {
        std_coef <- coef * stats::sd(data[[predictor]]) / sd_y
      } else{
        std_coef <- coef * stats::mad(data[[predictor]]) / sd_y
      }
    } else{
      std_coef <- coef / sd_y
    }
  } else if ("numeric" %in% param_type) {
    if (method %in% c("mean", "full")) {
      std_coef <- coef * stats::sd(data[[name]]) / sd_y
    } else{
      std_coef <- coef * stats::mad(data[[name]]) / sd_y
    }
  } else if ("intercept" %in% param_type) {
    std_coef <- NA
  } else if ("factor" %in% param_type) {
    std_coef <- coef / sd_y
  } else {
    std_coef <- coef / sd_y
  }

  std_coef
}






#' @keywords internal
.find_parameter_type <- function(name, data){

  if (grepl(":", name)) {
    var <- utils::tail(unlist(strsplit(name, ":", fixed = TRUE)), 1)
    return(c("interaction", var))
  } else if (name == "(Intercept)") {
    return(c("intercept"))
  } else if (name %in% names(data)) {
    return(c("numeric"))
  } else {
    facs <- data[sapply(data, is.factor)]
    facs_names <- c()
    for (fac in names(facs)) {
      facs_names <- c(facs_names, paste0(fac, unique(data[[fac]])))
    }

    if (name %in% facs_names) {
      return("factor")
    } else{
      return("unknown")
    }
  }
}
