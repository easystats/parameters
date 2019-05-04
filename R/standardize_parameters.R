#' Standardized Model Parameters
#'
#' Compute standardized model parameters (coefficients). See \href{https://easystats.github.io/parameters/articles/parameters_standardization.html}{this vignette}.
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be \code{"refit"} (default), "2sd", "full" or "classic".
#' @inheritParams standardize
#'
#' @details \strong{Methods:}
#' \itemize{
#'  \item \strong{refit}: This method is based on a complete model re-fit with a standardized version of data. Hence, this method is equal to standardizing the variables before fitting the model. It is the "purest" and the most accurate (Neter et al., 1989), but it is also the most computationally costly and long (especially for Bayesian models). This method is particularly recommended for complex models that include interactions or transformations (e.g., polynomial or spline terms). The \code{robust} (default to \code{FALSE}) argument enables a robust standardization of data, i.e., based on the \code{median} and \code{MAD} instead of the \code{mean} and \code{SD}.
#'  \item \strong{2sd}: Same as \code{method = "refit"}, however, standardization is done by dividing by two times the \code{SD} or \code{MAD} (depending on \code{robust}). This method is useful to obtain coefficients of continuous parameters comparable to coefficients related to binary predictors (see Gelman, 2008).
#'  \item \strong{full}: Post-hoc standardization of the model paramaters. The coefficients are divided by the standard deviation (or MAD if \code{robust}) of the outcome (which becomes their expression 'unit'). Then, the coefficients related to numeric variables are additionaly multiplied by the standard deviation (or MAD if \code{robust}) of the related term, so that they correspond to changes of 1 SD of the predictor (e.g., "A change in 1 SD of \code{x} is related to a change of 0.24 of the SD of \code{y}). This does not apply to binary variables or factors, so the coefficients are still related to changes in levels.
#'  \item \strong{classic}: This method is similar to \code{method = "full"}, but treats all variables as continuous: it also scales the coefficient by the standard deviation of factors (transformed to integers) or binary predictors. Altough being inapropriate for these cases, this method is the one implemented by default in other softwares, such as \code{sjstats::std_beta()} or \code{lm.beta::lm.beta()}.
#' }
#'
#'
#' @examples
#' library(parameters)
#' data(iris)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "refit", robust = TRUE)
#' standardize_parameters(model, method = "2sd")
#' standardize_parameters(model, method = "2sd", robust = TRUE)
#' standardize_parameters(model, method = "full")
#' standardize_parameters(model, method = "full", robust = TRUE)
#'
#' iris$binary <- ifelse(iris$Sepal.Width > 3, 1, 0)
#' model <- glm(binary ~ Species * Sepal.Length, data = iris, family = "binomial")
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "refit", robust = TRUE)
#' standardize_parameters(model, method = "full")
#' standardize_parameters(model, method = "full", robust = TRUE)
#'
#' @importFrom stats mad sd predict cor model.matrix
#' @importFrom insight get_parameters model_info get_data get_response
#' @importFrom utils tail
#'
#' @references
#' \itemize{
#'   \item Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear regression models.
#'   \item Gelman, A. (2008). Scaling regression inputs by dividing by two standard deviations. Statistics in medicine, 27(15), 2865-2873.
#' }
#' @export
standardize_parameters <- function(model, robust = FALSE, method = "refit", verbose = TRUE, ...) {

  method <- match.arg(method, choices = c("default", "refit", "2sd", "full", "partial", "classic"))

  # Refit
  if (method %in% c("refit", "2sd")) {
    std_model <- standardize(model, robust = robust, method = method, verbose = verbose, ...)

    # Extract parameters
    if(insight::model_info(model)$is_bayesian){
      std_params <- describe_posterior(insight::get_parameters(std_model), test=NULL, ...)
    } else{
      std_params <- insight::get_parameters(std_model)
      names(std_params) <- c("Parameter", "beta")
    }

  # Posthoc
  } else if (method %in% c("default", "full", "classic")) {

    # Extract parameters
    if(insight::model_info(model)$is_bayesian){
      params <- describe_posterior(insight::get_parameters(model), test=NULL, ...)
    } else{
      params <- insight::get_parameters(model)
      names(params) <- c("Parameter", "beta")
    }

    std_params <- params["Parameter"]
    for(estimate in tail(names(params), -1)){
      std_params <- cbind(std_params,
                          .standardize_parameters_full(model, params[c("Parameter", estimate)], robust, method)[2])
    }

  # Partial
  } else if (method == "partial") {
    stop("method='partial' not implemented yet :(")
  }

  std_params
}


#' @keywords internal
.standardize_parameters_full <- function(model, params, robust, method) {
  info <- insight::model_info(model)
  data <- insight::get_data(model)
  name_estimate <- tail(names(params), -1)

  # Linear models
  if (info$is_linear) {
    if (robust == FALSE) {
      sd_y <- stats::sd(insight::get_response(model))
    } else {
      sd_y <- stats::mad(insight::get_response(model))
    }

    std_params <- data.frame()
    for (name in params$Parameter) {
      coef <- params[params$Parameter == name, name_estimate]
      std_coef <- .standardize_parameter_full(model, name, coef, data, sd_y, robust, method)
      std_params <-
        rbind(std_params, data.frame("Parameter" = name, "estimate" = std_coef))
    }

  # Binomial models
  } else if (info$is_logit) {
    logit_y <- stats::predict(model)
    r <- stats::cor(insight::get_response(model), odds_to_probs(logit_y, log = TRUE))
    if (robust == FALSE) {
      sd_y <- stats::sd(logit_y)
    } else{
      sd_y <- stats::mad(logit_y)
    }

    std_params <- data.frame()
    for (name in params$Parameter) {
      coef <- params[params$Parameter == name, name_estimate]
      std_coef <- .standardize_parameter_full(model, name, coef, data, sd_y, robust, method)
      std_coef <- std_coef * r
      std_params <-
        rbind(std_params, data.frame("Parameter" = name, "estimate" = std_coef))
    }
  } else {
    stop("method='full' not applicable to standardize this type of model. Please use method='refit'.")
  }

  names(std_params) <- names(params)
  std_params
}


#' @keywords internal
.standardize_parameter_full <- function(model, name, coef, data, sd_y, robust, method) {
  param_type <- .find_parameter_type(name, data)

  if ("interaction" %in% param_type) {
    predictor <- param_type[[2]]
    if ("numeric" %in% .find_parameter_type(predictor, data)) {
      if (robust == FALSE) {
        std_coef <- coef * stats::sd(data[[predictor]]) / sd_y
      } else{
        std_coef <- coef * stats::mad(data[[predictor]]) / sd_y
      }
    } else{
      std_coef <- coef / sd_y
    }
  } else if ("numeric" %in% param_type) {
    if (robust == FALSE) {
      std_coef <- coef * stats::sd(data[[name]]) / sd_y
    } else{
      std_coef <- coef * stats::mad(data[[name]]) / sd_y
    }
  } else if ("intercept" %in% param_type) {
    std_coef <- NA
  } else if ("factor" %in% param_type) {
    if (method == "classic") {
      if (robust == FALSE) {
        std_coef <- coef * stats::sd(stats::model.matrix(model)[, name]) / sd_y
      } else{
        std_coef <- coef * stats::mad(stats::model.matrix(model)[, name]) / sd_y
      }
    } else {
      std_coef <- coef / sd_y
    }
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
