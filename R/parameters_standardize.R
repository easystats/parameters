#' Parameters standardization
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
#' parameters_standardize(model, method = "refit")
#' parameters_standardize(model, method = "refit", robust = TRUE)
#' parameters_standardize(model, method = "2sd")
#' parameters_standardize(model, method = "2sd", robust = TRUE)
#' parameters_standardize(model, method = "full")
#' parameters_standardize(model, method = "full", robust = TRUE)
#'
#' iris$binary <- ifelse(iris$Sepal.Width > 3, 1, 0)
#' model <- glm(binary ~ Species * Sepal.Length, data = iris, family = "binomial")
#' parameters_standardize(model, method = "refit")
#' parameters_standardize(model, method = "refit", robust = TRUE)
#' parameters_standardize(model, method = "full")
#' parameters_standardize(model, method = "full", robust = TRUE)
#'
#' @importFrom stats mad sd predict cor model.matrix
#' @importFrom insight get_parameters model_info get_data get_response
#' @importFrom utils tail
#' @importFrom bayestestR describe_posterior
#'
#' @references
#' \itemize{
#'   \item Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear regression models.
#'   \item Gelman, A. (2008). Scaling regression inputs by dividing by two standard deviations. Statistics in medicine, 27(15), 2865-2873.
#' }
#' @export
parameters_standardize <- function(model, robust = FALSE, method = "refit", verbose = TRUE, ...) {
  method <- match.arg(method, choices = c("default", "refit", "2sd", "full", "partial", "classic"))

  # Refit
  if (method %in% c("refit", "2sd")) {
    std_params <- .parameters_standardize_refit(model, robust = robust, method = method, verbose = verbose, ...)

    # Posthoc
  } else if (method %in% c("default", "full", "classic")) {
    std_params <- .parameters_standardize_full(model, param_names = NULL, param_values = NULL, robust = robust, method = method, verbose = verbose, ...)

    # Partial
  } else if (method == "partial") {
    stop("method='partial' not implemented yet :(")
  }

  names(std_params)[-1] <- paste0("Std_", names(std_params)[-1])
  std_params
}













# REFIT -------------------------------------------------------------------
#' @keywords internal
.parameters_standardize_refit <- function(model, robust = FALSE, method = "refit", verbose = TRUE, bootstrap = FALSE, ...){
  std_model <- standardize(model, robust = robust, method = method, verbose = verbose, ...)

  # Bayesian models
  if (insight::model_info(model)$is_bayesian) {
    std_params <- bayestestR::describe_posterior(std_model, dispersion = FALSE, ci = NULL, test = NULL, diagnostic = NULL, priors = FALSE, ...)
    std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]

  # Frequentist models
  } else {
    if(bootstrap){
      std_params <- parameters_bootstrap(std_model, ...)
      std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]
    } else{
      std_params <- insight::get_parameters(std_model, ...)
      names(std_params) <- c("Parameter", "Coefficient")
    }
  }
  std_params
}





















# POST-HOC -------------------------------------------------------------------
#' @keywords internal
.parameters_standardize_full <- function(model, param_names = NULL, param_values = NULL, robust = FALSE, method = "full", verbose = TRUE, ...) {

  # Get parameters
  if(is.null(param_names) | is.null(param_values) | length(param_names) != length(param_values)){
    params <- model_parameters(model, standardize = FALSE, ...)
    param_values <- params[names(params) %in% c("Coefficient", "Median", "Mean", "MAP")][, 1]
    param_names <- params$Parameter
  }

  # Get response variance
  sd_y <- .variance_response(model, robust = robust, method = method, ...)

  # Create parameter table
  param_table <- .parameters_types_table(param_names, param_values, insight::get_data(model))

  # Loop over all parameters
  std_params <- c()
  for(i in 1:nrow(param_table)){
    sd_x <- .variance_predictor(param_table$Type[i], param_table$Type[i], insight::get_data(model), robust = robust, method = method, ...)
    new_coef <- param_table$Value[i] * sd_x / sd_y
    std_params <- c(std_params, new_coef)
  }

  data.frame(Parameter = param_table$Parameter,
             Coefficient = std_params)
}




#' @keywords internal
.variance_response <- function(model, robust = FALSE, method = "full", ...){
  info <- insight::model_info(model)
  response <- insight::get_response(model)

  if (info$is_linear) {
    if (robust == FALSE) {
      sd_y <- stats::sd(response)
    } else {
      sd_y <- stats::mad(response)
    }
  } else if (info$is_logit) {
    if (insight::model_info(model)$is_bayesian) {
      logit_y <- rstanarm::posterior_predict(model, ...)
    } else{
      logit_y <- stats::predict(model, ...)
    }
    r <- stats::cor(response, odds_to_probs(logit_y, log = TRUE))
    if (robust == FALSE) {
      sd_y <- stats::sd(logit_y) * r
    } else {
      sd_y <- stats::mad(logit_y) * r
    }
  } else{
    stop(paste0("Standardization method ", method, " is not available for this kind of model."))
  }
  sd_y
}



#' @keywords internal
.variance_predictor <- function(type, variable, data, robust = FALSE, method = "full",  ...){
  if (type == "numeric") {
    if (robust == FALSE) {
      sd_x <- stats::sd(data[, variable])
    } else {
      sd_x <- stats::mad(data[, variable])
    }
  } else if (type == "factor") {
    if(method == "classic"){
      if (robust == FALSE) {
        sd_x <- stats::sd(as.numeric(data[, variable]))
      } else {
        sd_x <- stats::mad(as.numeric(data[, variable]))
      }
    } else{
      sd_x <- 1
    }
  } else if(type == "interaction"){
    if(is.numeric(data[, variable])){
      if (robust == FALSE) {
        sd_x <- stats::sd(data[, variable])
      } else {
        sd_x <- stats::mad(data[, variable])
      }
    } else if(is.factor(data[, variable])){
      if(method == "classic"){
        if (robust == FALSE) {
          sd_x <- stats::sd(as.numeric(data[, variable]))
        } else {
          sd_x <- stats::mad(as.numeric(data[, variable]))
        }
      } else{
        sd_x <- 1
      }
    } else{
      sd_x <- 1
    }
  } else{
    sd_x <- 1
  }
  sd_x
}



#'
#' #' @keywords internal
#' .parameters_standardize_full <- function(model, params, robust, method) {
#'   info <- insight::model_info(model)
#'   data <- insight::get_data(model)
#'   response <- insight::get_response(model)
#'   if (!is.numeric(response)) response <- as.numeric(as.character(response))
#'   name_estimate <- utils::tail(names(params), -1)
#'
#'   # Linear models
#'   if (info$is_linear) {
#'     if (robust == FALSE) {
#'       sd_y <- stats::sd(response)
#'     } else {
#'       sd_y <- stats::mad(response)
#'     }
#'
#'     std_params <- data.frame()
#'     for (name in params$Parameter) {
#'       coef <- params[params$Parameter == name, name_estimate]
#'       std_coef <- .standardize_parameter_full(model, name, coef, data, sd_y, robust, method)
#'       std_params <-
#'         rbind(std_params, data.frame("Parameter" = name, "estimate" = std_coef))
#'     }
#'
#'     # Binomial models
#'   } else if (info$is_logit) {
#'     if (insight::model_info(model)$is_bayesian) {
#'       stop(paste0("Standardization method ", method, " is not available for this kind of model."))
#'     }
#'     logit_y <- stats::predict(model)
#'     r <- stats::cor(response, odds_to_probs(logit_y, log = TRUE))
#'     if (robust == FALSE) {
#'       sd_y <- stats::sd(logit_y)
#'     } else {
#'       sd_y <- stats::mad(logit_y)
#'     }
#'
#'     std_params <- data.frame()
#'     for (name in params$Parameter) {
#'       coef <- params[params$Parameter == name, name_estimate]
#'       std_coef <- .standardize_parameter_full(model, name, coef, data, sd_y, robust, method)
#'       std_coef <- std_coef * r
#'       std_params <-
#'         rbind(std_params, data.frame("Parameter" = name, "estimate" = std_coef))
#'     }
#'   } else {
#'     stop("method='full' not applicable to standardize this type of model. Please use method='refit'.")
#'   }
#'
#'   names(std_params) <- names(params)
#'   std_params
#' }







#'
#' #' @keywords internal
#' .standardize_parameter_full <- function(param_name, param_value, data, sd_y, sd_x = NULL) {
#'   param_type <- .parameters_types(param_name, data)
#'
#'   if ("interaction" %in% param_type) {
#'     predictor <- param_type[[2]]
#'     if ("numeric" %in% .parameters_types(predictor, data)) {
#'       std_coef <- param_value * sd_x / sd_y
#'     } else {
#'       std_coef <- param_value / sd_y
#'     }
#'   } else if ("numeric" %in% param_type) {
#'     std_coef <- param_value * sd_x / sd_y
#'   } else if ("intercept" %in% param_type) {
#'     std_coef <- NA
#'   } else if ("factor" %in% param_type) {
#'     std_coef <- param_value / sd_y
#'   } else {
#'     std_coef <- param_value / sd_y
#'   }
#'
#'   std_coef
#' }
#'
#'
#'
#' #' @keywords internal
#' .standardize_parameter_classic <- function(param_value, sd_y, sd_x = NULL) {
#'   param_value * sd_x / sd_y
#' }




