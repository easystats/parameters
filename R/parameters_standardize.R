#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be \code{"refit"} (default), \code{"2sd"}, \code{"full"} or \code{"classic"}.
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
#' \donttest{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Species * Petal.Width, data = iris, iter = 500, refresh = 0)
#' parameters_standardize(model, method = "full", centrality = "all")
#' parameters_standardize(model, method = "full", robust = TRUE, centrality = "all")
#' }
#' @importFrom stats mad sd predict cor model.matrix
#' @importFrom insight get_parameters model_info get_data get_response
#' @importFrom utils tail
#' @importFrom bayestestR describe_posterior
#'
#' @return Standardized parameters.
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
    # params <- model_parameters(model, standardize = FALSE, ...)
    # param_values <- params[names(params) %in% c("Coefficient", "Median", "Mean", "MAP")]
    # param_names <- params$Parameter
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
.parameters_standardize_refit <- function(model, robust = FALSE, method = "refit", verbose = TRUE, bootstrap = FALSE, ...) {
  std_model <- standardize(model, robust = robust, method = method, verbose = verbose, ...)

  # Bayesian models
  if (insight::model_info(model)$is_bayesian) {
    std_params <- bayestestR::describe_posterior(std_model, dispersion = FALSE, ci = NULL, test = NULL, diagnostic = NULL, priors = FALSE, ...)
    std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]

    # Frequentist models
  } else {
    if (bootstrap) {
      std_params <- parameters_bootstrap(std_model, ...)
      std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]
    } else {
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
  if (is.null(param_names) | is.null(param_values) | length(param_names) != length(param_values)) {
    params <- model_parameters(model, standardize = FALSE, ...)
    param_values <- params[names(params) %in% c("Coefficient", "Median", "Mean", "MAP")]
    param_names <- params$Parameter
  }

  out <- data.frame(Parameter = param_names)
  param_table <- parameters_type(model)

  for (param_name in names(param_values)) {
    # Get response variance
    sd_y <- .variance_response(model, robust = robust, method = method, ...)

    # Create parameter table
    param_table$Value <- param_values[[param_name]]

    # Loop over all parameters
    std_params <- c()
    for (i in 1:nrow(param_table)) {
      sd_x <- .variance_predictor(type = param_table$Type[i], variable = param_table$Variable[i], data = insight::get_data(model), robust = robust, method = method, ...)
      new_coef <- param_table$Value[i] * sd_x / sd_y
      std_params <- c(std_params, new_coef)
    }

    out[[param_name]] <- std_params
  }

  out
}




#' @keywords internal
.variance_response <- function(model, robust = FALSE, method = "full", ...) {
  info <- insight::model_info(model)
  response <- insight::get_response(model)

  if (info$is_linear) {
    if (robust == FALSE) {
      sd_y <- stats::sd(response)
    } else {
      sd_y <- stats::mad(response)
    }
  } else {
    sd_y <- 1
  }
  sd_y
}



#' @keywords internal
.variance_predictor <- function(type, variable, data, robust = FALSE, method = "full", ...) {
  if (type == "intercept") {
    sd_x <- 0
  } else if (type == "numeric") {
    if (robust == FALSE) {
      sd_x <- stats::sd(data[, variable])
    } else {
      sd_x <- stats::mad(data[, variable])
    }
  } else if (type == "factor") {
    if (method == "classic") {
      if (robust == FALSE) {
        sd_x <- stats::sd(as.numeric(data[, variable]))
      } else {
        sd_x <- stats::mad(as.numeric(data[, variable]))
      }
    } else {
      sd_x <- 1
    }
  } else if (type == "interaction") {
    if (is.numeric(data[, variable])) {
      if (robust == FALSE) {
        sd_x <- stats::sd(data[, variable])
      } else {
        sd_x <- stats::mad(data[, variable])
      }
    } else if (is.factor(data[, variable])) {
      if (method == "classic") {
        if (robust == FALSE) {
          sd_x <- stats::sd(as.numeric(data[, variable]))
        } else {
          sd_x <- stats::mad(as.numeric(data[, variable]))
        }
      } else {
        sd_x <- 1
      }
    } else {
      sd_x <- 1
    }
  } else {
    sd_x <- 1
  }
  sd_x
}
