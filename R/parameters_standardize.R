#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be \code{"refit"} (default), \code{"2sd"}, \code{"smart"} or \code{"classic"}.
#' @inheritParams standardize
#'
#' @details \strong{Methods:}
#' \itemize{
#'  \item \strong{refit}: This method is based on a complete model re-fit with a standardized version of data. Hence, this method is equal to standardizing the variables before fitting the model. It is the "purest" and the most accurate (Neter et al., 1989), but it is also the most computationally costly and long (especially for Bayesian models). This method is particularly recommended for complex models that include interactions or transformations (e.g., polynomial or spline terms). The \code{robust} (default to \code{FALSE}) argument enables a robust standardization of data, i.e., based on the \code{median} and \code{MAD} instead of the \code{mean} and \code{SD}.
#'  \item \strong{2sd}: Same as \code{method = "refit"}, however, standardization is done by dividing by two times the \code{SD} or \code{MAD} (depending on \code{robust}). This method is useful to obtain coefficients of continuous parameters comparable to coefficients related to binary predictors (see Gelman, 2008).
#'  \item \strong{smart} (Standardization of Model's parameters with Adjustment, Reconnaissance and Transformation): Post-hoc standardization of the parameters, aiming at emulating the results obtained by "refit". The coefficients are divided by the standard deviation (or MAD if \code{robust}) of the outcome (which becomes their expression 'unit'). Then, the coefficients related to numeric variables are additionally multiplied by the standard deviation (or MAD if \code{robust}) of the related term, so that they correspond to changes of 1 SD of the predictor (e.g., "A change in 1 SD of \code{x} is related to a change of 0.24 of the SD of \code{y}). This does not apply to binary variables or factors, so the coefficients are still related to changes in levels.
#'  \item \strong{classic}: This method is similar to \code{method = "smart"}, but treats all variables as continuous: it also scales the coefficient by the standard deviation of model's matrix' parameter of factors levels (transformed to integers) or binary predictors. Although being inappropriate for these cases, this method is the one implemented by default in other software packages, such as \code{lm.beta::lm.beta()}.
#' }
#' When \code{method = "smart"} or \code{method = "classic"}, \code{parameters_standardize()}
#' also returns the standard errors for the standardized coefficients. Then, \code{ci()} can be
#' used to calculate confidence intervals for the standardized coefficients. See 'Examples'.
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
#' parameters_standardize(model, method = "smart")
#' parameters_standardize(model, method = "smart", robust = TRUE)
#'
#' # show CI
#' ps <- parameters_standardize(model, method = "smart", robust = TRUE)
#' ci(ps)
#'
#' iris$binary <- ifelse(iris$Sepal.Width > 3, 1, 0)
#' model <- glm(binary ~ Species * Sepal.Length, data = iris, family = "binomial")
#' parameters_standardize(model, method = "refit")
#' parameters_standardize(model, method = "refit", robust = TRUE)
#' parameters_standardize(model, method = "smart")
#' parameters_standardize(model, method = "smart", robust = TRUE)
#' \donttest{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Species * Petal.Width, data = iris, iter = 500, refresh = 0)
#' parameters_standardize(model, method = "smart", centrality = "all")
#' parameters_standardize(model, method = "smart", robust = TRUE, centrality = "all")
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
  method <- match.arg(method, choices = c("default", "refit", "2sd", "smart", "partial", "classic"))

  # Refit
  if (method %in% c("refit", "2sd")) {
    std_params <- .parameters_standardize_refit(model, robust = robust, method = method, verbose = verbose, ...)

    # Posthoc
  } else if (method %in% c("default", "smart", "classic")) {
    std_params <- .parameters_standardize_posthoc(model, param_names = NULL, param_values = NULL, robust = robust, method = method, verbose = verbose, ...)

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
#' @importFrom insight model_info get_data
#' @keywords internal
.parameters_standardize_posthoc <- function(model, param_names = NULL, param_values = NULL, robust = FALSE, method = "smart", verbose = TRUE, ...) {

  # Get parameters
  if (is.null(param_names) | is.null(param_values) | length(param_names) != length(param_values)) {
    params <- model_parameters(model, standardize = FALSE, component = "conditional", ...)
    param_values <- params[names(params) %in% c("Coefficient", "Median", "Mean", "MAP")]
    param_names <- params$Parameter

    if (verbose && insight::model_info(model)$is_zero_inflated) {
      warning("Post-hoc parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
    }
  }

  out <- data.frame(Parameter = param_names)

  # Get info
  deviations <- standardize_info(model, robust = robust)
  if (method == "classic") {
    relevant_col <- "Deviation_Classic"
  } else {
    relevant_col <- "Deviation_Smart"
  }

  # Loop over all parameters
  for (param_name in names(param_values)) {
    out[[param_name]] <- param_values[[param_name]] * deviations[[relevant_col]] / deviations$Deviation_Response
  }


  # Standardize SE if possible
  std_error <- tryCatch({
    se <- standard_error(model, component = "conditional")
    se$SE[unique(match(se$Parameter, param_names))]
  },
  error = function(e) {
    NULL
  })

  if (!is.null(std_error)) {
    std_error <- std_error * deviations[[relevant_col]] / deviations$Deviation_Response
  }

  # add standardized standard errors as attribute
  attr(out, "standard_error") <- std_error
  class(out) <- c("parameters_std_classic", class(out))

  out
}


