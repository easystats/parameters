#' Pool Model Parameters
#'
#' This function "pools" (i.e. combines) model parameters in a similar fashion as \code{mice::pool()}. However, this function pools parameters from \code{parameters_model} objects, as returned by \code{\link{model_parameters}}.
#'
#' @param x A list of \code{parameters_model} objects, as returned by
#'   \code{\link{model_parameters}}, or a list of model-objects that is
#'   supported by \code{model_parameters()}.
#' @param ... Currently not used.
#' @inheritParams model_parameters.default
#'
#' @note This function is still experimental. Results should be reliable,
#'   however, this function may not yet work with all types of models properly.
#'   In particular models with identical names for coefficients (for instance,
#'   models with zero-inflation, where predictors appear in the count and
#'   zero-inflated part) may fail, since the coefficient table is grouped by
#'   coefficient names for pooling. In such cases, coefficients of count and
#'   zero-inflated model parts would be combined.
#'
#' @details Averaging of parameters follows Rubin's rules (\cite{Rubin, 1987, p. 76}).
#'   The pooled degrees of freedom is based on the Barnard-Rubin adjustment for
#'   small samples (\cite{Barnard and Rubin, 1999}).
#'
#' @references
#' Barnard, J. and Rubin, D.B. (1999). Small sample degrees of freedom with multiple imputation. Biometrika, 86, 948-955.
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley and Sons.
#'
#' @examples
#' # example for multiple imputed datasets
#' if (require("mice")) {
#'   data("nhanes2")
#'   imp <- mice(nhanes2)
#'   models <- lapply(1:5, function(i) {
#'     lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
#'   })
#'   pool_parameters(models)
#'
#'   # should be identical to:
#'   m <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#'   summary(pool(m))
#' }
#' @return A data frame of indices related to the model's parameters.
#' @importFrom stats var pt
#' @importFrom insight is_model_supported is_model
#' @export
pool_parameters <- function(x, exponentiate = FALSE, ...) {

  if (all(sapply(x, insight::is_model)) && all(sapply(x, insight::is_model_supported))) {
    x <- lapply(x, model_parameters, ...)
  }

  if (!all(sapply(x, inherits, "parameters_model"))) {
    stop("'x' must be a list of 'parameters_model' objects, as returned by the 'model_parameters()' function.", call. = FALSE)
  }

  params <- do.call(rbind, x)
  len <- length(x)
  ci <- attributes(x[[1]]$ci)
  if (is.null(ci)) ci <- .95
  estimates <- split(params, factor(params$Parameter, levels = unique(x[[1]]$Parameter)))

  pooled_params <- do.call(rbind, lapply(estimates, function(i) {
    # pooled estimate
    pooled_estimate <- mean(i$Coefficient)

    # pooled standard error
    ubar <- mean(i$SE^2)
    tmp <- ubar + (1 + 1 / len) * stats::var(i$Coefficient)
    pooled_se <- sqrt(tmp)

    # pooled degrees of freedom, Barnard-Rubin adjustment for small samples
    df_column <- colnames(i)[grepl("(\\bdf\\b|\\bdf_error\\b)", colnames(i))][1]
    if (length(df_column)) {
      pooled_df <- .barnad_rubin(m = nrow(i), b = stats::var(i$Coefficient), t = tmp, dfcom = unique(i[[df_column]]))
    } else {
      pooled_df <- Inf
    }

    pooled_statistic <- pooled_estimate / pooled_se

    # confidence intervals
    alpha <- (1 + ci) / 2
    fac <- suppressWarnings(stats::qt(alpha, df = pooled_df))

    data.frame(
      Coefficient = pooled_estimate,
      SE = pooled_se,
      CI_low = pooled_estimate - pooled_se * fac,
      CI_high = pooled_estimate + pooled_se * fac,
      Statistic = pooled_statistic,
      df_error = pooled_df,
      p = 2 * stats::pt(abs(pooled_statistic), df = pooled_df, lower.tail = FALSE)
    )
  }))

  pooled_params$Parameter <- x[[1]]$Parameter
  pooled_params <- pooled_params[c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Statistic", "df_error", "p")]

  if (exponentiate) pooled_params <- .exponentiate_parameters(pooled_params)
  # this needs to be done extra here, cannot call ".add_model_parameters_attributes()"
  pooled_params <- .add_pooled_params_attributes(pooled_params, x[[1]], ci, exponentiate)
  class(pooled_params) <- c("parameters_model", "see_parameters_model", class(pooled_params))

  pooled_params
}



.barnad_rubin <- function(m, b, t, dfcom = 999999) {
  # fix for z-statistic
  if (is.null(dfcom) || all(is.na(dfcom)) || all(is.infinite(dfcom))) {
    return(Inf)
  }
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda ^ 2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  dfold * dfobs / (dfold + dfobs)
}



.add_pooled_params_attributes <- function(params, model, ci, exponentiate) {
  attr(params, "ci") <- ci
  attr(params, "exponentiate") <- exponentiate
  attr(params, "pretty_names") <- attributes(model)$pretty_names
  attr(params, "ordinal_model") <- attributes(model)$ordinal_model
  attr(params, "model_class") <- attributes(model)$model_class
  attr(params, "bootstrap") <- attributes(model)$bootstrap
  attr(params, "iterations") <- attributes(model)$iterations
  attr(params, "df_method") <- attributes(model)$df_method
  attr(params, "coefficient_name") <- attributes(model)$coefficient_name
  attr(params, "zi_coefficient_name") <- attributes(model)$zi_coefficient_name
  attr(params, "digits") <- attributes(model)$digits
  attr(params, "ci_digits") <- attributes(model)$ci_digits
  attr(params, "p_digits") <- attributes(model)$p_digits
  params
}