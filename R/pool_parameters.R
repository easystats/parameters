#' Pool Model Parameters
#'
#' This function "pools" (i.e. combines) model parameters in a similar fashion
#' as `mice::pool()`. However, this function pools parameters from
#' `parameters_model` objects, as returned by
#' [model_parameters()].
#'
#' @param x A list of `parameters_model` objects, as returned by
#'   [model_parameters()], or a list of model-objects that is supported by
#'   `model_parameters()`.
#' @param ... Currently not used.
#' @inheritParams model_parameters.default
#' @inheritParams bootstrap_model
#' @inheritParams model_parameters.merMod
#'
#' @note Models with multiple components, (for instance, models with zero-inflation,
#'   where predictors appear in the count and zero-inflation part) may fail in
#'   case of identical names for coefficients in the different model components,
#'   since the coefficient table is grouped by coefficient names for pooling. In
#'   such cases, coefficients of count and zero-inflation model parts would be
#'   combined. Therefore, the `component` argument defaults to
#'   `"conditional"` to avoid this.
#'
#'   Some model objects do not return standard errors (e.g. objects of class
#'   `htest`). For these models, no pooled confidence intervals nor p-values
#'   are returned.
#'
#' @details Averaging of parameters follows Rubin's rules (_Rubin, 1987, p. 76_).
#'   The pooled degrees of freedom is based on the Barnard-Rubin adjustment for
#'   small samples (_Barnard and Rubin, 1999_).
#'
#' @references
#' Barnard, J. and Rubin, D.B. (1999). Small sample degrees of freedom with
#' multiple imputation. Biometrika, 86, 948-955. Rubin, D.B. (1987). Multiple
#' Imputation for Nonresponse in Surveys. New York: John Wiley and Sons.
#'
#' @examples
#' # example for multiple imputed datasets
#' if (require("mice")) {
#'   data("nhanes2")
#'   imp <- mice(nhanes2, printFlag = FALSE)
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
#' @export
pool_parameters <- function(x,
                            exponentiate = FALSE,
                            effects = "fixed",
                            component = "conditional",
                            verbose = TRUE,
                            ...) {
  # check input, save original model -----

  original_model <- random_params <- NULL
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  if (all(sapply(x, insight::is_model)) && all(sapply(x, insight::is_model_supported))) {
    original_model <- x[[1]]

    # Add exceptions for models with uncommon components here ---------------
    exception_model_class <- "polr"

    # exceptions for "component" argument. Eg, MASS::polr has components
    # "alpha" and "beta", and "component" needs to be set to all by default
    if (identical(component, "conditional") && inherits(original_model, exception_model_class)) {
      component <- "all"
    }

    x <- lapply(x, model_parameters, effects = effects, component = component, ...)
  }

  if (!all(sapply(x, inherits, "parameters_model"))) {
    insight::format_error(
      "First argument `x` must be a list of `parameters_model` objects, as returned by the `model_parameters()` function."
    )
  }

  if (is.null(original_model)) {
    original_model <- .get_object(x[[1]])
  }

  if (isTRUE(attributes(x[[1]])$exponentiate) && verbose) {
    insight::format_warning(
      "Pooling on exponentiated parameters is not recommended. Please call `model_parameters()` with 'exponentiate = FALSE', and then call `pool_parameters(..., exponentiate = TRUE)`."
    )
  }


  # only pool for specific component -----

  original_x <- x
  if ("Component" %in% colnames(x[[1]]) && !datawizard::is_empty_object(component) && component != "all") {
    x <- lapply(x, function(i) {
      i <- i[i$Component == component, ]
      i$Component <- NULL
      i
    })
    if (verbose) {
      insight::format_warning(paste0("Pooling applied to the ", component, " model component."))
    }
  }


  # preparation ----

  params <- do.call(rbind, x)

  len <- length(x)
  ci <- attributes(original_x[[1]])$ci
  if (is.null(ci)) ci <- .95
  parameter_values <- x[[1]]$Parameter

  # exceptions ----

  # check for special models, like "htest", which have no "Parameter" columns
  if (!"Parameter" %in% colnames(params)) {
    # check for possible column names
    if (all(c("Parameter1", "Parameter2") %in% colnames(params))) {
      # create combined Parameter column
      params$Parameter <- paste0(params$Parameter1, " and ", params$Parameter2)
      # remove old columns
      params$Parameter1 <- NULL
      params$Parameter2 <- NULL
      # update values
      parameter_values <- paste0(x[[1]]$Parameter1, " and ", x[[1]]$Parameter2) #
    }
    # fix coefficient column
    colnames(params)[colnames(params) == "r"] <- "Coefficient"
    colnames(params)[colnames(params) == "rho"] <- "Coefficient"
    colnames(params)[colnames(params) == "tau"] <- "Coefficient"
    colnames(params)[colnames(params) == "Estimate"] <- "Coefficient"
    colnames(params)[colnames(params) == "Difference"] <- "Coefficient"
  }

  # split multiply (imputed) datasets by parameters,
  # but only for fixed effects. Filter random effects,
  # and save parameter names from fixed effects for later use...

  if (effects == "all" && "Effects" %in% colnames(params) && "random" %in% params$Effects) {
    random_params <- params[params$Effects == "random", ]
    params <- params[params$Effects != "random", ]
    parameter_values <- x[[1]]$Parameter[x[[1]]$Effects != "random"]
  }
  estimates <- split(params, factor(params$Parameter, levels = unique(parameter_values)))


  # pool estimates etc. -----

  pooled_params <- do.call(rbind, lapply(estimates, function(i) {
    # pooled estimate
    pooled_estimate <- mean(i$Coefficient)

    # special models that have no standard errors (like "htest" objects)
    if (is.null(i$SE) || all(is.na(i$SE))) {
      out <- data.frame(
        Coefficient = pooled_estimate,
        SE = NA,
        CI_low = NA,
        CI_high = NA,
        Statistic = NA,
        df_error = NA,
        p = NA,
        stringsAsFactors = FALSE
      )

      if (verbose) {
        insight::format_warning("Model objects had no standard errors. Cannot compute pooled confidence intervals and p-values.")
      }

      # regular models that have coefficients and standard errors
    } else {
      # pooled standard error
      ubar <- mean(i$SE^2)
      tmp <- ubar + (1 + 1 / len) * stats::var(i$Coefficient)
      pooled_se <- sqrt(tmp)

      # pooled degrees of freedom, Barnard-Rubin adjustment for small samples
      df_column <- colnames(i)[grepl("(\\bdf\\b|\\bdf_error\\b)", colnames(i))][1]
      if (length(df_column)) {
        pooled_df <- .barnad_rubin(m = nrow(i), b = stats::var(i$Coefficient), t = tmp, dfcom = unique(i[[df_column]]))
        # sanity check length
        if (length(pooled_df) > 1 && length(pooled_se) == 1) {
          pooled_df <- round(mean(pooled_df, na.rm = TRUE))
        }
      } else {
        pooled_df <- Inf
      }

      # pooled statistic
      pooled_statistic <- pooled_estimate / pooled_se

      # confidence intervals
      alpha <- (1 + ci) / 2
      fac <- suppressWarnings(stats::qt(alpha, df = pooled_df))

      out <- data.frame(
        Coefficient = pooled_estimate,
        SE = pooled_se,
        CI_low = pooled_estimate - pooled_se * fac,
        CI_high = pooled_estimate + pooled_se * fac,
        Statistic = pooled_statistic,
        df_error = pooled_df,
        p = 2 * stats::pt(abs(pooled_statistic), df = pooled_df, lower.tail = FALSE),
        stringsAsFactors = FALSE
      )
    }

    # add component, when pooling for all components
    if (identical(component, "all") && "Component" %in% colnames(i)) {
      out$Component <- i$Component[1]
    }
    out
  }))


  # pool random effect variances -----

  pooled_random <- NULL
  if (!is.null(random_params)) {
    estimates <- split(random_params, factor(random_params$Parameter, levels = unique(random_params$Parameter)))
    pooled_random <- do.call(rbind, lapply(estimates, function(i) {
      pooled_estimate <- mean(i$Coefficient, na.rm = TRUE)
      data.frame(
        Parameter = unique(i$Parameter),
        Coefficient = pooled_estimate,
        Effects = "random",
        stringsAsFactors = FALSE
      )
    }))
  }


  # reorder ------

  pooled_params$Parameter <- parameter_values
  columns <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Statistic", "df_error", "p", "Component")
  pooled_params <- pooled_params[intersect(columns, colnames(pooled_params))]


  # final attributes -----

  # exponentiate coefficients and SE/CI, if requested
  pooled_params <- .exponentiate_parameters(pooled_params, NULL, exponentiate)

  if (!is.null(pooled_random)) {
    pooled_params <- merge(pooled_params, pooled_random, all = TRUE, sort = FALSE)
  }

  # this needs to be done extra here, cannot call ".add_model_parameters_attributes()"
  pooled_params <- .add_pooled_params_attributes(
    pooled_params,
    model_params = original_x[[1]],
    model = original_model,
    ci,
    exponentiate,
    verbose = verbose
  )
  attr(pooled_params, "object_name") <- obj_name


  # pool sigma ----

  sig <- unlist(insight::compact_list(lapply(original_x, function(i) {
    attributes(i)$sigma
  })))

  if (!insight::is_empty_object(sig)) {
    attr(pooled_params, "sigma") <- mean(sig, na.rm = TRUE)
  }


  class(pooled_params) <- c("parameters_model", "see_parameters_model", class(pooled_params))
  pooled_params
}




# helper ------


.barnad_rubin <- function(m, b, t, dfcom = 999999) {
  # fix for z-statistic
  if (is.null(dfcom) || all(is.na(dfcom)) || all(is.infinite(dfcom))) {
    return(Inf)
  }
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  dfold * dfobs / (dfold + dfobs)
}



.add_pooled_params_attributes <- function(pooled_params, model_params, model, ci, exponentiate, verbose = TRUE) {
  info <- insight::model_info(model, verbose = FALSE)
  pretty_names <- attributes(model_params)$pretty_names
  if (length(pretty_names) < nrow(model_params)) {
    pretty_names <- c(pretty_names, model_params$Parameter[(length(pretty_names) + 1):nrow(model_params)])
  }
  attr(pooled_params, "ci") <- ci
  attr(pooled_params, "exponentiate") <- exponentiate
  attr(pooled_params, "pretty_names") <- pretty_names
  attr(pooled_params, "verbose") <- verbose
  attr(pooled_params, "ordinal_model") <- attributes(model_params)$ordinal_model
  attr(pooled_params, "model_class") <- attributes(model_params)$model_class
  attr(pooled_params, "bootstrap") <- attributes(model_params)$bootstrap
  attr(pooled_params, "iterations") <- attributes(model_params)$iterations
  attr(pooled_params, "ci_method") <- attributes(model_params)$ci_method
  attr(pooled_params, "digits") <- attributes(model_params)$digits
  attr(pooled_params, "ci_digits") <- attributes(model_params)$ci_digits
  attr(pooled_params, "p_digits") <- attributes(model_params)$p_digits
  # column name for coefficients
  coef_col <- .find_coefficient_type(info, exponentiate)
  attr(pooled_params, "coefficient_name") <- coef_col
  attr(pooled_params, "zi_coefficient_name") <- if (isTRUE(exponentiate)) {
    "Odds Ratio"
  } else {
    "Log-Odds"
  }
  # formula
  attr(pooled_params, "model_formula") <- insight::find_formula(model)
  pooled_params
}
