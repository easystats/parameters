# classes: .aov, .anova, aovlist, anova.rms, maov, afex_aov


#################### .aov ------


#' Parameters from ANOVAs
#'
#' @param model Object of class \code{\link{aov}}, \code{\link{anova}}, \code{aovlist}, \code{Gam}, \code{\link{manova}}, \code{Anova.mlm}, \code{afex_aov} or \code{maov}.
#' @param omega_squared Compute omega squared as index of effect size. Can be \code{"partial"} (the default, adjusted for effect size) or \code{"raw"}.
#' @param eta_squared Compute eta squared as index of effect size. Can be \code{"partial"} (the default, adjusted for effect size), \code{"raw"}  or \code{"adjusted"} (the latter option only for ANOVA-tables from mixed models).
#' @param epsilon_squared Compute epsilon squared as index of effect size. Can be \code{"partial"} (the default, adjusted for effect size) or \code{"raw"}.
#' @param df_error Denominator degrees of freedom (or degrees of freedom of the error estimate, i.e., the residuals). This is used to compute effect sizes for ANOVA-tables from mixed models. See 'Examples'. (Ignored for \code{afex_aov}.)
#' @param type Numeric, type of sums of squares. May be 1, 2 or 3. If 2 or 3, ANOVA-tables using \code{car::Anova()} will be returned. (Ignored for \code{afex_aov}.)
#' @param ci Confidence Interval (CI) level for effect sizes \code{omega_squared}, \code{eta_squared} etc. The default, \code{NULL}, will compute no confidence intervals. \code{ci} should be a scalar between 0 and 1.
#' @param test String, indicating the type of test for \code{Anova.mlm} to be returned. If \code{"multivariate"} (or \code{NULL}), returns the summary of the multivariate test (that is also given by the \code{print}-method). If \code{test = "univariate"}, returns the summary of the univariate test.
#' @param power Logical, if \code{TRUE}, adds a column with power for each parameter.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @note For ANOVA-tables from mixed models (i.e. \code{anova(lmer())}), only partial or adjusted effect sizes can be computed.
#'
#' @examples
#' if (requireNamespace("effectsize", quietly = TRUE)) {
#'   df <- iris
#'   df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#'   model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#'   model_parameters(
#'     model,
#'     omega_squared = "partial",
#'     eta_squared = "partial",
#'     epsilon_squared = "partial"
#'   )
#'
#'   model_parameters(
#'     model,
#'     omega_squared = "partial",
#'     eta_squared = "partial",
#'     ci = .9
#'   )
#'
#'   model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
#'   model_parameters(model)
#'   model_parameters(
#'     model,
#'     omega_squared = "partial",
#'     eta_squared = "partial",
#'     epsilon_squared = "partial"
#'   )
#'
#'   model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#'   model_parameters(model)
#'
#'   if (require("lme4")) {
#'     mm <- lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species),
#'       data = df
#'     )
#'     model <- anova(mm)
#'
#'     # simple parameters table
#'     model_parameters(model)
#'
#'     # parameters table including effect sizes
#'     model_parameters(
#'       model,
#'       eta_squared = "partial",
#'       ci = .9,
#'       df_error = dof_satterthwaite(mm)[2:3]
#'     )
#'   }
#' }
#' @export
model_parameters.aov <- function(model,
                                 omega_squared = NULL,
                                 eta_squared = NULL,
                                 epsilon_squared = NULL,
                                 df_error = NULL,
                                 type = NULL,
                                 ci = NULL,
                                 test = NULL,
                                 power = FALSE,
                                 verbose = TRUE,
                                 ...) {
  if (inherits(model, "aov") && !is.null(type) && type > 1) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning("Package 'car' required for type-2 or type-3 anova. Defaulting to type-1.", call. = FALSE)
    } else {
      model <- car::Anova(model, type = type)
    }
  }

  # exceptions
  if (insight::model_info(model)$is_levenetest) {
    return(model_parameters.htest(model, ...))
  }

  # extract standard parameters
  parameters <- .extract_parameters_anova(model, test)

  # add effect sizes, if available
  parameters <- .effectsizes_for_aov(model, parameters, omega_squared, eta_squared, epsilon_squared, df_error, ci, verbose = verbose)

  # add power, if possible
  if (isTRUE(power)) {
    parameters <- .power_for_aov(model, parameters)
  }

  # add attributes
  parameters <- .add_anova_attributes(parameters, model, ci, test = test, ...)

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
standard_error.aov <- function(model, ...) {
  params <- model_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = params$SE
  )
}


#' @export
p_value.aov <- function(model, ...) {
  params <- model_parameters(model)

  if (nrow(params) == 0) {
    return(NA)
  }

  if ("Group" %in% names(params)) {
    params <- params[params$Group == "Within", ]
  }

  if ("Residuals" %in% params$Parameter) {
    params <- params[params$Parameter != "Residuals", ]
  }

  if (!"p" %in% names(params)) {
    return(NA)
  }

  .data_frame(
    Parameter = params$Parameter,
    p = params$p
  )
}




#################### .anova ------


#' @export
standard_error.anova <- standard_error.aov


#' @export
p_value.anova <- p_value.aov


#' @export
model_parameters.anova <- model_parameters.aov





#################### .aov.list  ------


#' @export
standard_error.aovlist <- standard_error.aov


#' @export
p_value.aovlist <- p_value.aov


#' @export
model_parameters.aovlist <- model_parameters.aov




#################### others  ------


#' @export
model_parameters.anova.rms <- model_parameters.aov


#' @export
model_parameters.Anova.mlm <- model_parameters.aov


#' @export
model_parameters.maov <- model_parameters.aov


#' @export
model_parameters.afex_aov <- function(model,
                                      omega_squared = NULL,
                                      eta_squared = NULL,
                                      epsilon_squared = NULL,
                                      df_error = NULL,
                                      type = NULL,
                                      verbose = TRUE,
                                      ...) {
  if (inherits(model$Anova, "Anova.mlm")) {
    params <- model$anova_table
    with_df_and_p <- summary(model$Anova)$univariate.tests
    params$`Sum Sq` <- with_df_and_p[-1, 1]
    params$`Error SS` <- with_df_and_p[-1, 3]
    out <- model_parameters(params, df_error = NULL, type = NULL, ...)
  } else {
    out <- model_parameters(model$Anova, df_error = NULL, type = attr(model, "type"), ...)
  }

  out <- .effectsizes_for_aov(
    model,
    out,
    omega_squared = omega_squared,
    eta_squared = eta_squared,
    epsilon_squared = epsilon_squared,
    df_error = df_error,
    verbose = verbose,
    ...
  )

  if (!"Method" %in% names(out)) {
    out$Method <- "ANOVA estimation for factorial designs using 'afex'"
  }

  attr(out, "title") <- unique(out$Method)

  out
}




# helper ------------------------------


.effectsizes_for_aov <- function(model, parameters, omega_squared, eta_squared, epsilon_squared, df_error = NULL, ci = NULL, verbose = TRUE) {
  # user actually does not want to compute effect sizes
  if (is.null(omega_squared) && is.null(eta_squared) && is.null(epsilon_squared)) {
    return(parameters)
  }

  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' required for this function to work. Please install it.")
  }

  # set error-df, when provided.
  if (!is.null(df_error) && is.data.frame(model) && !any(c("DenDF", "den Df", "denDF", "df_error") %in% colnames(model))) {
    if (length(df_error) > nrow(model)) {
      stop("Number of degrees of freedom in argument 'df_error' is larger than number of parameters.")
    }
    model$df_error <- df_error
  }


  # set defaults
  if (isTRUE(omega_squared)) {
    omega_squared <- "partial"
  }
  if (isTRUE(eta_squared)) {
    eta_squared <- "partial"
  }
  if (isTRUE(epsilon_squared)) {
    epsilon_squared <- "partial"
  }


  # Omega squared
  if (!is.null(omega_squared)) {
    if (omega_squared == "partial") {
      fx <- effectsize::omega_squared(model, partial = TRUE, ci = ci, verbose = verbose)
    } else {
      fx <- effectsize::omega_squared(model, partial = FALSE, ci = ci, verbose = verbose)
    }
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  # Eta squared
  if (!is.null(eta_squared)) {
    if (eta_squared == "partial") {
      fx <- effectsize::eta_squared(model, partial = TRUE, ci = ci, verbose = verbose)
    } else {
      fx <- effectsize::eta_squared(model, partial = FALSE, ci = ci, verbose = verbose)
    }
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  # Epsilon squared
  if (!is.null(epsilon_squared)) {
    if (epsilon_squared == "partial") {
      fx <- effectsize::epsilon_squared(model, partial = TRUE, ci = ci, verbose = verbose)
    } else {
      fx <- effectsize::epsilon_squared(model, partial = FALSE, ci = ci, verbose = verbose)
    }
    parameters <- .add_effectsize_to_parameters(fx, parameters)
  }

  parameters
}




# internals --------------------------


.fix_effectsize_rows <- function(fx, parameters) {
  stat_column <- colnames(parameters)[colnames(parameters) %in% c("F", "t", "z", "statistic")]
  if (nrow(parameters) > length(fx)) {
    es <- rep_len(NA, length.out = nrow(parameters))
    es[!is.na(parameters[[stat_column]])] <- fx
    fx <- es
  }
  fx
}


# retrieves those rows in a "model_parameters" object where
# the statistic column is not missing
.valid_effectsize_rows <- function(parameters, fx_params) {
  stat_column <- colnames(parameters)[colnames(parameters) %in% c("F", "t", "z", "statistic")]
  out <- !is.na(parameters[[stat_column]])
  if (sum(out) > length(fx_params)) {
    out <- out & !is.na(match(parameters$Parameter, fx_params))
  }
  out
}


# add effect size column and related CI to the parameters
# data frame, automatically detecting the effect size name
.add_effectsize_to_parameters <- function(fx, params) {
  fx_params <- fx$Parameter
  if (is.null(fx_params)) {
    fx_params <- params$Parameter
  }
  fx$Parameter <- NULL
  fx$Response <- NULL
  fx$Group <- NULL
  es <- colnames(fx)[1]
  valid_rows <- .valid_effectsize_rows(params, fx_params)
  params[[es]][valid_rows] <- fx[[es]]

  if (!is.null(fx$CI_low)) {
    ci_low <- paste0(gsub("_partial$", "", es), "_CI_low")
    ci_high <- paste0(gsub("_partial$", "", es), "_CI_high")
    params[[ci_low]][valid_rows] <- fx$CI_low
    params[[ci_high]][valid_rows] <- fx$CI_high
  }

  params
}
