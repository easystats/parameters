# classes: .aov, .anova, aovlist, anova.rms, maov, afex_aov

# .aov ------

#' Parameters from ANOVAs
#'
#' @param model Object of class \code{\link{aov}}, \code{\link{anova}},
#'   \code{aovlist}, \code{Gam}, \code{\link{manova}}, \code{Anova.mlm},
#'   \code{afex_aov} or \code{maov}.
#' @param omega_squared Compute omega squared as index of effect size. Can be
#'   \code{"partial"} (the default, adjusted for effect size) or \code{"raw"}.
#' @param eta_squared Compute eta squared as index of effect size. Can be
#'   \code{"partial"} (the default, adjusted for effect size), \code{"raw"}  or
#'   \code{"adjusted"} (the latter option only for ANOVA-tables from mixed
#'   models).
#' @param epsilon_squared Compute epsilon squared as index of effect size. Can
#'   be \code{"partial"} (the default, adjusted for effect size) or
#'   \code{"raw"}.
#' @param df_error Denominator degrees of freedom (or degrees of freedom of the
#'   error estimate, i.e., the residuals). This is used to compute effect sizes
#'   for ANOVA-tables from mixed models. See 'Examples'. (Ignored for
#'   \code{afex_aov}.)
#' @param type Numeric, type of sums of squares. May be 1, 2 or 3. If 2 or 3,
#'   ANOVA-tables using \code{car::Anova()} will be returned. (Ignored for
#'   \code{afex_aov}.)
#' @param ci Confidence Interval (CI) level for effect sizes
#'   \code{omega_squared}, \code{eta_squared} etc. The default, \code{NULL},
#'   will compute no confidence intervals. \code{ci} should be a scalar between
#'   0 and 1.
#' @param test String, indicating the type of test for \code{Anova.mlm} to be
#'   returned. If \code{"multivariate"} (or \code{NULL}), returns the summary of
#'   the multivariate test (that is also given by the \code{print}-method). If
#'   \code{test = "univariate"}, returns the summary of the univariate test.
#' @param power Logical, if \code{TRUE}, adds a column with power for each
#'   parameter.
#' @param table_wide Logical that decides whether the ANOVA table should be in
#'   wide format, i.e. should the numerator and denominator degrees of freedom
#'   be in the same row. Default: \code{FALSE}.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @note For ANOVA-tables from mixed models (i.e. \code{anova(lmer())}), only
#'   partial or adjusted effect sizes can be computed. Note that type 3 ANOVAs
#'   with interactions involved only give sensible and informative results when
#'   covariates are mean-centred and factors are coded with orthogonal contrasts
#'   (such as those produced by \code{contr.sum}, \code{contr.poly}, or
#'   \code{contr.helmert}, but \emph{not} by the default \code{contr.treatment}).
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
#'   \dontrun{
#'     if (require("lme4")) {
#'       mm <- lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species),
#'         data = df
#'       )
#'       model <- anova(mm)
#'
#'       # simple parameters table
#'       model_parameters(model)
#'
#'       # parameters table including effect sizes
#'       model_parameters(
#'         model,
#'         eta_squared = "partial",
#'         ci = .9,
#'         df_error = dof_satterthwaite(mm)[2:3]
#'       )
#'     }
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
                                 parameters = NULL,
                                 table_wide = FALSE,
                                 verbose = TRUE,
                                 ...) {
  if (inherits(model, "aov") && !is.null(type) && type > 1) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning(insight::format_message("Package 'car' required for type-2 or type-3 anova. Defaulting to type-1."), call. = FALSE)
    } else {
      model <- car::Anova(model, type = type)
    }
  }

  # try to extract type of anova table
  if (is.null(type)) {
    type <- .anova_type(model)
  }

  # exceptions
  if (.is_levenetest(model)) {
    return(model_parameters.htest(model, ...))
  }

  # check contrasts
  if (verbose) {
    .check_anova_contrasts(model, type)
  }

  # extract standard parameters
  params <- .extract_parameters_anova(model, test)

  # add effect sizes, if available
  params <- .effectsizes_for_aov(
      model,
      parameters = params,
      omega_squared = omega_squared,
      eta_squared = eta_squared,
      epsilon_squared = epsilon_squared,
      df_error = df_error,
      ci = ci,
      verbose = verbose
    )

  # add power, if possible
  if (isTRUE(power)) {
    params <- .power_for_aov(model, params)
  }

  # filter parameters
  if (!is.null(parameters)) {
    params <- .filter_parameters(params, parameters, verbose = verbose)
  }

  # wide or long?
  if (table_wide) {
    params <- .anova_table_wide(params)
  }

  # add attributes
  params <- .add_anova_attributes(params, model, ci, test = test, ...)

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
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



# .anova ------

#' @export
standard_error.anova <- standard_error.aov

#' @export
p_value.anova <- p_value.aov

#' @export
model_parameters.anova <- model_parameters.aov



# .aov.list  ------

#' @export
standard_error.aovlist <- standard_error.aov

#' @export
p_value.aovlist <- p_value.aov

#' @export
model_parameters.aovlist <- model_parameters.aov



# .afex_aov  ------

#' @export
model_parameters.afex_aov <- function(model,
                                      omega_squared = NULL,
                                      eta_squared = NULL,
                                      epsilon_squared = NULL,
                                      df_error = NULL,
                                      type = NULL,
                                      parameters = NULL,
                                      verbose = TRUE,
                                      ...) {
  if (inherits(model$Anova, "Anova.mlm")) {
    params <- model$anova_table
    with_df_and_p <- summary(model$Anova)$univariate.tests
    params$`Sum Sq` <- with_df_and_p[-1, 1]
    params$`Error SS` <- with_df_and_p[-1, 3]
    out <- .extract_parameters_anova(params, test = NULL)
  } else {
    out <- .extract_parameters_anova(model$Anova, test = NULL)
  }

  out <- .effectsizes_for_aov(
    model,
    parameters = out,
    omega_squared = omega_squared,
    eta_squared = eta_squared,
    epsilon_squared = epsilon_squared,
    df_error = df_error,
    verbose = verbose,
    ...
  )

  # add attributes
  out <- .add_anova_attributes(out, model, ci, test = NULL, ...)

  # filter parameters
  if (!is.null(parameters)) {
    out <- .filter_parameters(out, parameters, verbose = verbose)
  }

  if (!"Method" %in% names(out)) {
    out$Method <- "ANOVA estimation for factorial designs using 'afex'"
  }

  attr(out, "title") <- unique(out$Method)
  class(out) <- unique(c("parameters_model", "see_parameters_model", class(out)))

  out
}



# others  ------

#' @export
model_parameters.anova.rms <- model_parameters.aov

#' @export
model_parameters.Anova.mlm <- model_parameters.aov

#' @export
model_parameters.maov <- model_parameters.aov


# helper ------------------------------

.anova_type <- function(model, type = NULL) {
  if (is.null(type)) {

    type_to_numeric <- function(type) {
      if (is.numeric(type)) {
        return(type)
      }
      switch(
        type,
        "1" = ,
        "I" = 1,
        "2" = ,
        "II" = 2,
        "3" = ,
        "III" = 3,
        1
      )
    }

    # default to 1
    type <- 1

    if (!is.null(attr(model, "type", exact = TRUE))) {
      type <- type_to_numeric(attr(model, "type", exact = TRUE))
    } else if (!is.null(attr(model, "heading"))) {
      heading <- attr(model, "heading")[1]
      if (grepl("(.*)Type (.*) Wald(.*)", heading)) {
        type <- type_to_numeric(trimws(gsub("(.*)Type (.*) Wald(.*)", "\\2", heading)))
      } else if (grepl("Type (.*) Analysis(.*)", heading)) {
        type <- type_to_numeric(trimws(gsub("Type (.*) Analysis(.*)", "\\1", heading)))
      } else if (grepl("(.*)Type (.*) tests(.*)", heading)) {
        type <- type_to_numeric(trimws(gsub("(.*)Type (.*) tests(.*)", "\\2", heading)))
      }
    } else if ("type" %in% names(model) && !is.null(model$type)) {
      type <- type_to_numeric(model$type)
    }
  }

  type
}


.check_anova_contrasts <- function(model, type) {
  # check only valid for anova tables of type III
  if (!is.null(type) && type == 3) {

    # check for interaction terms
    interaction_terms <- tryCatch(
      {
        insight::find_interactions(model, flatten = TRUE)
      },
      error = function(e) {
        if (is.data.frame(model)) {
          if (any(grepl(":", row.names(model), fixed = TRUE))) {
            TRUE
          } else {
            NULL
          }
        }
      }
    )

    # try to access data of model predictors
    predictors <- tryCatch(
      {
        insight::get_predictors(model)
      },
      error = function(e) {
        NULL
      }
    )

    # if data available, check contrasts and mean centering
    if (!is.null(predictors)) {
      treatment_contrasts_or_not_centered <- sapply(predictors, function(i) {
        if (is.factor(i)) {
          cn <- stats::contrasts(i)
          if (is.null(cn) || (all(cn %in% c(0, 1)))) {
            return(TRUE)
          }
        } else {
          if (abs(mean(i, na.rm = TRUE)) > 1e-2) {
            return(TRUE)
          }
        }
        return(FALSE)
      })
    } else {
      treatment_contrasts_or_not_centered <- FALSE
    }

    # successfully checked predictors, or if not possible, at least found interactions?
    if (!is.null(interaction_terms) && (any(treatment_contrasts_or_not_centered) || is.null(predictors))) {
      message(insight::format_message("Type 3 ANOVAs only give sensible and informative results when covariates are mean-centered and factors are coded with orthogonal contrasts (such as those produced by 'contr.sum', 'contr.poly', or 'contr.helmert', but *not* by the default 'contr.treatment')."))
    }
  }
}


.effectsizes_for_aov <- function(model,
                                 parameters,
                                 omega_squared,
                                 eta_squared,
                                 epsilon_squared,
                                 df_error = NULL,
                                 ci = NULL,
                                 verbose = TRUE) {
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


.is_levenetest <- function(x) {
  inherits(x, "anova") && !is.null(attributes(x)$heading) && all(isTRUE(grepl("Levene's Test", attributes(x)$heading, fixed = TRUE)))
}

# TODO: decide whether to move to `datawizard`?
# data: A dataframe from `model_parameters`
# ... Currently ignored

.anova_table_wide <- function(data, ...) {
  wide_anova <- function(x) {
    # creating numerator and denominator degrees of freedom
    if (length(idxResid <- x$Parameter == "Residuals")) {
      x$df_error <- x$df[idxResid]
      x$Sum_Squares_Error <- x$Sum_Squares[idxResid]
      x$Mean_Square_Error <- x$Sum_Squares[idxResid]
      x <- x[!idxResid, ]
    }
    x
  }

  if ("Group" %in% colnames(data)) {
    data <- split(data, data$Group)
    data <- lapply(data, wide_anova)
    data <- do.call(rbind, data)
  } else {
    data <- wide_anova(data)
  }

  # reorder columns
  col_order <- union(c('Parameter', 'F', 'df', 'df_error', 'p', 'Sum_Squares', 'Sum_Squares_Error', 'Mean_Square', 'Mean_Square_Error'), names(data))

  data[, col_order]
}
