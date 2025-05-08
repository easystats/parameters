# classes: .aov, .anova, aovlist, anova.rms, maov, afex_aov

# .aov ------

#' Parameters from ANOVAs
#'
#' @param model Object of class [aov()], [anova()],
#'   `aovlist`, `Gam`, [manova()], `Anova.mlm`,
#'   `afex_aov` or `maov`.
#' @param es_type The effect size of interest. Not that possibly not all
#'   effect sizes are applicable to the model object. See 'Details'. For Anova
#'   models, can also be a character vector with multiple effect size names.
#' @param df_error Denominator degrees of freedom (or degrees of freedom of the
#'   error estimate, i.e., the residuals). This is used to compute effect sizes
#'   for ANOVA-tables from mixed models. See 'Examples'. (Ignored for
#'   `afex_aov`.)
#' @param type Numeric, type of sums of squares. May be 1, 2 or 3. If 2 or 3,
#'   ANOVA-tables using `car::Anova()` will be returned. (Ignored for
#'   `afex_aov`.)
#' @param ci Confidence Interval (CI) level for effect sizes specified in
#'   `es_type`. The default, `NULL`, will compute no confidence
#'   intervals. `ci` should be a scalar between 0 and 1.
#' @param test String, indicating the type of test for `Anova.mlm` to be
#'   returned. If `"multivariate"` (or `NULL`), returns the summary of
#'   the multivariate test (that is also given by the `print`-method). If
#'   `test = "univariate"`, returns the summary of the univariate test.
#' @param power Logical, if `TRUE`, adds a column with power for each
#'   parameter.
#' @param table_wide Logical that decides whether the ANOVA table should be in
#'   wide format, i.e. should the numerator and denominator degrees of freedom
#'   be in the same row. Default: `FALSE`.
#' @param alternative A character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"two.sided"` (default, two-sided CI),
#'   `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
#'   (e.g., `"g"`, `"l"`, `"two"`...). See section *One-Sided CIs* in
#'   the [effectsize_CIs vignette](https://easystats.github.io/effectsize/).
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to [`effectsize::effectsize()`]. For example,
#'   to calculate _partial_ effect sizes types, use `partial = TRUE`. For objects
#'   of class `htest` or `BFBayesFactor`, `adjust = TRUE` can be used to return
#'   bias-corrected effect sizes, which is advisable for small samples and large
#'   tables. See also
#'   [`?effectsize::eta_squared`](https://easystats.github.io/effectsize/reference/eta_squared.html)
#'   for arguments `partial` and `generalized`;
#'   [`?effectsize::phi`](https://easystats.github.io/effectsize/reference/phi.html)
#'   for `adjust`; and
#'   [`?effectsize::oddratio`](https://easystats.github.io/effectsize/reference/oddsratio.html)
#'   for `log`.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @inherit effectsize::effectsize details
#'
#' @note For ANOVA-tables from mixed models (i.e. `anova(lmer())`), only
#'   partial or adjusted effect sizes can be computed. Note that type 3 ANOVAs
#'   with interactions involved only give sensible and informative results when
#'   covariates are mean-centred and factors are coded with orthogonal contrasts
#'   (such as those produced by `contr.sum`, `contr.poly`, or
#'   `contr.helmert`, but *not* by the default `contr.treatment`).
#'
#' @examplesIf requireNamespace("effectsize", quietly = TRUE)
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' model_parameters(model)
#'
#' model_parameters(model, es_type = c("omega", "eta"), ci = 0.9)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
#' model_parameters(model)
#' model_parameters(
#'   model,
#'   es_type = c("omega", "eta", "epsilon"),
#'   alternative = "greater"
#' )
#'
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' model_parameters(model)
#'
#' @examplesIf requireNamespace("lme4", quietly = TRUE) && requireNamespace("effectsize", quietly = TRUE)
#' \donttest{
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#' mm <- lme4::lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species), data = df)
#' model <- anova(mm)
#'
#' # simple parameters table
#' model_parameters(model)
#'
#' # parameters table including effect sizes
#' model_parameters(
#'   model,
#'   es_type = "eta",
#'   ci = 0.9,
#'   df_error = dof_satterthwaite(mm)[2:3]
#' )
#' }
#' @export
model_parameters.aov <- function(model,
                                 type = NULL,
                                 df_error = NULL,
                                 ci = NULL,
                                 alternative = NULL,
                                 p_adjust = NULL,
                                 test = NULL,
                                 power = FALSE,
                                 es_type = NULL,
                                 keep = NULL,
                                 drop = NULL,
                                 table_wide = FALSE,
                                 verbose = TRUE,
                                 ...) {
  # save model object, for later checks
  original_model <- model
  object_name <- insight::safe_deparse_symbol(substitute(model))

  if (inherits(model, "aov") && !is.null(type) && type > 1) {
    if (requireNamespace("car", quietly = TRUE)) {
      model <- car::Anova(model, type = type)
    } else {
      insight::format_warning("Package {.pkg car} required for type-2 or type-3 Anova. Defaulting to type-1.")
    }
  }

  # try to extract type of anova table
  if (is.null(type)) {
    type <- .anova_type(model, verbose = verbose)
  }

  # exceptions
  if (.is_levenetest(model)) {
    return(model_parameters.htest(model, ...))
  }

  # check contrasts
  if (verbose) {
    .check_anova_contrasts(original_model, type)
  }

  # extract standard parameters
  params <- .extract_parameters_anova(model, test, p_adjust = p_adjust, verbose = verbose)

  # add effect sizes, if available
  params <- .effectsizes_for_aov(
    model,
    params = params,
    es_type = es_type,
    df_error = df_error,
    ci = ci,
    alternative = alternative,
    verbose = FALSE, # we get messages for contrasts before
    ...
  )

  # add power, if possible
  if (isTRUE(power)) {
    params <- .power_for_aov(model, params)
  }

  # filter parameters
  if (!is.null(keep) || !is.null(drop)) {
    params <- .filter_parameters(params,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  # wide or long?
  if (table_wide) {
    params <- .anova_table_wide(params)
  }

  # add attributes
  params <- .add_anova_attributes(params, model, ci, test = test, alternative = alternative, ...)

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  attr(params, "object_name") <- object_name
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
                                      es_type = NULL,
                                      df_error = NULL,
                                      type = NULL,
                                      keep = NULL,
                                      drop = NULL,
                                      p_adjust = NULL,
                                      verbose = TRUE,
                                      ...) {
  if (inherits(model$Anova, "Anova.mlm")) {
    params <- model$anova_table
    with_df_and_p <- summary(model$Anova)$univariate.tests
    params$`Sum Sq` <- with_df_and_p[-1, 1]
    params$`Error SS` <- with_df_and_p[-1, 3]
    out <- .extract_parameters_anova(params, test = NULL, p_adjust = NULL, verbose)
    p_adjust <- .extract_p_adjust_afex(model, p_adjust)
  } else {
    p_adjust <- .extract_p_adjust_afex(model, p_adjust)
    out <- .extract_parameters_anova(model$Anova, test = NULL, p_adjust, verbose)
  }

  out <- .effectsizes_for_aov(
    model,
    params = out,
    es_type = es_type,
    df_error = df_error,
    verbose = verbose,
    ...
  )

  # add attributes
  out <- .add_anova_attributes(
    out,
    model,
    ci,
    test = NULL,
    alternative = NULL,
    p_adjust = p_adjust,
    ...
  )

  # filter parameters
  if (!is.null(keep) || !is.null(drop)) {
    out <- .filter_parameters(out,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  if (!"Method" %in% names(out)) {
    out$Method <- "ANOVA estimation for factorial designs using 'afex'"
  }

  attr(out, "title") <- unique(out$Method)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
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

#' @export
model_parameters.seqanova.svyglm <- model_parameters.aov


# helper ------------------------------

.anova_type <- function(model, type = NULL, verbose = TRUE) {
  if (is.null(type)) {
    type_to_numeric <- function(type) {
      if (is.numeric(type)) {
        return(type)
      }
      # nolint start
      switch(type,
        `1` = ,
        `I` = 1,
        `2` = ,
        `II` = 2,
        `3` = ,
        `III` = 3,
        1
      )
      # nolint end
    }

    # default to 1
    type <- 1

    if (inherits(model, "anova.rms")) {
      type <- 2
    } else if (!is.null(attr(model, "type", exact = TRUE))) {
      type <- type_to_numeric(attr(model, "type", exact = TRUE))
    } else if (!is.null(attr(model, "heading"))) {
      heading <- attr(model, "heading")[1]
      if (grepl("(.*)Type (.*) Wald(.*)", heading)) {
        type <- type_to_numeric(insight::trim_ws(gsub("(.*)Type (.*) Wald(.*)", "\\2", heading)))
      } else if (grepl("Type (.*) Analysis(.*)", heading)) {
        type <- type_to_numeric(insight::trim_ws(gsub("Type (.*) Analysis(.*)", "\\1", heading)))
      } else if (grepl("(.*)Type (.*) tests(.*)", heading)) {
        type <- type_to_numeric(insight::trim_ws(gsub("(.*)Type (.*) tests(.*)", "\\2", heading)))
      }
    } else if ("type" %in% names(model) && !is.null(model$type)) {
      type <- type_to_numeric(model$type)
    }
  }

  type
}


.anova_alternative <- function(params, alternative) {
  alternative_footer <- NULL
  if (!is.null(alternative)) {
    alternative <- insight::validate_argument(
      tolower(alternative),
      c("two.sided", "greater", "less")
    )
    if (alternative != "two.sided") {
      ci_low <- which(endsWith(colnames(params), "CI_low"))
      ci_high <- which(endsWith(colnames(params), "CI_high"))
      if (length(ci_low) && length(ci_high)) {
        bound <- if (alternative == "less") params[[ci_low[1]]][1] else params[[ci_high[1]]][1]
        bound <- insight::format_value(bound, digits = 2)
        side <- if (alternative == "less") "lower" else "upper"
        alternative_footer <- sprintf(
          "One-sided CIs: %s bound fixed at [%s].",
          side, bound
        )
      }
    }
  }
  alternative_footer
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
    predictors <- .safe(insight::get_predictors(model))

    # if data available, check contrasts and mean centering
    if (is.null(predictors)) {
      treatment_contrasts_or_not_centered <- FALSE
    } else {
      treatment_contrasts_or_not_centered <- vapply(predictors, function(i) {
        if (is.factor(i)) {
          cn <- stats::contrasts(i)
          if (is.null(cn) || (all(cn %in% c(0, 1)))) {
            return(TRUE)
          }
        } else if (abs(mean(i, na.rm = TRUE)) > 1e-2) {
          return(TRUE)
        }
        FALSE
      }, TRUE)
    }

    # successfully checked predictors, or if not possible, at least found interactions?
    if (!is.null(interaction_terms) && (any(treatment_contrasts_or_not_centered) || is.null(predictors))) {
      insight::format_alert(
        "Type 3 ANOVAs only give sensible and informative results when covariates are mean-centered and factors are coded with orthogonal contrasts (such as those produced by `contr.sum`, `contr.poly`, or `contr.helmert`, but *not* by the default `contr.treatment`)." # nolint
      )
    }
  }
}


.effectsizes_for_aov <- function(model,
                                 params,
                                 es_type = NULL,
                                 df_error = NULL,
                                 ci = NULL,
                                 alternative = NULL,
                                 verbose = TRUE,
                                 ...) {
  # user actually does not want to compute effect sizes
  if (is.null(es_type)) {
    return(params)
  }

  # is valid effect size?
  if (!all(es_type %in% c("eta", "omega", "epsilon", "f", "f2"))) {
    return(params)
  }

  insight::check_if_installed("effectsize")

  # set error-df, when provided.
  if (!is.null(df_error) && is.data.frame(model) &&
    !any(c("DenDF", "den Df", "denDF", "df_error") %in% colnames(model))) {
    if (length(df_error) > nrow(model)) {
      insight::format_error(
        "Number of degrees of freedom in argument `df_error` is larger than number of parameters."
      )
    }
    model$df_error <- df_error
  }

  # multiple effect sizes possible
  for (es in es_type) {
    fx <- effectsize::effectsize(
      model,
      type = es,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    params <- .add_effectsize_to_parameters(fx, params)
    # warn only once
    verbose <- FALSE
  }

  params
}


# internals --------------------------

# add effect size column and related CI to the parameters
# data frame, automatically detecting the effect size name
.add_effectsize_to_parameters <- function(fx, params) {
  if (!is.null(fx$CI_low)) {
    # find name of current effect size
    es <- effectsize::get_effectsize_name(colnames(fx))

    # and add CI-name to effect size, to have specific
    # CI columns for this particular effect size
    ci_low <- paste0(gsub("_partial$", "", es), "_CI_low")
    ci_high <- paste0(gsub("_partial$", "", es), "_CI_high")

    # rename columns
    fx[[ci_low]] <- fx$CI_low
    fx[[ci_high]] <- fx$CI_high

    # delete old or duplicated columns
    fx$CI_low <- NULL
    fx$CI_high <- NULL
    fx$CI <- NULL
  }

  params$.id <- seq_len(nrow(params))
  params <- merge(
    params,
    fx,
    all.x = TRUE,
    sort = FALSE,
    by = intersect(c("Response", "Group", "Parameter"), intersect(colnames(params), colnames(fx)))
  )
  params <- params[order(params$.id), ]
  params$.id <- NULL
  params
}


.is_levenetest <- function(x) {
  inherits(x, "anova") &&
    !is.null(attributes(x)$heading) &&
    all(isTRUE(grepl("Levene's Test", attributes(x)$heading, fixed = TRUE)))
}


# data: A dataframe from `model_parameters`
# ... Currently ignored

.anova_table_wide <- function(data, ...) {
  wide_anova <- function(x) {
    # creating numerator and denominator degrees of freedom
    idxResid <- which(x$Parameter == "Residuals")
    if (length(idxResid)) {
      x$df_error <- x$df[idxResid]
      x$Sum_Squares_Error <- x$Sum_Squares[idxResid]
      x$Mean_Square_Error <- x$Mean_Square[idxResid]
      x <- x[-idxResid, ]
    }
    x
  }

  if ("Group" %in% colnames(data)) {
    data <- split(data, data$Group)
    data <- lapply(data, wide_anova)
    data <- Filter(function(x) nrow(x) >= 1L, data)
    cols <- unique(unlist(lapply(data, colnames)))
    data <- lapply(data, function(x) {
      x[, setdiff(cols, colnames(x))] <- NA
      x
    })
    data <- do.call(rbind, data)
  } else {
    data <- wide_anova(data)
  }

  # reorder columns
  col_order <- union(c("Parameter", "F", "df", "df_error", "p"), names(data))

  data[, col_order]
}


#' @keywords internal
.extract_p_adjust_afex <- function(model, p_adjust) {
  if (is.null(p_adjust) && inherits(model, "afex_aov")) {
    p_adjust <- attr(model$anova_table, "p_adjust_method")

    if (p_adjust == "none") {
      p_adjust <- NULL
    }
  }

  p_adjust
}
