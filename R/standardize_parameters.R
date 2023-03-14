#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be
#'   `"refit"` (default), `"posthoc"`, `"smart"`, `"basic"`, `"pseudo"` or
#'   `"sdy"`. See Details'.
#' @param include_response If `TRUE` (default), the response value will also be
#'   standardized. If `FALSE`, only the predictors will be standardized. For
#'   GLMs the response value will never be standardized (see *Generalized Linear
#'   Models* section).
#' @inheritParams datawizard::standardize.default
#' @inheritParams effectsize::chisq_to_phi
#' @param ... For `standardize_parameters()`, arguments passed to
#'   [model_parameters()], such as:
#' - `ci_method`, `centrality` for Mixed models and Bayesian models...
#' - `exponentiate`, ...
#' - etc.
#'
#' @details
#'
#' ## Standardization Methods
#' - **refit**: This method is based on a complete model re-fit with a
#' standardized version of the data. Hence, this method is equal to
#' standardizing the variables before fitting the model. It is the "purest" and
#' the most accurate (Neter et al., 1989), but it is also the most
#' computationally costly and long (especially for heavy models such as Bayesian
#' models). This method is particularly recommended for complex models that
#' include interactions or transformations (e.g., polynomial or spline terms).
#' The `robust` (default to `FALSE`) argument enables a robust standardization
#' of data, i.e., based on the `median` and `MAD` instead of the `mean` and
#' `SD`. **See [standardize()] for more details.**
#'   - **Note** that `standardize_parameters(method = "refit")` may not return
#'   the same results as fitting a model on data that has been standardized with
#'   `standardize()`; `standardize_parameters()` used the data used by the model
#'   fitting function, which might not be same data if there are missing values.
#'   see the `remove_na` argument in `standardize()`.
#' - **posthoc**: Post-hoc standardization of the parameters, aiming at
#' emulating the results obtained by "refit" without refitting the model. The
#' coefficients are divided by the standard deviation (or MAD if `robust`) of
#' the outcome (which becomes their expression 'unit'). Then, the coefficients
#' related to numeric variables are additionally multiplied by the standard
#' deviation (or MAD if `robust`) of the related terms, so that they correspond
#' to changes of 1 SD of the predictor (e.g., "A change in 1 SD of `x` is
#' related to a change of 0.24 of the SD of `y`). This does not apply to binary
#' variables or factors, so the coefficients are still related to changes in
#' levels. This method is not accurate and tend to give aberrant results when
#' interactions are specified.
#' - **basic**: This method is similar to `method = "posthoc"`, but treats all
#' variables as continuous: it also scales the coefficient by the standard
#' deviation of model's matrix' parameter of factors levels (transformed to
#' integers) or binary predictors. Although being inappropriate for these cases,
#' this method is the one implemented by default in other software packages,
#' such as [lm.beta::lm.beta()].
#' - **smart** (Standardization of Model's parameters with Adjustment,
#' Reconnaissance and Transformation - *experimental*): Similar to `method =
#' "posthoc"` in that it does not involve model refitting. The difference is
#' that the SD (or MAD if `robust`) of the response is computed on the relevant
#' section of the data. For instance, if a factor with 3 levels A (the
#' intercept), B and C is entered as a predictor, the effect corresponding to B
#' vs. A will be scaled by the variance of the response at the intercept only.
#' As a results, the coefficients for effects of factors are similar to a Glass'
#' delta.
#' - **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method, the
#' response and the predictor are standardized based on the level of prediction
#' (levels are detected with [performance::check_heterogeneity_bias()]): Predictors
#' are standardized based on their SD at level of prediction (see also
#' [datawizard::demean()]); The outcome (in linear LMMs) is standardized based
#' on a fitted random-intercept-model, where `sqrt(random-intercept-variance)`
#' is used for level 2 predictors, and `sqrt(residual-variance)` is used for
#' level 1 predictors (Hoffman 2015, page 342). A warning is given when a
#' within-group variable is found to have access between-group variance.
#' - **sdy** (*for logistic regression models only*): This y-standardization
#' is useful when comparing coefficients of logistic regression models across
#' models for the same sample. Unobserved heterogeneity varies across models
#' with different independent variables, and thus, odds ratios from the same
#' predictor of different models cannot be compared directly. The
#' y-standardization makes coefficients "comparable across models by dividing
#' them with the estimated standard deviation of the latent variable for each
#' model" (Mood 2010). Thus, whenever one has multiple logistic regression models
#' that are fit to the same data and share certain predictors (e.g. nested
#' models), it can be useful to use this standardization approach to make
#' log-odds or odds ratios comparable.
#'
#' ## Transformed Variables
#' When the model's formula contains transformations (e.g. `y ~ exp(X)`) `method
#' = "refit"` will give different results compared to `method = "basic"`
#' (`"posthoc"` and `"smart"` do not support such transformations): While
#' `"refit"` standardizes the data *prior* to the transformation (e.g.
#' equivalent to `exp(scale(X))`), the `"basic"` method standardizes the
#' transformed data (e.g. equivalent to `scale(exp(X))`).
#' \cr\cr
#' See the *Transformed Variables* section in [standardize.default()] for more
#' details on how different transformations are dealt with when `method =
#' "refit"`.
#'
#' ## Confidence Intervals
#' The returned confidence intervals are re-scaled versions of the
#' unstandardized confidence intervals, and not "true" confidence intervals of
#' the standardized coefficients (cf. Jones & Waller, 2015).
#'
#' ## Generalized Linear Models
#' Standardization for generalized linear models (GLM, GLMM, etc) is done only
#' with respect to the predictors (while the outcome remains as-is,
#' unstandardized) - maintaining the interpretability of the coefficients (e.g.,
#' in a binomial model: the exponent of the standardized parameter is the OR of
#' a change of 1 SD in the predictor, etc.)
#'
#' ## Dealing with Factors
#' `standardize(model)` or `standardize_parameters(model, method = "refit")` do
#' *not* standardize categorical predictors (i.e. factors) / their
#' dummy-variables, which may be a different behaviour compared to other R
#' packages (such as \pkg{lm.beta}) or other software packages (like SPSS). To
#' mimic such behaviours, either use `standardize_parameters(model, method =
#' "basic")` to obtain post-hoc standardized parameters, or standardize the data
#' with `datawizard::standardize(data, force = TRUE)` *before* fitting the
#' model.
#'
#' @return A data frame with the standardized parameters (`Std_*`, depending on
#'   the model type) and their CIs (`CI_low` and `CI_high`). Where applicable,
#'   standard errors (SEs) are returned as an attribute (`attr(x,
#'   "standard_error")`).
#'
#' @family standardize
#' @family effect size indices
#'
#' @seealso See also [package vignette](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).
#'
#' @examples
#' model <- lm(len ~ supp * dose, data = ToothGrowth)
#' standardize_parameters(model, method = "refit")
#' \donttest{
#' standardize_parameters(model, method = "posthoc")
#' standardize_parameters(model, method = "smart")
#' standardize_parameters(model, method = "basic")
#'
#' # Robust and 2 SD
#' standardize_parameters(model, robust = TRUE)
#' standardize_parameters(model, two_sd = TRUE)
#'
#' model <- glm(am ~ cyl * mpg, data = mtcars, family = "binomial")
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "posthoc")
#' standardize_parameters(model, method = "basic", exponentiate = TRUE)
#' }
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' \donttest{
#' m <- lme4::lmer(mpg ~ cyl + am + vs + (1 | cyl), mtcars)
#' standardize_parameters(m, method = "pseudo", ci_method = "satterthwaite")
#' }
#'
#' @examplesIf require("rstanarm", quietly = TRUE)
#' \donttest{
#' model <- rstanarm::stan_glm(rating ~ critical + privileges, data = attitude, refresh = 0)
#' standardize_posteriors(model, method = "refit", verbose = FALSE)
#' standardize_posteriors(model, method = "posthoc", verbose = FALSE)
#' standardize_posteriors(model, method = "smart", verbose = FALSE)
#' head(standardize_posteriors(model, method = "basic", verbose = FALSE))
#' }
#'
#' @references
#' - Hoffman, L. (2015). Longitudinal analysis: Modeling within-person fluctuation
#'   and change. Routledge.
#'
#' - Jones, J. A., & Waller, N. G. (2015). The normal-theory and asymptotic
#'   distribution-free (ADF) covariance matrix of standardized regression
#'   coefficients: theoretical extensions and finite sample behavior.
#'   Psychometrika, 80(2), 365-378.
#'
#' - Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear
#'   regression models.
#'
#' - Gelman, A. (2008). Scaling regression inputs by dividing by two standard
#'   deviations. Statistics in medicine, 27(15), 2865-2873.
#'
#' - Mood C. Logistic Regression: Why We Cannot Do What We Think We Can Do, and
#'   What We Can Do About It. European Sociological Review (2010) 26:67–82.
#'
#' @export
#' @aliases standardise_parameters
standardize_parameters <- function(model,
                                   method = "refit",
                                   ci = 0.95,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   include_response = TRUE,
                                   verbose = TRUE,
                                   ...) {
  UseMethod("standardize_parameters")
}

#' @export
standardise_parameters <- standardize_parameters

#' @export
standardize_parameters.default <- function(model,
                                           method = "refit",
                                           ci = 0.95,
                                           robust = FALSE,
                                           two_sd = FALSE,
                                           include_response = TRUE,
                                           verbose = TRUE,
                                           ...) {
  # check for valid input
  .is_model_valid(model)

  object_name <- insight::safe_deparse_symbol(substitute(model))
  method <- match.arg(method, c("refit", "posthoc", "smart", "basic", "classic", "pseudo", "sdy"))

  m_info <- .get_model_info(model, ...)
  include_response <- include_response && .safe_to_standardize_response(m_info, verbose = verbose)

  if (method == "refit") {
    model <- datawizard::standardize(model,
      robust = robust, two_sd = two_sd,
      include_response = include_response,
      verbose = verbose, m_info = m_info
    )
  }

  # need model_parameters to return the parameters, not the terms
  if (inherits(model, "aov")) {
    class(model) <- class(model)[class(model) != "aov"]
  }
  pars <- model_parameters(model, ci = ci, standardize = NULL, effects = "fixed", as_draws = TRUE, ...)

  # should post hoc exponentiate?
  exponentiate <- isTRUE(eval(match.call()[["exponentiate"]], envir = parent.frame()))
  coefficient_name <- attr(pars, "coefficient_name")

  if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo", "sdy")) {
    if (m_info$is_multivariate) {
      insight::format_error(
        "Cannot post-hoc standardize multivariate models. Try using method \"refit\" instead."
      )
    }
    if (method == "sdy" && !m_info$is_binomial) {
      insight::format_error("Method \"sdy\" is only applicable to logistic regression models.")
    }

    pars <- .standardize_parameters_posthoc(
      pars, method, model, m_info, robust, two_sd, exponentiate,
      include_response, verbose
    )

    method <- attr(pars, "std_method")
    robust <- attr(pars, "robust")
  }

  ## clean cols
  if (!is.null(ci)) pars$CI <- attr(pars, "ci")
  colnm <- c("Component", "Response", "Group", "Parameter", utils::head(.col_2_scale, -2), "CI", "CI_low", "CI_high")
  pars <- pars[, colnm[colnm %in% colnames(pars)]]

  if (!is.null(coefficient_name) && coefficient_name %in% c("Odds Ratio", "Risk Ratio", "IRR", "Prevalence Ratio")) {
    colnames(pars)[colnames(pars) == "Coefficient"] <- gsub(" ", "_", coefficient_name, fixed = TRUE)
  }

  i <- colnames(pars) %in% c("Coefficient", "Median", "Mean", "MAP", "Odds_Ratio", "Risk_Ratio", "IRR", "Prevalence_Ratio")
  colnames(pars)[i] <- paste0("Std_", colnames(pars)[i])

  ## SE attribute?
  if ("SE" %in% colnames(pars)) {
    attr(pars, "standard_error") <- pars$SE
    pars$SE <- NULL
  }

  ## attributes
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "object_name") <- object_name
  attr(pars, "ci") <- ci
  attr(pars, "include_response") <- include_response
  class(pars) <- c("parameters_standardized", "effectsize_table", "see_effectsize_table", "data.frame")

  pars
}


#' @export
standardize_parameters.mediate <- function(model,
                                           method = "refit",
                                           ci = 0.95,
                                           robust = FALSE,
                                           two_sd = FALSE,
                                           include_response = TRUE,
                                           verbose = TRUE,
                                           ...) {
  if (method != "refit") {
    warning("Only `method=\"refit\"` is supported for mediation models.", immediate. = TRUE, call. = FALSE)
  }

  NextMethod("standardize_parameters",
    method = "refit", ci = ci, robust = robust,
    two_sd = two_sd, include_response = include_response, verbose = verbose
  )
}


#' @export
standardize_parameters.parameters_model <- function(model,
                                                    method = "refit",
                                                    ci = NULL,
                                                    robust = FALSE,
                                                    two_sd = FALSE,
                                                    include_response = TRUE,
                                                    verbose = TRUE,
                                                    ...) {
  if (method == "refit") {
    insight::format_error(
      "Argument `refit` not supported for standardizing results from `model_parameters()`.",
    )
  }

  if (!is.null(ci)) {
    insight::format_alert(
      "Argument `ci` not supported for standardizing results from `model_parameters()`. It is ignored."
    )
  }

  pars <- model
  ci <- attr(pars, "ci")
  model <- .get_object(pars)
  if (is.null(model)) model <- attr(pars, "object")

  m_info <- .get_model_info(model, ...)
  include_response <- include_response && .safe_to_standardize_response(m_info, verbose = verbose)

  exponentiate <- attr(pars, "exponentiate")
  if (is.null(exponentiate)) {
    exponentiate <- FALSE
  }
  pars <- .standardize_parameters_posthoc(
    pars, method, model, m_info, robust, two_sd, exponentiate, include_response, verbose
  )
  method <- attr(pars, "std_method")
  robust <- attr(pars, "robust")

  ## clean cols
  if (!is.null(ci)) pars$CI <- attr(pars, "ci")
  colnm <- c("Component", "Response", "Group", "Parameter", utils::head(.col_2_scale, -2), "CI", "CI_low", "CI_high")
  pars <- pars[, colnm[colnm %in% colnames(pars)]]
  i <- colnames(pars) %in% c("Coefficient", "Median", "Mean", "MAP")
  colnames(pars)[i] <- paste0("Std_", colnames(pars)[i])

  ## SE attribute?
  if ("SE" %in% colnames(pars)) {
    attr(pars, "standard_error") <- pars$SE
    pars$SE <- NULL
  }

  ## attributes
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "ci") <- ci
  attr(pars, "include_response") <- include_response
  class(pars) <- c("parameters_standardized", "effectsize_table", "see_effectsize_table", "data.frame")

  pars
}


#' @export
standardize_parameters.bootstrap_model <- function(model,
                                                   method = "refit",
                                                   ci = 0.95,
                                                   robust = FALSE,
                                                   two_sd = FALSE,
                                                   include_response = TRUE,
                                                   verbose = TRUE,
                                                   ...) {
  object_name <- insight::safe_deparse_symbol(substitute(model))
  method <- match.arg(method, c("refit", "posthoc", "smart", "basic", "classic", "pseudo", "sdy"))

  pars <- model
  model <- attr(pars, "original_model")

  m_info <- .get_model_info(model, ...)
  include_response <- include_response && .safe_to_standardize_response(m_info, verbose = verbose)

  if (method == "refit") {
    insight::format_error("The `refit` method is not supported for bootstrapped models.")
    ## But it would look something like this:
    # model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose, m_info = m_info)
    # model <- parameters::bootstrap_model(model, iterations = 1000, verbose = verbose)
    # return(model)
  }

  # need model_parameters to return the parameters, not the terms
  if (inherits(model, "aov")) class(model) <- class(model)[class(model) != "aov"]


  if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo")) {
    pars <- .standardize_posteriors_posthoc(pars, method, model, m_info, robust, two_sd, include_response, verbose)

    method <- attr(pars, "std_method")
    robust <- attr(pars, "robust")
  }

  pars <- bayestestR::describe_posterior(pars,
    centrality = "median",
    ci = ci, ci_method = "quantile",
    test = NULL
  )
  names(pars)[names(pars) == "Median"] <- "Std_Coefficient"


  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "object_name") <- object_name
  attr(pars, "ci") <- ci
  attr(pars, "include_response") <- include_response
  class(pars) <- c("parameters_standardized", "effectsize_table", "see_effectsize_table", "data.frame")

  pars
}


#' @export
standardize_parameters.bootstrap_parameters <- function(model,
                                                        method = "refit",
                                                        ci = 0.95,
                                                        robust = FALSE,
                                                        two_sd = FALSE,
                                                        include_response = TRUE,
                                                        verbose = TRUE,
                                                        ...) {
  standardize_parameters(attr(model, "boot_samples"),
    method = method,
    ci = ci,
    robust = robust,
    two_sd = two_sd,
    include_response = include_response,
    verbose = verbose,
    ...
  )
}


#' @export
standardize_parameters.model_fit <- function(model,
                                             method = "refit",
                                             ci = 0.95,
                                             robust = FALSE,
                                             two_sd = FALSE,
                                             include_response = TRUE,
                                             verbose = TRUE,
                                             ...) {
  standardize_parameters(
    model$fit,
    method = method,
    ci = ci,
    robust = robust,
    two_sd = two_sd,
    include_response = include_response,
    verbose = verbose,
    ...
  )
}




# methods --------------------------------

#' @export
format.parameters_standardized <- function(x,
                                           digits = 2,
                                           format = c("text", "markdown", "html"),
                                           ...) {
  format <- match.arg(format)

  footer <- subtitle <- NULL
  caption <- sprintf("Standardization method: %s", attr(x, "std_method"))

  # robust / two_sd
  if (attr(x, "two_sd") || attr(x, "robust")) {
    footer <- sprintf(
      "Scaled by %s %s%s from the %s.",
      ifelse(attr(x, "two_sd"), "two", "one"),
      ifelse(attr(x, "robust"), "MAD", "SD"),
      ifelse(attr(x, "two_sd"), "s", ""),
      ifelse(attr(x, "robust"), "median", "mean")
    )
  }

  # include_response
  if (!attr(x, "include_response")) {
    footer <- c(footer, "Response is unstandardized.")
  }

  if (format %in% c("markdown", "text") && !is.null(footer)) {
    footer <- lapply(footer, function(ftr) {
      c(paste0("\n- ", ftr), "blue")
    })
  }
  attr(x, "table_footer") <- footer

  if (format %in% c("markdown", "text") && !is.null(caption)) {
    caption <- c(paste0("# ", caption), "blue")
  }
  attr(x, "table_caption") <- caption
  attr(x, "table_subtitle") <- subtitle

  attr(x, "ci") <- NULL
  attr(x, "ci_method") <- NULL

  insight::format_table(x, digits = digits, ci_digits = digits, preserve_attributes = TRUE, ...)
}


#' @export
print.parameters_standardized <- function(x, digits = 2, ...) {
  x_fmt <- format(x, digits = digits, output = "text", ...)
  cat(insight::export_table(x_fmt, format = NULL, ...))
  invisible(x)
}

#' @export
print_md.parameters_standardized <- function(x, digits = 2, ...) {
  x_fmt <- format(x, digits = digits, output = "markdown", ...)
  insight::export_table(x_fmt, format = "markdown", ...)
}

#' @export
print_html.parameters_standardized <- function(x, digits = 2, ...) {
  x_fmt <- format(x, digits = digits, output = "html", ...)
  insight::export_table(x_fmt, format = "html", ...)
}





# helper -------------------------


#' @keywords internal
.standardize_parameters_posthoc <- function(pars,
                                            method,
                                            model,
                                            mi,
                                            robust,
                                            two_sd,
                                            exponentiate,
                                            include_response,
                                            verbose) {
  # Sanity Check for "pseudo"
  method <- .should_pseudo(method, model, mi)

  method <- .cant_smart_or_posthoc(method, model, mi, pars$Parameter)

  if (robust && method == "pseudo") {
    insight::format_alert("`robust` standardization not available for `pseudo` method.")
    robust <- FALSE
  }


  ## Get scaling factors
  deviations <- standardize_info(
    model,
    robust = robust,
    include_pseudo = method == "pseudo",
    two_sd = two_sd,
    model_info = mi
  )
  i_missing <- setdiff(seq_len(nrow(pars)), seq_len(nrow(deviations)))
  unstd <- pars
  if (length(i_missing)) {
    deviations[i_missing, ] <- NA
  }

  if (method == "basic") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Basic"
  } else if (method == "posthoc") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "smart") {
    col_dev_resp <- "Deviation_Response_Smart"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "pseudo") {
    col_dev_resp <- "Deviation_Response_Pseudo"
    col_dev_pred <- "Deviation_Pseudo"
  } else if (method == "sdy") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_SDy"
    include_response <- FALSE
  } else {
    insight::format_error("`method` must be one of \"basic\", \"posthoc\", \"smart\", \"pseudo\" or \"sdy\".")
  }


  .dev_pred <- deviations[[col_dev_pred]]
  .dev_resp <- deviations[[col_dev_resp]]
  if (!include_response) .dev_resp <- 1
  .dev_factor <- .dev_pred / .dev_resp

  # Sapply standardization
  pars[, colnames(pars) %in% .col_2_scale] <- lapply(
    pars[, colnames(pars) %in% .col_2_scale, drop = FALSE],
    function(x) {
      if (exponentiate) {
        if (method == "sdy") {
          exp(x * .dev_factor)
        } else {
          x^.dev_factor
        }
      } else {
        x * .dev_factor
      }
    }
  )

  to_complete <- apply(pars[, colnames(pars) %in% .col_2_scale], 1, anyNA)
  if (length(i_missing) || any(to_complete)) {
    i_missing <- union(i_missing, which(to_complete))

    pars[i_missing, colnames(pars) %in% .col_2_scale] <-
      unstd[i_missing, colnames(pars) %in% .col_2_scale]
  }

  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust

  pars
}


#' @keywords internal
.col_2_scale <- c("Coefficient", "Median", "Mean", "MAP", "SE", "CI_low", "CI_high")


#' @keywords internal
.cant_smart_or_posthoc <- function(method, model, mi, params) {
  if (method %in% c("smart", "posthoc")) {
    cant_posthocsmart <- FALSE

    if (mi$is_linear && !colnames(stats::model.frame(model))[1] == insight::find_response(model)) {
      can_posthocsmart <- TRUE
    }

    # factors are allowed
    if (!cant_posthocsmart && !all(params == insight::clean_names(params) | grepl("(as.factor|factor)\\(", params))) {
      cant_posthocsmart <- TRUE
    }

    if (cant_posthocsmart) {
      insight::format_alert(
        "Method `", method, "` does not currently support models with transformed parameters.",
        "Reverting to `basic` method. Concider using the `refit` method directly."
      )
      method <- "basic"
    }
  }
  method
}


#' @keywords internal
.should_pseudo <- function(method, model, mi) {
  if (method == "pseudo" && !(mi$is_mixed && length(insight::find_random(model)$random) == 1)) {
    insight::format_alert(
      "`pseudo` method only available for 2-level (G)LMMs.",
      "Setting method to `basic`."
    )
    method <- "basic"
  }
  method
}


#' @keywords internal
.safe_to_standardize_response <- function(info, verbose = TRUE) {
  if (is.null(info)) {
    if (verbose) {
      warning(insight::format_message(
        "Unable to verify if response should not be standardized.",
        "Response will be standardized."
      ), immediate. = TRUE, call. = FALSE)
    }
    return(TRUE)
  }

  # check if model has a response variable that should not be standardized.
  info$is_linear &&
    !info$family == "inverse.gaussian" &&
    !info$is_survival &&
    !info$is_censored

  # # alternative would be to keep something like:
  # !info$is_count &&
  #   !info$is_ordinal &&
  #   !info$is_multinomial &&
  #   !info$is_beta &&
  #   !info$is_censored &&
  #   !info$is_binomial &&
  #   !info$is_survival
  # # And then treating response for "Gamma()" or "inverse.gaussian" similar to
  # # log-terms...
}


#' @keywords internal
.get_model_info <- function(model, model_info = NULL, ...) {
  if (is.null(model_info)) model_info <- insight::model_info(model, verbose = FALSE)

  model_info
}
