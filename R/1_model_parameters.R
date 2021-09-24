# default methods, glm (almost default)

#################### .default ----------------------

#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{[Correlations, t-tests, ...][model_parameters.htest] (`htest`, `pairwise.htest`)}
#'  \item{[ANOVAs][model_parameters.aov] (`aov`, `anova`, \pkg{afex}, ...)}
#'  \item{[Regression models][model_parameters.default] (`lm`, `glm`, \pkg{survey}, ...)}
#'  \item{[Additive models][model_parameters.cgam] (`gam`, `gamm`, ...)}
#'  \item{[Zero-inflated models][model_parameters.zcpglm] (`hurdle`, `zeroinfl`, `zerocount`)}
#'  \item{[Multinomial, ordinal and cumulative link models][model_parameters.mlm] (`bracl`, `multinom`, `mlm`, ...)}
#'  \item{[Other special models][model_parameters.averaging] (`model.avg`, `betareg`, `glmx`, ...)}
#'  \item{[Mixed models][model_parameters.merMod] (\pkg{lme4}, \pkg{nlme}, \pkg{glmmTMB}, \pkg{afex}, ...)}
#'  \item{[Bayesian tests][model_parameters.BFBayesFactor] (\pkg{BayesFactor})}
#'  \item{[Bayesian models][model_parameters.stanreg] (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, \pkg{blavaan}, ...)}
#'  \item{[PCA and FA][model_parameters.principal] (\pkg{psych})}
#'  \item{[CFA and SEM][model_parameters.lavaan] (\pkg{lavaan})}
#'  \item{[Cluster models][model_parameters.kmeans] (k-means, ...)}
#'  \item{[Meta-Analysis via linear (mixed) models][model_parameters.rma] (`rma`, `metaplus`, \pkg{metaBMA}, ...)}
#'  \item{[Hypothesis testing][model_parameters.glht] (`glht`, \pkg{PMCMRplus})}
#'  \item{[Robust statistical tests][model_parameters.t1way] (\pkg{WRS2})}
#'  \item{[Multiply imputed repeated analyses][model_parameters.mira] (`mira`)}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods. Non-documented
#'   arguments are `digits`, `p_digits`, `ci_digits` and
#'   `footer_digits` to set the number of digits for the output.
#'   `group` can also be passed to the `print()` method. See details
#'   in [print.parameters_model()] and 'Examples' in
#'   [model_parameters.default()].
#'
#' @seealso [insight::standardize_names()] to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note The [`print()`][print.parameters_model] method has several
#'   arguments to tweak the output. There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}, and a dedicated
#'   method for use inside rmarkdown files,
#'   [`print_md()`][print_md.parameters_model].
#'
#' @details
#' ## Standardization of model coefficients:
#' Standardization is based on [effectsize::standardize_parameters()]. In case
#' of `standardize = "refit"`, the data used to fit the model will be
#' standardized and the model is completely refitted. In such cases, standard
#' errors and confidence intervals refer to the standardized coefficient. The
#' default, `standardize = "refit"`, never standardizes categorical predictors
#' (i.e. factors), which may be a different behaviour compared to other R
#' packages or other software packages (like SPSS). To mimic behaviour of SPSS
#' or packages such as \pkg{lm.beta}, use `standardize = "basic"`.
#'
#' @inheritSection effectsize::standardize_parameters Standardization Methods:

#' @section Labeling the Degrees of Freedom:
#' Throughout the \pkg{parameters} package, we decided to label the residual
#' degrees of freedom *df_error*. The reason for this is that these degrees
#' of freedom not always refer to the residuals. For certain models, they refer
#' to the estimate error - in a linear model these are the same, but in - for
#' instance - any mixed effects model, this isn't strictly true. Hence, we
#' think that `df_error` is the most generic label for these degrees of
#' freedom.
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#'
#' @references
#' \itemize{
#'   \item Hoffman, L. (2015). Longitudinal analysis: Modeling within-person fluctuation and change. Routledge.
#'   \item Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear regression models.
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}


# DF naming convention --------------------


# DF column naming
# F has df, df_error
# t has df_error
# z has df_error = Inf
# Chisq has df
# https://github.com/easystats/parameters/issues/455



#' @rdname model_parameters
#' @export
parameters <- model_parameters


#' Parameters from (General) Linear Models
#'
#' Extract and compute indices and measures to describe parameters of (general)
#' linear models (GLMs).
#'
#' @param model Model object.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param bootstrap Should estimates be based on bootstrapped model? If
#'   `TRUE`, then arguments of [Bayesian
#'   regressions][model_parameters.stanreg] apply (see also
#'   [`bootstrap_parameters()`][bootstrap_parameters]).
#' @param iterations The number of bootstrap replicates. This only apply in the
#'   case of bootstrapped frequentist models.
#' @param standardize The method used for standardizing the parameters. Can be
#'   `NULL` (default; no standardization), `"refit"` (for re-fitting the model
#'   on standardized data) or one of `"basic"`, `"posthoc"`, `"smart"`,
#'   `"pseudo"`. See 'Details' in [effectsize::standardize_parameters()].
#'   **Important:**
#'   - The `"refit"` method does *not* standardized categorical predictors (i.e.
#'   factors), which may be a different behaviour compared to other R packages
#'   (such as \pkg{lm.beta}) or other software packages (like SPSS). to mimic
#'   such behaviours, either use `standardize="basic"` or standardize the data
#'   with `effectsize::standardize(force=TRUE)` *before* fitting the model.
#'   - For mixed models, when using methods other than `"refit"`, only the fixed
#'   effects will be returned.
#'   - Robust estimation (i.e. `robust=TRUE`) of standardized parameters only
#'   works when `standardize="refit"`.
#' @param exponentiate Logical, indicating whether or not to exponentiate the
#'   the coefficients (and related confidence intervals). This is typical for
#'   logistic regression, or more generally speaking, for models with log
#'   or logit links. **Note:** Delta-method standard errors are also
#'   computed (by multiplying the standard errors by the transformed
#'   coefficients). This is to mimic behaviour of other software packages, such
#'   as Stata, but these standard errors poorly estimate uncertainty for the
#'   transformed coefficient. The transformed confidence interval more clearly
#'   captures this uncertainty. For `compare_parameters()`,
#'   `exponentiate = "nongaussian"` will only exponentiate coefficients
#'   from non-Gaussian families.
#' @param robust Logical, if `TRUE`, robust standard errors are calculated
#'   (if possible), and confidence intervals and p-values are based on these
#'   robust standard errors. Additional arguments like `vcov_estimation` or
#'   `vcov_type` are passed down to other methods, see
#'   [`standard_error_robust()`][standard_error_robust] for details
#'   and [this vignette](https://easystats.github.io/parameters/articles/model_parameters_robust.html)
#'   for working examples.
#' @param component Model component for which parameters should be shown. May be
#'   one of `"conditional"`, `"precision"` (\pkg{betareg}),
#'   `"scale"` (\pkg{ordinal}), `"extra"` (\pkg{glmx}),
#'   `"marginal"` (\pkg{mfx}), `"conditional"` or `"full"` (for
#'   `MuMIn::model.avg()`) or `"all"`.
#' @param p_adjust Character vector, if not `NULL`, indicates the method to
#'   adjust p-values. See [stats::p.adjust()] for details. Further
#'   possible adjustment methods are `"tukey"`, `"scheffe"`,
#'   `"sidak"` and `"none"` to explicitly disable adjustment for
#'   `emmGrid` objects (from \pkg{emmeans}).
#' @param ci_method Method for computing degrees of freedom for
#'   confidence intervals (CI). Only applies to models of class `glm` or
#'   `polr` or if `bootstrap = TRUE`. For `glm` and `polr`, `ci_method` may be
#'   `"profile"` or `"wald"`. If `bootstrap = TRUE`, see argument `ci_method`
#'   in [bayestestR::describe_posterior()].
#' @param df_method Deprecated. Please use `ci_method`.
#' @param summary Logical, if `TRUE`, prints summary information about the
#'   model (model formula, number of observations, residual standard deviation
#'   and more).
#' @param keep,drop Character containing a regular expression pattern that
#'   describes the parameters that should be included in the returned data frame
#'   (for `keep`), resp. parameters to exclude (`drop`). `keep` may also be a
#'   named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If `keep` is a character vector, every parameter
#'   name in the *"Parameter"* column that matches the regular expression in
#'   `parameters` will be selected from the returned data frame (and vice versa,
#'   all parameter names matching `drop` will be excluded). Furthermore, if
#'   `keep` has more than one element, these will be merged with an `OR`
#'   operator into a regular expression pattern like this: `"(one|two|three)"`.
#'   If `keep` is a named list of regular expression patterns, the names of the
#'   list-element should equal the column name where selection should be
#'   applied. This is useful for model objects where `model_parameters()`
#'   returns multiple columns with parameter components, like in
#'   [model_parameters.lavaan()]. Note that the regular expression pattern
#'   should match the parameter names as they are stored in the returned data
#'   frame, which can be different from how they are printed. Inspect the
#'   `$Parameter` column of the parameters table to get the exact parameter
#'   names.
#' @param parameters Deprecated, alias for `keep`.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to or from other methods. For instance, when
#'   `bootstrap = TRUE`, arguments like `type` or `parallel` are
#'   passed down to `bootstrap_model()`, and arguments like `ci_method`
#'   are passed down to [bayestestR::describe_posterior()].
#'
#' @seealso [insight::standardize_names()] to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model)
#'
#' # bootstrapped parameters
#' model_parameters(model, bootstrap = TRUE)
#'
#' # standardized parameters
#' model_parameters(model, standardize = "refit")
#'
#' # different p-value style in output
#' model_parameters(model, p_digits = 5)
#' model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")
#'
#' # logistic regression model
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#'
#' # show odds ratio / exponentiated coefficients
#' model_parameters(model, exponentiate = TRUE)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.default <- function(model,
                                     ci = .95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     robust = FALSE,
                                     p_adjust = NULL,
                                     summary = FALSE,
                                     keep = NULL,
                                     drop = NULL,
                                     parameters = keep,
                                     verbose = TRUE,
                                     ...) {
  out <- tryCatch(
    {
      .model_parameters_generic(
        model = model,
        ci = ci,
        bootstrap = bootstrap,
        iterations = iterations,
        merge_by = "Parameter",
        standardize = standardize,
        exponentiate = exponentiate,
        robust = robust,
        p_adjust = p_adjust,
        summary = summary,
        keep_parameters = keep,
        drop_parameters = drop,
        verbose = verbose,
        ...
      )
    },
    error = function(e) {
      fail <- NA
      attr(fail, "error") <- gsub("  ", " ", gsub("\\n", "", e$message), fixed = TRUE)
      fail
    }
  )

  if (length(out) == 1 && isTRUE(is.na(out))) {
    msg <- insight::format_message(
      paste0("Sorry, `model_parameters()` failed with the following error (possible class '", class(model)[1], "' not supported):\n"),
      attr(out, "error")
    )
    stop(msg, call. = FALSE)
  } else if (is.null(out)) {
    stop(paste0("Sorry, `model_parameters()` does currently not work for objects of class '", class(model)[1], "'."), call. = FALSE)
  }

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


.model_parameters_generic <- function(model,
                                      ci = .95,
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      merge_by = "Parameter",
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      effects = "fixed",
                                      component = "conditional",
                                      robust = FALSE,
                                      ci_method = NULL,
                                      p_adjust = NULL,
                                      summary = FALSE,
                                      keep_parameters = NULL,
                                      drop_parameters = NULL,
                                      verbose = TRUE,
                                      df_method = ci_method,
                                      ...) {

  ## TODO remove later
  if (!missing(df_method)) {
    message(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."))
    ci_method <- df_method
  }

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
      ci_method = ci_method,
      ...
    )
  } else {
    params <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      merge_by = merge_by,
      standardize = standardize,
      effects = effects,
      robust = robust,
      ci_method = ci_method,
      p_adjust = p_adjust,
      keep_parameters = keep_parameters,
      drop_parameters = drop_parameters,
      verbose = verbose,
      ...
    )
  }

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    bootstrap,
    iterations,
    df_method = ci_method,
    p_adjust = p_adjust,
    summary = summary,
    verbose = verbose,
    ...
  )

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}




#################### .glm ----------------------


#' @rdname model_parameters.default
#' @export
model_parameters.glm <- function(model,
                                 ci = .95,
                                 ci_method = "profile",
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 summary = FALSE,
                                 verbose = TRUE,
                                 df_method = ci_method,
                                 ...) {

  ## TODO remove later
  if (!missing(df_method)) {
    message(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."))
    ci_method <- df_method
  }

  if (insight::n_obs(model) > 1e4 && ci_method == "profile") {
    message(insight::format_message("Profiled confidence intervals may take longer time to compute. Use 'ci_method=\"wald\"' for faster computation of CIs."))
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    robust = robust,
    p_adjust = p_adjust,
    summary = summary,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
