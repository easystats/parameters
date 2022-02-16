# default methods, glm (almost default)

#################### .default ----------------------

#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{[Correlations, t-tests, ...][model_parameters.htest] (`htest`, `pairwise.htest`)}
#'  \item{[ANOVAs][model_parameters.aov] (`aov`, `anova`, **afex**, ...)}
#'  \item{[Regression models][model_parameters.default] (`lm`, `glm`, **survey**, ...)}
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
#'   [**see**-package](https://easystats.github.io/see/), and a dedicated
#'   method for use inside rmarkdown files,
#'   [`print_md()`][print_md.parameters_model].
#'
#' @section Standardization of model coefficients:
#' Standardization is based on [effectsize::standardize_parameters()]. In case
#' of `standardize = "refit"`, the data used to fit the model will be
#' standardized and the model is completely refitted. In such cases, standard
#' errors and confidence intervals refer to the standardized coefficient. The
#' default, `standardize = "refit"`, never standardizes categorical predictors
#' (i.e. factors), which may be a different behaviour compared to other R
#' packages or other software packages (like SPSS). To mimic behaviour of SPSS
#' or packages such as \pkg{lm.beta}, use `standardize = "basic"`.
#'
#' @section
#'
#' Standardization Methods:
#'
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
#' **Note** that `standardize_parameters(method = "refit")` may not return
#' the same results as fitting a model on data that has been standardized with
#' `standardize()`; `standardize_parameters()` used the data used by the model
#' fitting function, which might not be same data if there are missing values.
#' see the `remove_na` argument in `standardize()`.
#'
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
#'
#' - **basic**: This method is similar to `method = "posthoc"`, but treats all
#' variables as continuous: it also scales the coefficient by the standard
#' deviation of model's matrix' parameter of factors levels (transformed to
#' integers) or binary predictors. Although being inappropriate for these cases,
#' this method is the one implemented by default in other software packages,
#' such as [lm.beta::lm.beta()].
#'
#' - **smart** (Standardization of Model's parameters with Adjustment,
#' Reconnaissance and Transformation - *experimental*): Similar to `method =
#' "posthoc"` in that it does not involve model refitting. The difference is
#' that the SD (or MAD if `robust`) of the response is computed on the relevant
#' section of the data. For instance, if a factor with 3 levels A (the
#' intercept), B and C is entered as a predictor, the effect corresponding to B
#' vs. A will be scaled by the variance of the response at the intercept only.
#' As a results, the coefficients for effects of factors are similar to a Glass'
#' delta.
#'
#' - **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method, the
#' response and the predictor are standardized based on the level of prediction
#' (levels are detected with [performance::check_heterogeneity_bias()]): Predictors
#' are standardized based on their SD at level of prediction (see also
#' [datawizard::demean()]); The outcome (in linear LMMs) is standardized based
#' on a fitted random-intercept-model, where `sqrt(random-intercept-variance)`
#' is used for level 2 predictors, and `sqrt(residual-variance)` is used for
#' level 1 predictors (Hoffman 2015, page 342). A warning is given when a
#' within-group variable is found to have access between-group variance.
#'
#' @section Labeling the Degrees of Freedom:
#' Throughout the \pkg{parameters} package, we decided to label the residual
#' degrees of freedom *df_error*. The reason for this is that these degrees
#' of freedom not always refer to the residuals. For certain models, they refer
#' to the estimate error - in a linear model these are the same, but in - for
#' instance - any mixed effects model, this isn't strictly true. Hence, we
#' think that `df_error` is the most generic label for these degrees of
#' freedom.
#'
#' @section Confidence intervals and approximation of degrees of freedom:
#' There are different ways of approximating the degrees of freedom depending
#' on different assumptions about the nature of the model and its sampling
#' distribution. The `ci_method` argument modulates the method for computing degrees
#' of freedom (df) that are used to calculate confidence intervals (CI) and the
#' related p-values. Following options are allowed, depending on the model
#' class:
#'
#' **Classical methods:**
#'
#' Classical inference is generally based on the **Wald method**.
#' The Wald approach to inference computes a test statistic by dividing the
#' parameter estimate by its standard error (Coefficient / SE),
#' then comparing this statistic against a t- or normal distribution.
#' This approach can be used to compute CIs and p-values.
#'
#' `"wald"`:
#' - Applies to *non-Bayesian models*. For *linear models*, CIs
#'   computed using the Wald method (SE and a *t-distribution with residual df*);
#'   p-values computed using the Wald method with a *t-distribution with residual df*.
#'   For other models, CIs computed using the Wald method (SE and a *normal distribution*);
#'   p-values computed using the Wald method with a *normal distribution*.
#'
#' `"normal"`
#' - Applies to *non-Bayesian models*. Compute Wald CIs and p-values,
#'   but always use a normal distribution.
#'
#' `"residual"`
#' - Applies to *non-Bayesian models*. Compute Wald CIs and p-values,
#'   but always use a *t-distribution with residual df* when possible. If the
#'   residual df for a model cannot be determined, a normal distribution is
#'   used instead.
#'
#' **Methods for mixed models:**
#'
#' Compared to fixed effects (or single-level) models, determining appropriate
#' df for Wald-based inference in mixed models is more difficult.
#' See [the R GLMM FAQ](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable)
#' for a discussion.
#'
#' Several approximate methods for computing df are available, but you should
#' also consider instead using profile likelihood (`"profile"`) or bootstrap ("`boot"`)
#' CIs and p-values instead.
#'
#' `"satterthwaite"`
#' - Applies to *linear mixed models*. CIs computed using the
#'   Wald method (SE and a *t-distribution with Satterthwaite df*); p-values
#'   computed using the Wald method with a *t-distribution with Satterthwaite df*.
#'
#' `"kenward"`
#' - Applies to *linear mixed models*. CIs computed using the Wald
#'   method (*Kenward-Roger SE* and a *t-distribution with Kenward-Roger df*);
#'   p-values computed using the Wald method with *Kenward-Roger SE and t-distribution with Kenward-Roger df*.
#'
#' `"ml1"`
#' - Applies to *linear mixed models*. CIs computed using the Wald
#'   method (SE and a *t-distribution with m-l-1 approximated df*); p-values
#'   computed using the Wald method with a *t-distribution with m-l-1 approximated df*.
#'   See [`ci_ml1()`].
#'
#' `"betwithin"`
#' - Applies to *linear mixed models* and *generalized linear mixed models*.
#'   CIs computed using the Wald method (SE and a *t-distribution with between-within df*);
#'   p-values computed using the Wald method with a *t-distribution with between-within df*.
#'   See [`ci_betwithin()`].
#'
#' **Likelihood-based methods:**
#'
#' Likelihood-based inference is based on comparing the likelihood for the
#' maximum-likelihood estimate to the the likelihood for models with one or more
#' parameter values changed (e.g., set to zero or a range of alternative values).
#' Likelihood ratios for the maximum-likelihood and alternative models are compared
#' to a \eqn{\chi}-squared distribution to compute CIs and p-values.
#'
#' `"profile"`
#' - Applies to *non-Bayesian models* of class `glm`, `polr` or `glmmTMB`.
#'   CIs computed by *profiling the likelihood curve for a parameter*, using
#'   linear interpolation to find where likelihood ratio equals a critical value;
#'   p-values computed using the Wald method with a *normal-distribution* (note:
#'   this might change in a future update!)
#'
#' `"uniroot"`
#' - Applies to *non-Bayesian models* of class `glmmTMB`. CIs
#'   computed by *profiling the likelihood curve for a parameter*, using root
#'   finding to find where likelihood ratio equals a critical value; p-values
#'   computed using the Wald method with a *normal-distribution* (note: this
#'   might change in a future update!)
#'
#' **Methods for bootstrapped or Bayesian models:**
#'
#' Bootstrap-based inference is based on **resampling** and refitting the model
#' to the resampled datasets. The distribution of parameter estimates across
#' resampled datasets is used to approximate the parameter's sampling
#' distribution. Depending on the type of model, several different methods for
#' bootstrapping and constructing CIs and p-values from the bootstrap
#' distribution are available.
#'
#' For Bayesian models, inference is based on drawing samples from the model
#' posterior distribution.
#'
#' `"quantile"` (or `"eti"`)
#' - Applies to *all models (including Bayesian models)*.
#'   For non-Bayesian models, only applies if `bootstrap = TRUE`. CIs computed
#'   as *equal tailed intervals* using the quantiles of the bootstrap or
#'   posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::eti()`].
#'
#' `"hdi"`
#' - Applies to *all models (including Bayesian models)*. For non-Bayesian
#'   models, only applies if `bootstrap = TRUE`. CIs computed as *highest density intervals*
#'   for the bootstrap or posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::hdi()`].
#'
#' `"bci"` (or `"bcai"`)
#' - Applies to *all models (including Bayesian models)*.
#'   For non-Bayesian models, only applies if `bootstrap = TRUE`. CIs computed
#'   as *bias corrected and accelerated intervals* for the bootstrap or
#'   posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::bci()`].
#'
#' `"si"`
#' - Applies to *Bayesian models* with proper priors. CIs computed as
#'   *support intervals* comparing the posterior samples against the prior samples;
#'   p-values are based on the *probability of direction*. See [`bayestestR::si()`].
#'
#' `"boot"`
#' - Applies to *non-Bayesian models* of class `merMod`. CIs computed
#'   using *parametric bootstrapping* (simulating data from the fitted model);
#'   p-values computed using the Wald method with a *normal-distribution)*
#'   (note: this might change in a future update!).
#'
#' For all iteration-based methods other than `"boot"`
#' (`"hdi"`, `"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, `"bcai"`),
#' p-values are based on the probability of direction ([`bayestestR::p_direction()`]),
#' which is converted into a p-value using [`bayestestR::pd_to_p()`].
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#'
#' @references
#'
#'   - Hoffman, L. (2015). Longitudinal analysis: Modeling within-person
#'   fluctuation and change. Routledge.
#'
#'   - Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear
#'   regression models.
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
#'   with `datawizard::standardize(force=TRUE)` *before* fitting the model.
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
#'   confidence intervals (CI) and the related p-values. Allowed are following
#'   options (which vary depending on the model class): `"residual"`,
#'   `"normal"`, `"likelihood"`, `"satterthwaite"`, `"kenward"`, `"wald"`,
#'   `"profile"`, `"boot"`, `"uniroot"`, `"ml1"`, `"betwithin"`, `"hdi"`,
#'   `"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, or `"bcai"`. See section
#'   _Confidence intervals and approximation of degrees of freedom_ in
#'   [`model_parameters()`] for further details. When `ci_method=NULL`, in most
#'   cases `"wald"` is used then.
#' @param df_method Deprecated. Please use `ci_method`.
#' @param summary Logical, if `TRUE`, prints summary information about the
#'   model (model formula, number of observations, residual standard deviation
#'   and more).
#' @param keep,drop Character containing a regular expression pattern that
#'   describes the parameters that should be included (for `keep`) or excluded
#'   (for `drop`) in the returned data frame. `keep` may also be a
#'   named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If `keep` is a character vector, every parameter
#'   name in the *"Parameter"* column that matches the regular expression in
#'   `keep` will be selected from the returned data frame (and vice versa,
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
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
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
#' \donttest{
#' # logistic regression model
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#'
#' # show odds ratio / exponentiated coefficients
#' model_parameters(model, exponentiate = TRUE)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.default <- function(model,
                                     ci = .95,
                                     ci_method = NULL,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     robust = FALSE,
                                     p_adjust = NULL,
                                     summary = getOption("parameters_summary", FALSE),
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
        ci_method = ci_method,
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
  if (!missing(df_method) && !identical(ci_method, df_method)) {
    warning(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."), call. = FALSE)
    ci_method <- df_method
  }

  # Processing
  if (bootstrap) {
    # set default method for bootstrapped CI
    if (is.null(ci_method) || missing(ci_method)) {
      ci_method <- "quantile"
    }

    params <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
      ci_method = ci_method,
      ...
    )
  } else {
    # set default method for CI
    if (is.null(ci_method) || missing(ci_method)) {
      ci_method <- "wald"
    }

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
    ci_method = ci_method,
    p_adjust = p_adjust,
    robust = robust,
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
                                 ci_method = NULL,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 summary = getOption("parameters_summary", FALSE),
                                 verbose = TRUE,
                                 df_method = ci_method,
                                 ...) {

  # set default
  if (is.null(ci_method)) {
    ci_method <- ifelse(isTRUE(bootstrap), "quantile", "profile")
  }

  ## TODO remove later
  if (!missing(df_method) && !identical(ci_method, df_method)) {
    warning(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."), call. = FALSE)
    ci_method <- df_method
  }

  if (insight::n_obs(model) > 1e4 && identical(ci_method, "profile")) {
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
