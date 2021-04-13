# default methods, glm (almost default)

#################### .default ----------------------

#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations, t-tests, ...} (\code{htest}, \code{pairwise.htest})}
#'  \item{\link[=model_parameters.aov]{ANOVAs} (\code{aov}, \code{anova}, \pkg{afex}, ...)}
#'  \item{\link[=model_parameters.default]{Regression models} (\code{lm}, \code{glm}, \pkg{survey}, ...)}
#'  \item{\link[=model_parameters.cgam]{Additive models} (\code{gam}, \code{gamm}, ...)}
#'  \item{\link[=model_parameters.zcpglm]{Zero-inflated models} (\code{hurdle}, \code{zeroinfl}, \code{zerocount})}
#'  \item{\link[=model_parameters.mlm]{Multinomial, ordinal and cumulative link models} (\code{bracl}, \code{multinom}, \code{mlm}, ...)}
#'  \item{\link[=model_parameters.averaging]{Other special models} (\code{model.avg}, \code{betareg}, \code{glmx}, ...)}
#'  \item{\link[=model_parameters.merMod]{Mixed models} (\pkg{lme4}, \pkg{nlme}, \pkg{glmmTMB}, \pkg{afex}, ...)}
#'  \item{\link[=model_parameters.BFBayesFactor]{Bayesian tests} (\pkg{BayesFactor})}
#'  \item{\link[=model_parameters.stanreg]{Bayesian models} (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, \pkg{blavaan}, ...)}
#'  \item{\link[=model_parameters.principal]{PCA and FA} (\pkg{psych})}
#'  \item{\link[=model_parameters.lavaan]{CFA and SEM} (\pkg{lavaan})}
#'  \item{\link[=model_parameters.kmeans]{Cluster models} (k-means, ...)}
#'  \item{\link[=model_parameters.rma]{Meta-Analysis via linear (mixed) models} (\code{rma}, \code{metaplus}, \pkg{metaBMA}, ...)}
#'  \item{\link[=model_parameters.glht]{Hypothesis testing} (\code{glht}, \pkg{PMCMRplus})}
#'  \item{\link[=model_parameters.t1way]{Robust statistical tests} (\pkg{WRS2})}
#'  \item{\link[=model_parameters.mira]{Multiply imputed repeated analyses} (\code{mira})}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods. Non-documented
#'   arguments are \code{digits}, \code{p_digits} and \code{ci_digits} to set
#'   the number of digits for the output. See 'Examples' in
#'   \code{\link{model_parameters.default}}.
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note The \code{\link[=print.parameters_model]{print()}} method has several
#'   arguments to tweak the output. There is also a
#'   \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method}
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}, and a dedicated
#'   method for use inside rmarkdown files,
#'   \code{\link[=print_md.parameters_model]{print_md()}}.
#'
#' @details Standardization is based on
#'   \code{\link[effectsize:standardize_parameters]{standardize_parameters()}}.
#'   In case of \code{standardize = "refit"}, the data used to fit the model
#'   will be standardized and the model is completely refitted. In such cases,
#'   standard errors and confidence intervals refer to the standardized
#'   coefficient.
#'
#' @section Labeling the Degrees of Freedom:
#' Throughout the \pkg{parameters} package, we decided to label the residual
#' degrees of freedom \emph{df_error}. The reason for this is that these degrees
#' of freedom not always refer to the residuals. For certain models, they refer
#' to the estimate error - in a linear model these are the same, but in - for
#' instance - any mixed effects model, this isn't strictly true. Hence, we
#' think that \code{df_error} is the most generic label for these degrees of
#' freedom.
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
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
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param bootstrap Should estimates be based on bootstrapped model? If
#'   \code{TRUE}, then arguments of \link[=model_parameters.stanreg]{Bayesian
#'   regressions} apply (see also
#'   \code{\link[=bootstrap_parameters]{bootstrap_parameters()}}).
#' @param iterations The number of bootstrap replicates. This only apply in the
#'   case of bootstrapped frequentist models.
#' @param standardize The method used for standardizing the parameters. Can be
#'   \code{"refit"}, \code{"posthoc"}, \code{"smart"}, \code{"basic"},
#'   \code{"pseudo"} or \code{NULL} (default) for no standardization. See
#'   'Details' in \code{\link[effectsize]{standardize_parameters}}. Note that
#'   robust estimation (i.e. \code{robust=TRUE}) of standardized parameters only
#'   works when \code{standardize="refit"}.
#' @param exponentiate Logical, indicating whether or not to exponentiate the
#'   the coefficients (and related confidence intervals). This is typical for,
#'   say, logistic regressions, or more generally speaking: for models with log
#'   or logit link. \strong{Note:} standard errors are also transformed (by
#'   multiplying the standard errors with the exponentiated coefficients), to
#'   mimic behaviour of other software packages, such as Stata. For
#'   \code{compare_parameters()}, \code{exponentiate = "nongaussian"} will only
#'   exponentiate coefficients for all models except those from Gaussian family.
#' @param robust Logical, if \code{TRUE}, robust standard errors are calculated
#'   (if possible), and confidence intervals and p-values are based on these
#'   robust standard errors. Additional arguments like \code{vcov_estimation} or
#'   \code{vcov_type} are passed down to other methods, see
#'   \code{\link[=standard_error_robust]{standard_error_robust()}} for details
#'   and \href{https://easystats.github.io/parameters/articles/model_parameters_robust.html}{this vignette}
#'   for working examples.
#' @param component Model component for which parameters should be shown. May be
#'   one of \code{"conditional"}, \code{"precision"} (\pkg{betareg}),
#'   \code{"scale"} (\pkg{ordinal}), \code{"extra"} (\pkg{glmx}),
#'   \code{"marginal"} (\pkg{mfx}), \code{"conditional"} or \code{"full"} (for
#'   \code{MuMIn::model.avg()}) or \code{"all"}.
#' @param p_adjust Character vector, if not \code{NULL}, indicates the method to
#'   adjust p-values. See \code{\link[stats]{p.adjust}} for details. Further
#'   possible adjustment methods are \code{"tukey"}, \code{"scheffe"},
#'   \code{"sidak"} and \code{"none"} to explicitly disable adjustment for
#'   \code{emmGrid} objects (from \pkg{emmeans}).
#' @param df_method Method for computing degrees of freedom for confidence
#'   intervals (CI). Only applies to models of class \code{glm} or \code{polr}.
#'   May be \code{"profile"} or \code{"wald"}.
#' @param summary Logical, if \code{TRUE}, prints summary information about the
#'   model (model formula, number of observations, residual standard deviation
#'   and more).
#' @param parameters Regular expression pattern that describes the parameters
#'   that should be returned.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to or from other methods. For instance, when
#'   \code{bootstrap = TRUE}, arguments like \code{ci_method} are passed down to
#'   \code{\link[bayestestR]{describe_posterior}}.
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to
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
                                     parameters = NULL,
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
        filter_parameters = parameters,
        verbose = verbose,
        ...
      )
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
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
                                      df_method = NULL,
                                      p_adjust = NULL,
                                      summary = FALSE,
                                      filter_parameters = NULL,
                                      verbose = TRUE,
                                      ...) {

  # to avoid "match multiple argument error", check if "component" was
  # already used as argument and passed via "...".
  mc <- match.call()
  comp_argument <- parse(text = .safe_deparse(mc))[[1]]$component

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
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
      df_method = df_method,
      p_adjust = p_adjust,
      filter_parameters = filter_parameters,
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
    df_method = df_method,
    p_adjust = p_adjust,
    summary = summary,
    verbose = verbose,
    ...
  )

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}




#################### .glm ----------------------


#' @importFrom insight n_obs
#' @rdname model_parameters.default
#' @export
model_parameters.glm <- function(model,
                                 ci = .95,
                                 df_method = "profile",
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {
  if (insight::n_obs(model) > 1e4 && df_method == "profile") {
    message("Profiled confidence intervals may take longer time to compute. Use 'df_method=\"wald\"' for faster computation of CIs.")
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    df_method = df_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    robust = robust,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
