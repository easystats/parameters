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
#'   arguments are \code{digits}, \code{p_digits}, \code{ci_digits} and
#'   \code{footer_digits} to set the number of digits for the output.
#'   \code{group} can also be passed to the \code{print()} method. See details
#'   in \code{\link{print.parameters_model}} and 'Examples' in
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
#' @details
#' \subsection{Standardization of model coefficients}{
#'   Standardization is based on \code{\link[effectsize:standardize_parameters]{standardize_parameters()}}.
#'   In case of \code{standardize = "refit"}, the data used to fit the model
#'   will be standardized and the model is completely refitted. In such cases,
#'   standard errors and confidence intervals refer to the standardized
#'   coefficient. The default, \code{standardize = "refit"}, never standardizes
#'   categorical predictors (i.e. factors), which may be a different behaviour
#'   compared to other R packages or other software packages (like SPSS).
#'   To mimic behaviour of SPSS or packages such as \pkg{lm.beta}, use
#'   \code{standardize = "basic"}.
#'   }
#' \subsection{Methods of standardization}{
#'   For full details, please refer to \code{\link[effectsize:standardize_parameters]{standardize_parameters()}}.
#'   \describe{
#'     \item{\strong{refit}}{
#'       This method is based on a complete model re-fit with a standardized version
#'       of the data. Hence, this method is equal to standardizing the variables
#'       before fitting the model. It is the "purest" and the most accurate
#'       (Neter et al., 1989), but it is also the most computationally costly and
#'       long (especially for heavy models such as Bayesian models).The
#'       \code{robust} argument (default to \code{FALSE}) enables a robust standardization
#'       of data, i.e., based on the \code{median} and \code{MAD} instead of the
#'       \code{mean} and \code{SD}.
#'     }
#'     \item{\strong{posthoc}}{
#'       Post-hoc standardization of the parameters, aiming at emulating the
#'       results obtained by \code{"refit"} without refitting the model. The coefficients
#'       are divided by the standard deviation (or MAD if \code{robust=TRUE}) of
#'       the outcome (which becomes their expression 'unit'). Then, the coefficients
#'       related to numeric variables are additionally multiplied by the standard
#'       deviation (or MAD) of the related terms, so that they correspond to
#'       changes of 1 SD of the predictor. This does not apply to binary
#'       variables or factors, so the coefficients are still related to changes in
#'       levels. This method is not accurate and tend to give aberrant results when
#'       interactions are specified.
#'     }
#'     \item{\strong{smart}}{
#'       (Standardization of Model's parameters with Adjustment, Reconnaissance
#'       and Transformation - \emph{experimental}): Similar to \code{method="posthoc"}
#'       in that it does not involve model refitting. The difference is that the
#'       SD (or MAD) of the response is computed on the relevant section of the
#'       data. For instance, if a factor with 3 levels A (the intercept), B and C
#'       is entered as a predictor, the effect corresponding to B vs. A will be
#'       scaled by the variance of the response at the intercept only. As a results,
#'       the coefficients for effects of factors are similar to a Glass' delta.
#'     }
#'     \item{\strong{basic}}{
#'       This method is similar to \code{method="posthoc"}, but treats all
#'       variables as continuous: it also scales the coefficient by the standard
#'       deviation of model's matrix' parameter of factors levels (transformed to
#'       integers) or binary predictors. Although being inappropriate for these cases,
#'       this method is the one implemented by default in other software packages,
#'       such as \code{lm.beta::lm.beta()}.
#'     }
#'     \item{\strong{pseudo} (\emph{for 2-level (G)LMMs only})}{
#'       In this (post-hoc) method, the response and the predictor are standardized
#'       based on the level of prediction (levels are detected with \code{\link{check_heterogeneity}}:
#'       Predictors are standardized based on their SD at level of prediction
#'       (see also \code{\link{demean}}). The outcome (in linear LMMs) is
#'       standardized based on a fitted random-intercept-model, where
#'       \code{sqrt(random-intercept-variance)} is used for level 2 predictors,
#'       and \code{sqrt(residual-variance)} is used for level 1 predictors
#'       (Hoffman 2015, page 342). A warning is given when a within-group variable
#'       is found to have access between-group variance.
#'     }
#'   }
#' }
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
#'   'Details' in \code{\link[effectsize]{standardize_parameters}}.
#'   \strong{Important:} Categorical predictors (i.e. factors) are \emph{never}
#'   standardized by default, which may be a different behaviour compared to
#'   other R packages or other software packages (like SPSS). If standardizing
#'   categorical predictors is desired, either use \code{standardize="basic"}
#'   to mimic behaviour of SPSS or packages such as \pkg{lm.beta}, or standardize
#'   the data with \code{effectsize::standardize(force=TRUE)} before fitting
#'   the model. Robust estimation (i.e. \code{robust=TRUE}) of standardized
#'   parameters only works when \code{standardize="refit"}.
#' @param exponentiate Logical, indicating whether or not to exponentiate the
#'   the coefficients (and related confidence intervals). This is typical for
#'   logistic regression, or more generally speaking, for models with log
#'   or logit links. \strong{Note:} Delta-method standard errors are also
#'   computed (by multiplying the standard errors by the transformed
#'   coefficients). This is to mimic behaviour of other software packages, such
#'   as Stata, but these standard errors poorly estimate uncertainty for the
#'   transformed coefficient. The transformed confidence interval more clearly
#'   captures this uncertainty. For \code{compare_parameters()},
#'   \code{exponentiate = "nongaussian"} will only exponentiate coefficients
#'   from non-Gaussian families.
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
#' @param parameters Character containing a regular expression pattern
#'   that describes the parameters that should be returned from the data frame, or
#'   a named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If \code{parameters} is a character vector, every
#'   parameter in the \emph{"Parameters"} column that matches the regular expression
#'   in \code{parameters} will be selected from the returned data frame. For
#'   instance, in order to include all parametres that do \emph{NOT} start with
#'   "Sepal", one can write \code{"^Sepal"} (the caret symbol \code{^} being used
#'   to omit certain patterns). Furthermore, if \code{parameters} has more than
#'   one element, these will be merged with an \code{OR} operator into a regular
#'   expression pattern like this: \code{"(one|two|three)"}. If \code{parameters}
#'   is a named list of regular expression patterns, the names of the list-element
#'   should equal the column name where selection should be applied. This is useful
#'   for model objects where \code{model_parameters()} returns multiple columns
#'   with parameter components, like in \code{\link{model_parameters.lavaan}}.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to or from other methods. For instance, when
#'   \code{bootstrap = TRUE}, arguments like \code{type} or \code{parallel} are
#'   passed down to \code{bootstrap_model()}, and arguments like \code{ci_method}
#'   are passed down to \code{\link[bayestestR]{describe_posterior}}.
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
                                 summary = FALSE,
                                 verbose = TRUE,
                                 ...) {
  if (insight::n_obs(model) > 1e4 && df_method == "profile") {
    message(insight::format_message("Profiled confidence intervals may take longer time to compute. Use 'df_method=\"wald\"' for faster computation of CIs."))
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
    summary = summary,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
