############# .merMod -----------------


#' @title Parameters from Mixed Models
#' @name model_parameters.merMod
#'
#' @description Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects (`"fixed"`), random
#'   effects (`"random"`), or both (`"all"`) be returned? Only applies
#'   to mixed models. May be abbreviated. If the calculation of random effects
#'   parameters takes too long, you may use `effects = "fixed"`.
#' @param wb_component Logical, if `TRUE` and models contains within- and
#'   between-effects (see `datawizard::demean()`), the `Component` column
#'   will indicate which variables belong to the within-effects,
#'   between-effects, and cross-level interactions. By default, the
#'   `Component` column indicates, which parameters belong to the
#'   conditional or zero-inflation component of the model.
#' @param include_sigma Logical, if `TRUE`, includes the residual standard
#'   deviation. For mixed models, this is defined as the sum of the distribution-specific
#'   variance and the variance for the additive overdispersion term (see
#'   [insight::get_variance()] for details). Defaults to `FALSE` for mixed models
#'   due to the longer computation time.
#' @param ci_random Logical, if `TRUE`, includes the confidence intervals for
#'   random effects parameters. Only applies if `effects` is not `"fixed"` and
#'   if `ci` is not `NULL`. Set `ci_random = FALSE` if computation of the model
#'   summary is too much time consuming. By default, `ci_random = NULL`, which
#'   uses a heuristic to guess if computation of confidence intervals for random
#'   effects is fast enough or not. For models with larger sample size and/or
#'   more complex random effects structures, confidence intervals will not be
#'   computed by default, for simpler models or fewer observations, confidence
#'   intervals will be included. Set explicitly to `TRUE` or `FALSE` to enforce
#'   or omit calculation of confidence intervals.
#' @param ... Arguments passed to or from other methods. For instance, when
#'   `bootstrap = TRUE`, arguments like `type` or `parallel` are
#'   passed down to `bootstrap_model()`.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.stanreg
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @section Confidence intervals for random effects variances:
#' For models of class `merMod` and `glmmTMB`, confidence intervals for random
#' effect variances can be calculated.
#'
#' - For models of from package **lme4**, when `ci_method` is either `"profile"`
#' or `"boot"`, and `effects` is either `"random"` or `"all"`, profiled resp.
#' bootstrapped confidence intervals are computed for the random effects.
#'
#' - For all other options of `ci_method`, and only when the **merDeriv**
#' package is installed, confidence intervals for random effects are based on
#' normal-distribution approximation, using the delta-method to transform
#' standard errors for constructing the intervals around the log-transformed
#' SD parameters. These are than back-transformed, so that random effect
#' variances, standard errors and confidence intervals are shown on the original
#' scale. Due to the transformation, the intervals are asymmetrical, however,
#' they are within the correct bounds (i.e. no negative interval for the SD,
#' and the interval for the correlations is within the range from -1 to +1).
#'
#' - For models of class `glmmTMB`, confidence intervals for random effect
#' variances always use a Wald t-distribution approximation.
#'
#' @section Singular fits (random effects variances near zero):
#' If a model is "singular", this means that some dimensions of the
#' variance-covariance matrix have been estimated as exactly zero. This
#' often occurs for mixed models with complex random effects structures.
#'
#' There is no gold-standard about how to deal with singularity and which
#' random-effects specification to choose. One way is to fully go Bayesian
#' (with informative priors). Other proposals are listed in the documentation
#' of [`performance::check_singularity()`]. However, since version 1.1.9, the
#' **glmmTMB** allows to use priors in a frequentist framework, too. One
#' recommendation is to use a Gamma prior (_Chung et al. 2013_). The mean may
#' vary from 1 to very large values (like `1e8`), and the shape parameter should
#' be set to a value of 2.5. You can then `update()` your model with the specified
#' prior. In **glmmTMB**, the code would look like this:
#'
#' ````
#' # "model" is an object of class gmmmTMB
#'prior <- data.frame(
#'  prior = "gamma(1, 2.5)", # mean can be 1, but even 1e8
#'  class = "ranef" # for random effects
#' )
#' model_with_priors <- update(model, priors = prior)
#' ```
#'
#' Large values for the mean parameter of the Gamma prior have no large impact
#' on the random effects variances in terms of a "bias". Thus, if `1` doesn't
#' fix the singular fit, you can try larger values.
#'
#' @section Dispersion parameters in *glmmTMB*:
#' For some models from package **glmmTMB**, both the dispersion parameter and
#' the residual variance from the random effects parameters are shown. Usually,
#' these are the same but presented on different scales, e.g.
#'
#' ```
#' model <- glmmTMB(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
#' exp(fixef(model)$disp) # 0.09902987
#' sigma(model)^2         # 0.09902987
#' ```
#'
#' For models where the dispersion parameter and the residual variance are
#' the same, only the residual variance is shown in the output.
#'
#' @seealso [insight::standardize_names()] to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note If the calculation of random effects parameters takes too long, you may
#' use `effects = "fixed"`. There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#' implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @references
#' Chung Y, Rabe-Hesketh S, Dorie V, Gelman A, and Liu J. 2013. "A Nondegenerate
#' Penalized Likelihood Estimator for Variance Parameters in Multilevel Models."
#' Psychometrika 78 (4): 685–709. \doi{10.1007/s11336-013-9328-2}
#'
#' @examplesIf require("lme4") && require("glmmTMB")
#' library(parameters)
#' data(mtcars)
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model)
#'
#' \donttest{
#' data(Salamanders, package = "glmmTMB")
#' model <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' model_parameters(model, effects = "all")
#'
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model, bootstrap = TRUE, iterations = 50, verbose = FALSE)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model,
                                    ci = 0.95,
                                    ci_method = NULL,
                                    ci_random = NULL,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "all",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    wb_component = TRUE,
                                    summary = getOption("parameters_mixed_summary", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    include_sigma = FALSE,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    ...) {
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else {
      ci_method <- switch(insight::find_statistic(model),
        `t-statistic` = "residual",
        "wald"
      )
    }
  }

  # p-values, CI and se might be based of wald, or KR
  ci_method <- tolower(ci_method)

  if (isTRUE(bootstrap)) {
    ci_method <- match.arg(
      ci_method,
      choices = c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai")
    )
  } else {
    ci_method <- match.arg(
      ci_method,
      choices = c(
        "wald", "normal", "residual", "ml1", "betwithin", "satterthwaite",
        "kenward", "kr", "boot", "profile", "uniroot"
      )
    )
  }

  # which component to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  params <- params_random <- params_variance <- NULL

  # post hoc standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_alert(
        "Standardizing coefficients only works for fixed effects of the mixed model."
      )
    }
    effects <- "fixed"
  }

  # for refit, we completely refit the model, than extract parameters,
  # ci etc. as usual - therefor, we set "standardize" to NULL
  if (!is.null(standardize) && standardize == "refit") {
    model <- datawizard::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  if (effects %in% c("fixed", "all")) {
    # Processing
    if (bootstrap) {
      params <- bootstrap_parameters(
        model,
        iterations = iterations,
        ci = ci,
        ...
      )
      if (effects != "fixed") {
        effects <- "fixed"
        if (verbose) {
          insight::format_alert("Bootstrapping only returns fixed effects of the mixed model.")
        }
      }
    } else {
      fun_args <- list(
        model,
        ci = ci,
        ci_method = ci_method,
        standardize = standardize,
        p_adjust = p_adjust,
        wb_component = wb_component,
        keep_parameters = keep,
        drop_parameters = drop,
        verbose = verbose,
        include_sigma = include_sigma,
        summary = summary,
        vcov = vcov,
        vcov_args = vcov_args
      )
      fun_args <- c(fun_args, dots)
      params <- do.call(".extract_parameters_mixed", fun_args)
    }

    params$Effects <- "fixed"

    # exponentiate coefficients and SE/CI, if requested
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  att <- attributes(params)

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  if (effects %in% c("random", "all") && isFALSE(group_level)) {
    params_variance <- .extract_random_variances(
      model,
      ci = ci,
      effects = effects,
      ci_method = ci_method,
      ci_random = ci_random,
      verbose = verbose
    )
  }

  # merge random and fixed effects, if necessary
  if (!is.null(params) && (!is.null(params_random) || !is.null(params_variance))) {
    params$Level <- NA
    params$Group <- ""

    if (is.null(params_random)) {
      params <- params[match(colnames(params_variance), colnames(params))]
    } else {
      params <- params[match(colnames(params_random), colnames(params))]
    }
  }

  params <- rbind(params, params_random, params_variance)
  # remove empty column
  if (!is.null(params$Level) && all(is.na(params$Level))) {
    params$Level <- NULL
  }

  # due to rbind(), we lose attributes from "extract_parameters()",
  # so we add those attributes back here...
  if (!is.null(att)) {
    attributes(params) <- utils::modifyList(att, attributes(params))
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci = ci,
    exponentiate,
    bootstrap,
    iterations,
    ci_method = ci_method,
    p_adjust = p_adjust,
    verbose = verbose,
    summary = summary,
    group_level = group_level,
    wb_component = wb_component,
    ...
  )


  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @rdname ci.default
#' @export
ci.merMod <- function(x,
                      ci = 0.95,
                      dof = NULL,
                      method = "wald",
                      iterations = 500,
                      ...) {
  method <- tolower(method)
  method <- match.arg(method, choices = c(
    "wald", "ml1", "betwithin", "kr",
    "satterthwaite", "kenward", "boot",
    "profile", "residual", "normal"
  ))

  # bootstrapping
  if (method == "boot") {
    out <- lapply(ci, function(ci, x) .ci_boot_merMod(x, ci, iterations, ...), x = x)
    out <- do.call(rbind, out)
    row.names(out) <- NULL

    # profiled CIs
  } else if (method == "profile") {
    pp <- suppressWarnings(stats::profile(x, which = "beta_"))
    out <- lapply(ci, function(i) .ci_profile_merMod(x, ci = i, profiled = pp, ...))
    out <- do.call(rbind, out)

    # all others
  } else {
    out <- .ci_generic(model = x, ci = ci, dof = dof, method = method, ...)
  }

  out
}


#' @rdname standard_error
#' @export
standard_error.merMod <- function(model,
                                  effects = "fixed",
                                  method = NULL,
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  ...) {
  dots <- list(...)
  effects <- match.arg(effects, choices = c("fixed", "random"))

  if (effects == "random") {
    out <- .standard_errors_random(model)
    return(out)
  }

  if (is.null(method)) {
    method <- "wald"
  } else if ((method == "robust" && is.null(vcov)) ||
    # deprecated argument
    isTRUE(list(...)[["robust"]])) {
    vcov <- "vcovHC"
  }

  if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
    fun_args <- list(model,
      vcov = vcov,
      vcov_args = vcov_args
    )
    fun_args <- c(fun_args, dots)
    out <- do.call("standard_error.default", fun_args)
    return(out)
  }

  # kenward approx
  if (method %in% c("kenward", "kr")) {
    out <- se_kenward(model)
    return(out)
  } else {
    # Classic and Satterthwaite SE
    out <- se_mixed_default(model)
    return(out)
  }
}



# helpers --------------

.standard_errors_random <- function(model) {
  insight::check_if_installed("lme4")

  rand.se <- lme4::ranef(model, condVar = TRUE)
  n.groupings <- length(rand.se)

  for (m in 1:n.groupings) {
    vars.m <- attr(rand.se[[m]], "postVar")

    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]

    names.full <- dimnames(rand.se[[m]])
    rand.se[[m]] <- array(NA, c(J, K))

    for (j in 1:J) {
      rand.se[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
    }
    dimnames(rand.se[[m]]) <- list(names.full[[1]], names.full[[2]])
  }
  rand.se
}



se_mixed_default <- function(model) {
  params <- insight::find_parameters(model,
    effects = "fixed",
    component = "conditional",
    flatten = TRUE
  )
  .data_frame(Parameter = params, SE = .get_se_from_summary(model))
}




#' @export
p_value.merMod <- p_value.cpglmm
