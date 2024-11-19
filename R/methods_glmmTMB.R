# Package glmmTMB


# model_parameters -----


#' @title Parameters from Mixed Models
#' @name model_parameters.glmmTMB
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
#'   `bootstrap = TRUE`, arguments like `type` or `parallel` are passed down to
#'   `bootstrap_model()`.
#'
#' Further non-documented arguments are:
#'
#' - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the number of
#'   digits for the output. `groups` can be used to group coefficients. These
#'   arguments will be passed to the print-method, or can directly be used in
#'   `print()`, see documentation in [`print.parameters_model()`].
#' - If `s_value = TRUE`, the p-value will be replaced by the S-value in the
#'   output (cf. _Rafi and Greenland 2020_).
#' - `pd` adds an additional column with the _probability of direction_ (see
#'   [`bayestestR::p_direction()`] for details). Furthermore, see 'Examples' for
#'   this function.
#' - For developers, whose interest mainly is to get a "tidy" data frame of
#'   model summaries, it is recommended to set `pretty_names = FALSE` to speed
#'   up computation of the summary table.
#'
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.brmsfit
#' @inheritParams simulate_model
#'
#' @inheritSection model_parameters.zcpglm Model components
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
#' **glmmTMB** package allows to use priors in a frequentist framework, too. One
#' recommendation is to use a Gamma prior (_Chung et al. 2013_). The mean may
#' vary from 1 to very large values (like `1e8`), and the shape parameter should
#' be set to a value of 2.5. You can then `update()` your model with the specified
#' prior. In **glmmTMB**, the code would look like this:
#'
#' ```
#' # "model" is an object of class gmmmTMB
#' prior <- data.frame(
#'   prior = "gamma(1, 2.5)",  # mean can be 1, but even 1e8
#'   class = "ranef"           # for random effects
#' )
#' model_with_priors <- update(model, priors = prior)
#' ```
#'
#' Large values for the mean parameter of the Gamma prior have no large impact
#' on the random effects variances in terms of a "bias". Thus, if `1` doesn't
#' fix the singular fit, you can safely try larger values.
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
model_parameters.glmmTMB <- function(model,
                                     ci = 0.95,
                                     ci_method = "wald",
                                     ci_random = NULL,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     effects = "all",
                                     component = "all",
                                     group_level = FALSE,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     wb_component = TRUE,
                                     summary = getOption("parameters_mixed_summary", FALSE),
                                     include_info = getOption("parameters_mixed_info", FALSE),
                                     include_sigma = FALSE,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  insight::check_if_installed("glmmTMB")

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    verbose = verbose
  )

  # p-values, CI and se might be based on different df-methods
  ci_method <- .check_df_method(ci_method)

  # which components to return?
  effects <- insight::validate_argument(
    effects,
    c("fixed", "random", "all")
  )
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion")
  )

  # standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_warning(
        "Standardizing coefficients only works for fixed effects of the mixed model."
      )
    }
    effects <- "fixed"
  }

  # fix argument, if model has only conditional component
  cs <- stats::coef(summary(model))
  has_zeroinf <- insight::model_info(model, verbose = FALSE)$is_zero_inflated
  has_disp <- is.list(cs) && !is.null(cs$disp)

  if (!has_zeroinf && !has_disp && component != "conditional") {
    component <- "conditional"
  }

  params <- params_random <- params_variance <- NULL
  dispersion_param <- FALSE

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
          insight::format_warning("Bootstrapping only returns fixed effects of the mixed model.")
        }
      }
    } else {
      fun_args <- list(
        model,
        ci = ci,
        component = component,
        merge_by = c("Parameter", "Component"),
        standardize = standardize,
        effects = "fixed",
        ci_method = ci_method,
        p_adjust = p_adjust,
        keep_parameters = NULL,
        drop_parameters = NULL,
        verbose = verbose,
        vcov = NULL,
        vcov_args = NULL,
        keep_component_column = component != "conditional",
        include_sigma = include_sigma,
        wb_component = wb_component,
        include_info = include_info
      )
      fun_args <- c(fun_args, dot_args)
      params <- do.call(".extract_parameters_generic", fun_args)
    }

    # add dispersion parameter
    if (
      # must be glmmTMB
      inherits(model, "glmmTMB") &&
        # don't print dispersion if already present
        (is.null(component) || !"dispersion" %in% params$Component) &&
        # don't print dispersion for zi-component
        component %in% c("conditional", "all", "dispersion") &&
        # if effects = "fixed" and component = "conditional", don't include dispersion
        !(component == "conditional" && effects == "fixed")
    ) {
      dispersion_param <- insight::get_parameters(model, component = "dispersion")
      if (!is.null(dispersion_param)) {
        # add component column
        if (is.null(params$Component)) {
          params$Component <- "conditional"
        }
        params[nrow(params) + 1, ] <- NA
        params[nrow(params), "Parameter"] <- dispersion_param$Parameter[1]
        params[nrow(params), "Coefficient"] <- stats::sigma(model)
        params[nrow(params), "Component"] <- dispersion_param$Component[1]
        params[nrow(params), c("CI_low", "CI_high")] <- tryCatch(
          suppressWarnings(stats::confint(model, parm = "sigma", method = "wald", level = ci)[1:2]),
          error = function(e) {
            if (verbose) {
              insight::format_alert(
                "Cannot compute standard errors and confidence intervals for sigma parameter.",
                "Your model may suffer from singularity (see '?lme4::isSingular' and '?performance::check_singularity')." # nolint
              )
            }
            c(NA, NA)
          }
        )
        dispersion_param <- TRUE
      }
    }

    # exponentiate coefficients and SE/CI, if requested
    params <- .exponentiate_parameters(params, model, exponentiate)

    params$Effects <- "fixed"
  }

  att <- attributes(params)
  random_effects <- insight::find_random(model, flatten = TRUE)

  # check if any random effects at all
  if (!is.null(random_effects) && effects %in% c("random", "all")) {
    # add random parameters or variances
    if (isTRUE(group_level)) {
      params_random <- .extract_random_parameters(model, ci = ci, effects = effects, component = component)
      if (length(random_effects) > 1) {
        insight::format_alert(
          "Cannot extract confidence intervals for random variance parameters from models with more than one grouping factor." # nolint
        )
      }
    } else {
      params_variance <- .extract_random_variances(
        model,
        ci = ci,
        effects = effects,
        component = component,
        ci_method = ci_method,
        ci_random = ci_random,
        verbose = verbose
      )
      # remove redundant dispersion parameter
      if (isTRUE(dispersion_param) && !is.null(params) && !is.null(params$Component)) {
        disp <- which(params$Component == "dispersion")
        res <- which(params_variance$Group == "Residual")
        # check if we have dispersion parameter, and either no sigma
        # or sigma equals dispersion
        if (length(disp) > 0 &&
          length(res) > 0 &&
          isTRUE(all.equal(params_variance$Coefficient[res],
            params$Coefficient[disp],
            tolerance = 1e-5
          ))) {
          params <- params[-disp, ]
        }
      }
    }
  }

  # merge random and fixed effects, if necessary
  if (!is.null(params) && (!is.null(params_random) || !is.null(params_variance))) {
    params$Level <- NA
    params$Group <- ""
    # add component column
    if (!"Component" %in% colnames(params)) {
      if (component %in% c("zi", "zero_inflated")) {
        params$Component <- "zero_inflated"
      } else {
        params$Component <- "conditional"
      }
    }

    # reorder
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

  # filter parameters
  if (!is.null(keep) || !is.null(drop)) {
    params <- .filter_parameters(params, keep, drop, verbose = verbose)
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
    ci_method = ci_method,
    p_adjust = p_adjust,
    verbose = verbose,
    group_level = group_level,
    include_info = include_info,
    wb_component = wb_component,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


# ci -----


#' @export
ci.glmmTMB <- function(x,
                       ci = 0.95,
                       dof = NULL,
                       method = "wald",
                       component = "all",
                       verbose = TRUE,
                       ...) {
  method <- tolower(method)
  method <- insight::validate_argument(
    method,
    c("wald", "normal", "ml1", "betwithin", "profile", "uniroot", "robust")
  )
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion")
  )

  if (is.null(.check_component(x, component, verbose = verbose))) {
    return(NULL)
  }

  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(x)[1],
    function_name = "ci",
    verbose = verbose
  )

  # profiled CIs
  if (method == "profile") {
    if (length(ci) > 1) {
      pp <- stats::profile(x)
    } else {
      pp <- NULL
    }
    out <- lapply(ci, function(i) .ci_profile_glmmTMB(x, ci = i, profiled = pp, component = component, ...))
    do.call(rbind, out)

    # uniroot CIs
  } else if (method == "uniroot") {
    out <- lapply(ci, function(i) .ci_uniroot_glmmTMB(x, ci = i, component = component, ...))
    do.call(rbind, out)
  } else {
    # all other
    .ci_generic(model = x, ci = ci, dof = dof, method = method, component = component, ...)
  }
}


# standard_error -----

#' @export
standard_error.glmmTMB <- function(model,
                                   effects = "fixed",
                                   component = "all",
                                   verbose = TRUE,
                                   ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion")
  )
  effects <- insight::validate_argument(
    effects,
    c("fixed", "random")
  )

  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "standard_error",
    verbose = verbose
  )

  if (effects == "random") {
    if (!all(insight::check_if_installed(c("TMB", "glmmTMB"), quietly = TRUE))) {
      return(NULL)
    }

    s1 <- TMB::sdreport(model$obj, getJointPrecision = TRUE)
    s2 <- sqrt(s1$diag.cov.random)
    rand.ef <- glmmTMB::ranef(model)[[1]]

    rand.se <- lapply(rand.ef, function(.x) {
      cnt <- nrow(.x) * ncol(.x)
      s3 <- s2[1:cnt]
      s2 <- s2[-(1:cnt)]
      d <- as.data.frame(matrix(sqrt(s3), ncol = ncol(.x), byrow = TRUE))
      colnames(d) <- colnames(.x)
      d
    })
  } else {
    if (is.null(.check_component(model, component, verbose = verbose))) {
      return(NULL)
    }

    cs <- insight::compact_list(stats::coef(summary(model)))
    x <- lapply(names(cs), function(i) {
      .data_frame(
        Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
        SE = as.vector(cs[[i]][, 2]),
        Component = i
      )
    })

    se <- do.call(rbind, x)
    se$Component <- .rename_values(se$Component, "cond", "conditional")
    se$Component <- .rename_values(se$Component, "zi", "zero_inflated")
    se$Component <- .rename_values(se$Component, "disp", "dispersion")

    .filter_component(se, component)
  }
}




# simulate model -----


#' @export
simulate_model.glmmTMB <- function(model,
                                   iterations = 1000,
                                   component = "all",
                                   verbose = FALSE,
                                   ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zi", "zero_inflated", "dispersion")
  )
  info <- insight::model_info(model, verbose = FALSE)

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (!is.list(info)) {
    info <- NULL
  }

  has_zeroinflated <- !is.null(info) && isTRUE(info$is_zero_inflated)
  has_dispersion <- !is.null(info) && isTRUE(info$is_dispersion)


  # check component-argument ----

  if (component == "all") {
    if (!has_zeroinflated && !has_dispersion) {
      if (verbose) {
        insight::print_color(
          "No zero-inflation and dispersion components. Simulating from conditional parameters.\n",
          "red"
        )
      }
      component <- "conditional"
    } else if (!has_zeroinflated && has_dispersion) {
      if (verbose) {
        insight::print_color(
          "No zero-inflation component. Simulating from conditional and dispersion parameters.\n",
          "red"
        )
      }
      component <- c("conditional", "dispersion")
    } else if (has_zeroinflated && !has_dispersion) {
      if (verbose) {
        insight::print_color(
          "No dispersion component. Simulating from conditional and zero-inflation parameters.\n",
          "red"
        )
      }
      component <- c("conditional", "zero_inflated")
    }
  } else if (component %in% c("zi", "zero_inflated") && !has_zeroinflated) {
    insight::format_error("No zero-inflation model found.")
  } else if (component == "dispersion" && !has_dispersion) {
    insight::format_error("No dispersion model found.")
  }


  if (is.null(iterations)) iterations <- 1000

  if (all(component == c("conditional", "zero_inflated"))) {
    d1 <- .simulate_model(model, iterations, component = "conditional", ...)
    d2 <- .simulate_model(model, iterations, component = "zero_inflated", ...)
    colnames(d2) <- paste0(colnames(d2), "_zi")
    d <- cbind(d1, d2)
  } else if (all(component == c("conditional", "dispersion"))) {
    d1 <- .simulate_model(model, iterations, component = "conditional", ...)
    d2 <- .simulate_model(model, iterations, component = "dispersion", ...)
    colnames(d2) <- paste0(colnames(d2), "_disp")
    d <- cbind(d1, d2)
  } else if (all(component == "all")) {
    d1 <- .simulate_model(model, iterations, component = "conditional", ...)
    d2 <- .simulate_model(model, iterations, component = "zero_inflated", ...)
    d3 <- .simulate_model(model, iterations, component = "dispersion", ...)
    colnames(d2) <- paste0(colnames(d2), "_zi")
    colnames(d3) <- paste0(colnames(d3), "_disp")
    d <- cbind(d1, d2, d3)
  } else if (all(component == "conditional")) {
    d <- .simulate_model(model, iterations, component = "conditional", ...)
  } else if (all(component %in% c("zi", "zero_inflated"))) {
    d <- .simulate_model(model, iterations, component = "zero_inflated", ...)
  } else {
    d <- .simulate_model(model, iterations, component = "dispersion", ...)
  }

  class(d) <- c("parameters_simulate_model", class(d))
  attr(d, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  d
}




# simulate_parameters -----

#' @export
simulate_parameters.glmmTMB <- function(model,
                                        iterations = 1000,
                                        centrality = "median",
                                        ci = 0.95,
                                        ci_method = "quantile",
                                        test = "p-value",
                                        ...) {
  sim_data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(
    data = sim_data,
    test = test,
    centrality = centrality,
    ci = ci,
    ci_method = ci_method,
    ...
  )

  params <- insight::get_parameters(model, ...)
  if ("Effects" %in% colnames(params) && insight::n_unique(params$Effects) > 1) {
    out$Effects <- params$Effects
  }

  if ("Component" %in% colnames(params) && insight::n_unique(params$Component) > 1) {
    out$Component <- params$Component
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}
