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
#'   conditional or zero-inflated component of the model.
#' @param include_sigma Logical, if `TRUE`, includes the residual standard
#'   deviation. For mixed models, this is defined as the sum of the distribution-specific
#'   variance and the variance for the additive overdispersion term (see
#'   [insight::get_variance()] for details). Defaults to `FALSE` for mixed models
#'   due to the longer computation time.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.stanreg
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @section Confidence intervals for random effect variances:
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
#' standard errors for constructing the intervals. Due to the transformation,
#' the intervals are asymmetrical, however, they are within the correct bounds
#' (i.e. no negative interval for the SD, and the interval for the correlations
#' is within the range from -1 to +1).
#'
#' - For models of class `glmmTMB`, confidence intervals for random effect
#' variances always use a Wald t-distribution approximation.
#'
#' \cr \cr Note that confidence intervals for random effects from **lme4** models
#' that use the normal-distribution approximation (i.e. when `ci_method` is
#' neither `"profile"` nor `"boot"`) are often an unreliable measure of
#' uncertainty. Thus, profiled or bootstrapped confidence intervals are
#' preferred if more precise intervals are required.
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
#' use `effects = "fixed"`. There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(parameters)
#' if (require("lme4")) {
#'   data(mtcars)
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model)
#' }
#' \donttest{
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   model_parameters(model, effects = "all")
#' }
#'
#' if (require("lme4")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model, bootstrap = TRUE, iterations = 50)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model,
                                    ci = .95,
                                    bootstrap = FALSE,
                                    ci_method = NULL,
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
                                    parameters = keep,
                                    verbose = TRUE,
                                    df_method = ci_method,
                                    include_sigma = FALSE,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    ...) {

  dots <- list(...)

  ## TODO remove later
  if (!missing(df_method) && !identical(ci_method, df_method)) {
    warning(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."), call. = FALSE)
    ci_method <- df_method
  }

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else {
      ci_method <- switch(insight::find_statistic(model),
        "t-statistic" = "residual",
        "wald"
      )
    }
  }

  # p-values, CI and se might be based of wald, or KR
  ci_method <- tolower(ci_method)

  if (isTRUE(bootstrap)) {
    ci_method <- match.arg(ci_method, c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai"))
  } else {
    ci_method <- match.arg(ci_method, choices = c("wald", "normal", "residual", "ml1", "betwithin", "satterthwaite", "kenward", "kr", "boot", "profile", "uniroot"))
  }

  # which component to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  params <- params_random <- params_variance <- NULL

  # post hoc standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      warning(insight::format_message("Standardizing coefficients only works for fixed effects of the mixed model."), call. = FALSE)
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
          warning(insight::format_message("Bootstrapping only returns fixed effects of the mixed model."), call. = FALSE)
        }
      }
    } else {
      args <- list(
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
        vcov_args = vcov_args)
      args <- c(args, dots)
      params <- do.call(".extract_parameters_mixed", args)
    }

    params$Effects <- "fixed"

    if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
      params <- .exponentiate_parameters(params, model, exponentiate)
    }
  }

  att <- attributes(params)

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  if (effects %in% c("random", "all") && isFALSE(group_level)) {
    params_variance <- .extract_random_variances(model, ci = ci, effects = effects, ci_method = ci_method)
  }

  # merge random and fixed effects, if necessary
  if (!is.null(params) && (!is.null(params_random) || !is.null(params_variance))) {
    params$Level <- NA
    params$Group <- ""

    if (!is.null(params_random)) {
      params <- params[match(colnames(params_random), colnames(params))]
    } else {
      params <- params[match(colnames(params_variance), colnames(params))]
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


  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
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
                                  effects = c("fixed", "random"),
                                  method = NULL,
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  ...) {


  dots <- list(...)

  effects <- match.arg(effects)

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
    args <- list(model,
                 vcov = vcov,
                 vcov_args = vcov_args)
    args <- c(args, dots)
    out <- do.call("standard_error.default", args)
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
