# Package lme4; .merMod, .lmerMod

############# .lmerMod -----------------


#' p-values for Mixed Models
#'
#' This function attempts to return, or compute, p-values of mixed models.
#'
#' @param model A statistical model.
#' @param method For mixed models, can be [`"wald"()`][p_value_wald] (default), [`"ml1"()`][p_value_ml1], [`"betwithin"()`][p_value_betwithin], [`"satterthwaite"()`][p_value_satterthwaite] or [`"kenward"()`][p_value_kenward]. For models that are supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, may also be `method = "robust"` to compute p-values based ob robust standard errors.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @details By default, p-values are based on Wald-test approximations (see [p_value_wald()]). For certain situations, the "m-l-1" rule might be a better approximation. That is, for `method = "ml1"`, [p_value_ml1()] is called. For `lmerMod` objects, if `method = "kenward"`, p-values are based on Kenward-Roger approximations, i.e. [p_value_kenward()] is called, and `method = "satterthwaite"` calls [p_value_satterthwaite()].
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @note `p_value_robust()` resp. `p_value(method = "robust")`
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   `vcov_estimation = "CR"` for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @examples
#' if (require("lme4")) {
#'   data(iris)
#'   model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   p_value(model)
#' }
#' @export
p_value.lmerMod <- function(model, method = "wald", ...) {
  method <- tolower(method)
  method <- match.arg(method, c("wald", "ml1", "betwithin", "satterthwaite", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "ml1") {
    p_value_ml1(model, ...)
  } else if (method == "betwithin") {
    p_value_betwithin(model, ...)
  } else if (method == "satterthwaite") {
    p_value_satterthwaite(model, ...)
  } else if (method %in% c("kr", "kenward")) {
    p_value_kenward(model, ...)
  }
}




############# .merMod -----------------


#' @title Parameters from Mixed Models
#' @name model_parameters.merMod
#'
#' @description Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects (`"fixed"`), random
#'   effects (`"random"`), or both (`"all"`) be returned? Only applies
#'   to mixed models. May be abbreviated.
#' @param ci_method Method for computing degrees of freedom for p values,
#'   standard errors and confidence intervals (CI). By default (`NULL`),
#'   returns residual degrees of freedom for linear mixed models, or `Inf`
#'   for all other distributional families. May be `"wald"`,
#'   `"residual"` (for both see [degrees_of_freedom()]),
#'   `"ml1"` (see [dof_ml1()]), `"betwithin"` (see [dof_betwithin()]),
#'   `"satterthwaite"` (see [dof_satterthwaite()]) or `"kenward"` (see
#'   [dof_kenward()]). The options `"boot"`, `"profile"` and `"uniroot"` only
#'   affect confidence intervals; in this case, bootstrapped resp. profiled
#'   confidence intervals are computed. `"uniroot"` only applies to models of
#'   class `glmmTMB`. For models of class `lmerMod`, when `ci_method = "wald"`,
#'   residual degrees of freedom are returned. Note that when `ci_method` is
#'   not `NULL`, `"wald"` or `"residual"`, robust standard errors etc. cannot
#'   be computed.
#' @param wb_component Logical, if `TRUE` and models contains within- and
#'   between-effects (see `datawizard::demean()`), the `Component` column
#'   will indicate which variables belong to the within-effects,
#'   between-effects, and cross-level interactions. By default, the
#'   `Component` column indicates, which parameters belong to the
#'   conditional or zero-inflated component of the model.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.stanreg
#'
#' @section Confidence intervals for random effect variances:
#' When `df_method = "profile"` and `effects` is either `"random"` or `"all"`,
#' profiled confidence intervals are computed for the random effects. For all
#' other options of `df_method`, confidence intervals for random effects will
#' be missing.
#'
#' @seealso [insight::standardize_names()] to
#'   rename columns into a consistent, standardized naming scheme.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
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
                                    df_method = NULL,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "all",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    robust = FALSE,
                                    p_adjust = NULL,
                                    wb_component = TRUE,
                                    summary = FALSE,
                                    keep = NULL,
                                    drop = NULL,
                                    parameters = keep,
                                    verbose = TRUE,
                                    ...) {

  # set default
  if (is.null(df_method)) {
    df_method <- switch(insight::find_statistic(model),
      "t-statistic" = "residual",
      "wald"
    )
  }

  # p-values, CI and se might be based of wald, or KR
  df_method <- tolower(df_method)
  df_method <- match.arg(df_method, choices = c("wald", "residual", "ml1", "betwithin", "satterthwaite", "kenward", "boot", "profile", "uniroot"))

  # which component to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  params <- params_random <- params_variance <- NULL

  # standardize only works for fixed effects...
  if (!is.null(standardize)) {
    if (!missing(effects) && effects != "fixed" && verbose) {
      warning(insight::format_message("Standardizing coefficients only works for fixed effects of the mixed model."), call. = FALSE)
    }
    effects <- "fixed"
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
      params <- .extract_parameters_mixed(
        model,
        ci = ci,
        df_method = df_method,
        robust = robust,
        standardize = standardize,
        p_adjust = p_adjust,
        wb_component = wb_component,
        keep_parameters = keep,
        drop_parameters = drop,
        verbose = verbose,
        ...
      )
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
    params_variance <- .extract_random_variances(model, ci = ci, effects = effects, df_method = df_method)
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
    ci = ifelse(effects == "random" && isFALSE(group_level), NA, ci),
    exponentiate,
    bootstrap,
    iterations,
    df_method,
    p_adjust = p_adjust,
    verbose = verbose,
    summary = summary,
    group_level = group_level,
    ...
  )


  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @title Confidence Intervals (CI)
#' @name ci.merMod
#'
#' @description Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param method For mixed models, can be [`"wald"()`][p_value_wald]
#'   (default), [`"ml1"()`][p_value_ml1] or
#'   [`"betwithin"()`][p_value_betwithin]. For linear mixed model, can
#'   also be [`"satterthwaite"()`][p_value_satterthwaite],
#'   [`"kenward"()`][p_value_kenward] or `"boot"` (see
#'   `lme4::confint.merMod`). For (generalized) linear models, can be
#'   `"robust"` to compute confidence intervals based on robust covariance
#'   matrix estimation, and for generalized linear models and models from
#'   packages \pkg{lme4} or \pkg{glmmTMB}, may also be `"profile"`,
#'   `"uniroot"` or `"wald"` (default).
#' @param ... Arguments passed down to `standard_error_robust()` when
#'   confidence intervals or p-values based on robust standard errors should be
#'   computed.
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams p_value
#' @inheritParams ci_wald
#'
#' @return A data frame containing the CI bounds.
#'
#' @note `ci_robust()` resp. `ci(method = "robust")`
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   `vcov_estimation = "CR"` for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("glmmTMB")) {
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'
#'   ci(model)
#'   ci(model, component = "zi")
#' }
#' }
#' @export
ci.merMod <- function(x,
                      ci = 0.95,
                      method = c("wald", "ml1", "betwithin", "satterthwaite", "kenward", "boot", "profile", "residual"),
                      ...) {
  method <- tolower(method)
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(model = x, ci = ci, dof = Inf)

    # residual df
  } else if (method == "residual") {
    out <- ci_wald(model = x, ci = ci, dof = degrees_of_freedom(x, method = "residual"))

    # ml1 approx
  } else if (method == "ml1") {
    out <- ci_ml1(x, ci)

    # betwithin approx
  } else if (method == "betwithin") {
    out <- ci_betwithin(x, ci)

    # Satterthwaite
  } else if (method == "satterthwaite") {
    out <- ci_satterthwaite(x, ci)

    # Kenward approx
  } else if (method %in% c("kenward", "kr")) {
    out <- ci_kenward(x, ci)

    # bootstrapping
  } else if (method == "boot") {
    out <- lapply(ci, function(ci, x) .ci_boot_merMod(x, ci, ...), x = x)
    out <- do.call(rbind, out)
    row.names(out) <- NULL

    # profiles CIs
  } else if (method == "profile") {
    pp <- suppressWarnings(stats::profile(x, which = "beta_"))
    out <- lapply(ci, function(i) .ci_profile_merMod(x, ci = i, profiled = pp, ...))
    out <- do.call(rbind, out)
  }

  out
}


#' @rdname standard_error
#' @export
standard_error.merMod <- function(model,
                                  effects = c("fixed", "random"),
                                  method = NULL,
                                  ...) {
  effects <- match.arg(effects)

  if (effects == "random") {
    .standard_errors_random(model)
  } else {
    if (is.null(method)) method <- "wald"
    robust <- !is.null(method) && method == "robust"

    if (isTRUE(robust)) {
      standard_error_robust(model, ...)
    } else {
      # kenward approx
      if (method %in% c("kenward", "kr")) {
        se_kenward(model)
      } else {
        # Classic and Satterthwaite SE
        se_mixed_default(model)
      }
    }
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




#' @rdname p_value.lmerMod
#' @export
p_value.merMod <- p_value.cpglmm
