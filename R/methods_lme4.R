# Package lme4; .merMod, .lmerMod

############# .lmerMod -----------------


#' p-values for Mixed Models
#'
#' This function attempts to return, or compute, p-values of mixed models.
#'
#' @param model A statistical model.
#' @param method For mixed models, can be \code{\link[=p_value_wald]{"wald"}} (default), \code{\link[=p_value_ml1]{"ml1"}}, \code{\link[=p_value_betwithin]{"betwithin"}}, \code{\link[=p_value_satterthwaite]{"satterthwaite"}} or \code{\link[=p_value_kenward]{"kenward"}}. For models that are supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, may also be \code{method = "robust"} to compute p-values based ob robust standard errors.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @details By default, p-values are based on Wald-test approximations (see \code{\link{p_value_wald}}). For certain situations, the "m-l-1" rule might be a better approximation. That is, for \code{method = "ml1"}, \code{\link{p_value_ml1}} is called. For \code{lmerMod} objects, if \code{method = "kenward"}, p-values are based on Kenward-Roger approximations, i.e. \code{\link{p_value_kenward}} is called, and \code{method = "satterthwaite"} calls \code{\link{p_value_satterthwaite}}.
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @note \code{p_value_robust()} resp. \code{p_value(method = "robust")}
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   \code{vcov_estimation = "CR"} for cluster-robust standard errors) and will
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
#' @param effects Should parameters for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @param details Logical, if \code{TRUE}, a summary of the random effects is included. See \code{\link{random_parameters}} for details.
#' @param df_method Method for computing degrees of freedom for p values, standard errors and confidence intervals (CI). May be \code{"wald"} (default, see \code{\link{degrees_of_freedom}}), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"betwithin"} (see \code{\link{dof_betwithin}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}) or \code{"kenward"} (see \code{\link{dof_kenward}}). The options \code{df_method = "boot"}, \code{df_method = "profile"} and \code{df_method = "uniroot"} only affect confidence intervals; in this case, bootstrapped resp. profiled confidence intervals are computed. \code{"uniroot"} only applies to models of class \code{glmmTMB}. Note that when \code{df_method} is not \code{"wald"}, robust standard errors etc. cannot be computed.
#' @param wb_component Logical, if \code{TRUE} and models contains within- and between-effects (see \code{\link{demean}}), the \code{Component} column will indicate which variables belong to the within-effects, between-effects, and cross-level interactions. By default, the \code{Component} column indicates, which parameters belong to the conditional or zero-inflated component of the model.
#' @inheritParams model_parameters.default
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
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
#'   model_parameters(model, details = TRUE)
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
                                    df_method = "wald",
                                    iterations = 1000,
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    robust = FALSE,
                                    details = FALSE,
                                    p_adjust = NULL,
                                    wb_component = TRUE,
                                    verbose = TRUE,
                                    ...) {

  # p-values, CI and se might be based of wald, or KR
  df_method <- tolower(df_method)
  df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin", "satterthwaite", "kenward", "boot", "profile", "uniroot"))

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <-
      .extract_parameters_mixed(
        model,
        ci = ci,
        df_method = df_method,
        robust = robust,
        standardize = standardize,
        p_adjust = p_adjust,
        wb_component = wb_component,
        ...
      )
  }


  if (exponentiate) params <- .exponentiate_parameters(params)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    bootstrap,
    iterations,
    df_method,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
  }

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
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param method For mixed models, can be \code{\link[=p_value_wald]{"wald"}} (default), \code{\link[=p_value_ml1]{"ml1"}} or \code{\link[=p_value_betwithin]{"betwithin"}}. For linear mixed model, can also be \code{\link[=p_value_satterthwaite]{"satterthwaite"}}, \code{\link[=p_value_kenward]{"kenward"}} or \code{"boot"} (see \code{lme4::confint.merMod}). For (generalized) linear models, can be \code{"robust"} to compute confidence intervals based on robust covariance matrix estimation, and for generalized linear models and models from packages \pkg{lme4} or \pkg{glmmTMB}, may also be \code{"profile"}, \code{"uniroot"} or \code{"wald"} (default).
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed.
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams p_value
#' @inheritParams ci_wald
#'
#' @return A data frame containing the CI bounds.
#'
#' @note \code{ci_robust()} resp. \code{ci(method = "robust")}
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   \code{vcov_estimation = "CR"} for cluster-robust standard errors) and will
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
#' @importFrom stats profile
#' @export
ci.merMod <- function(x,
                      ci = 0.95,
                      method = c("wald", "ml1", "betwithin", "satterthwaite", "kenward", "boot", "profile"),
                      ...) {
  method <- tolower(method)
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(model = x, ci = ci, dof = Inf)

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
    pp <- stats::profile(x)
    out <- lapply(ci, function(i) .ci_profile_merMod(x, ci = i, profiled = pp, ...))
    out <- do.call(rbind, out)
  }

  out
}


#' @rdname standard_error
#' @export
standard_error.merMod <- function(model, effects = c("fixed", "random"), method = NULL, ...) {
  effects <- match.arg(effects)
  if (is.null(method)) method <- "wald"
  robust <- !is.null(method) && method == "robust"

  if (effects == "random") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required to calculate standard errors for random effects. Please install it.")
    }

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
  } else {
    if (isTRUE(robust)) {
      standard_error_robust(model, ...)
    } else {
      # Classic and Satterthwaite SE
      if (method %in% c("wald", "satterthwaite")) {
        .data_frame(
          Parameter = insight::find_parameters(model,
                                               effects = "fixed",
                                               component = "conditional",
                                               flatten = TRUE
          ),
          SE = .get_se_from_summary(model)
        )
        # ml1 approx
      } else if (method == "ml1") {
        se_ml1(model)
      } else if (method == "betwithin") {
        se_betwithin(model)
        # Kenward approx
      } else if (method %in% c("kenward", "kr")) {
        se_kenward(model)
      }
    }
  }
}


#' @rdname p_value.lmerMod
#' @export
p_value.merMod <- p_value.cpglmm
