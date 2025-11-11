#' Degrees of Freedom (DoF)
#'
#' Estimate or extract degrees of freedom of models parameters.
#'
#' @param model A statistical model.
#' @param method Type of approximation for the degrees of freedom. Can be one of
#' the following:
#'
#'   + `"residual"` (aka `"analytical"`) returns the residual degrees of
#'     freedom, which usually is what [`stats::df.residual()`] returns. If a
#'     model object has no method to extract residual degrees of freedom, these
#'     are calculated as `n-p`, i.e. the number of observations minus the number
#'     of estimated parameters. If residual degrees of freedom cannot be extracted
#'     by either approach, returns `Inf`.
#'   + `"wald"` returns residual (aka analytical) degrees of freedom for models
#'     with t-statistic, `1` for models with Chi-squared statistic, and `Inf` for
#'     all other models. Also returns `Inf` if residual degrees of freedom cannot
#'     be extracted.
#'   + `"normal"` always returns `Inf`.
#'   + `"model"` returns model-based degrees of freedom, i.e. the number of
#'     (estimated) parameters.
#'   + For mixed models, can also be `"ml1"` (or `"m-l-1"`, approximation of
#'     degrees of freedom based on a "m-l-1" heuristic as suggested by _Elff et
#'     al. 2019_) or `"between-within"` (or `"betwithin"`).
#'   + For mixed models of class `merMod`, `type` can also be `"satterthwaite"`
#'     or `"kenward-roger"` (or `"kenward"`). See 'Details'.
#'
#' Usually, when degrees of freedom are required to calculate p-values or
#' confidence intervals, `type = "wald"` is likely to be the best choice in
#' most cases.
#' @param ... Currently not used.
#'
#' @note
#' In many cases, `degrees_of_freedom()` returns the same as `df.residuals()`,
#' or `n-k` (number of observations minus number of parameters). However,
#' `degrees_of_freedom()` refers to the model's *parameters* degrees of freedom
#' of the distribution for the related test statistic. Thus, for models with
#' z-statistic, results from `degrees_of_freedom()` and `df.residuals()` differ.
#' Furthermore, for other approximation methods like `"kenward"` or
#' `"satterthwaite"`, each model parameter can have a different degree of
#' freedom.
#'
#' @examplesIf require("lme4", quietly = TRUE)
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' dof(model)
#'
#' model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
#' dof(model)
#' \donttest{
#' model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' dof(model)
#'
#' if (require("rstanarm", quietly = TRUE)) {
#'   model <- stan_glm(
#'     Sepal.Length ~ Petal.Length * Species,
#'     data = iris,
#'     chains = 2,
#'     refresh = 0
#'   )
#'   dof(model)
#' }
#' }
#' @export
degrees_of_freedom <- function(model, method = "analytical", ...) {
  insight::get_df(x = model, type = method, ...)
}

#' @rdname degrees_of_freedom
#' @export
dof <- degrees_of_freedom


# Helper, check args ------------------------------

.dof_method_ok <- function(model, method, type = "df_method", verbose = TRUE, ...) {
  if (is.null(method)) {
    return(TRUE)
  }

  method <- tolower(method)

  # exceptions 1
  if (inherits(model, c("polr", "glm", "svyglm", "svyolr"))) {
    # fmt: skip
    if (method %in% c(
      "analytical", "any", "fit", "profile", "residual",
      "wald", "nokr", "likelihood", "normal"
    )) {
      return(TRUE)
    } else {
      if (verbose) {
        insight::format_alert(sprintf("`%s` must be one of \"wald\", \"residual\" or \"profile\". Using \"wald\" now.", type)) # nolint
      }
      return(FALSE)
    }
  }

  # exceptions 2
  if (inherits(model, c("phylolm", "phyloglm"))) {
    if (
      method %in%
        c("analytical", "any", "fit", "residual", "wald", "nokr", "normal", "boot")
    ) {
      return(TRUE)
    } else {
      if (verbose) {
        insight::format_alert(sprintf(
          "`%s` must be one of \"wald\", \"normal\" or \"boot\". Using \"wald\" now.",
          type
        ))
      }
      return(FALSE)
    }
  }

  info <- insight::model_info(model, verbose = FALSE)
  if (!is.null(info) && isFALSE(info$is_mixed) && method == "boot") {
    if (verbose) {
      insight::format_alert(sprintf(
        "`%s=boot` only works for mixed models of class `merMod`. To bootstrap this model, use `bootstrap=TRUE, ci_method=\"bcai\"`.",
        type
      ))
    }
    return(TRUE)
  }

  # fmt: skip
  if (is.null(info) || !info$is_mixed) {
    if (!(method %in% c(
      "analytical", "any", "fit", "betwithin", "nokr", "wald", "ml1",
      "profile", "boot", "uniroot", "residual", "normal"
    ))) {
      if (verbose) {
        insight::format_alert(sprintf(
          "`%s` must be one of \"residual\", \"wald\", \"normal\", \"profile\", \"boot\", \"uniroot\", \"betwithin\" or \"ml1\". Using \"wald\" now.",
          type
        ))
      }
      return(FALSE)
    }
    return(TRUE)
  }

  # fmt: skip
  if (!(method %in% c(
    "analytical", "any", "fit", "satterthwaite", "betwithin", "kenward",
    "kr", "nokr", "wald", "ml1", "profile", "boot", "uniroot", "residual",
    "normal"
  ))) {
    if (verbose) {
      insight::format_alert(sprintf(
        "`%s` must be one of \"residual\", \"wald\", \"normal\", \"profile\", \"boot\", \"uniroot\", \"kenward\", \"satterthwaite\", \"betwithin\" or \"ml1\". Using \"wald\" now.",
        type
      ))
    }
    return(FALSE)
  }

  if (!info$is_linear && method %in% c("satterthwaite", "kenward", "kr")) {
    if (verbose) {
      insight::format_alert(sprintf(
        "`%s`-degrees of freedoms are only available for linear mixed models.",
        method
      ))
    }
    return(FALSE)
  }

  return(TRUE)
}
