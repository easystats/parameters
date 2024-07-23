#' Degrees of Freedom (DoF)
#'
#' Estimate or extract degrees of freedom of models parameters.
#'
#' @param model A statistical model.
#' @param method Can be `"analytical"` (default, DoFs are estimated based
#'   on the model type), `"residual"` in which case they are directly taken
#'   from the model if available (for Bayesian models, the goal (looking for
#'   help to make it happen) would be to refit the model as a frequentist one
#'   before extracting the DoFs), `"ml1"` (see [dof_ml1()]), `"betwithin"`
#'   (see [dof_betwithin()]), `"satterthwaite"` (see [`dof_satterthwaite()`]),
#'   `"kenward"` (see [`dof_kenward()`]) or `"any"`, which tries to extract DoF
#'   by any of those methods, whichever succeeds. See 'Details'.
#' @param ... Currently not used.
#'
#' @details
#' Methods for calculating degrees of freedom:
#'
#' - `"analytical"` for models of class `lmerMod`, Kenward-Roger approximated
#'   degrees of freedoms are calculated, for other models, `n-k` (number of
#'   observations minus number of parameters).
#' - `"residual"` tries to extract residual degrees of freedom, and returns
#'   `Inf` if residual degrees of freedom could not be extracted.
#' - `"any"` first tries to extract residual degrees of freedom, and if these
#'   are not available, extracts analytical degrees of freedom.
#' - `"nokr"` same as `"analytical"`, but does not Kenward-Roger approximation
#'   for models of class `lmerMod`. Instead, always uses `n-k` to calculate df
#'   for any model.
#' - `"normal"` returns `Inf`.
#' - `"wald"` returns residual df for models with t-statistic, and `Inf` for all other models.
#' - `"kenward"` calls [`dof_kenward()`].
#' - `"satterthwaite"` calls [`dof_satterthwaite()`].
#' - `"ml1"` calls [`dof_ml1()`].
#' - `"betwithin"` calls [`dof_betwithin()`].
#'
#' For models with z-statistic, the returned degrees of freedom for model parameters
#' is `Inf` (unless `method = "ml1"` or `method = "betwithin"`), because there is
#' only one distribution for the related test statistic.
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
  if (inherits(model, c("polr", "glm", "svyglm"))) {
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
    if (method %in% c("analytical", "any", "fit", "residual", "wald", "nokr", "normal", "boot")) {
      return(TRUE)
    } else {
      if (verbose) {
        insight::format_alert(sprintf("`%s` must be one of \"wald\", \"normal\" or \"boot\". Using \"wald\" now.", type)) # nolint
      }
      return(FALSE)
    }
  }

  info <- insight::model_info(model, verbose = FALSE)
  if (!is.null(info) && isFALSE(info$is_mixed) && method == "boot") {
    if (verbose) {
      insight::format_alert(sprintf("`%s=boot` only works for mixed models of class `merMod`. To bootstrap this model, use `bootstrap=TRUE, ci_method=\"bcai\"`.", type)) # nolint
    }
    return(TRUE)
  }

  if (is.null(info) || !info$is_mixed) {
    if (!(method %in% c("analytical", "any", "fit", "betwithin", "nokr", "wald", "ml1", "profile", "boot", "uniroot", "residual", "normal"))) { # nolint
      if (verbose) {
        insight::format_alert(sprintf("`%s` must be one of \"residual\", \"wald\", \"normal\", \"profile\", \"boot\", \"uniroot\", \"betwithin\" or \"ml1\". Using \"wald\" now.", type)) # nolint
      }
      return(FALSE)
    }
    return(TRUE)
  }

  if (!(method %in% c("analytical", "any", "fit", "satterthwaite", "betwithin", "kenward", "kr", "nokr", "wald", "ml1", "profile", "boot", "uniroot", "residual", "normal"))) { # nolint
    if (verbose) {
      insight::format_alert(sprintf("`%s` must be one of \"residual\", \"wald\", \"normal\", \"profile\", \"boot\", \"uniroot\", \"kenward\", \"satterthwaite\", \"betwithin\" or \"ml1\". Using \"wald\" now.", type)) # nolint
    }
    return(FALSE)
  }

  if (!info$is_linear && method %in% c("satterthwaite", "kenward", "kr")) {
    if (verbose) {
      insight::format_alert(sprintf("`%s`-degrees of freedoms are only available for linear mixed models.", method))
    }
    return(FALSE)
  }

  return(TRUE)
}
