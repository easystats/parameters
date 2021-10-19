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
#' @details Methods for calculating degrees of freedom:
#' \itemize{
#' \item `"analytical"` for models of class `lmerMod`, Kenward-Roger approximated degrees of freedoms are calculated, for other models, `n-k` (number of observations minus number of parameters).
#' \item `"residual"` tries to extract residual degrees of freedom, and returns `Inf` if residual degrees of freedom could not be extracted.
#' \item `"any"` first tries to extract residual degrees of freedom, and if these are not available, extracts analytical degrees of freedom.
#' \item `"nokr"` same as `"analytical"`, but does not Kenward-Roger approximation for models of class `lmerMod`. Instead, always uses `n-k` to calculate df for any model.
#' \item `"normal"` returns `Inf`.
#' \item `"wald"` returns residual df for models with t-statistic, and `Inf` for all other models.
#' \item `"kenward"` calls [`dof_kenward()`].
#' \item `"satterthwaite"` calls [`dof_satterthwaite()`].
#' \item `"ml1"` calls [`dof_ml1()`].
#' \item `"betwithin"` calls [`dof_betwithin()`].
#' }
#' For models with z-statistic, the returned degrees of freedom for model parameters is `Inf` (unless `method = "ml1"` or `method = "betwithin"`), because there is only one distribution for the related test statistic.
#'
#' @note In many cases, `degrees_of_freedom()` returns the same as
#' `df.residuals()`, or `n-k` (number of observations minus number of
#' parameters). However, `degrees_of_freedom()` refers to the model's
#' *parameters* degrees of freedom of the distribution for the related test
#' statistic. Thus, for models with z-statistic, results from `degrees_of_freedom()`
#' and `df.residuals()` differ. Furthermore, for other approximation methods
#' like `"kenward"` or `"satterthwaite"`, each model parameter can have
#' a different degree of freedom.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' dof(model)
#'
#' model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
#' dof(model)
#' \dontrun{
#' if (require("lme4", quietly = TRUE)) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   dof(model)
#' }
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
degrees_of_freedom <- function(model, ...) {
  UseMethod("degrees_of_freedom")
}




#' @rdname degrees_of_freedom
#' @export
degrees_of_freedom.default <- function(model, method = "analytical", ...) {

  if (is.null(method)) {
    method <- "wald"
  }
  method <- tolower(method)

  method <- match.arg(method, choices = c(
    "analytical", "any", "fit", "ml1", "betwithin", "satterthwaite", "kenward",
    "nokr", "wald", "kr", "profile", "boot", "uniroot", "residual", "normal",
    "likelihood"
  ))

  if (!.dof_method_ok(model, method, ...) || method %in% c("profile", "likelihood", "boot", "uniroot")) {
    method <- "any"
  }

  dot_args <- list(...)
  if ("statistic" %in% names(dot_args)) {
    stat <- dot_args[["statistic"]]
  } else {
    stat <- insight::find_statistic(model)
  }

  # for z-statistic, always return Inf
  if (!is.null(stat) && stat == "z-statistic" && !(method %in% c("ml1", "betwithin"))) {
    if (method == "residual") {
      return(.degrees_of_freedom_residual(model, verbose = FALSE))
    } else {
      return(Inf)
    }
  }

  # Chi2-distributions usually have 1 df
  if (!is.null(stat) && stat == "chi-squared statistic") {
    if (method == "residual") {
      return(.degrees_of_freedom_residual(model, verbose = FALSE))
    } else {
      return(1)
    }
  }

  if (method == "any") {
    dof <- .degrees_of_freedom_residual(model, verbose = FALSE)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(model, kenward = FALSE)
    }
  } else if (method == "ml1") {
    dof <- dof_ml1(model)
  } else if (method == "wald") {
    dof <- .degrees_of_freedom_residual(model, verbose = FALSE)
  } else if (method == "normal") {
    dof <- Inf
  } else if (method == "satterthwaite") {
    dof <- dof_satterthwaite(model)
  } else if (method == "betwithin") {
    dof <- dof_betwithin(model)
  } else if (method %in% c("kenward", "kr")) {
    dof <- dof_kenward(model)
  } else if (method == "analytical") {
    dof <- .degrees_of_freedom_analytical(model, kenward = TRUE)
  } else if (method == "nokr") {
    dof <- .degrees_of_freedom_analytical(model, kenward = FALSE)
  } else {
    dof <- .degrees_of_freedom_residual(model)
  }

  if (!is.null(dof) && length(dof) > 0 && all(dof == 0)) {
    warning("Model has zero degrees of freedom!", call. = FALSE)
  }

  dof
}

#' @rdname degrees_of_freedom
#' @export
dof <- degrees_of_freedom





# Analytical approach ------------------------------


#' @keywords internal
.degrees_of_freedom_analytical <- function(model, kenward = TRUE) {
  nparam <- n_parameters(model)
  n <- insight::n_obs(model)

  if (is.null(n)) {
    n <- Inf
  }

  if (isTRUE(kenward) && inherits(model, "lmerMod")) {
    dof <- as.numeric(dof_kenward(model))
  } else {
    dof <- rep(n - nparam, nparam)
  }

  dof
}





# Model approach (Residual df) ------------------------------


#' @keywords internal
.degrees_of_freedom_residual <- function(model, verbose = TRUE) {
  if (.is_bayesian_model(model) && !inherits(model, c("bayesx", "blmerMod", "bglmerMod"))) {
    model <- bayestestR::bayesian_as_frequentist(model)
  }

  # 1st try
  dof <- try(stats::df.residual(model), silent = TRUE)

  # 2nd try
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    junk <- utils::capture.output(dof = try(summary(model)$df[2], silent = TRUE))
  }

  # 3rd try, nlme
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    dof <- try(unname(model$fixDF$X), silent = TRUE)
  }

  # last try
  if (inherits(dof, "try-error") || is.null(dof) || all(is.na(dof))) {
    dof <- Inf
    if (verbose) {
      warning("Could not extract degrees of freedom.", call. = FALSE)
    }
  }


  # special cases
  # if (inherits(model, "gam")) {
  #   dof <- .dof_fit_gam(model, dof)
  # }

  dof
}




# residual df - for models with residual df, but no "df.residual()" method --------------


#' @keywords internal
.degrees_of_freedom_no_dfresid_method <- function(model, method = NULL) {
  if (identical(method, "normal")) {
    return(Inf)
  } else if (!is.null(method) && method %in% c("ml1", "satterthwaite", "betwithin")) {
    degrees_of_freedom.default(model, method = method)
  } else {
    .degrees_of_freedom_analytical(model, kenward = FALSE)
  }
}






# helper --------------

.dof_fit_gam <- function(model, dof) {
  params <- insight::find_parameters(model)
  if (!is.null(params$conditional)) {
    dof <- rep(dof, length(params$conditional))
  }
  if (!is.null(params$smooth_terms)) {
    s <- summary(model)
    dof <- c(dof, s$s.table[, "Ref.df"])
  }
  dof
}


# Helper, check args ------------------------------

.dof_method_ok <- function(model, method, type = "df_method", verbose = TRUE, ...) {
  if (is.null(method)) {
    return(TRUE)
  }

  method <- tolower(method)
  if (inherits(model, c("polr", "glm", "svyglm"))) {
    if (method %in% c(
      "analytical", "any", "fit", "profile", "residual",
      "wald", "nokr", "likelihood", "normal"
    )) {
      return(TRUE)
    } else {
      if (verbose) {
        warning(insight::format_message(sprintf("'%s' must be one of 'wald', 'residual' or 'profile'. Using 'wald' now.", type)), call. = FALSE)
      }
      return(FALSE)
    }
  }

  info <- insight::model_info(model, verbose = FALSE)
  if (!is.null(info) && isFALSE(info$is_mixed) && method == "boot") {
    if (verbose) {
      warning(insight::format_message(sprintf("'%s=boot' only works for mixed models of class 'merMod'. To bootstrap this model, use `bootstrap=TRUE, ci_method=\"bcai\"`.", type)), call. = FALSE)
    }
    return(TRUE)
  }

  if (is.null(info) || !info$is_mixed) {
    if (!(method %in% c("analytical", "any", "fit", "betwithin", "nokr", "wald", "ml1", "profile", "boot", "uniroot", "residual", "normal"))) {
      if (verbose) {
        warning(insight::format_message(sprintf("'%s' must be one of 'residual', 'wald', normal', 'profile', 'boot', 'uniroot', 'betwithin' or 'ml1'. Using 'wald' now.", type)), call. = FALSE)
      }
      return(FALSE)
    }
    return(TRUE)
  }

  if (!(method %in% c("analytical", "any", "fit", "satterthwaite", "betwithin", "kenward", "kr", "nokr", "wald", "ml1", "profile", "boot", "uniroot", "residual", "normal"))) {
    if (verbose) {
      warning(insight::format_message(sprintf("'%s' must be one of 'residual', 'wald', 'normal', 'profile', 'boot', 'uniroot', 'kenward', 'satterthwaite', 'betwithin' or 'ml1'. Using 'wald' now.", type)), call. = FALSE)
    }
    return(FALSE)
  }

  if (!info$is_linear && method %in% c("satterthwaite", "kenward", "kr")) {
    if (verbose) {
      warning(sprintf("'%s'-degrees of freedoms are only available for linear mixed models.", method), call. = FALSE)
    }
    return(FALSE)
  }

  return(TRUE)
}




# helper

.is_bayesian_model <- function(x) {
  inherits(x, c(
    "brmsfit", "stanfit", "MCMCglmm", "stanreg",
    "stanmvreg", "bmerMod", "BFBayesFactor", "bamlss",
    "bayesx", "mcmc", "bcplm", "bayesQR", "BGGM",
    "meta_random", "meta_fixed", "meta_bma", "blavaan",
    "blrm"
  ))
}
