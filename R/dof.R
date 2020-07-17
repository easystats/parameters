#' Degrees of Freedom (DoF)
#'
#' Estimate or extract degrees of freedom of models parameters.
#'
#' @param model A statistical model.
#' @param method Can be \code{"analytical"} (default, DoFs are estimated based on the model type), \code{"fit"}, in which case they are directly taken from the model if available (for Bayesian models, the goal (looking for help to make it happen) would be to refit the model as a frequentist one before extracting the DoFs), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"betwithin"} (see \code{\link{dof_betwithin}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}), \code{"kenward"} (see \code{\link{dof_kenward}}) or \code{"any"}, which tries to extract DoF by any of those methods, whichever succeeds.
#' @param ... Currently not used.
#'
#' @details Methods for calculating degrees of freedom:
#' \itemize{
#' \item \code{"analytical"} for models of class \code{lmerMod}, Kenward-Roger approximated degrees of freedoms are calculated, for other models, \code{n-k} (number of observations minus number of parameters).
#' \item \code{"fit"} tries to extract residual degrees of freedom, and returns \code{Inf} if residual degrees of freedom could not be extracted.
#' \item \code{"any"} first tries to extract residual degrees of freedom, and if these are not available, extracts analytical degrees of freedom.
#' \item \code{"nokr"} same as \code{"analytical"}, but does not Kenward-Roger approximation for models of class \code{lmerMod}. Instead, always uses \code{n-k} to calculate df for any model.
#' \item \code{"wald"} returns \code{Inf}.
#' \item \code{"kenward"} calls \code{\link{dof_kenward}}.
#' \item \code{"satterthwaite"} calls \code{\link{dof_satterthwaite}}.
#' \item \code{"ml1"} calls \code{\link{dof_ml1}}.
#' \item \code{"betwithin"} calls \code{\link{dof_betwithin}}.
#' }
#' For models with z-statistic, the returned degrees of freedom for model parameters is \code{Inf} (unless \code{method = "ml1"} or \code{method = "betwithin"}), because there is only one distribution for the related test statistic.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' dof(model)
#'
#' model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
#' dof(model)
#'
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   dof(model)
#' }
#' \donttest{
#' if (require("rstanarm")) {
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


#' @importFrom insight find_statistic
#' @rdname degrees_of_freedom
#' @export
degrees_of_freedom.default <- function(model, method = "analytical", ...) {
  method <- tolower(method)
  method <- match.arg(method, c("analytical", "any", "fit", "ml1", "betwithin", "satterthwaite", "kenward", "nokr", "wald"))

  if (!.dof_method_ok(model, method)) {
    method <- "any"
  }

  # for z-statistic, always return Inf
  stat <- insight::find_statistic(model)
  if (!is.null(stat) && stat == "z-statistic" && !(method %in% c("ml1", "betwithin"))) {
    return(Inf)
  }

  if (method == "any") {
    dof <- .degrees_of_freedom_fit(model, verbose = FALSE)
    if (is.null(dof) || all(is.infinite(dof)) || anyNA(dof)) {
      dof <- .degrees_of_freedom_analytical(model, kenward = FALSE)
    }
  } else if (method == "ml1") {
    dof <- dof_ml1(model)
  } else if (method == "wald") {
    dof <- Inf
  } else if (method == "satterthwaite") {
    dof <- dof_satterthwaite(model)
  } else if (method == "betwithin") {
    dof <- dof_betwithin(model)
  } else if (method == "kenward") {
    dof <- dof_kenward(model)
  } else if (method == "analytical") {
    dof <- .degrees_of_freedom_analytical(model)
  } else if (method == "nokr") {
    dof <- .degrees_of_freedom_analytical(model, kenward = FALSE)
  } else {
    dof <- .degrees_of_freedom_fit(model)
  }

  if (!is.null(dof) && length(dof) > 0 && all(dof == 0)) {
    warning("Model has zero degrees of freedom!", call. = FALSE)
  }

  dof
}

#' @rdname degrees_of_freedom
#' @export
dof <- degrees_of_freedom


#' @export
degrees_of_freedom.emmGrid <- function(model,...) {
  summary(model)$df
}

#' @export
degrees_of_freedom.glht <- function(model,...) {
  model$df
}

#' @export
degrees_of_freedom.logitor <- function(model,...) {
  degrees_of_freedom.default(model$fit, ...)
}

#' @export
degrees_of_freedom.poissonirr <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.negbinirr <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.poissonmfx <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.logitmfx <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.negbinmfx <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.probitmfx <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.betaor <- degrees_of_freedom.logitor

#' @export
degrees_of_freedom.betamfx <- degrees_of_freedom.logitor







# Analytical approach ------------------------------


#' @keywords internal
.degrees_of_freedom_analytical <- function(model, kenward = TRUE) {
  nparam <- n_parameters(model)
  n <- insight::n_obs(model)

  if (isTRUE(kenward) && inherits(model, "lmerMod")) {
    dof <- as.numeric(dof_kenward(model))
  } else {
    dof <- rep(n - nparam, nparam)
  }

  dof
}







# Model approach (Residual df) ------------------------------

#' @importFrom bayestestR bayesian_as_frequentist
#' @importFrom stats df.residual
#' @keywords internal
.degrees_of_freedom_fit <- function(model, verbose = TRUE) {
  info <- insight::model_info(model, verbose = FALSE)

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (!is.null(info) && is.list(info) && info$is_bayesian && !inherits(model, "bayesx")) {
    model <- bayestestR::bayesian_as_frequentist(model)
  }

  # 1st try
  dof <- try(stats::df.residual(model), silent = TRUE)

  # 2nd try
  if (inherits(dof, "try-error") || is.null(dof)) {
    junk <- utils::capture.output(dof = try(summary(model)$df[2], silent = TRUE))
  }

  # 3rd try, nlme
  if (inherits(dof, "try-error") || is.null(dof)) {
    dof <- try(unname(model$fixDF$X), silent = TRUE)
  }

  # last try
  if (inherits(dof, "try-error") || is.null(dof)) {
    dof <- Inf
    if (verbose) {
      insight::print_color("Could not extract degrees of freedom.\n", "red")
    }
  }


  # special cases
  if (inherits(model, "gam")) {
    dof <- .dof_fit_gam(model, dof)
  }

  dof
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

.dof_method_ok <- function(model, method) {
  if (is.null(method)) {
    return(TRUE)
  }
  if (inherits(model, c("polr", "glm")) && method %in% c("profile", "wald")) {
    return(TRUE)
  }

  info <- insight::model_info(model, verbose = FALSE)
  if (is.null(info) || !info$is_mixed) {
    return(FALSE)
  }
  method <- tolower(method)
  if (!(method %in% c("analytical", "any", "fit", "satterthwaite", "betwithin", "kenward", "kr", "nokr", "wald", "ml1"))) {
    warning("'df_method' must be one of 'wald', 'kenward', 'satterthwaite', 'betwithin' or ' ml1'. Using 'wald' now.", call. = FALSE)
    return(FALSE)
  }
  if (!info$is_linear && method %in% c("satterthwaite", "kenward", "kr")) {
    warning(sprintf("'%s'-degrees of freedoms are only available for linear mixed models.", method), call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}
