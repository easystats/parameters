#' Degrees of Freedom (DoF)
#'
#' Estimate or extract degrees of freedom of models.
#'
#' @param model A statistical model.
#' @param method Can be \code{"analytical"} (default, DoFs are estimated based on the model type), \code{"fit"}, in which case they are directly taken from the model if available (for Bayesian models, the goal (looking for help to make it happen) would be to refit the model as a frequentist one before extracting the DoFs), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"betwithin"} (see \code{\link{dof_betwithin}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}), \code{"kenward"} (see \code{\link{dof_kenward}}) or \code{"any"}, which tries to extract DoF by any of those methods, whichever succeeds.
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
degrees_of_freedom <- function(model, method = "analytical") {
  method <- tolower(method)
  method <- match.arg(method, c("analytical", "any", "fit", "ml1", "betwithin", "satterthwaite", "kenward", "nokr", "wald"))

  if (!.dof_method_ok(model, method)) {
    method <- "any"
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

  dof
}

#' @rdname degrees_of_freedom
#' @export
dof <- degrees_of_freedom









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





#' @importFrom bayestestR bayesian_as_frequentist
#' @importFrom stats df.residual
#' @keywords internal
.degrees_of_freedom_fit <- function(model, verbose = TRUE) {
  info <- insight::model_info(model)

  if (info$is_bayesian) {
    model <- bayestestR::bayesian_as_frequentist(model)
  }

  # 1st try
  dof <- try(stats::df.residual(model), silent = TRUE)

  # 2nd try
  if (inherits(dof, "try-error") || is.null(dof)) {
    dof <- try(summary(model)$df[2], silent = TRUE)
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

  dof
}




.dof_method_ok <- function(model, method) {
  if (is.null(method)) {
    return(TRUE)
  }
  if (!insight::model_info(model)$is_mixed) {
    return(FALSE)
  }
  method <- tolower(method)
  if (!(method %in% c("analytical", "any", "fit", "satterthwaite", "betwithin", "kenward", "kr", "nokr", "wald", "ml1"))) {
    warning("'df_method' must be one of 'wald', 'kenward', 'satterthwaite', 'betwithin' or ' ml1'. Using 'wald' now.", call. = FALSE)
    return(FALSE)
  }
  if (!insight::model_info(model)$is_linear && method %in% c("satterthwaite", "kenward", "kr")) {
    warning(sprintf("'%s'-degrees of freedoms are only available for linear mixed models.", method), call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}
