#' Parameters from Mixed Models
#'
#' Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @inheritParams model_parameters.default
#' @param df_method Method for computing degrees of freedom for p values, standard errors and confidence intervals (CI). May be \code{"wald"} (default, see \code{\link{degrees_of_freedom}}), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}) or \code{"kenward"} (see \code{\link{dof_kenward}}). Note that when \code{df_method} is not \code{"wald"}, robust standard errors etc. cannot be computed.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("lme4") && require("glmmTMB")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model)
#'
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   model_parameters(model)
#' }
#' \donttest{
#' if (require("lme4")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model, bootstrap = TRUE, iterations = 50)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model, ci = .95, bootstrap = FALSE, df_method = "wald", iterations = 1000, standardize = NULL, exponentiate = FALSE, robust = FALSE, summary_random = TRUE, ...) {
  # p-values, CI and se might be based of wald, or KR
  df_method <- match.arg(df_method, choices = c("wald", "ml1", "satterthwaite", "kenward"))

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_mixed(model, ci = ci, df_method = df_method, robust = robust, standardize = standardize, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)

  if (isTRUE(summary_random)) {
    attr(parameters, "summary_random") <- .randomeffects_summary(model)
  }

  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.lme <- model_parameters.merMod





# Mixed Models with zero inflation ------------------------------------

#' @inheritParams simulate_model
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), standardize = NULL, exponentiate = FALSE, summary_random = TRUE, ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = component, standardize = standardize, robust = FALSE, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)

  if (isTRUE(summary_random)) {
    attr(parameters, "summary_random") <- .randomeffects_summary(model)
  }

  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB




# helper -----------------------------------

.n_randomeffects <- function(model) {
  sapply(insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)], function(i) length(unique(i, na.rm = TRUE)))
}



#' @importFrom insight find_random get_variance find_random_slopes n_obs
.randomeffects_summary <- function(model) {
  out <- list()

  re_variances <- suppressWarnings(insight::get_variance(model))
  model_re <- insight::find_random(model, split_nested = FALSE, flatten = TRUE)
  model_rs <- unlist(insight::find_random_slopes(model))

  if (length(re_variances) && !is.na(re_variances) && !is.null(re_variances)) {
    # Residual Variance (Sigma^2)
    out$Sigma2 <- re_variances$var.residual
    out <- c(out, as.list(NA))

    # Random Intercept Variance
    var_intercept <- as.list(re_variances$var.intercept)
    names(var_intercept) <- paste0("tau00_", names(re_variances$var.intercept))
    out <- c(out, var_intercept)

    # Random Slope Variance
    if (!.is_empty_object(re_variances$var.slope) && !.is_empty_object(model_rs)) {
      var_slope <- as.list(re_variances$var.slope)
      names(var_slope) <- paste0("tau11_", names(re_variances$var.slope))
      out <- c(out, var_slope)
      out <- c(out, as.list(NA))
    } else {
      out <- c(out, as.list(NA))
    }

    # Slope-Intercept Correlation
    if (!.is_empty_object(re_variances$cor.slope_intercept) && !.is_empty_object(model_rs)) {
      cor_slope_intercept <- as.list(re_variances$cor.slope_intercept)
      names(cor_slope_intercept) <- paste0("rho01_", model_re, ".", model_rs)
      out <- c(out, cor_slope_intercept)
      out <- c(out, as.list(NA))
    }

    # ICC & R2
    out$R2_marginal <- re_variances$var.fixed / (re_variances$var.fixed + re_variances$var.residual)
    if (!.is_empty_object(re_variances$var.random) && !is.na(re_variances$var.random)) {
      out$R2_conditional <- (re_variances$var.fixed + re_variances$var.random) / (re_variances$var.fixed + re_variances$var.random + re_variances$var.residual)
      out$ICC <- re_variances$var.random / (re_variances$var.random + re_variances$var.residual)
    }
    out <- c(out, as.list(NA))
  }

  # Number of levels per random-effect groups
  n_re <- as.list(.n_randomeffects(model))
  names(n_re) <- paste0("N_", names(n_re))
  out <- c(out, n_re)

  # number of observations
  out$Observations <- insight::n_obs(model)

  # make nice data frame
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  out$Statistic <- rownames(out)
  rownames(out) <- NULL
  colnames(out) <- c("Value", "Statistic")

  out[c(2:1)]
}
