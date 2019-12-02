#' Parameters of (General) Linear Models
#'
#' Extract and compute indices and measures to describe parameters of (general) linear models (GLMs).
#'
#' @param model Model object.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param bootstrap Should estimates be based on bootstrapped model? If \code{TRUE}, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply (see also \code{\link[=parameters_bootstrap]{parameters_bootstrap()}}).
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootstrapped frequentist models.
#' @param standardize The method used for standardizing the parameters. Can be \code{"refit"}, \code{"posthoc"}, \code{"smart"}, \code{"basic"} or \code{NULL} (default) for no standardization. See 'Details' in \code{\link[effectsize]{standardize_parameters}}.
#' @param exponentiate Logical, indicating whether or not to exponentiate the the coefficients (and related confidence intervals). This is typical for, say, logistic regressions, or more generally speaking: for models with log or logit link.
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model)
#'
#' # bootstrapped parameters
#' model_parameters(model, bootstrap = TRUE)
#'
#' # standardized parameters
#' model_parameters(model, standardize = "refit")
#'
#' # different p-value style in output
#' model_parameters(model, p_digits = 5)
#' model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")
#'
#' # logistic regression model
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.default <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



.model_parameters_generic <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, merge_by = "Parameter", standardize = NULL, exponentiate = FALSE, ...) {
  # to avoid "match multiple argument error", check if "component" was
  # already used as argument and passed via "...".
  mc <- match.call()
  comp_argument <- parse(text = .safe_deparse(mc))[[1]]$component

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- if (is.null(comp_argument)) {
      .extract_parameters_generic(model, ci = ci, component = "conditional", merge_by = merge_by, standardize = standardize, ...)
    } else {
      .extract_parameters_generic(model, ci = ci, merge_by = merge_by, standardize = standardize, ...)
    }
  }

  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}



#' @export
model_parameters.lme <- model_parameters.default

#' @export
model_parameters.lm <- model_parameters.default

#' @export
model_parameters.glm <- model_parameters.default

#' @export
model_parameters.clm2 <- model_parameters.default

#' @export
model_parameters.svyglm.nb <- model_parameters.default

#' @export
model_parameters.svyglm.zip <- model_parameters.default

#' @export
model_parameters.glimML <- model_parameters.default

#' @export
model_parameters.tobit <- model_parameters.default

#' @export
model_parameters.polr <- model_parameters.default

#' @export
model_parameters.clm <- model_parameters.default

#' @export
model_parameters.rq <- model_parameters.default

#' @export
model_parameters.crq <- model_parameters.default

#' @export
model_parameters.nlrq <- model_parameters.default

#' @export
model_parameters.speedglm <- model_parameters.default

#' @export
model_parameters.speedlm <- model_parameters.default

#' @export
model_parameters.iv_robust <- model_parameters.default

#' @export
model_parameters.glmRob <- model_parameters.default

#' @export
model_parameters.lmRob <- model_parameters.default

#' @export
model_parameters.lmrob <- model_parameters.default

#' @export
model_parameters.glmrob <- model_parameters.default

#' @export
model_parameters.gls <- model_parameters.default

#' @export
model_parameters.feis <- model_parameters.default

#' @export
model_parameters.coxph <- model_parameters.default

#' @export
model_parameters.lrm <- model_parameters.default

#' @export
model_parameters.biglm <- model_parameters.default

#' @export
model_parameters.lm_robust <- model_parameters.default

#' @export
model_parameters.geeglm <- model_parameters.default

#' @export
model_parameters.gee <- model_parameters.default

#' @export
model_parameters.ols <- model_parameters.default

#' @export
model_parameters.rms <- model_parameters.default

#' @export
model_parameters.vglm <- model_parameters.default

#' @export
model_parameters.logistf <- model_parameters.default

#' @export
model_parameters.coxme <- model_parameters.default

#' @export
model_parameters.censReg <- model_parameters.default

#' @export
model_parameters.flexsurvreg <- model_parameters.default

#' @export
model_parameters.crch <- model_parameters.default

#' @export
model_parameters.truncreg <- model_parameters.default

#' @export
model_parameters.plm <- model_parameters.default

#' @export
model_parameters.survreg <- model_parameters.default

#' @export
model_parameters.psm <- model_parameters.default

#' @export
model_parameters.ivreg <- model_parameters.default

#' @export
model_parameters.LORgee <- model_parameters.default





# other special cases ------------------------------------------------


#' @export
model_parameters.betareg <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



#' @export
model_parameters.mlm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
model_parameters.multinom <- model_parameters.mlm


#' @export
model_parameters.brmultinom <- model_parameters.mlm


#' @export
model_parameters.bracl <- model_parameters.mlm


#' @importFrom stats qt setNames
#' @export
model_parameters.rma <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {

  meta_analysis_overall <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  alpha <- (1 + ci) / 2

  rma_parameters <- if (!is.null(model$slab)) {
    sprintf("Study %s", model$slab)
  } else {
    sprintf("Study %i", 1:model[["k"]])
  }

  rma_coeffients <- as.vector(model$yi)
  rma_se <- as.vector(sqrt(model$vi))
  rma_ci_low <- rma_coeffients - rma_se * stats::qt(alpha, df = Inf)
  rma_ci_high <- rma_coeffients + rma_se * stats::qt(alpha, df = Inf)
  rma_statistic <- rma_coeffients / rma_se
  rma_ci_p <- 2 * stats::pt(abs(rma_statistic), df = Inf, lower.tail = FALSE)

  meta_analysis_studies <- data.frame(
    Parameter = rma_parameters,
    Coefficient = rma_coeffients,
    SE = rma_se,
    CI_low = rma_ci_low,
    CI_high = rma_ci_high,
    z = rma_statistic,
    df_error = NA,
    p = rma_ci_p,
    Weight = 1 / as.vector(model$vi),
    stringsAsFactors = FALSE
  )

  original_attributes <- attributes(meta_analysis_overall)
  out <- merge(meta_analysis_studies, meta_analysis_overall, all = TRUE, sort = FALSE)

  original_attributes$names <- names(out)
  original_attributes$row.names <- 1:nrow(out)
  original_attributes$pretty_names <- stats::setNames(out$Parameter, out$Parameter)
  attributes(out) <- original_attributes

  # no df
  out$df_error <- NULL
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  out
}
