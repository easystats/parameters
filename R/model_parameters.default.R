#' @rdname model_parameters.merMod
#' @export
model_parameters.lme <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    ...
  )
}



.model_parameters_generic <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, merge_by = "Parameter", ...) {
  # to avoid "match multiple argument error", check if "component" was
  # already used as argument and passed via "...".
  mc <- match.call()
  comp_argument <- parse(text = .safe_deparse(mc))[[1]]$component

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- if (is.null(comp_argument)) {
      .extract_parameters_generic(model, ci = ci, component = "conditional", merge_by = merge_by, ...)
    } else {
      .extract_parameters_generic(model, ci = ci, merge_by = merge_by, ...)
    }
  }



  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}




#' @export
model_parameters.clm2 <- model_parameters.lme

#' @export
model_parameters.svyglm.nb <- model_parameters.lme

#' @export
model_parameters.svyglm.zip <- model_parameters.lme

#' @export
model_parameters.glimML <- model_parameters.lme

#' @export
model_parameters.tobit <- model_parameters.lme

#' @export
model_parameters.polr <- model_parameters.lme

#' @export
model_parameters.clm <- model_parameters.lme

#' @export
model_parameters.rq <- model_parameters.lme

#' @export
model_parameters.crq <- model_parameters.lme

#' @export
model_parameters.nlrq <- model_parameters.lme

#' @export
model_parameters.speedglm <- model_parameters.lme

#' @export
model_parameters.speedlm <- model_parameters.lme

#' @export
model_parameters.iv_robust <- model_parameters.lme

#' @export
model_parameters.glmRob <- model_parameters.lme

#' @export
model_parameters.lmRob <- model_parameters.lme

#' @export
model_parameters.lmrob <- model_parameters.lme

#' @export
model_parameters.glmrob <- model_parameters.lme

#' @export
model_parameters.gls <- model_parameters.lme

#' @export
model_parameters.feis <- model_parameters.lme

#' @export
model_parameters.coxph <- model_parameters.lme

#' @export
model_parameters.betareg <- model_parameters.lme

#' @export
model_parameters.lrm <- model_parameters.lme

#' @export
model_parameters.biglm <- model_parameters.lme

#' @export
model_parameters.lm_robust <- model_parameters.lme

#' @export
model_parameters.geeglm <- model_parameters.lme

#' @export
model_parameters.gee <- model_parameters.lme

#' @export
model_parameters.ols <- model_parameters.lme

#' @export
model_parameters.rms <- model_parameters.lme

#' @export
model_parameters.vglm <- model_parameters.lme

#' @export
model_parameters.logistf <- model_parameters.lme

#' @export
model_parameters.coxme <- model_parameters.lme

#' @export
model_parameters.censReg <- model_parameters.lme

#' @export
model_parameters.flexsurvreg <- model_parameters.lme

#' @export
model_parameters.crch <- model_parameters.lme

#' @export
model_parameters.truncreg <- model_parameters.lme

#' @export
model_parameters.plm <- model_parameters.lme

#' @export
model_parameters.survreg <- model_parameters.lme

#' @export
model_parameters.psm <- model_parameters.lme

#' @export
model_parameters.ivreg <- model_parameters.lme

#' @export
model_parameters.LORgee <- model_parameters.lme

#' @export
model_parameters.multinom <- model_parameters.lme




# other special cases ------------------------------------------------


#' @export
model_parameters.mlm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    ...
  )
}
