# .wbm, .wbgee

# model parameters -------------------


#' @inheritParams model_parameters.merMod
#' @export
model_parameters.wbm <- function(model,
                                 ci = .95,
                                 effects = "fixed",
                                 group_level = FALSE,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 exponentiate = FALSE,
                                 details = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {

  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  params <- params_random <- params_variance <- att <- NULL

  if (effects %in% c("fixed", "all")) {
    params <- .model_parameters_generic(
      model = model,
      ci = ci,
      bootstrap = bootstrap,
      iterations = iterations,
      merge_by = c("Parameter", "Component"),
      standardize = NULL,
      exponentiate = exponentiate,
      robust = FALSE,
      p_adjust = p_adjust,
      ...
    )
    params$Effects <- "fixed"
    att <- attributes(params)
  }

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  if (effects %in% c("random", "all") && isFALSE(group_level)) {
    params_variance <- .extract_random_variances(model, ci = ci, effects = effects)
  }


  # merge random and fixed effects, if necessary
  if (!is.null(params) && (!is.null(params_random) || !is.null(params_variance))) {
    params$Level <- NA
    params$Group <- ""

    # reorder
    if (!is.null(params_random)) {
      params <- params[match(colnames(params_random), colnames(params))]
    } else {
      params <- params[match(colnames(params_variance), colnames(params))]
    }
  }

  params <- rbind(params, params_random, params_variance)
  if (!is.null(att)) {
    attributes(params) <- utils::modifyList(att, attributes(params))
  }

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
    if (verbose) {
      message("Argument 'details' is deprecated. Please use 'group_level'.")
    }
  }

  params
}


#' @export
model_parameters.wbgee <- model_parameters.wbm




# standard errors -------------------


#' @export
standard_error.wbm <- function(model, ...) {
  s <- summary(model)
  se <- c(
    s$within_table[, "S.E."],
    s$between_table[, "S.E."],
    s$ints_table[, "S.E."]
  )
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Component = params$Component
  )
}


#' @export
standard_error.wbgee <- standard_error.wbm




# p values -------------------


#' @export
p_value.wbm <- function(model, ...) {
  s <- summary(model)
  p <- c(
    s$within_table[, "p"],
    s$between_table[, "p"],
    s$ints_table[, "p"]
  )
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p),
    Component = params$Component
  )
}


#' @export
p_value.wbgee <- p_value.wbm
