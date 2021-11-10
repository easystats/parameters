# .wbm, .wbgee

# model parameters -------------------


#' @inheritParams model_parameters.merMod
#' @export
model_parameters.wbm <- function(model,
                                 ci = .95,
                                 effects = "all",
                                 group_level = FALSE,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 exponentiate = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 include_sigma = FALSE,
                                 ...) {
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))

  params <- .mixed_model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = NULL,
    exponentiate = exponentiate,
    effects = effects,
    robust = FALSE,
    p_adjust = p_adjust,
    group_level = group_level,
    ci_method = NULL,
    include_sigma = include_sigma,
    ...
  )

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", "data.frame")


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





# utils -------------------


.mixed_model_parameters_generic <- function(model,
                                            ci,
                                            bootstrap,
                                            iterations,
                                            merge_by,
                                            standardize,
                                            exponentiate,
                                            effects,
                                            robust,
                                            p_adjust,
                                            group_level,
                                            ci_method,
                                            include_sigma = FALSE,
                                            ...) {
  params <- params_random <- params_variance <- att <- NULL

  if (effects %in% c("fixed", "all")) {
    params <- .model_parameters_generic(
      model = model,
      ci = ci,
      bootstrap = bootstrap,
      iterations = iterations,
      merge_by = merge_by,
      standardize = standardize,
      exponentiate = exponentiate,
      effects = "fixed",
      robust = FALSE,
      p_adjust = p_adjust,
      ci_method = ci_method,
      include_sigma = include_sigma,
      ...
    )
    params$Effects <- "fixed"
    att <- attributes(params)
  }

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  if (effects %in% c("random", "all") && isFALSE(group_level)) {
    params_variance <- .extract_random_variances(model, ci = ci, effects = effects, ci_method = ci_method)
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

  # remove empty column
  if (!is.null(params$Level) && all(is.na(params$Level))) {
    params$Level <- NULL
  }

  params
}
