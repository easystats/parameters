#' @export
model_parameters.mjoint <- function(model,
                                    ci = .95,
                                    effects = "fixed",
                                    component = c("all", "conditional", "survival"),
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    keep = NULL,
                                    drop = NULL,
                                    parameters = keep,
                                    verbose = TRUE,
                                    ...) {
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  component <- match.arg(component)

  params <- params_variance <- NULL

  if (effects %in% c("fixed", "all")) {
    # Processing
    params <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = FALSE,
      robust = FALSE,
      p_adjust = p_adjust,
      keep_parameters = keep,
      drop_parameters = drop,
      ...
    )

    if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
      params <- .exponentiate_parameters(params, model, exponentiate)
    }
    params$Effects <- "fixed"
  }

  if (effects %in% c("random", "all")) {
    params_variance <- .extract_random_variances(model, ci = ci, effects = effects, df_method = NULL)
    params_variance$Component <- "conditional"
  }


  # merge random and fixed effects, if necessary
  if (!is.null(params) && !is.null(params_variance)) {
    params$Level <- NA
    params$Group <- ""
    # add component column
    if (!"Component" %in% colnames(params)) {
      params$Component <- "conditional"
    }

    # reorder
    params <- params[match(colnames(params_variance), colnames(params))]
  }

  params <- rbind(params, params_variance)
  # remove empty column
  if (!is.null(params$Level) && all(is.na(params$Level))) {
    params$Level <- NULL
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci = ifelse(effects == "random", NA, ci),
    exponentiate,
    df_method = NULL,
    p_adjust = p_adjust,
    verbose = verbose,
    group_level = FALSE,
    ...
  )

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
p_value.mjoint <- function(model, component = c("all", "conditional", "survival"), ...) {
  component <- match.arg(component)
  s <- summary(model)

  params <- rbind(
    data.frame(
      Parameter = rownames(s$coefs.long),
      p = unname(s$coefs.long[, 4]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = rownames(s$coefs.surv),
      p = unname(s$coefs.surv[, 4]),
      Component = "survival",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  params
}


#' @export
ci.mjoint <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, dof = Inf, ...)
}


#' @export
standard_error.mjoint <- function(model, component = c("all", "conditional", "survival"), ...) {
  component <- match.arg(component)
  s <- summary(model)

  params <- rbind(
    data.frame(
      Parameter = rownames(s$coefs.long),
      SE = unname(s$coefs.long[, 2]),
      Component = "conditional",
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    data.frame(
      Parameter = rownames(s$coefs.surv),
      SE = unname(s$coefs.surv[, 2]),
      Component = "survival",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  params
}
