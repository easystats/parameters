# Package glmmTMB


# model_parameters -----


#' @importFrom stats coef
#' @inheritParams simulate_model
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model,
                                     ci = .95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     df_method = NULL,
                                     details = FALSE,
                                     p_adjust = NULL,
                                     wb_component = TRUE,
                                     verbose = TRUE,
                                     ...) {
  component <- match.arg(component)

  # p-values, CI and se might be based on different df-methods
  df_method <- .check_df_method(df_method)

  # fix argument, if model has only conditional component
  cs <- stats::coef(summary(model))
  has_zeroinf <- insight::model_info(model)$is_zero_inflated
  has_disp <- is.list(cs) && !is.null(cs$disp)

  if (!has_zeroinf && !has_disp && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = standardize,
      robust = FALSE,
      df_method = df_method,
      p_adjust = p_adjust,
      wb_component = wb_component,
      ...
    )
  }


  # add dispersion parameter
  if (inherits(model, "glmmTMB") && !is.null(params$Component) && !"dispersion" %in% params$Component) {
    dispersion_param <- insight::get_parameters(model, component = "dispersion")
    if (!is.null(dispersion_param)) {
      params[nrow(params) + 1, ] <- NA
      params[nrow(params), "Parameter"] <- dispersion_param$Parameter[1]
      params[nrow(params), "Coefficient"] <- dispersion_param$Estimate[1]
      params[nrow(params), "Component"] <- dispersion_param$Component[1]
    }
  }


  if (exponentiate) params <- .exponentiate_parameters(params)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    df_method = df_method,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
  }

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


# ci -----


#' @rdname ci.merMod
#' @export
ci.glmmTMB <- function(x,
                       ci = .95,
                       component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                       method = c("wald", "ml1", "betwithin", "robust", "profile"),
                       verbose = TRUE,
                       ...) {
  method <- tolower(method)
  method <- match.arg(method)
  component <- match.arg(component)

  if (is.null(.check_component(x, component, verbose = verbose))) {
    return(NULL)
  }

  if (method == "robust") {
    ci_wald(model = x, ci = ci, dof = Inf, component = component, robust = TRUE)
  } else if (method == "wald") {
    ci_wald(model = x, ci = ci, dof = Inf, component = component, robust = FALSE)
  } else if (method == "ml1") {
    ci_ml1(model = x, ci = ci)
  } else if (method == "betwithin") {
    ci_betwithin(model = x, ci = ci)
  } else if (method == "profile") {
    pp <- stats::profile(x)
    out <- lapply(ci, function(i) .ci_profile_merMod(model = x, ci = i, profiled = pp, component = component, ...))
    out <- do.call(rbind, out)
  }
}


# standard_error -----


#' @rdname standard_error
#' @export
standard_error.glmmTMB <- function(model,
                                   effects = c("fixed", "random"),
                                   component = c("all", "conditional", "zi", "zero_inflated", "dispersion"),
                                   verbose = TRUE,
                                   ...) {
  component <- match.arg(component)
  effects <- match.arg(effects)

  if (effects == "random") {
    if (requireNamespace("TMB", quietly = TRUE) && requireNamespace("glmmTMB", quietly = TRUE)) {
      s1 <- TMB::sdreport(model$obj, getJointPrecision = TRUE)
      s2 <- sqrt(s1$diag.cov.random)
      rand.ef <- glmmTMB::ranef(model)[[1]]
      rand.se <- lapply(rand.ef, function(.x) {
        cnt <- nrow(.x) * ncol(.x)
        s3 <- s2[1:cnt]
        s2 <- s2[-(1:cnt)]
        d <- as.data.frame(matrix(sqrt(s3), ncol = ncol(.x), byrow = TRUE))
        colnames(d) <- colnames(.x)
        d
      })
      rand.se
    } else {
      return(NULL)
    }
  } else {
    if (is.null(.check_component(model, component, verbose = verbose))) {
      return(NULL)
    }

    cs <- .compact_list(stats::coef(summary(model)))
    x <- lapply(names(cs), function(i) {
      .data_frame(
        Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
        SE = as.vector(cs[[i]][, 2]),
        Component = i
      )
    })

    se <- do.call(rbind, x)
    se$Component <- .rename_values(se$Component, "cond", "conditional")
    se$Component <- .rename_values(se$Component, "zi", "zero_inflated")
    se$Component <- .rename_values(se$Component, "disp", "dispersion")

    .filter_component(se, component)
  }
}


# p_value -----


#' @importFrom insight find_parameters
#' @importFrom stats coef
#' @rdname p_value.lmerMod
#' @export
p_value.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
      Component = i
    )
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")
  p$Component <- .rename_values(p$Component, "disp", "dispersion")

  .filter_component(p, component)
}




# simulate model -----


#' @importFrom stats vcov setNames
#' @importFrom insight get_parameters
#' @rdname simulate_model
#' @export
simulate_model.glmmTMB <- function(model, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), verbose = FALSE, ...) {
  component <- match.arg(component)
  info <- insight::model_info(model, verbose = FALSE)

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (!is.list(info)) {
    info <- NULL
  }

  has_zeroinflated <- !is.null(info) && isTRUE(info$is_zero_inflated)
  has_dispersion <- !is.null(info) && isTRUE(info$is_dispersion)


  # check component-argument ----

  if (component == "all") {
    if (!has_zeroinflated && !has_dispersion) {
      if (verbose) insight::print_color("No zero-inflation and dispersion components. Simulating from conditional parameters.\n", "red")
      component <- "conditional"
    } else if (!has_zeroinflated && has_dispersion) {
      if (verbose) insight::print_color("No zero-inflation component. Simulating from conditional and dispersion parameters.\n", "red")
      component <- c("conditional", "dispersion")
    } else if (has_zeroinflated && !has_dispersion) {
      if (verbose) insight::print_color("No dispersion component. Simulating from conditional and zero-inflation parameters.\n", "red")
      component <- c("conditional", "zero_inflated")
    }
  } else if (component %in% c("zi", "zero_inflated") && !has_zeroinflated) {
    stop("No zero-inflation model found.")
  } else if (component == "dispersion" && !has_dispersion) {
    stop("No dispersion model found.")
  }


  if (is.null(iterations)) iterations <- 1000

  if (all(component == c("conditional", "zero_inflated"))) {
    d1 <- .simulate_model(model, iterations, component = "conditional")
    d2 <- .simulate_model(model, iterations, component = "zero_inflated")
    colnames(d2) <- paste0(colnames(d2), "_zi")
    d <- cbind(d1, d2)
  } else if (all(component == c("conditional", "dispersion"))) {
    d1 <- .simulate_model(model, iterations, component = "conditional")
    d2 <- .simulate_model(model, iterations, component = "dispersion")
    colnames(d2) <- paste0(colnames(d2), "_disp")
    d <- cbind(d1, d2)
  } else if (all(component == "all")) {
    d1 <- .simulate_model(model, iterations, component = "conditional")
    d2 <- .simulate_model(model, iterations, component = "zero_inflated")
    d3 <- .simulate_model(model, iterations, component = "dispersion")
    colnames(d2) <- paste0(colnames(d2), "_zi")
    colnames(d3) <- paste0(colnames(d3), "_disp")
    d <- cbind(d1, d2, d3)
  } else if (all(component == "conditional")) {
    d <- .simulate_model(model, iterations, component = "conditional")
  } else if (all(component %in% c("zi", "zero_inflated"))) {
    d <- .simulate_model(model, iterations, component = "zero_inflated")
  } else {
    d <- .simulate_model(model, iterations, component = "dispersion")
  }

  class(d) <- c("parameters_simulate_model", class(d))
  attr(d, "object_name") <- .safe_deparse(substitute(model))
  d
}




# simulate_parameters -----


#' @rdname simulate_parameters
#' @export
simulate_parameters.glmmTMB <- function(model,
                                        iterations = 1000,
                                        centrality = "median",
                                        ci = .95,
                                        ci_method = "quantile",
                                        test = "p-value",
                                        ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <-
    .summary_bootstrap(
      data = data,
      test = test,
      centrality = centrality,
      ci = ci,
      ci_method = ci_method,
      ...
    )

  params <- insight::get_parameters(model, ...)
  if ("Effects" %in% colnames(params) && .n_unique(params$Effects) > 1) {
    out$Effects <- params$Effects
  }

  if ("Component" %in% colnames(params) && .n_unique(params$Component) > 1) {
    out$Component <- params$Component
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci

  out
}
