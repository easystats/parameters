
#' @rdname ci.merMod
#' @export
ci.zeroinfl <- ci.glmmTMB

#' @rdname ci.merMod
#' @export
ci.hurdle <- ci.glmmTMB

#' @export
ci.zerocount <- ci.glmmTMB


#' @rdname standard_error
#' @export
standard_error.zeroinfl <- function(model,
                                    component = c("all", "conditional", "zi", "zero_inflated"),
                                    method = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")

    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      SE = as.vector(stats[, 2]),
      Component = comp
    )
  })

  se <- do.call(rbind, x)
  se$Component <- .rename_values(se$Component, "cond", "conditional")
  se$Component <- .rename_values(se$Component, "zi", "zero_inflated")

  .filter_component(se, component)
}


#' @export
standard_error.hurdle <- standard_error.zeroinfl


#' @export
standard_error.zerocount <- standard_error.zeroinfl


#' Parameters from Zero-Inflated Models
#'
#' Parameters from zero-inflated models.
#'
#' @param model A model with zero-inflation component.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("pscl")) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.zeroinfl <- function(model,
                                      ci = .95,
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      component = c("all", "conditional", "zi", "zero_inflated"),
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      robust = FALSE,
                                      p_adjust = NULL,
                                      verbose = TRUE,
                                      ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }


  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = standardize,
      robust = robust,
      p_adjust = p_adjust,
      ...
    )
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)

  parameters <- .add_model_parameters_attributes(
    parameters,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.hurdle <- model_parameters.zeroinfl


#' @export
model_parameters.zerocount <- model_parameters.zeroinfl


#' @export
simulate_parameters.zeroinfl <- function(model,
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

  params <- insight::get_parameters(model)
  if ("Effects" %in% colnames(params) && .n_unique(params$Effects) > 1) {
    out$Effects <- params$Effects
  }

  if ("Component" %in% colnames(params) && .n_unique(params$Component) > 1) {
    out$Component <- params$Component
  }

  if (inherits(model, c("zeroinfl", "hurdle", "zerocount"))) {
    out$Parameter <- gsub("^(count_|zero_)", "", out$Parameter)
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci

  out
}

#' @export
simulate_parameters.hurdle <- simulate_parameters.zeroinfl

#' @export
simulate_parameters.zerocount <- simulate_parameters.zeroinfl


