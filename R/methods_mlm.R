# classes: .mlm

#################### .mlm


#' Parameters from multinomial or cumulative link models
#'
#' Parameters from multinomial or cumulative link models
#'
#' @param model A model with multinomial or categorical response value.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @details Multinomial or cumulative link models, i.e. models where the
#'   response value (dependent variable) is categorical and has more than two
#'   levels, usually return coefficients for each response level. Hence, the
#'   output from `model_parameters()` will split the coefficient tables
#'   by the different levels of the model's response.
#'
#' @seealso [insight::standardize_names()] to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("brglm2", quietly = TRUE)) {
#'   data("stemcell")
#'   model <- bracl(
#'     research ~ as.numeric(religion) + gender,
#'     weights = frequency,
#'     data = stemcell,
#'     type = "ML"
#'   )
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.mlm <- function(model,
                                 ci = .95,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
standard_error.mlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- lapply(names(cs), function(x) {
    params <- cs[[x]]
    .data_frame(
      Parameter = rownames(params),
      SE = params[, "Std. Error"],
      Response = gsub("^Response (.*)", "\\1", x)
    )
  })
  insight::text_remove_backticks(do.call(rbind, se), verbose = FALSE)
}


#' @export
p_value.mlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- lapply(names(cs), function(x) {
    params <- cs[[x]]
    .data_frame(
      Parameter = rownames(params),
      p = params[, "Pr(>|t|)"],
      Response = gsub("^Response (.*)", "\\1", x)
    )
  })
  insight::text_remove_backticks(do.call(rbind, p), verbose = FALSE)
}


#' @export
ci.mlm <- function(x, ci = .95, ...) {
  if (is.null(insight::find_weights(x))) {
    out <- lapply(ci, function(i) {
      .ci <- stats::confint(x, level = i, ...)
      rn <- rownames(.ci)
      .data_frame(
        Parameter = gsub("([^\\:]+)(\\:)(.*)", "\\3", rn),
        CI = i,
        CI_low = .ci[, 1],
        CI_high = .ci[, 2],
        Response = gsub("([^\\:]+)(\\:)(.*)", "\\1", rn)
      )
    })
    out <- insight::text_remove_backticks(do.call(rbind, out), verbose = FALSE)
  } else {
    out <- .data_frame(.ci_generic(x, ci = ci, ...), Response = insight::get_parameters(x)$Response)
  }
  out
}


#' @export
simulate_model.mlm <- function(model, iterations = 1000, ...) {
  responses <- insight::find_response(model, combine = FALSE)
  out <- .simulate_model(model, iterations, component = "conditional", effects = "fixed")

  cn <- paste0(colnames(out), rep(responses, each = length(colnames(out)) / length(responses)))
  colnames(out) <- cn

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}


#' @export
simulate_parameters.mlm <- function(model,
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

  out$Response <- NA
  responses <- insight::find_response(model, combine = FALSE)
  for (i in responses) {
    out$Response[grepl(paste0(i, "$"), out$Parameter)] <- i
    out$Parameter <- gsub(paste0(i, "$"), "", out$Parameter)
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "object_class") <- class(model)
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}
