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
#' @inheritSection model_parameters.zcpglm Model components
#'
#' @examplesIf require("brglm2", quietly = TRUE)
#' data("stemcell", package = "brglm2")
#' model <- brglm2::bracl(
#'   research ~ as.numeric(religion) + gender,
#'   weights = frequency,
#'   data = stemcell,
#'   type = "ML"
#' )
#' model_parameters(model)
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.mlm <- function(model,
                                 ci = 0.95,
                                 vcov = NULL,
                                 vcov_args = NULL,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 p_adjust = NULL,
                                 keep = NULL,
                                 drop = NULL,
                                 verbose = TRUE,
                                 ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    vcov = vcov,
    vcov_args = vcov_args,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.mlm <- function(model,
                               vcov = NULL,
                               vcov_args = NULL,
                               ...) {
  se <- standard_error.default(model, vcov = vcov, vcov_args = vcov_args, ...)
  est <- insight::get_parameters(model, ...)
  # assumes se and est are sorted the same way
  if (isTRUE(nrow(se) == nrow(est)) && "Parameter" %in% colnames(est) && "Response" %in% colnames(est)) {
    se$Parameter <- est$Parameter
    se$Response <- est$Response
    return(se)
  } else {
    # manually
    if (!is.null(vcov)) {
      insight::format_warning(
        "Unable to extract the variance-covariance matrix requested in `vcov`."
      )
    }
    cs <- stats::coef(summary(model))
    se <- lapply(names(cs), function(x) {
      params <- cs[[x]]
      .data_frame(
        Parameter = rownames(params),
        SE = params[, "Std. Error"],
        Response = gsub("^Response (.*)", "\\1", x)
      )
    })
    se <- insight::text_remove_backticks(do.call(rbind, se), verbose = FALSE)
    return(se)
  }
}


#' @export
p_value.mlm <- function(model, vcov = NULL, vcov_args = NULL, ...) {
  out <- p_value.default(model, vcov = vcov, vcov_args = vcov_args, ...)
  est <- insight::get_parameters(model, ...)
  # assumes out and est are sorted the same way
  if (isTRUE(nrow(out) == nrow(est)) && "Parameter" %in% colnames(est) && "Response" %in% colnames(est)) {
    out$Parameter <- est$Parameter
    out$Response <- est$Response

    # manually
  } else {
    if (!is.null(vcov)) {
      insight::format_warning(
        "Unable to extract the variance-covariance matrix requested in `vcov`."
      )
    }
    cs <- stats::coef(summary(model))
    p <- lapply(names(cs), function(x) {
      params <- cs[[x]]
      .data_frame(
        Parameter = rownames(params),
        p = params[, "Pr(>|t|)"],
        Response = gsub("^Response (.*)", "\\1", x)
      )
    })
    out <- insight::text_remove_backticks(do.call(rbind, p), verbose = FALSE)
  }

  return(out)
}


#' @export
ci.mlm <- function(x,
                   vcov = NULL,
                   vcov_args = NULL,
                   ci = 0.95, ...) {
  # .ci_generic may not handle weights properly (not sure)
  if (is.null(insight::find_weights(x)) && is.null(vcov)) {
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

    # .ci_generic does handle `vcov` correctly.
  } else {
    out <- .data_frame(
      .ci_generic(
        x,
        ci = ci,
        vcov = vcov,
        vcov_args = vcov_args,
        ...
      )
    )

    resp <- insight::get_parameters(x)$Response
    if (!"Response" %in% colnames(out) && nrow(out) == length(resp)) {
      out[["Response"]] <- resp
    } else if (!isTRUE(all(out$Response == resp))) {
      insight::format_error(
        "Unable to assign labels to the model's parameters.",
        "Please report this problem to the {.pkg parameters} issue tracker:",
        "{.url https://github.com/easystats/parameters/issues}"
      )
    }
  }
  out
}


#' @export
simulate_model.mlm <- function(model, iterations = 1000, ...) {
  responses <- insight::find_response(model, combine = FALSE)
  out <- .simulate_model(model, iterations, component = "conditional", effects = "fixed", ...)

  cn <- paste0(colnames(out), rep(responses, each = length(colnames(out)) / length(responses)))
  colnames(out) <- cn

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
simulate_parameters.mlm <- function(model,
                                    iterations = 1000,
                                    centrality = "median",
                                    ci = 0.95,
                                    ci_method = "quantile",
                                    test = "p-value",
                                    ...) {
  sim_data <- simulate_model(model, iterations = iterations, ...)
  out <-
    .summary_bootstrap(
      data = sim_data,
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "object_class") <- class(model)
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}
