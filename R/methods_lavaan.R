# Packages lavaan, blavaan


# model parameters ---------------------------


#' @rdname model_parameters.principal
#' @export
model_parameters.lavaan <- function(model,
                                    ci = 0.95,
                                    standardize = FALSE,
                                    component = c("regression", "correlation", "loading", "defined"),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  params <- .extract_parameters_lavaan(model,
    ci = ci,
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  # Filter
  if (all(component == "all")) {
    component <- c("regression", "correlation", "loading", "variance", "defined", "mean")
  }
  params <- params[tolower(params$Component) %in% component, ]

  # add class-attribute for printing
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))
  attr(params, "ci") <- ci
  attr(params, "model") <- model
  params
}



#' @export
model_parameters.blavaan <- function(model,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = "pd",
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     diagnostic = c("ESS", "Rhat"),
                                     component = "all",
                                     standardize = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    diagnostic = diagnostic,
    effects = "all",
    standardize = standardize,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  # Filter
  if (!all(component == "all")) {
    params <- params[tolower(params$Component) %in% component, ]
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate = FALSE,
    ci_method = ci_method,
    verbose = verbose,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))

  params
}




# ci ---------------------------


#' @export
ci.lavaan <- function(x, ci = 0.95, ...) {
  out <- .extract_parameters_lavaan(model = x, ci = ci, ...)
  out$CI <- ci
  out[out$Operator != "~1", c("To", "Operator", "From", "CI", "CI_low", "CI_high")]
}




# SE ---------------------------


#' @export
standard_error.lavaan <- function(model, ...) {
  out <- .extract_parameters_lavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "SE")]
}


#' @export
standard_error.blavaan <- function(model, ...) {
  params <- insight::get_parameters(model, ...)

  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}




# p-value ---------------------------


#' @export
p_value.lavaan <- function(model, ...) {
  out <- .extract_parameters_lavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "p")]
}


#' @export
p_value.blavaan <- p_value.BFBayesFactor




# print ---------------------------

#' @export
print.parameters_sem <- function(x, digits = 2, ci_digits = digits, p_digits = 3, ...) {
  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  verbose <- .additional_arguments(x, "verbose", TRUE)

  formatted_table <- format(
    x = x,
    digits = digits,
    ci_digits,
    p_digits = p_digits,
    format = "text",
    ci_brackets = TRUE,
    ci_width = "auto",
    ...
  )
  cat(insight::export_table(formatted_table, format = "text", ...))

  if (isTRUE(verbose)) {
    .print_footer_cimethod(x)
  }

  invisible(x)
}


#' @export
#' @inheritParams stats::predict
predict.parameters_sem <- function(object, newdata = NULL, ...) {
  insight::check_if_installed("lavaan")

  as.data.frame(lavaan::lavPredict(
    attributes(object)$model,
    newdata = newdata,
    method = "EBM",
    ...
  ))
}
