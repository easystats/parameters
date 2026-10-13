# Packages lavaan, blavaan

# model parameters ---------------------------

#' @rdname model_parameters.principal
#' @export
model_parameters.lavaan <- function(
  model,
  ci = 0.95,
  standardize = FALSE,
  component = c("regression", "correlation", "loading", "defined"),
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  params <- .extract_parameters_lavaan(
    model,
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
model_parameters.blavaan <- function(
  model,
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
  ...
) {
  # standardized parameters are summarized from the standardized draws. The
  # methods of `standardize_parameters()`, including "refit", do not work for
  # SEM.
  posterior <- model
  if (isFALSE(standardize)) {
    standardize <- NULL
  }
  if (!is.null(standardize)) {
    std_type <- .blavaan_std_type(standardize)
    if (!is.null(std_type)) {
      if (isTRUE(test == "all")) {
        insight::format_error(
          "`test = \"all\"` is not supported when standardizing;",
          "Please specify the tests you want to perform using the `test` argument."
        )
      }
      posterior <- .blavaan_standardized_draws(model, type = std_type, verbose = verbose)
      test <- .remove_scale_dependent_tests(test, verbose)
    } else if (verbose) {
      insight::format_warning(
        "`standardize` should be one of `TRUE`, \"all\", \"std.all\", \"latent\", \"std.lv\", \"no_exogenous\" or \"std.nox\" for models from package `blavaan`.", # nolint
        "Returning unstandardized parameters."
      )
    }
    standardize <- NULL
  }

  # Processing
  params <- .extract_parameters_bayesian(
    posterior,
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

  # draws have no component information, so we take it from the model
  if (is.null(params$Component)) {
    cp <- insight::clean_parameters(model)
    params$Component <- cp$Component[match(params$Parameter, cp$Parameter)]
    params <- datawizard::data_relocate(params, "Component", after = "Parameter")
  }

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


# the type of `blavaan::standardizedPosterior()` for a value of `standardize`,
# mapped as in `model_parameters.lavaan()`, or `NULL` for unsupported values
.blavaan_std_type <- function(standardize) {
  if (isTRUE(standardize)) {
    return("std.all")
  }
  if (!is.character(standardize) || length(standardize) != 1L) {
    return(NULL)
  }
  switch(
    standardize,
    all = ,
    std.all = "std.all",
    latent = ,
    std.lv = "std.lv",
    no_exogenous = ,
    std.nox = "std.nox",
    NULL
  )
}


.blavaan_standardized_draws <- function(model, type = "std.all", verbose = TRUE) {
  # older versions of insight give the standardized draws shifted names
  if (.insight_version() < "1.5.4.17") {
    insight::format_error(
      "Standardized parameters for models from package `blavaan` require package `insight` version 1.5.4.17 or higher.", # nolint
      "Please update `insight`, for example with `install.packages(\"insight\", repos = \"https://easystats.r-universe.dev\")`." # nolint
    )
  }
  if (type == "std.all") {
    return(insight::get_parameters(model, standardize = TRUE))
  }

  # older versions of insight do not pass `type` on to blavaan
  if (.insight_version() < "1.5.4.29") {
    insight::format_error(
      paste0(
        "Standardized parameters of type \"", type,
        "\" for models from package `blavaan` require package `insight` version 1.5.4.29 or higher."
      ),
      "Please update `insight`, for example with `install.packages(\"insight\", repos = \"https://easystats.r-universe.dev\")`." # nolint
    )
  }

  # lavaan warns for each draw when the model has no exogenous covariates,
  # because std.nox then equals std.all. We warn once instead.
  no_exogenous <- FALSE
  draws <- withCallingHandlers(
    insight::get_parameters(model, standardize = TRUE, type = type),
    warning = function(w) {
      msg <- gsub("\\s+", " ", conditionMessage(w))
      if (grepl("`std.nox' values are identical to `std.all' values", msg, fixed = TRUE)) {
        no_exogenous <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  if (no_exogenous && verbose) {
    insight::format_warning(
      "Standardized parameters of type \"std.nox\" are identical to those of type \"std.all\", because the model has no exogenous covariates." # nolint
    )
  }
  draws
}


.insight_version <- function() {
  utils::packageVersion("insight")
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
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", 2)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", 3)
  }

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
