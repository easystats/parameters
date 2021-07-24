# Packages lavaan, blavaan


# model parameters ---------------------------


#' Parameters from CFA/SEM models
#'
#' Format CFA/SEM objects from the lavaan package (Rosseel, 2012; Merkle and Rosseel 2018).
#'
#' @param model CFA or SEM created by the `lavaan::cfa` or `lavaan::sem`
#'   functions.
#' @param standardize Return standardized parameters (standardized coefficients).
#'   Can be `TRUE` (or `"all"` or `"std.all"`) for standardized
#'   estimates based on both the variances of observed and latent variables;
#'   `"latent"` (or `"std.lv"`) for standardized estimates based
#'   on the variances of the latent variables only; or `"no_exogenous"`
#'   (or `"std.nox"`) for standardized estimates based on both the
#'   variances of observed and latent variables, but not the variances of
#'   exogenous covariates. See `lavaan::standardizedsolution` for details.
#' @inheritParams model_parameters.default
#' @param component What type of links to return. Can be `"all"` or some of `c("regression", "correlation", "loading", "variance", "mean")`.
#' @param ... Arguments passed to or from other methods.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(parameters)
#'
#' # lavaan -------------------------------------
#' if (require("lavaan", quietly = TRUE)) {
#'
#'   # Confirmatory Factor Analysis (CFA) ---------
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9 "
#'   model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'   model_parameters(model)
#'   model_parameters(model, standardize = TRUE)
#'
#'   # filter parameters
#'   model_parameters(
#'     model,
#'     parameters = list(
#'       To = "^(?!visual)",
#'       From = "^(?!(x7|x8))"
#'     )
#'   )
#'
#'   # Structural Equation Model (SEM) ------------
#'
#'   structure <- "
#'     # latent variable definitions
#'       ind60 =~ x1 + x2 + x3
#'       dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'       dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'     # regressions
#'       dem60 ~ ind60
#'       dem65 ~ ind60 + dem60
#'     # residual correlations
#'       y1 ~~ y5
#'       y2 ~~ y4 + y6
#'       y3 ~~ y7
#'       y4 ~~ y8
#'       y6 ~~ y8
#'   "
#'   model <- lavaan::sem(structure, data = PoliticalDemocracy)
#'   model_parameters(model)
#'   model_parameters(model, standardize = TRUE)
#' }
#' @return A data frame of indices related to the model's parameters.
#'
#' @references \itemize{
#'   \item Rosseel Y (2012). lavaan: An R Package for Structural Equation
#'   Modeling. Journal of Statistical Software, 48(2), 1-36.
#'
#'   \item Merkle EC , Rosseel Y (2018). blavaan: Bayesian Structural Equation
#'   Models via Parameter Expansion. Journal of Statistical Software, 85(4),
#'   1-30. http://www.jstatsoft.org/v85/i04/
#' }
#' @export
model_parameters.lavaan <- function(model,
                                    ci = 0.95,
                                    standardize = FALSE,
                                    component = c("regression", "correlation", "loading", "defined"),
                                    keep = NULL,
                                    drop = NULL,
                                    parameters = keep,
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
                                     ci = .89,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 1.0,
                                     diagnostic = c("ESS", "Rhat"),
                                     component = "all",
                                     standardize = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     parameters = keep,
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

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))

  params
}




# ci ---------------------------


#' @export
ci.lavaan <- function(x, ci = .95, ...) {
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
print.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  verbose <- .additional_arguments(x, "verbose", TRUE)
  ci_method <- .additional_arguments(x, "ci_method", NULL)

  formatted_table <- format(x = x, digits = digits, ci_digits, p_digits = p_digits, format = "text", ci_brackets = TRUE, ci_width = "auto", ...)
  cat(insight::export_table(formatted_table, format = "text", ...))

  if (isTRUE(verbose)) {
    .print_footer_cimethod(ci_method)
  }

  invisible(x)
}


#' @export
predict.parameters_sem <- function(object, newdata = NULL, ...) {
  insight::check_if_installed("lavaan")

  as.data.frame(lavaan::lavPredict(
    attributes(object)$model,
    newdata = newdata,
    method = "EBM",
    ...
  ))
}
