# classes: .bracl, .multinom, .brmultinom

## TODO add ci_method later?

############# .bracl --------------


#' @rdname model_parameters.mlm
#' @export
model_parameters.bracl <- function(model,
                                   ci = 0.95,
                                   bootstrap = FALSE,
                                   iterations = 1000,
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   p_adjust = NULL,
                                   summary = getOption("parameters_summary", FALSE),
                                   keep = NULL,
                                   drop = NULL,
                                   verbose = TRUE,
                                   ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    verbose = verbose
  )

  # detect number of levels of response
  resp <- insight::get_response(model)

  # for cbind(), response is a data frame, not a factor. We then need to use
  # number of columns as "nl"
  if (is.data.frame(resp)) {
    nl <- ncol(resp)
  } else {
    nl <- .safe(nlevels(factor(resp)), 0)
  }

  # merge by response as well if more than 2 levels
  if (nl > 2) {
    merge_by <- c("Parameter", "Response")
  } else {
    merge_by <- "Parameter"
  }

  fun_args <- list(
    model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    summary = summary,
    vcov = NULL,
    vcov_args = NULL
  )
  fun_args <- c(fun_args, dot_args)

  out <- do.call(".model_parameters_generic", fun_args)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
ci.bracl <- function(x, ci = 0.95, method = NULL, verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(x)[1],
    function_name = "ci",
    verbose = verbose
  )

  params <- insight::get_parameters(x)
  out <- .ci_generic(model = x, ci = ci, method = method, ...)
  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }
  out
}


#' @export
standard_error.bracl <- function(model, verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "standard_error",
    verbose = verbose
  )

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  se <- smry[[2]]
  names(se) <- rownames(smry)

  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Response = params$Response
  )
}


#' @export
p_value.bracl <- function(model, verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "p_value",
    verbose = verbose
  )

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  p <- smry[[4]]
  names(p) <- rownames(smry)

  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p),
    Response = params$Response
  )
}




############# .multinom --------------


#' @export
model_parameters.multinom <- function(model,
                                      ci = 0.95,
                                      ci_method = "normal",
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      p_adjust = NULL,
                                      summary = getOption("parameters_summary", FALSE),
                                      keep = NULL,
                                      drop = NULL,
                                      verbose = TRUE,
                                      ...) {
  model_parameters.bracl(
    model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    summary = summary,
    keep = keep,
    drop = drop,
    verbose = verbose,
    ...
  )
}


#' @export
ci.multinom <- function(x, ci = 0.95, method = "normal", verbose = TRUE, ...) {
  ci.bracl(x, ci = ci, method = method, verbose = verbose, ...)
}


#' @export
standard_error.multinom <- function(model, ...) {
  se <- tryCatch(
    {
      std_err <- summary(model)$standard.errors
      if (is.null(std_err)) {
        vc <- insight::get_varcov(model)
        std_err <- as.vector(sqrt(diag(vc)))
      } else {
        if (is.matrix(std_err)) {
          tmp <- NULL
          for (i in seq_len(nrow(std_err))) {
            tmp <- c(tmp, as.vector(std_err[i, ]))
          }
        } else {
          tmp <- as.vector(std_err)
        }
        std_err <- tmp
      }
      std_err
    },
    error = function(e) {
      vc <- insight::get_varcov(model)
      as.vector(sqrt(diag(vc)))
    }
  )

  params <- insight::get_parameters(model)

  if ("Response" %in% colnames(params)) {
    .data_frame(
      Parameter = params$Parameter,
      SE = se,
      Response = params$Response
    )
  } else {
    .data_frame(
      Parameter = params$Parameter,
      SE = se
    )
  }
}


#' @export
p_value.multinom <- function(model, method = "normal", ...) {
  stat <- insight::get_statistic(model)
  out <- p_value.default(model, method = method, ...)
  if (!is.null(stat$Response)) {
    out$Response <- stat$Response
  }
  out
}


#' @export
simulate_parameters.multinom <- function(model,
                                         iterations = 1000,
                                         centrality = "median",
                                         ci = 0.95,
                                         ci_method = "quantile",
                                         test = "p-value",
                                         ...) {
  sim_data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(
    data = sim_data,
    test = test,
    centrality = centrality,
    ci = ci,
    ci_method = ci_method,
    ...
  )

  params <- insight::get_parameters(model)
  out$Parameter <- params$Parameter
  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "centrality") <- centrality

  out
}




############# .brmultinom --------------


#' @export
model_parameters.brmultinom <- model_parameters.bracl


#' @export
ci.brmultinom <- ci.bracl


#' @export
standard_error.brmultinom <- standard_error.multinom


#' @export
p_value.brmultinom <- p_value.multinom
