# classes: .bracl, .multinom, .brmultinom


############# .bracl --------------


#' @rdname model_parameters.mlm
#' @export
model_parameters.bracl <- function(model,
                                   ci = .95,
                                   bootstrap = FALSE,
                                   iterations = 1000,
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   p_adjust = NULL,
                                   verbose = TRUE,
                                   ...) {

  # detect number of levels of response
  nl <- tryCatch(
    {
      nlevels(factor(insight::get_response(model)))
    },
    error = function(e) {
      0
    }
  )

  # merge by response as well if more than 2 levels
  if (nl > 2) {
    merge_by <- c("Parameter", "Response")
  } else {
    merge_by <- "Parameter"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
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
ci.bracl <- function(x, ci = .95, method = NULL, robust = FALSE, ...) {
  params <- insight::get_parameters(x)
  out <- .ci_generic(model = x, ci = ci, dof = Inf, method = method, robust = robust, ...)
  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }
  out
}


#' @export
standard_error.bracl <- function(model, ...) {
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
p_value.bracl <- function(model, ...) {
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
model_parameters.multinom <- model_parameters.bracl


#' @export
ci.multinom <- ci.bracl


#' @export
standard_error.multinom <- function(model, ...) {
  se <- tryCatch(
    {
      stderr <- summary(model)$standard.errors
      if (is.null(stderr)) {
        vc <- insight::get_varcov(model)
        stderr <- as.vector(sqrt(diag(vc)))
      } else {
        if (is.matrix(stderr)) {
          tmp <- c()
          for (i in 1:nrow(stderr)) {
            tmp <- c(tmp, as.vector(stderr[i, ]))
          }
        } else {
          tmp <- as.vector(stderr)
        }
        stderr <- tmp
      }
      stderr
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
p_value.multinom <- function(model, ...) {
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  out <- .data_frame(
    Parameter = stat$Parameter,
    p = as.vector(p)
  )
  if (!is.null(stat$Response)) {
    out$Response <- stat$Response
  }
  out
}


#' @export
simulate_parameters.multinom <- function(model,
                                         iterations = 1000,
                                         centrality = "median",
                                         ci = .95,
                                         ci_method = "quantile",
                                         test = "p-value",
                                         ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(
    data = data,
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
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "iterations") <- iterations
  attr(out, "ci") <- ci

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
