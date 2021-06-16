#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param type Character string specifying the type of bootstrap. For mixed models
#'   of class \code{merMod} or \code{glmmTMB}, may be \code{"parametric"} (default) or
#'   \code{"semiparametric"} (see \code{?lme4::bootMer} for details). For all
#'   other models, see argument \code{sim} in \code{?boot::boot} (defaults to
#'   \code{"ordinary"}).
#' @param parallel The type of parallel operation to be used (if any).
#' @param n_cpus Number of processes to be used in parallel operation.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams p_value
#'
#' @return A data frame of bootstrapped estimates.
#'
#' @details By default, \code{boot::boot()} is used to generate bootstraps from
#' the model data, which are then used to \code{update()} the model, i.e. refit
#' the model with the bootstrapped samples. For \code{merMod} objects (\pkg{lme4})
#' or models from \pkg{glmmTMB}, the \code{lme4::bootMer()} function is used to
#' obtain bootstrapped samples. \code{bootstrap_parameters()} summarizes the
#' bootstrapped model estimates.
#'
#' @section Using with \code{emmeans}:
#' The output can be passed directly to the various functions from the
#' \code{emmeans} package, to obtain bootstrapped estimates, contrasts, simple
#' slopes, etc. and their confidence intervals. These can then be passed to
#' \code{model_parameter()} to obtain standard errors, p-values, etc (see
#' example).
#' \cr\cr
#' Note that that p-values returned here are estimated under the assumption of
#' \emph{translation equivariance}: that shape of the sampling distribution is
#' unaffected by the null being true or not. If this assumption does not hold,
#' p-values can be biased, and it is suggested to use proper permutation tests
#' to obtain non-parametric p-values.
#'
#' @seealso \code{\link{bootstrap_parameters}}, \code{\link{simulate_model}}, \code{\link{simulate_parameters}}
#'
#' @examples
#' \dontrun{
#' if (require("boot", quietly = TRUE)) {
#'   model <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#'   b <- bootstrap_model(model)
#'   print(head(b))
#'
#'   if (require("emmeans")) {
#'     est <- emmeans(b, consec ~ cyl)
#'     print(model_parameters(est))
#'   }
#' }
#' }
#' @export
bootstrap_model <- function(model,
                            iterations = 1000,
                            ...) {
  UseMethod("bootstrap_model")
}






#' @rdname bootstrap_model
#' @export
bootstrap_model.default <- function(model,
                                    iterations = 1000,
                                    type = "ordinary",
                                    parallel = c("no", "multicore", "snow"),
                                    n_cpus = 1,
                                    verbose = FALSE,
                                    ...) {
  insight::check_if_installed("boot")

  type <- match.arg(type, choices = c("ordinary", "parametric", "balanced", "permutation", "antithetic"))
  parallel <- match.arg(parallel)

  model_data <- data <- insight::get_data(model)
  model_response <- insight::find_response(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

    if (inherits(model, "biglm")) {
      fit <- suppressMessages(stats::update(model, moredata = d))
    } else {
      if (verbose) {
        fit <- stats::update(model, data = d)
      } else {
        fit <- suppressMessages(stats::update(model, data = d))
      }
    }

    params <- insight::get_parameters(fit, verbose = FALSE)
    n_params <- insight::n_parameters(model)

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }

    return(params)
  }

  if (type == "parametric") {
    f <- function(x, mle) {
      out <- model_data
      resp <- simulate(x, nsim = 1)
      out[[model_response]] <- resp
      return(out)
    }
    results <- boot::boot(
      data = data,
      statistic = boot_function,
      R = iterations,
      sim = type,
      parallel = parallel,
      ncpus = n_cpus,
      model = model,
      ran.gen = f
    )
  } else {
    results <- boot::boot(
      data = data,
      statistic = boot_function,
      R = iterations,
      sim = type,
      parallel = parallel,
      ncpus = n_cpus,
      model = model
    )
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::get_parameters(model, verbose = FALSE)$Parameter

  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


#' @rdname bootstrap_model
#' @export
bootstrap_model.merMod <- function(model,
                                   iterations = 1000,
                                   type = "parametric",
                                   parallel = c("no", "multicore", "snow"),
                                   n_cpus = 1,
                                   verbose = FALSE,
                                   ...) {
  insight::check_if_installed("lme4")

  type <- match.arg(type, choices = c("parametric", "semiparametric"))
  parallel <- match.arg(parallel)

  boot_function <- function(model) {
    params <- insight::get_parameters(model, verbose = FALSE)
    n_params <- insight::n_parameters(model)

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }
    return(params)
  }

  if (verbose) {
    results <- lme4::bootMer(
      model,
      boot_function,
      nsim = iterations,
      type = type,
      parallel = parallel,
      ncpus = n_cpus,
      ...
    )
  } else {
    results <- suppressMessages(lme4::bootMer(
      model,
      boot_function,
      nsim = iterations,
      verbose = FALSE,
      type = type,
      parallel = parallel,
      ncpus = n_cpus,
      ...
    ))
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::find_parameters(model, effects = "fixed")$conditional
  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


#' @export
bootstrap_model.glmmTMB <- bootstrap_model.merMod






# bootstrap_model.htest <- function(model, n = 1000, verbose = FALSE, ...) {
#   data <- insight::get_data(model)
#
#   boot_function <- function(model, data, indices) {
#     d <- data[indices, ] # allows boot to select sample
#
#     if (verbose) {
#       fit <- suppressMessages(update(model, data = d))
#     } else {
#       fit <- update(model, data = d)
#     }
#
#     return(model$estimate)
#   }
#
#   results <- boot::boot(data = data, statistic = boot_function, R = n, model = model)
#
#   return(results)
# }











#' @export
as.data.frame.lm <- function(x,
                             row.names = NULL,
                             optional = FALSE,
                             iterations = 1000,
                             verbose = FALSE,
                             ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}


#' @export
as.data.frame.merMod <- function(x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 iterations = 1000,
                                 verbose = FALSE,
                                 ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}


#' @export
as.data.frame.glmmTMB <- function(x,
                                  row.names = NULL,
                                  optional = FALSE,
                                  iterations = 1000,
                                  verbose = FALSE,
                                  ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}
