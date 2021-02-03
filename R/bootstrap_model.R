#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams p_value
#'
#' @return A data frame of bootstrapped estimates.
#'
#' @section Using with \code{emmeans}:
#' The output can be passed directly to the various functions from the
#' \code{emmeans} package, to obtain bootstrapped estimates, contrasts, simple
#' slopes, etc, and their confidence intervals. These can then be passed to
#' \code{model_parameter()} to obtain standard errors, p-values, etc (see
#' example).
#' \cr\cr
#' Note that that p-values returned here are "reversed" p-values - they are the
#' probability under the bootstrapped distribution of obtaining the null (0) or
#' more extreme.
#'
#' @seealso \code{\link{bootstrap_parameters}}, \code{\link{simulate_model}}, \code{\link{simulate_parameters}}
#'
#' @examples
#' \donttest{
#' if (require("boot")) {
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
bootstrap_model <- function(model, iterations = 1000, verbose = FALSE, ...) {
  UseMethod("bootstrap_model")
}






#' @importFrom stats coef update setNames complete.cases
#' @importFrom insight get_data find_parameters get_parameters
#' @export
bootstrap_model.default <- function(model, iterations = 1000, verbose = FALSE, ...) {
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("Package 'boot' needed for this function to work. Please install it.")
  }

  data <- insight::get_data(model)

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

    params <- insight::get_parameters(fit)
    n_params <- insight::n_parameters(model)

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }

    return(params)
  }

  results <- boot::boot(data = data, statistic = boot_function, R = iterations, model = model)

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::get_parameters(model)$Parameter

  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}



#' @export
bootstrap_model.merMod <- function(model, iterations = 1000, verbose = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
  }

  boot_function <- function(model) {
    params <- insight::get_parameters(model)
    n_params <- insight::n_parameters(model)

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }
    return(params)
  }

  if (verbose) {
    results <- suppressMessages(lme4::bootMer(model, boot_function, nsim = iterations, verbose = FALSE))
  } else {
    results <- lme4::bootMer(model, boot_function, nsim = iterations)
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::find_parameters(model, effects = "fixed")$conditional
  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}








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
