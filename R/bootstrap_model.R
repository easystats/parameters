#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param verbose Hide possible refit messages.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame.
#'
#' @seealso \code{\link{bootstrap_parameters}}, \code{\link{simulate_model}}, \code{\link{simulate_parameters}}
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' head(bootstrap_model(model))
#' @export
bootstrap_model <- function(model, iterations = 1000, verbose = FALSE, ...) {
  UseMethod("bootstrap_model")
}






#' @importFrom stats coef update setNames
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
    params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    return(params)
  }

  results <- boot::boot(data = data, statistic = boot_function, R = iterations, model = model)

  out <- as.data.frame(results$t)
  names(out) <- insight::get_parameters(model)$Parameter

  out
}



#' @export
bootstrap_model.merMod <- function(model, iterations = 1000, verbose = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
  }

  boot_function <- function(model) {
    params <- insight::get_parameters(model)
    params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    return(params)
  }

  if (verbose) {
    results <- suppressMessages(lme4::bootMer(model, boot_function, nsim = iterations, ...))
  } else {
    results <- lme4::bootMer(model, boot_function, nsim = iterations, ...)
  }

  out <- as.data.frame(results$t)
  names(out) <- insight::find_parameters(model, effects = "fixed")$conditional

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
as.data.frame.lm <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, verbose = FALSE, ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}


#' @export
as.data.frame.merMod <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, verbose = FALSE, ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}


#' @export
as.data.frame.glmmTMB <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, verbose = FALSE, ...) {
  bootstrap_model(x, iterations = iterations, verbose = verbose, ...)
}
