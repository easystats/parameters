#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data.frame of estimates.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' head(model_bootstrap(model))
#'
#' @param model Statistical model.
#' @param iterations The number of bootstrap replicates.
#' @param verbose Hide possible refit messages.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_bootstrap <- function(model, iterations = 1000, verbose = FALSE, ...) {
  UseMethod("model_bootstrap")
}









#' @importFrom stats coef update setNames
#' @importFrom insight get_data find_parameters get_parameters
#' @importFrom boot boot
#' @export
model_bootstrap.lm <- function(model, iterations = 1000, verbose = FALSE, ...) {
  data <- insight::get_data(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

    if (verbose) {
      fit <- suppressMessages(update(model, data = d))
    } else {
      fit <- stats::update(model, data = d)
    }


    params <- insight::get_parameters(fit)
    params <- stats::setNames(params$estimate, params$parameter) # Transform to named vector
    return(params)
  }

  results <- boot::boot(data = data, statistic = boot_function, R = iterations, model = model)

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}





#' @export
model_bootstrap.merMod <- function(model, iterations = 1000, verbose = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
  }

  boot_function <- function(model) {
    params <- insight::get_parameters(model)
    params <- stats::setNames(params$estimate, params$parameter) # Transform to named vector
    return(params)
  }

  if (verbose) {
    results <- suppressMessages(lme4::bootMer(model, boot_function, nsim = iterations, ...))
  } else {
    results <- lme4::bootMer(model, boot_function, nsim = iterations, ...)
  }

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}








# model_bootstrap.htest <- function(model, n = 1000, verbose = FALSE, ...) {
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
  return(model_bootstrap(x, iterations = iterations, verbose = verbose, ...))
}


#' @export
as.data.frame.merMod <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, verbose = FALSE, ...) {
  return(model_bootstrap(x, iterations = iterations, verbose = verbose, ...))
}
