#' Model bootstrapping
#'
#' Bootstrap the model n times to return a data.frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of bootstrap replicates.
#' @param silent Hide possible refit messages.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_bootstrap <- function(model, iterations = 1000, silent = FALSE, ...) {
  UseMethod("model_bootstrap")
}








#' Model bootstrapping
#'
#' @inheritParams model_bootstrap
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_bootstrap(model)
#' @importFrom stats coef
#' @importFrom insight get_data find_parameters get_parameters
#' @importFrom boot boot
#' @export
model_bootstrap.lm <- function(model, iterations = 1000, silent = FALSE, ...) {
  data <- insight::get_data(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

    if (silent) {
      fit <- suppressMessages(update(model, data = d))
    } else {
      fit <- update(model, data = d)
    }


    params <- insight::get_parameters(fit)
    params <- setNames(params$estimate, params$parameter) # Transform to named vector
    return(params)
  }

  results <- boot::boot(data = data, statistic = boot_function, R = iterations, model = model)

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}





#' @importFrom utils install.packages
#' @export
#' @rdname model_bootstrap.lm
model_bootstrap.merMod <- function(model, iterations = 1000, silent = FALSE, ...) {
  if (!requireNamespace("lme4")) {
    warning("This function needs `lme4` to be installed... installing now.")
    install.packages("lme4")
    requireNamespace("lme4")
  }

  boot_function <- function(model) {
    params <- insight::get_parameters(model)
    params <- setNames(params$estimate, params$parameter) # Transform to named vector
    return(params)
  }

  if (silent) {
    results <- suppressMessages(lme4::bootMer(model, boot_function, nsim = iterations, ...))
  } else {
    results <- lme4::bootMer(model, boot_function, nsim = iterations, ...)
  }

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}








# model_bootstrap.htest <- function(model, n = 1000, silent = FALSE, ...) {
#   data <- insight::get_data(model)
#
#   boot_function <- function(model, data, indices) {
#     d <- data[indices, ] # allows boot to select sample
#
#     if (silent) {
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










#' Coerce to a Data Frame
#'
#'
#' @param x Any R object.
#' @param row.names Not used.
#' @param optional Not used.
#' @param iterations The number of bootstrap replicates.
#' @param silent Hide possible refit messages.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @method as.data.frame lm
#' @export
as.data.frame.lm <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, silent = FALSE, ...) {
  return(model_bootstrap(x, iterations = iterations, silent = silent, ...))
}


#' @export
as.data.frame.merMod <- function(x, row.names = NULL, optional = FALSE, iterations = 1000, silent = FALSE, ...) {
  return(model_bootstrap(x, iterations = iterations, silent = silent, ...))
}
