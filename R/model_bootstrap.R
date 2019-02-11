#' Model bootstrapping
#'
#' Bootstrap the model n times to return a data.frame of estimates.
#'
#' @param model Statistical model.
#' @param n The number of bootstrap replicates.
#' @param silent Hide possible refit messages.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_bootstrap <- function(model, n = 1000, silent=FALSE, ...) {
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
model_bootstrap.lm <- function(model, n = 1000, silent=FALSE, ...) {
  data <- insight::get_data(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

    if(silent){
      fit <- suppressMessages(update(model, data = d))
    } else{
      fit <- update(model, data = d)
    }


    params <- insight::get_parameters(fit)
    params <- setNames(params$estimate, params$parameter) # Transform to named vector
    return(params)
  }

  results <- boot::boot(data = data, statistic = boot_function, R = n, model = model)

  df <- as.data.frame(results$t)
  names(df) <- insight::find_parameters(model)$conditional

  return(df)
}

#' @export
model_bootstrap.merMod <- model_bootstrap.lm



#' Coerce to a Data Frame
#'
#'
#' @param x Any R object.
#' @param row.names Not used.
#' @param optional Not used.
#' @param n The number of bootstrap replicates.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @method as.data.frame lm
#' @export
as.data.frame.lm <- function(x, row.names = NULL, optional = FALSE, n = 1000, ...) {
  return(model_bootstrap(x, n = n))
}


#' @export
as.data.frame.merMod <- as.data.frame.lm




