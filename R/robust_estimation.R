#' Robust estimation
#'
#' \code{standard_error_robust()}, \code{ci_robust()} and \code{p_value_robust()}
#' attempt to return indices based on robust estimation of the variance-covariance
#' matrix, using the packages \pkg{sandwich} and \pkg{clubSandwich}.
#'
#' @param model A model.
#' @param vcov_estimation String, indicating the suffix of the \code{vcov*()}-function
#'   from the \pkg{sandwich} or \pkg{clubSandwich} package, e.g. \code{vcov_estimation = "CL"}
#'   (which calls \code{\link[sandwich]{vcovCL}} to compute clustered covariance matrix
#'   estimators), or \code{vcov_estimation = "HC"} (which calls
#'   \code{\link[sandwich:vcovHC]{vcovHC()}} to compute heteroskedasticity-consistent
#'   covariance matrix estimators).
#' @param vcov_type Character vector, specifying the estimation type for the
#'   robust covariance matrix estimation (see \code{\link[sandwich:vcovHC]{vcovHC()}}
#'   or \code{clubSandwich::vcovCR()} for details).
#' @param vcov_args List of named vectors, used as additional arguments that
#'   are passed down to the \pkg{sandwich}-function specified in \code{vcov_estimation}.
#' @param ... Arguments passed to or from other methods. For \code{standard_error()},
#'   if \code{method = "robust"}, arguments \code{vcov_estimation}, \code{vcov_type}
#'   and \code{vcov_args} can be passed down to \code{standard_error_robust()}.
#' @inheritParams ci.merMod
#'
#' @note These functions rely on the \pkg{sandwich} or \pkg{clubSandwich} package
#'   (the latter if \code{vcov_estimation = "CR"} for cluster-robust standard errors)
#'   and will thus only work for those models supported by those packages.
#'
#' @examples
#' # robust standard errors, calling sandwich::vcovHC(type="HC3") by default
#' model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
#' standard_error_robust(model)
#'
#' # cluster-robust standard errors, using clubSandwich
#' iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))
#' standard_error_robust(
#'   model,
#'   vcov_type = "CR2",
#'   vcov_args = list(cluster = iris$cluster)
#' )
#' @return A data frame.
#' @export
standard_error_robust <- function(model,
                                  vcov_estimation = "HC",
                                  vcov_type = NULL,
                                  vcov_args = NULL,
                                  ...) {
  # exceptions
  if (inherits(model, "gee")) {
    return(standard_error(model, method = "robust", ...))
  }

  # check for existing vcov-prefix
  if (!grepl("^vcov", vcov_estimation)) {
    vcov_estimation <- paste0("vcov", vcov_estimation)
  }

  robust <- .robust_covariance_matrix(
    model,
    vcov_fun = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )

  robust[, c("Parameter", "SE")]
}



#' @rdname standard_error_robust
#' @export
p_value_robust <- function(model,
                           vcov_estimation = "HC",
                           vcov_type = NULL,
                           vcov_args = NULL,
                           ...) {
  # exceptions
  if (inherits(model, "gee")) {
    return(p_value(model, method = "robust", ...))
  }

  # check for existing vcov-prefix
  if (!grepl("^vcov", vcov_estimation)) {
    vcov_estimation <- paste0("vcov", vcov_estimation)
  }

  robust <- .robust_covariance_matrix(
    model,
    vcov_fun = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )

  robust[, c("Parameter", "p")]
}




#' @rdname standard_error_robust
#' @export
ci_robust <- function(model,
                      ci = 0.95,
                      vcov_estimation = "HC",
                      vcov_type = NULL,
                      vcov_args = NULL,
                      ...) {
  ci_wald(
    model = model,
    ci = ci,
    component = "conditional",
    robust = TRUE,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )
}




#' @importFrom insight n_obs
#' @importFrom stats coef pnorm pt
.robust_covariance_matrix <- function(x, vcov_fun = "vcovHC", vcov_type = NULL, vcov_args = NULL) {
  # fix default, if necessary
  if (!is.null(vcov_type) && vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
    vcov_fun <- "vcovCR"
  }

  # set default for clubSandwich
  if (vcov_fun == "vcovCR" && is.null(vcov_type)) {
    vcov_type <- "CR0"
  }

  # check if required package is available
  if (vcov_fun == "vcovCR") {
    if (!requireNamespace("clubSandwich", quietly = TRUE)) {
      stop("Package `clubSandwich` needed for this function. Please install and try again.")
    }
    .vcov <- do.call(clubSandwich::vcovCR, c(list(obj = x, type = vcov_type), vcov_args))
  } else {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package `sandwich` needed for this function. Please install and try again.")
    }
    vcov_fun <- get(vcov_fun, asNamespace("sandwich"))
    .vcov <- do.call(vcov_fun, c(list(x = x, type = vcov_type), vcov_args))
  }

  # get coefficients
  params <- insight::get_parameters(x)

  se <- sqrt(diag(.vcov))
  dendf <- degrees_of_freedom(x, method = "any")
  t.stat <- params$Estimate / se

  if (is.null(dendf)) {
    p.value <- 2 * stats::pnorm(abs(t.stat), lower.tail = FALSE)
  } else {
    p.value <- 2 * stats::pt(abs(t.stat), df = dendf, lower.tail = FALSE)
  }


  .data_frame(
    Parameter = params$Parameter,
    Estimate = params$Estimate,
    SE = se,
    Statistic = t.stat,
    p = p.value
  )
}
