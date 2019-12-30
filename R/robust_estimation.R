#' @rdname standard_error
#' @export
standard_error_robust <- function(model,
                                  vcov_estimation = "HC",
                                  vcov_type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3"),
                                  vcov_args = NULL,
                                  ...) {

  # exceptions
  if (inherits(model, "gee")) {
    return(standard_error(model, method = "robust", ...))
  }

  # match arguments
  vcov_type <- match.arg(vcov_type)

  robust <- .robust_covariance_matrix(
    model,
    vcov_fun = paste0("vcov", vcov_estimation),
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )

  robust[, c("Parameter", "SE")]
}



#' @rdname p_value
#' @export
p_value_robust <- function(model,
                           vcov_estimation = "HC",
                           vcov_type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3"),
                           vcov_args = NULL,
                           ...) {
  # match arguments
  vcov_type <- match.arg(vcov_type)

  robust <- .robust_covariance_matrix(
    model,
    vcov_fun = paste0("vcov", vcov_estimation),
    vcov_type = vcov_type,
    vcov_args = vcov_args
  )

  robust[, c("Parameter", "p")]
}




#' @rdname ci.merMod
#' @export
ci_robust <- function(model,
                      ci = 0.95,
                      vcov_estimation = "HC",
                      vcov_type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3"),
                      vcov_args = NULL,
                      ...) {
  vcov_type <- match.arg(vcov_type)

  ci_wald(
    model = model,
    ci = ci,
    component = "conditional",
    robust = TRUE,
    vcov_estimation = vcov_estimation,
    vcov_type = vcov_type,
    vcov_args = vcov_args,
    ...
  )
}




#' @importFrom insight n_obs
#' @importFrom stats coef pnorm pt
.robust_covariance_matrix <- function(x,
                       vcov_fun = "vcovHC",
                       vcov_type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3"),
                       vcov_args = NULL) {

  # match arguments
  vcov_type <- match.arg(vcov_type)

  # check if required package is available
  if (vcov_type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
    if (!requireNamespace("clubSandwich", quietly = TRUE)) {
      stop("Package `clubSandwich` needed for this function. Please install and try again.")
    }
    package <- "clubSandwich"
  } else {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package `sandwich` needed for this function. Please install and try again.")
    }
    package <- "sandwich"
  }

  # get coefficients
  est <- stats::coef(x)

  # compute robust standard errors based on vcov
  if (package == "sandwich") {
    vcov_fun <- get(vcov_fun, asNamespace("sandwich"))
    .vcov <- do.call(vcov_fun, c(list(x = x, type = vcov_type), vcov_args))
  } else {
    vcov_fun <- clubSandwich::vcovCR
    .vcov <- do.call(vcov_fun, c(list(obj = x, type = vcov_type), vcov_args))
  }

  se <- sqrt(diag(.vcov))
  dendf <- degrees_of_freedom(x, method = "any")
  t.stat <- est / se

  if (is.null(dendf)) {
    p.value <- 2 * stats::pnorm(abs(t.stat), lower.tail = FALSE)
  } else {
    p.value <- 2 * stats::pt(abs(t.stat), df = dendf, lower.tail = FALSE)
  }


  .data_frame(
    Parameter = .remove_backticks_from_string(names(est)),
    Estimate = est,
    SE = se,
    Statistic = t.stat,
    p = p.value
  )
}
