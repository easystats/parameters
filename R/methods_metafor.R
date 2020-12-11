
#' @export
ci.rma <- function(x, ci = .95, ...) {
  params <- insight::get_parameters(x)
  out <- tryCatch(
    {
      tmp <- lapply(ci, function(i) {
        model <- stats::update(x, level = i)
        .data_frame(
          Parameter = params$Parameter,
          CI = i * 100,
          CI_low = as.vector(model$ci.lb),
          CI_high = as.vector(model$ci.ub)
        )
      })
      .remove_backticks_from_parameter_names(do.call(rbind, tmp))
    },
    error = function(e) {
      NULL
    }
  )
  if (is.null(out)) {
    se <- standard_error(x)
    out <- lapply(ci, function(i) {
      alpha <- (1 + i) / 2
      fac <- stats::qnorm(alpha)
      .data_frame(
        Parameter = params$Parameter,
        CI = i * 100,
        CI_low = params$Estimate - as.vector(se$SE) * fac,
        CI_high = params$Estimate + as.vector(se$SE) * fac
      )
    })
    out <- .remove_backticks_from_parameter_names(do.call(rbind, out))
  }
  out
}


#' @export
standard_error.rma <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = model[["se"]]
  )
}
