# model_parameters --------------------

#' @rdname model_parameters.averaging
#' @export
model_parameters.logistf <- model_parameters.glm

#' @export
model_parameters.flic <- model_parameters.glm

#' @export
model_parameters.flac <- model_parameters.glm


# ci --------------------

#' @export
ci.logistf <- ci.glm

#' @export
ci.flic <- ci.glm

#' @export
ci.flac <- ci.glm


# SE --------------------

#' @export
standard_error.logistf <- function(model, ...) {
  vc <- insight::get_varcov(model, ...)
  se <- sqrt(diag(vc))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}

#' @export
standard_error.flic <- standard_error.logistf

#' @export
standard_error.flac <- standard_error.logistf


# p --------------------

#' @export
p_value.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(s$prob)),
    p = as.vector(s$prob)
  )
}

#' @export
p_value.flic <- p_value.logistf

#' @export
p_value.flac <- p_value.logistf
