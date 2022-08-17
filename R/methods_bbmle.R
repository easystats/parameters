#' @rdname model_parameters.averaging
#' @export
model_parameters.mle2 <- model_parameters.glm


#' @export
ci.mle2 <- ci.glm


#' @export
standard_error.mle2 <- function(model, ...) {
  insight::check_if_installed("bbmle")
  s <- bbmle::summary(model)
  .data_frame(
    Parameter = names(s@coef[, 2]),
    SE = unname(s@coef[, 2])
  )
}


#' @export
p_value.mle2 <- function(model, ...) {
  insight::check_if_installed("bbmle")
  s <- bbmle::summary(model)
  .data_frame(
    Parameter = names(s@coef[, 4]),
    p = unname(s@coef[, 4])
  )
}


#' @export
format_parameters.mle2 <- function(model, ...) {
  NULL
}
