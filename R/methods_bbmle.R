
#' @export
ci.mle2 <- ci.glm


#' @export
standard_error.mle2 <- function(model, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract standard errors.", call. = FALSE)
  }
  s <- bbmle::summary(model)
  .data_frame(
    Parameter = names(s@coef[, 2]),
    SE = unname(s@coef[, 2])
  )
}


#' @export
p_value.mle2 <- function(model, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract p-values.", call. = FALSE)
  }
  s <- bbmle::summary(model)
  .data_frame(
    Parameter = names(s@coef[, 4]),
    p = unname(s@coef[, 4])
  )
}


#' @export
model_parameters.mle2 <- model_parameters.glm
