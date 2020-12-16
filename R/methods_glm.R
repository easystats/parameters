# classes: .glm


#################### .glm


#' @export
standard_error.glm <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    standard_error_robust(model, ...)
  } else {
    se <- .get_se_from_summary(model)
    .data_frame(
      Parameter = names(se),
      SE = as.vector(se)
    )
  }
}
