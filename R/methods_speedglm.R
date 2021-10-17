#' @export
p_value.speedlm <- function(model, ...) {
  p <- p_value.default(model, ...)
  if (!is.numeric(p$p)) {
    p$p <- tryCatch(
      {
        as.numeric(as.character(p$p))
      },
      error = function(e) {
        p$p
      }
    )
  }
  p
}
