#' @export
degrees_of_freedom.serp <- function(model, method = "normal", ...) {
  if (is.null(method)) {
    method <- "wald"
  }

  method <- match.arg(tolower(method), choices = c("analytical", "any", "fit", "wald", "residual", "normal"))

  if (method %in% c("residual", "fit")) {
    model$rdf
  } else {
    degrees_of_freedom.default(model, method = method, ...)
  }
}
