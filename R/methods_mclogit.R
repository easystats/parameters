#' @export
standard_error.mclogit <- function(model, ...) {
  s <- stats::coef(summary(model))
  out <- data.frame(
    Parameter = gsub("(.*)~(.*)", "\\2", rownames(s)),
    SE = unname(s[, "Std. Error"]),
    Response = gsub("(.*)~(.*)", "\\1", rownames(s)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
