#' @rdname p_value_satterthwaite
#' @importFrom insight get_parameters
#' @export
se_satterthwaite <- function(model) {
  UseMethod("se_satterthwaite")
}

#' @export
se_satterthwaite.default <- function(model) {
  standard_error(model)
}

#' @importFrom stats setNames
#' @export
se_satterthwaite.lme <- function(model) {
  if (!requireNamespace("lavaSearch2", quietly = TRUE)) {
    stop("Package `lavaSearch2` required for Satterthwaite approximation.", call. = FALSE)
  }
  params <- insight::get_parameters(model, effects = "fixed")
  lavaSearch2::sCorrect(model) <- TRUE
  s <- lavaSearch2::summary2(model)

  data.frame(
    Parameter = params$Parameter,
    SE = as.vector(s$tTable[, "Std.Error"]),
    stringsAsFactors = FALSE
  )
}

#' @export
se_satterthwaite.gls <- se_satterthwaite.lme
