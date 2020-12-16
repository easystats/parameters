
#' @export
ci.clm <- ci.tobit


#' @rdname ci.merMod
#' @export
ci.clm2 <- function(x, ci = .95, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}


#' @export
ci.clmm2 <- ci.clm2


#' @rdname standard_error
#' @importFrom insight get_parameters
#' @export
standard_error.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  stats <- .get_se_from_summary(model)
  parms <- insight::get_parameters(model, component = component)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Component = parms$Component
  )
}


#' @export
standard_error.clmm2 <- standard_error.clm2


#' @export
model_parameters.clmm2 <- model_parameters.clm2


#' @rdname model_parameters.merMod
#' @export
model_parameters.clmm <- model_parameters.cpglmm
