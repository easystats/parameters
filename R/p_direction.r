#' @export
p_direction.lm <- function(x, dof = NULL, ...) {
  # check for valid input
  .is_model_valid(x)
  # required parameters
  stats <- insight::get_statistic(x)
  if (is.null(dof)) {
    dof <- degrees_of_freedom(x)
  }
  pd <- stats::pt(abs(stats$Statistic), df = dof, lower.tail = TRUE)
  if (any(pd < 0.5)) {
    pd[pd < 0.5] <- 1 - pd[pd < 0.5]
  }
  out <- .data_frame(Parameter = stats$Parameter, pd = pd)
  if (!is.null(stats$Component)) {
    out$Component <- stats$Component
  }
  out
}
