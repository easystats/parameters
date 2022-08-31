#' @export
p_direction.lm <- function(x, dof = NULL, ...) {
  # check for valid input
  .is_model_valid(x)
  # required parameters
  stats <- insight::get_statistic(x)
  if (is.null(dof)) {
    dof <- degrees_of_freedom(x)
  }
  out <- .data_frame(
    Parameter = stats$Parameter,
    pd = stats::pt(abs(stats$Statistic), df = dof, lower.tail = TRUE)
  )
  if (!is.null(stats$Component)) {
    out$Component <- stats$Component
  }
  out
}
