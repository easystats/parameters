#' @export
p_direction.lm <- function(x, ...) {
  # check for valid input
  .is_model_valid(x)
  # required parameters
  stats <- insight::get_statistic(x)
  dof <- degrees_of_freedom(x)
  stats::pt(abs(stats$Statistic), df = dof, lower.tail = TRUE)
}
