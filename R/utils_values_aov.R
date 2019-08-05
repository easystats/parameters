#' @keywords internal
.values_aov <- function(params) {

  # number of observations
  N <- sum(params$df) + 1


  if ("Group" %in% names(params)) {
    if ("Within" %in% params$Parameter) {
      stop("Omega squared not implemented yet for repeated-measures ANOVAs.")
    }
  }

  # get mean squared of residuals
  Mean_Square_residuals <- sum(params[params$Parameter == "Residuals", ]$Mean_Square)
  # get sum of squares of residuals
  Sum_Squares_residuals <- sum(params[params$Parameter == "Residuals", ]$Sum_Squares)
  # get total sum of squares
  Sum_Squares_total <- sum(params$Sum_Squares)
  # number of terms in model
  N_terms <- nrow(params) - 1



  list(
    "Mean_Square_residuals" = Mean_Square_residuals,
    "Sum_Squares_residuals" = Sum_Squares_residuals,
    "Sum_Squares_total" = Sum_Squares_total,
    "n_terms" = N_terms,
    "n" = N
  )
}
