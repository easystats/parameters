#' @export
model_parameters.emmGrid <- function(model, ci = .95, p_adjust = NULL, ...) {
  if (is.null(p_adjust)) {
    p_adjust <- "none"
  }

  s <- summary(model, level = ci, adjust = p_adjust)
  params <- as.data.frame(s)

  # get statistic and p
  statistic <- insight::get_statistic(model, ci = ci, adjust = p_adjust)
  p <- p_value(model, ci = ci, adjust = p_adjust)

  params$Statistic <- statistic$Statistic
  params$p <- p$p

  # Renaming
  names(params) <- gsub("Statistic", gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE), names(params))
  names(params) <- gsub("Std. Error", "SE", names(params))
  names(params) <- gsub("emmean", "Estimate", names(params))
  names(params) <- gsub("lower.CL", "CI_low", names(params))
  names(params) <- gsub("upper.CL", "CI_high", names(params))

  # Reorder
  estimate_pos <- which(colnames(s) == "emmean")
  order <- c(colnames(params)[1:(estimate_pos - 1)], "Estimate", "SE", "CI", "CI_low", "CI_high", "t", "z", "df", "df_error", "p")
  params <- params[order[order %in% names(params)]]

  params <- suppressWarnings(.add_model_parameters_attributes(params, model, ci, exponentiate = FALSE, ...))
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
