#' @export
model_parameters.emmGrid <- function(model,
                                     ci = .95,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
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
  names(params) <- gsub(model@misc$estName, "Estimate", names(params))
  names(params) <- gsub("lower.CL", "CI_low", names(params))
  names(params) <- gsub("upper.CL", "CI_high", names(params))
  names(params) <- gsub("asymp.LCL", "CI_low", names(params))
  names(params) <- gsub("asymp.UCL", "CI_high", names(params))

  # check if we have CIs
  if (!any(grepl("^CI_", colnames(params)))) {
    df_column <- grep("(df|df_error)", colnames(params))
    if (length(df_column) > 0) {
      df <- params[[df_column[1]]]
    } else {
      df <- Inf
    }
    fac <- stats::qt((1 + ci) / 2, df = df)
    params$CI_low <- params$Estimate - fac * params$SE
    params$CI_high <- params$Estimate + fac * params$SE
  }

  # Reorder
  estimate_pos <- which(colnames(s) == model@misc$estName)
  parameter_names <- colnames(params)[1:(estimate_pos - 1)]
  order <- c(parameter_names, "Estimate", "SE", "CI_low", "CI_high", "t", "z", "df", "df_error", "p")
  params <- params[order[order %in% names(params)]]

  # rename
  names(params) <- gsub("Estimate", "Coefficient", names(params))

  params <- suppressWarnings(.add_model_parameters_attributes(params, model, ci, exponentiate = FALSE, ...))
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(params, "parameter_names") <- parameter_names

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}



#' @export
model_parameters.emm_list <- function(model,
                                      ci = .95,
                                      exponentiate = FALSE,
                                      p_adjust = NULL,
                                      verbose = TRUE,
                                      ...) {
  params <-
    suppressMessages(suppressWarnings(
      .extract_parameters_generic(
        model,
        ci = ci,
        component = "conditional",
        merge_by = c("Parameter", "Component"),
        standardize = NULL,
        effects = "fixed",
        robust = FALSE,
        df_method = NULL,
        p_adjust = p_adjust,
        ...
      )
    ))

  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ...)

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
