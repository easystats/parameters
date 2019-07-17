#' Parameters Table Formatting
#'
#' @param x A dataframe of model's parameters.
#' @param clean_names Clean parameters' names if possible.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#' as.data.frame(parameters_table(x))
#'
#' @export
parameters_table <- function(x, clean_names = TRUE, ...){
  UseMethod("parameters_table")
}


#' @export
parameters_table.parameters_model <- function(x, clean_names = TRUE, ...){

  # Format parameters names
  if(clean_names & !is.null(attributes(x)$clean_names)){
    x$Parameter <- attributes(x)$clean_names
  }

  # Format specific columns
  if("DoF" %in% names(x)) x$DoF <- format_value(x$DoF, protect_integers = TRUE)
  if("DoF_residual" %in% names(x)) x$DoF_residual <- format_value(x$DoF_residual, protect_integers = TRUE)
  if("p" %in% names(x)) x$p <- ifelse(x$p < 0.001, "< .001", substring(format_value(x$p, digits = 3), 2))
  if(all(c("CI_low", "CI_high") %in% names(x))) {
    ci_colname <- sprintf("%i%% CI", attributes(x)$ci * 100)
    x[ci_colname] <- format_ci(x$CI_low, x$CI_high, ci = NULL)
    ci_position <- which(names(x) == "CI_low")
    x <- x[c(names(x)[1:(ci_position-1)], ci_colname, names(x)[ci_position:(length(names(x))-1)])]  # Replace at initial position
    x$CI_low <- x$CI_high <- NULL
  }
  # Standardized
  std_cols <- names(x)[grepl("Std_", names(x))]
  x[std_cols] <- format_value(x[std_cols])
  names(x)[grepl("Std_", names(x))] <- paste0(gsub("Std_", "", std_cols), " (std.)")

  # Partial
  x[names(x)[grepl("_partial", names(x))]] <- format_value(x[names(x)[grepl("_partial", names(x))]])
  names(x)[grepl("_partial", names(x))] <- paste0(gsub("_partial", "", names(x)[grepl("_partial", names(x))]), " (partial)")

  # Bayesian
  if("Prior_Location" %in% names(x)) x$Prior_Location <- format_value(x$Prior_Location, protect_integers = TRUE)
  if("Prior_Scale" %in% names(x)) x$Prior_Scale <- format_value(x$Prior_Scale, protect_integers = TRUE)
  if("BF" %in% names(x)) x$BF <- ifelse(x$BF > 999, "> 999", format_value(x$BF, protect_integers = TRUE))
  if("pd" %in% names(x)) x$pd <- ifelse(x$pd < 1, paste0(format_value(x$pd * 100), "%"), "100%")
  if("ROPE_Percentage" %in% names(x)) x$ROPE_Percentage <- ifelse(x$ROPE_Percentage == 0, "0%",
                                                                  ifelse(x$ROPE_Percentage == 1, "100%",
                                                                         paste0(format_value(x$pd * 100), "%")))
  names(x)[names(x) == "ROPE_Percentage"] <- "% in ROPE"

  # Priors
  if(all(c("Prior_Distribution", "Prior_Location", "Prior_Scale") %in% names(x))) {
    x$Prior <- paste0(tools::toTitleCase(x$Prior_Distribution),
                      " (",
                      x$Prior_Location,
                      " +- ",
                      x$Prior_Scale,
                      ")")

    col_position <- which(names(x) == "Prior_Distribution")
    x <- x[c(names(x)[1:(col_position-1)], "Prior", names(x)[col_position:(length(names(x))-1)])]  # Replace at initial position
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- NULL
  }

  if("Rhat" %in% names(x)) x$Rhat <- format_value(x$Rhat, digits = 3)
  if("ESS" %in% names(x)) x$ESS <- format_value(x$ESS, protect_integers = TRUE)

  # Format remaining columns
  other_cols <- c("Coefficient", "r", "rho", "Difference", "Median", "Mean", "Mean_Parameter1", "Mean_Parameter2", "Mean_Group1", "Mean_Group2", "Sum_Squares", "Mean_Square", "MAD", "SE", "SD", "t", "S", "F", "z")
  x[other_cols[other_cols %in% names(x)]] <- format_value(x[other_cols[other_cols %in% names(x)]])

  x
}


#' @export
parameters_table.parameters_sem <- parameters_table.parameters_model

#' @export
print.parameters_model <- function(x, clean_names = TRUE, ...) {
  formatted_table <- parameters_table(x, clean_names = clean_names, ...)
  cat(format_table(formatted_table))
}