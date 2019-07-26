#' Parameters Table Formatting
#'
#' @param x A dataframe of model's parameters.
#' @param clean_names Clean parameters' names if possible.
#' @inheritParams format_p
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#' as.data.frame(parameters_table(x))
#'
#' \dontrun{
#' library(rstanarm)
#' x <- model_parameters(stan_glm(Sepal.Length ~ Species, data = iris), ci = c(0.69, 0.89, 0.95))
#' as.data.frame(parameters_table(x))
#' }
#'
#' @return A data.frame.
#'
#' @export
parameters_table <- function(x, clean_names = TRUE, stars = FALSE, ...){

  x <- as.data.frame(x)

  # Format parameters names
  if(clean_names & !is.null(attributes(x)$clean_names)){
    x$Parameter <- attributes(x)$clean_names
  }

  # Format specific columns
  if("df" %in% names(x)) x$df <- format_value(x$df, protect_integers = TRUE)
  if("df_residual" %in% names(x)) x$df_residual <- format_value(x$df_residual, protect_integers = TRUE)
  names(x)[names(x) == "df_residual"] <- "df"
  if("p" %in% names(x)) x$p <- format_p(x$p, name = NULL, stars = stars)

  # CI
  ci_low <- names(x)[grep("CI_low*", names(x))]
  ci_high <- names(x)[grep("CI_high*", names(x))]
  if(length(ci_low) >= 1 & length(ci_low) == length(ci_high)){
    ci_colname <- sprintf("%i%% CI", attributes(x)$ci * 100)
    for(i in 1:length(ci_colname)){
      x[ci_colname[i]] <- format_ci(x[[ci_low[i]]], x[[ci_high[i]]], ci = NULL)
    }
    # Replace at initial position
    ci_position <- which(names(x) == ci_low[1])
    x <- x[c(names(x)[0:(ci_position-1)][!names(x)[0:(ci_position-1)] %in% ci_colname], ci_colname, names(x)[ci_position:(length(names(x))-1)][!names(x)[ci_position:(length(names(x))-1)] %in% ci_colname])]
    x <- x[!names(x) %in% c(ci_low, ci_high)]
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
  if("BF" %in% names(x)) x$BF <- format_bf(x$BF, name = NULL, stars = stars)
  if("pd" %in% names(x)) x$pd <- format_pd(x$pd, name = NULL, stars = stars)
  if("ROPE_Percentage" %in% names(x)) x$ROPE_Percentage <- format_rope(x$ROPE_Percentage, name = NULL)
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
    x <- x[c(names(x)[0:(col_position-1)], "Prior", names(x)[col_position:(length(names(x))-1)])]  # Replace at initial position
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- NULL
  }

  if("Rhat" %in% names(x)) x$Rhat <- format_value(x$Rhat, digits = 3)
  if("ESS" %in% names(x)) x$ESS <- format_value(x$ESS, protect_integers = TRUE)

  # Format remaining columns
  other_cols <- names(x)[sapply(x, is.numeric)]
  x[other_cols[other_cols %in% names(x)]] <- format_value(x[other_cols[other_cols %in% names(x)]])


  # SEM links
  if(all(c("To", "Operator", "From") %in% names(x))) {
    x$Link <- paste(x$To, x$Operator, x$From)

    col_position <- which(names(x) == "To")
    x <- x[c(names(x)[0:(col_position-1)], "Link", names(x)[col_position:(length(names(x))-1)])]  # Replace at initial position
    x$To <- x$Operator <- x$From <- NULL
  }

  x
}



#' @export
print.parameters_model <- function(x, clean_names = TRUE, ...) {
  formatted_table <- parameters_table(x, clean_names = clean_names, ...)
  cat(format_table(formatted_table))
}