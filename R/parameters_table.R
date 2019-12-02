#' Parameters Table Formatting
#'
#' @param x A dataframe of model's parameters.
#' @param pretty_names Pretty parameters' names.
#' @inheritParams format_p
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#' as.data.frame(parameters_table(x))
#' \donttest{
#' library(rstanarm)
#' x <- model_parameters(stan_glm(Sepal.Length ~ Species, data = iris), ci = c(0.69, 0.89, 0.95))
#' as.data.frame(parameters_table(x))
#' }
#'
#' @return A data.frame.
#'
#' @importFrom tools toTitleCase
#' @importFrom insight format_value
#' @importFrom stats na.omit
#' @export
parameters_table <- function(x, pretty_names = TRUE, stars = FALSE, ...) {

  # check if user supplied digits attributes
  digits <- attributes(x)$digits
  ci_digits <- attributes(x)$ci_digits
  p_digits <- attributes(x)$p_digits

  if (is.null(digits)) digits <- 2
  if (is.null(ci_digits)) ci_digits <- 2
  if (is.null(p_digits)) p_digits <- 3

  x <- as.data.frame(x)

  # Format parameters names
  if (pretty_names & !is.null(attributes(x)$pretty_names)) {
    x$Parameter <- attributes(x)$pretty_names[x$Parameter]
  }

  # Format specific columns
  if ("n_Obs" %in% names(x)) x$n_Obs <- insight::format_value(x$n_Obs, protect_integers = TRUE)
  if ("n_Missing" %in% names(x)) x$n_Missing <- insight::format_value(x$n_Missing, protect_integers = TRUE)
  # generic df
  if ("df" %in% names(x)) x$df <- insight::format_value(x$df, protect_integers = TRUE)
  # residual df
  if ("df_residual" %in% names(x)) x$df_residual <- insight::format_value(x$df_residual, protect_integers = TRUE)
  names(x)[names(x) == "df_residual"] <- "df"
  # df for errors
  if ("df_error" %in% names(x)) x$df_error <- insight::format_value(x$df_error, protect_integers = TRUE)
  names(x)[names(x) == "df_error"] <- "df"

  # P values
  if ("p" %in% names(x)) {
    x$p <- format_p(x$p, stars = stars, name = NULL, missing = "", digits = p_digits)
    x$p <- format(x$p, justify = "left")
  }

  # CI
  ci_low <- names(x)[grep("CI_low*", names(x))]
  ci_high <- names(x)[grep("CI_high*", names(x))]
  if (length(ci_low) >= 1 & length(ci_low) == length(ci_high)) {
    if (is.null(attributes(x)$ci)) {
      ci_colname <- "CI"
    } else {
      ci_colname <- sprintf("%i%% CI", attributes(x)$ci * 100)
    }
    # Get characters to align the CI
    max_len_low <- max(unlist(lapply(stats::na.omit(round(x[ci_low], ci_digits)), function(.i) nchar(as.character(.i)))))
    max_len_high <- max(unlist(lapply(stats::na.omit(round(x[ci_high], ci_digits)), function(.i) nchar(as.character(.i)))))
    for (i in 1:length(ci_colname)) {
      x[ci_colname[i]] <- format_ci(x[[ci_low[i]]], x[[ci_high[i]]], ci = NULL, digits = ci_digits, width_low = max_len_low, width_high = max_len_high, brackets = TRUE)
    }
    # Replace at initial position
    ci_position <- which(names(x) == ci_low[1])
    x <- x[c(names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname], ci_colname, names(x)[ci_position:(length(names(x)) - 1)][!names(x)[ci_position:(length(names(x)) - 1)] %in% ci_colname])]
    x <- x[!names(x) %in% c(ci_low, ci_high)]
  }

  # Standardized
  std_cols <- names(x)[grepl("Std_", names(x))]
  x[std_cols] <- insight::format_value(x[std_cols], digits = digits)
  names(x)[grepl("Std_", names(x))] <- paste0(gsub("Std_", "", std_cols), " (std.)")

  # Partial
  x[names(x)[grepl("_partial", names(x))]] <- insight::format_value(x[names(x)[grepl("_partial", names(x))]])
  names(x)[grepl("_partial", names(x))] <- paste0(gsub("_partial", "", names(x)[grepl("_partial", names(x))]), " (partial)")

  # metafor
  if ("Weight" %in% names(x)) x$Weight <- insight::format_value(x$Weight, protect_integers = TRUE)

  # Bayesian
  if ("Prior_Location" %in% names(x)) x$Prior_Location <- insight::format_value(x$Prior_Location, protect_integers = TRUE)
  if ("Prior_Scale" %in% names(x)) x$Prior_Scale <- insight::format_value(x$Prior_Scale, protect_integers = TRUE)
  if ("BF" %in% names(x)) x$BF <- format_bf(x$BF, name = NULL, stars = stars)
  if ("pd" %in% names(x)) x$pd <- format_pd(x$pd, name = NULL, stars = stars)
  if ("ROPE_Percentage" %in% names(x)) x$ROPE_Percentage <- format_rope(x$ROPE_Percentage, name = NULL)
  names(x)[names(x) == "ROPE_Percentage"] <- "% in ROPE"

  # Priors
  if (all(c("Prior_Distribution", "Prior_Location", "Prior_Scale") %in% names(x))) {
    x$Prior <- paste0(
      tools::toTitleCase(x$Prior_Distribution),
      " (",
      x$Prior_Location,
      " +- ",
      x$Prior_Scale,
      ")"
    )

    col_position <- which(names(x) == "Prior_Distribution")
    x <- x[c(names(x)[0:(col_position - 1)], "Prior", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$Prior_Distribution <- x$Prior_Location <- x$Prior_Scale <- NULL
  }

  if ("Rhat" %in% names(x)) x$Rhat <- insight::format_value(x$Rhat, digits = 3)
  if ("ESS" %in% names(x)) x$ESS <- insight::format_value(x$ESS, protect_integers = TRUE)

  # Format remaining columns
  other_cols <- names(x)[sapply(x, is.numeric)]
  x[other_cols[other_cols %in% names(x)]] <- insight::format_value(x[other_cols[other_cols %in% names(x)]], digits = digits)


  # SEM links
  if (all(c("To", "Operator", "From") %in% names(x))) {
    x$Link <- paste(x$To, x$Operator, x$From)

    col_position <- which(names(x) == "To")
    x <- x[c(names(x)[0:(col_position - 1)], "Link", names(x)[col_position:(length(names(x)) - 1)])] # Replace at initial position
    x$To <- x$Operator <- x$From <- NULL
  }

  x
}
