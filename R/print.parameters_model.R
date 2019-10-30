#' @title Print model parameters
#' @name print
#'
#' @description A \code{print()}-method for objects from \code{\link[=model_parameters]{model_parameters()}}.
#'
#' @param x An object returned by \code{\link[=model_parameters]{model_parameters()}}.
#' @param split_components Logical, if \code{TRUE} (default), For models with
#'   multiple components (zero-inflation, smooth terms, ...), each component is
#'   printed in a separate table. If \code{FALSE}, model parameters are printed
#'   in a single table and a \code{Component} column is added to the output.
#' @inheritParams parameters_table
#' @return \code{NULL}
#'
#' @examples
#' library(parameters)
#' library(glmmTMB)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' mp <- model_parameters(model)
#'
#' print(mp, pretty_names = FALSE)
#'
#' print(mp, split_components = FALSE)
#' @importFrom insight format_table
#' @export
print.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, ...) {

  if (!is.null(attributes(x)$title)) {
    insight::print_color(paste0("# ", attributes(x)$title, "\n\n"), "blue")
  }

  if ("Component" %in% names(x) && length(unique(x$Component)) > 1 && split_components) {
    .print_model_parms_components(x, pretty_names, ...)
  } else if ("Response" %in% names(x) && length(unique(x$Response)) > 1 && split_components) {
    .print_model_parms_components(x, pretty_names, split_column = "Response", ...)
  } else {
    formatted_table <- parameters_table(x, pretty_names = pretty_names, ...)
    cat(insight::format_table(formatted_table))
  }
}


#' @keywords internal
.print_model_parms_components <- function(x, pretty_names, split_column = "Component", ...) {

  # check if user supplied digits attributes
  digits <- attributes(x)$digits
  ci_digits <- attributes(x)$ci_digits
  p_digits <- attributes(x)$p_digits
  is_ordinal_model <- attributes(x)$ordinal_model

  # make sure we have correct sorting here...
  tables <- split(x, factor(x[[split_column]], levels = unique(x[[split_column]])))

  for (type in names(tables)) {

    # Don't print Component column
    tables[[type]][[split_column]] <- NULL

    # Smooth terms statistics
    if ("t / F" %in% names(tables[[type]])) {
      if (type == "smooth_terms") {
        names(tables[[type]])[names(tables[[type]]) == "t / F"] <- "F"
      }
      if (type == "conditional") {
        names(tables[[type]])[names(tables[[type]]) == "t / F"] <- "t"
      }
    }

    if ("z / Chisq" %in% names(tables[[type]])) {
      if (type == "smooth_terms") {
        names(tables[[type]])[names(tables[[type]]) == "z / Chisq"] <- "Chisq"
      }
      if (type == "conditional") {
        names(tables[[type]])[names(tables[[type]]) == "z / Chisq"] <- "z"
      }
    }

    # Don't print se and ci if all are missing
    if (all(is.na(tables[[type]]$SE))) tables[[type]]$SE <- NULL
    if (all(is.na(tables[[type]]$CI_low))) tables[[type]]$CI_low <- NULL
    if (all(is.na(tables[[type]]$CI_high))) tables[[type]]$CI_high <- NULL

    # Don't print if empty col
    tables[[type]][sapply(tables[[type]], function(x){all(x == "") | all(is.na(x))})] <- NULL

    attr(tables[[type]], "digits") <- digits
    attr(tables[[type]], "ci_digits") <- ci_digits
    attr(tables[[type]], "p_digits") <- p_digits

    formatted_table <- parameters_table(tables[[type]], pretty_names = pretty_names, ...)

    component_name <- switch(
      type,
      "mu" = ,
      "conditional" = "Conditional",
      "zero_inflated" = "Zero-Inflated",
      "smooth_sd" = "Smooth Terms (SD)",
      "smooth_terms" = "Smooth Terms",
      "sigma" = "Sigma",
      "Correlation" = "Correlation",
      "Loading" = "Loading",
      "nu" = "Nu",
      "tau" = "Tau",
      type
    )


    if (split_column == "Response" && is_ordinal_model) {
      s1 <- "Response level:"
      s2 <- component_name
    } else {
      s1 <- component_name
      s2 <- split_column
    }


    # Print
    insight::print_color(sprintf("# %s %s\n\n", s1, tolower(s2)), "blue")
    cat(insight::format_table(formatted_table))
    cat("\n")
  }
}
