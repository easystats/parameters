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


  # For Bayesian models, we need to prettify parameter names here...

  mc <- attributes(x)$model_class
  cp <- attributes(x)$cleaned_parameters
  if (!is.null(mc) && !is.null(cp) && mc %in% c("stanreg", "stanmvreg", "brmsfit")) {
    x$Parameter <- cp
    pretty_names <- FALSE
  }


  split_by <- ""
  split_by <- c(split_by, ifelse("Component" %in% names(x) && length(unique(x$Component)) > 1, "Component", ""))
  split_by <- c(split_by, ifelse("Effects" %in% names(x) && length(unique(x$Effects)) > 1, "Effects", ""))
  split_by <- c(split_by, ifelse("Response" %in% names(x) && length(unique(x$Response)) > 1, "Response", ""))

  split_by <- split_by[nchar(split_by) > 0]

  if (split_components && !is.null(split_by) && length(split_by)) {
    .print_model_parms_components(x, pretty_names, split_column = split_by, ...)
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

  # set up split-factor
  if (length(split_column) > 1) {
    split_by <- lapply(split_column, function(i) x[[i]])
  } else {
    split_by <- list(x[[split_column]])
  }
  names(split_by) <- split_column

  # make sure we have correct sorting here...
  tables <- split(x, f = split_by)

  for (type in names(tables)) {

    # Don't print Component column
    for (i in split_column) {
      tables[[type]][[i]] <- NULL
    }

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
      "fixed" = ,
      "conditional" = "Fixed Effects",
      "random" = "Random Effects",
      "conditional.fixed" = "Fixed Effects (Count Model)",
      "conditional.random" = "Random Effects (Count Model)",
      "zero_inflated" = "Zero-Inflated",
      "zero_inflated.fixed" = "Fixed Effects (Zero-Inflated Model)",
      "zero_inflated.random" = "Random Effects (Zero-Inflated Model)",
      "smooth_sd" = "Smooth Terms (SD)",
      "smooth_terms" = "Smooth Terms",
      "sigma" = "Sigma",
      "Correlation" = "Correlation",
      "Loading" = "Loading",
      "nu" = "Nu",
      "tau" = "Tau",
      type
    )


    if (length(split_column) > 1) {
      s1 <- component_name
      s2 <- ""
    } else if (split_column == "Response" && is_ordinal_model) {
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
