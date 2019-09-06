#' @title Print model parameters
#' @name print
#'
#' @description A \code{print()}-method for objects from \code{\link[=model_parameters]{model_parameters()}}.
#'
#' @param x An object returned by \code{\link[=model_parameters]{model_parameters()}}.
#' @param split_component Logical, if \code{TRUE} (default), For models with
#'   multiple components (zero-inflation, smooth terms, ...), each component is
#'   printed in a separate table. If \code{FALSE}, model parameters are printed
#'   in a single table and a \code{Component} column is added to the output.
#' @inheritParams parameters_table
#'
#' @return \code{NULL}
#'
#' @examples
#' library(parameters)
#' library(glmmTMB)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ mined,
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
  if ("Component" %in% names(x) && length(unique(x$Component)) > 1 && split_components) {
    .print_model_parms_components(x, pretty_names, ...)
  } else {
    formatted_table <- parameters_table(x, pretty_names = pretty_names, ...)
    cat(insight::format_table(formatted_table))
  }
}



.print_model_parms_components <- function(x, pretty_names, ...) {
  l <- split(x, x$Component)

  for (i in names(l)) {
    comp <- switch(
      i,
      "mu" = ,
      "conditional" = "Conditional",
      "zero_inflated" = "Zero-Inflated",
      "smooth_terms" = "Smooth Terms",
      "sigma" = "Sigma",
      "nu" = "Nu",
      "tau" = "Tau"
    )

    insight::print_color(sprintf("# %s component\n\n", comp), "blue")

    # don't print Component column
    l[[i]]$Component <- NULL

    # don't print se and ci if all are missing
    if (all(is.na(l[[i]]$SE))) l[[i]]$SE <- NULL
    if (all(is.na(l[[i]]$CI_low))) l[[i]]$CI_low <- NULL
    if (all(is.na(l[[i]]$CI_high))) l[[i]]$CI_high <- NULL

    formatted_table <- parameters_table(l[[i]], pretty_names = pretty_names, ...)
    cat(insight::format_table(formatted_table))
    cat("\n")
  }
}