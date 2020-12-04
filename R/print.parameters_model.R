#' @title Print model parameters
#' @name print.parameters_model
#'
#' @description A \code{print()}-method for objects from \code{\link[=model_parameters]{model_parameters()}}.
#'
#' @param x An object returned by \code{\link[=model_parameters]{model_parameters()}}.
#' @param split_components Logical, if \code{TRUE} (default), For models with
#'   multiple components (zero-inflation, smooth terms, ...), each component is
#'   printed in a separate table. If \code{FALSE}, model parameters are printed
#'   in a single table and a \code{Component} column is added to the output.
#' @param select Character vector (or numeric index) of column names that should
#'   be printed. If \code{NULL} (default), all columns are printed. The shortcut
#'   \code{select = "minimal"} prints coefficient, confidence intervals and p-values,
#'   while \code{select = "short"} prints coefficient, standard errors and p-values.
#' @param show_sigma Logical, if \code{TRUE}, adds information about the residual
#'   standard deviation.
#' @param show_formula Logical, if \code{TRUE}, adds the model formula to the output.
#' @inheritParams insight::parameters_table
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#' @inheritSection model_parameters Labeling the Degrees of Freedom
#'
#' @return Invisibly returns the original input object.
#'
#' @seealso There is a dedicated method to use inside rmarkdown files, \code{\link[=print_md.parameters_model]{print_md()}}.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("glmmTMB")) {
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   mp <- model_parameters(model)
#'
#'   print(mp, pretty_names = FALSE)
#'
#'   print(mp, split_components = FALSE)
#'
#'   print(mp, select = c("Parameter", "Coefficient", "SE"))
#'
#'   print(mp, select = "minimal")
#' }
#' }
#'
#' @importFrom insight export_table parameters_table
#' @export
print.parameters_model <- function(x,
                                   pretty_names = TRUE,
                                   split_components = TRUE,
                                   select = NULL,
                                   digits = 2,
                                   ci_digits = 2,
                                   p_digits = 3,
                                   show_sigma = FALSE,
                                   show_formula = FALSE,
                                   ...) {
  # save original input
  orig_x <- x

  # get attributes
  res <- attributes(x)$details
  sigma <- attributes(x)$sigma
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # set defaults, if necessary
  if (is.null(sigma)) {
    show_sigma <- FALSE
  }

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)


  # print header
  if (!is.null(attributes(x)$title)) {
    insight::print_color(paste0("# ", attributes(x)$title, "\n\n"), "blue")
  } else if (!is.null(res)) {
    insight::print_color("# Fixed Effects\n\n", "blue")
  }

  formatted_table <- format(
    x,
    pretty_names = pretty_names,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = TRUE,
    format = "text"
  )

  # prepare footer
  footer <- NULL

  # footer: residual standard deviation
  if (!is.null(sigma) && isTRUE(show_sigma)) {
    footer <- sprintf("\nResidual standard deviation: %.*f", digits, sigma)
  }

  # footer: p-adjustment
  if (!is.null(p_adjust) && p_adjust != "none" && "p" %in% colnames(x) && isTRUE(verbose)) {
    p_adj_string <- switch(
      p_adjust,
      "holm" = "Holm (1979)",
      "hochberg" = "Hochberg (1988)",
      "homnmel" = "Hochberg (1988)",
      "bonferroni" = "Bonferroni",
      "fdr" = ,
      "BH" = "Benjamini & Hochberg (1995)",
      "BY" = " Benjamini & Yekutieli (2001)",
      p_adjust
    )
    footer <- paste0(footer, "\np-value adjustment method: ", p_adj_string)
  }

  # footer: model formula
  if (!is.null(model_formula) && isTRUE(show_formula)) {
    footer <- paste0(footer, "\nModel: ", model_formula)
  }

  cat(insight::export_table(formatted_table, format = "text", footer = c(footer, "blue")))

  # for Bayesian models
  if (!is.null(ci_method) && isTRUE(verbose)) {
    ci_method <- switch(
      toupper(ci_method),
      "HDI" = "highest density intervals",
      "ETI" = "equal-tailed intervals",
      "SI" = "support intervals",
      "uncertainty intervals"
    )
    message(paste0("\nUsing ", ci_method, " as credible intervals."))
  }

  # print summary for random effects
  if (!is.null(res)) {
    if (isTRUE(show_sigma)) {
      cat("\n\n")
    } else {
      cat("\n")
    }
    .print_random_parameters(res, digits = digits)
  }
  invisible(orig_x)
}

#' @export
print.parameters_simulate <- print.parameters_model

#' @export
print.parameters_brms_meta <- print.parameters_model





# Random effects ------------------

#' @export
print.parameters_random <- function(x, digits = 2, ...) {
  .print_random_parameters(x, digits = digits)
  invisible(x)
}





# Stan models ------------------

#' @export
print.parameters_stan <- function(x,
                                  split_components = TRUE,
                                  select = NULL,
                                  digits = 2,
                                  ci_digits = 2,
                                  p_digits = 3,
                                  ...) {
  orig_x <- x
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)


  formatted_table <- format(
    x,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    format = "text",
    ci_width = "auto",
    ci_brackets = TRUE,
    ...
  )
  cat(insight::export_table(formatted_table, format = "text"))

  if (!is.null(ci_method) && isTRUE(verbose)) {
    ci_method <- switch(
      toupper(ci_method),
      "HDI" = "highest density intervals",
      "ETI" = "equal-tailed intervals",
      "SI" = "support intervals",
      "uncertainty intervals"
    )
    message(paste0("\nUsing ", ci_method, " as credible intervals."))
  }

  invisible(orig_x)
}






# helper --------------------


#' @importFrom insight print_color
#' @keywords internal
.print_random_parameters <- function(random_params, digits = 2) {
  insight::print_color("# Random Effects\n\n", "blue")

  # create SD
  random_params$SD <- NA
  var_components <- random_params$Description %in% c("Within-Group Variance", "Between-Group Variance")
  random_params$SD[var_components] <- sqrt(random_params$Value[var_components])

  # format values
  random_params$Value <- format(sprintf("%g", round(random_params$Value, digits = digits)), justify = "right")
  random_params$SD[var_components] <- format(sprintf("(%g)", round(random_params$SD[var_components], digits = digits)), justify = "right")

  # create summary-information for each component
  random_params$Line <- ""
  random_params$Term[is.na(random_params$Term)] <- ""
  random_params$SD[is.na(random_params$SD)] <- ""

  non_empty <- random_params$Term != "" & random_params$Type != ""
  random_params$Line[non_empty] <- sprintf("%s (%s)", random_params$Type[non_empty], random_params$Term[non_empty])

  non_empty <- random_params$Term != "" & random_params$Type == ""
  random_params$Line[non_empty] <- sprintf("%s", random_params$Term[non_empty])

  # final fix, indentions
  random_params$Line <- sprintf("  %s", format(random_params$Line))
  max_len <- max(nchar(random_params$Line)) + 2

  out <- split(random_params, factor(random_params$Description, levels = unique(random_params$Description)))

  for (i in out) {
    if ("Within-Group Variance" %in% i$Description) {
      insight::print_color(format("Within-Group Variance", width = max_len), color = "blue")
      cat(sprintf("%s %s\n", i$Value, i$SD))
    } else if ("Between-Group Variance" %in% i$Description) {
      insight::print_color("Between-Group Variance\n", "blue")
      for (j in 1:nrow(i)) {
        cat(sprintf("%s  %s %s\n", i$Line[j], i$Value[j], i$SD[j]))
      }
    } else if ("Correlations" %in% i$Description) {
      insight::print_color("Correlations\n", "blue")
      for (j in 1:nrow(i)) {
        cat(sprintf("%s  %s\n", i$Line[j], i$Value[j]))
      }
    } else if ("N" %in% i$Description) {
      insight::print_color("N (groups per factor)\n", "blue")
      for (j in 1:nrow(i)) {
        cat(sprintf("  %s%s\n", format(i$Term[j], width = max_len - 2), i$Value[j]))
      }
    } else if ("Observations" %in% i$Description) {
      insight::print_color(format("Observations", width = max_len), color = "blue")
      cat(sprintf("%s\n", i$Value))
    }
  }
}
