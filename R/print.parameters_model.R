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
#' @param caption Table caption as string. If \code{NULL}, no table caption is printed.
#' @param footer_digits Number of decimal places for values in the footer summary.
#' @param group Named character vector, can be used to group parameters in the
#'   printed output. The lefthand-side (names) indicate the names of a group,
#'   which will be inserted as "header row", while the righthand-side (values)
#'   should match the name of a parameter where the group starts. See 'Examples'.
#' @inheritParams insight::format_table
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#' @inheritSection model_parameters Labeling the Degrees of Freedom
#'
#' @return Invisibly returns the original input object.
#'
#' @seealso There is a dedicated method to use inside rmarkdown files,
#'   \code{\link[=print_md.parameters_model]{print_md()}}.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("glmmTMB", quietly = TRUE)) {
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
#'
#' # group parameters ------
#'
#' data(iris)
#' model <- lm(
#'   Sepal.Width ~ Petal.Length + Species + Sepal.Length,
#'   data = iris
#' )
#' # don't select "Intercept" parameter
#' mp <- model_parameters(m, parameters = "^(?!\\(Intercept)")
#' print(mp, group = c("Group Petal" = "Petal.Length",
#'                     "Group Sepal" = "Sepal.Length",
#'                     "Group Species = "Species"))
#' }
#' @export
print.parameters_model <- function(x,
                                   pretty_names = TRUE,
                                   split_components = TRUE,
                                   select = NULL,
                                   caption = NULL,
                                   digits = 2,
                                   ci_digits = 2,
                                   p_digits = 3,
                                   footer_digits = 3,
                                   show_sigma = FALSE,
                                   show_formula = FALSE,
                                   zap_small = FALSE,
                                   group = NULL,
                                   ...) {
  # save original input
  orig_x <- x


  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", ci_digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }
  if (missing(footer_digits)) {
    footer_digits <- .additional_arguments(x, "footer_digits", footer_digits)
  }


  # table caption
  table_caption <- .print_caption(x, caption, format = "text")

  # main table
  formatted_table <- .print_core(
    x = x,
    pretty_names = pretty_names,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    zap_small = zap_small,
    ci_width = "auto",
    ci_brackets = TRUE,
    format = "text",
    group = group,
    ...
  )

  # footer
  footer <- .print_footer(
    x,
    digits = footer_digits,
    show_sigma = show_sigma,
    show_formula = show_formula
  )

  # get attributes
  ci_method <- .additional_arguments(x, "ci_method", NULL)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # print main table
  cat(insight::export_table(
    formatted_table,
    format = "text",
    caption = table_caption,
    footer = footer
  ))

  # for Bayesian models
  if (isTRUE(verbose)) {
    .print_footer_cimethod(ci_method)
  }

  invisible(orig_x)
}

#' @export
print.parameters_simulate <- print.parameters_model

#' @export
print.parameters_brms_meta <- print.parameters_model




# helper ------------------


.print_core <- function(x,
                        pretty_names = TRUE,
                        split_components = TRUE,
                        select = NULL,
                        digits = 2,
                        ci_digits = 2,
                        p_digits = 3,
                        zap_small = FALSE,
                        ci_width = "auto",
                        ci_brackets = TRUE,
                        format = "text",
                        group = NULL,
                        ...) {
  format(
    x,
    pretty_names = pretty_names,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    zap_small = zap_small,
    format = format,
    group = group,
    ...
  )
}


.print_footer <- function(x,
                          digits = 3,
                          show_sigma = FALSE,
                          show_formula = FALSE,
                          format = "text") {
  # get attributes
  sigma <- attributes(x)$sigma
  show_summary <- isTRUE(attributes(x)$show_summary)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # override defaults. if argument "summary" is called in "model_parameters()",
  # this overrides the defaults...
  show_sigma <- ifelse(show_summary, TRUE, show_sigma)
  show_formula <- ifelse(show_summary, TRUE, show_formula)
  show_r2 <- .additional_arguments(x, "show_summary", FALSE)

  # set defaults, if necessary
  if (is.null(sigma)) {
    show_sigma <- FALSE
  }

  .format_footer(
    x,
    digits = digits,
    verbose = verbose,
    show_sigma = show_sigma,
    show_formula = show_formula,
    show_r2 = show_r2,
    format = format
  )
}



.print_caption <- function(x, caption = NULL, format = "text") {
  title_attribute <- attributes(x)$title[1]

  # check effects and component parts
  if (!is.null(x$Effects) && all(x$Effects == "random")) {
    eff_name <- "Random"
  } else {
    eff_name <- "Fixed"
  }
  if (!is.null(x$Component) && all(x$Component == "zero_inflated")) {
    zero_inflated <- " (Zero-Inflated Model)"
  } else {
    zero_inflated <- ""
  }

  if (identical(format, "html") && is.null(caption)) {
    if (isTRUE(attributes(x)$is_ggeffects)) {
      table_caption <- title_attribute
    } else {
      table_caption <- "Model Summary"
    }
  } else if (isTRUE(attributes(x)$ordinal_model)) {
    table_caption <- ""
  } else if (!is.null(title_attribute) && is.null(caption)) {
    if (length(title_attribute) == 1 && title_attribute == "") {
      table_caption <- NULL
    } else {
      table_caption <- title_attribute
    }
  } else if (!is.null(caption) && caption != "") {
    table_caption <- caption
  } else if (!is.null(caption) && caption == "") {
    table_caption <- NULL
  } else if (identical(format, "text")) {
    table_caption <- c(paste0("# ", eff_name, " Effects", zero_inflated), "blue")
  } else {
    table_caption <- paste0(eff_name, " Effects", zero_inflated)
  }

  table_caption
}





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
  ci_method <- .additional_arguments(x, "ci_method", NULL)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # check if user supplied digits attributes
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", ci_digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }


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

  if (isTRUE(verbose)) {
    .print_footer_cimethod(ci_method)
  }

  invisible(orig_x)
}






# helper --------------------


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
