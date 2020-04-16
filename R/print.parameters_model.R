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
#' @param select Character vector (or numeric index) of column names that should
#'   be printed. If \code{NULL} (default), all columns are printed.
#' @inheritParams parameters_table
#' @return \code{NULL}
#'
#' @examples
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
#'   print(mp, select = c("Parameter", "Coefficient", "CI_low", "CI_high"))
#' }
#' @importFrom insight format_table
#' @export
print.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  res <- attributes(x)$details

  # check if user supplied digits attributes
  if (missing(digits) && !is.null(attributes(x)$digits)) digits <- attributes(x)$digits
  if (missing(ci_digits) && !is.null(attributes(x)$ci_digits)) ci_digits <- attributes(x)$ci_digits
  if (missing(p_digits) && !is.null(attributes(x)$p_digits)) p_digits <- attributes(x)$p_digits

  # minor fix for nested Anovas
  if ("Group" %in% colnames(x) && sum(x$Parameter == "Residuals") > 1) {
    colnames(x)[which(colnames(x) == "Group")] <- "Subgroup"
  }

  if (!is.null(select)) {
    if (is.numeric(select)) select <- colnames(x)[select]
    select <- union(select, c("Component", "Effects", "Response", "Subgroup"))
    to_remove <- setdiff(colnames(x), select)
    x[to_remove] <- NULL
  }

  if (!is.null(attributes(x)$title)) {
    insight::print_color(paste0("# ", attributes(x)$title, "\n\n"), "blue")
  } else if (!is.null(res)) {
    insight::print_color("# Fixed Effects\n\n", "blue")
  }

  # For Bayesian models, we need to prettify parameter names here...

  mc <- attributes(x)$model_class
  cp <- attributes(x)$cleaned_parameters
  if (!is.null(mc) && !is.null(cp) && mc %in% c("stanreg", "stanmvreg", "brmsfit")) {
    if (length(cp) == length(x$Parameter)) {
      x$Parameter <- cp
    }
    pretty_names <- FALSE
  }


  split_by <- ""
  split_by <- c(split_by, ifelse("Component" %in% names(x) && .n_unique(x$Component) > 1, "Component", ""))
  split_by <- c(split_by, ifelse("Effects" %in% names(x) && .n_unique(x$Effects) > 1, "Effects", ""))
  split_by <- c(split_by, ifelse("Response" %in% names(x) && .n_unique(x$Response) > 1, "Response", ""))
  split_by <- c(split_by, ifelse("Subgroup" %in% names(x) && .n_unique(x$Subgroup) > 1, "Subgroup", ""))

  split_by <- split_by[nchar(split_by) > 0]

  if (split_components && !is.null(split_by) && length(split_by)) {
    .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ...)
  } else {
    formatted_table <- parameters_table(x, pretty_names = pretty_names, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ...)
    cat(insight::format_table(formatted_table))
  }

  # print summary for random effects
  if (!is.null(res)) {
    cat("\n")
    .print_random_parameters(res, digits = digits)
  }
}




#' @export
print.parameters_random <- function(x, digits = 2, ...) {
  .print_random_parameters(x, digits = digits)
}




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





#' @keywords internal
.print_model_parms_components <- function(x, pretty_names, split_column = "Component", digits = 2, ci_digits = 2, p_digits = 3, ...) {

  # check if user supplied digits attributes
  is_ordinal_model <- attributes(x)$ordinal_model

  if (is.null(is_ordinal_model)) is_ordinal_model <- FALSE

  # make sure we have correct order of levels from split-factor
  x[split_column] <- lapply(x[split_column], function(i) {
    if (!is.factor(i)) i <- factor(i, levels = unique(i))
    i
  })

  # set up split-factor
  if (length(split_column) > 1) {
    split_by <- lapply(split_column, function(i) x[[i]])
  } else {
    split_by <- list(x[[split_column]])
  }
  names(split_by) <- split_column

  # make sure we have correct sorting here...
  tables <- split(x, f = split_by)

  # sanity check - only preserve tables with any data in data frames
  tables <- tables[sapply(tables, nrow) > 0]

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
    tables[[type]][sapply(tables[[type]], function(x) {
      all(x == "") | all(is.na(x))
    })] <- NULL

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
      "simplex.fixed" = ,
      "simplex" = "Monotonic Effects",
      "smooth_sd" = "Smooth Terms (SD)",
      "smooth_terms" = "Smooth Terms",
      "sigma" = "Sigma",
      "Correlation" = "Correlation",
      "SD/Cor" = "SD / Correlation",
      "Loading" = "Loading",
      "scale" = ,
      "scale.fixed" = "Scale Parameters",
      "extra" = ,
      "extra.fixed" = "Extra Parameters",
      "nu" = "Nu",
      "tau" = "Tau",
      "within" = "Within-Effects",
      "between" = "Between-Effects",
      "interactions" = "(Cross-Level) Interactions",
      "precision" = ,
      "precision." = "Precision",
      type
    )


    # tweaking of sub headers

    if ("DirichletRegModel" %in% attributes(x)$model_class) {
      if (grepl("^conditional\\.", component_name) || split_column == "Response") {
        s1 <- "Response level:"
        s2 <- gsub("^conditional\\.(.*)", "\\1", component_name)
      } else {
        s1 <- component_name
        s2 <- ""
      }
    } else if (length(split_column) > 1) {
      s1 <- component_name
      s2 <- ""
    } else if (split_column == "Response" && is_ordinal_model) {
      s1 <- "Response level:"
      s2 <- component_name
    } else if (split_column == "Subgroup") {
      s1 <- component_name
      s2 <- ""
    } else if (component_name %in% c("Within-Effects", "Between-Effects")) {
      s1 <- component_name
      s2 <- ""
    } else {
      s1 <- component_name
      s2 <- split_column
    }


    # Print
    if (component_name != "rewb-contextual") {
      insight::print_color(sprintf("# %s %s\n\n", s1, tolower(s2)), "blue")
    }
    cat(insight::format_table(formatted_table))
    cat("\n")
  }
}



#' @export
print.parameters_stan <- function(x, split_components = TRUE, select = NULL, ...) {
  cp <- attributes(x)$parameter_info

  # check if user supplied digits attributes
  ci <- attributes(x)$ci
  digits <- attributes(x)$digits
  ci_digits <- attributes(x)$ci_digits
  p_digits <- attributes(x)$p_digits

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup", "Function"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    out <- insight::print_parameters(cp, x)

    for (i in out) {
      insight::print_color(paste0("# ", attr(i, "main_title")), "blue")
      cat(" ")
      insight::print_color(attr(i, "sub_title"), "red")
      cat("\n\n")

      rem <- which(colnames(i) %in% c("Parameter", "Component", "Effects", "Group", "Response", "Subgroup", "Function"))
      i <- i[, -rem]

      colnames(i)[1] <- "Parameter"
      attr(i, "ci") <- ci
      attr(i, "digits") <- digits
      attr(i, "ci_digits") <- ci_digits
      attr(i, "p_digits") <- p_digits

      formatted_table <- parameters_table(i, pretty_names = FALSE, ...)
      cat(insight::format_table(formatted_table))
      cat("\n")
    }
  }
}