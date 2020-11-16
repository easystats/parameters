#' @export
knit_print.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, show_sigma = FALSE, show_formula = FALSE, ...) {
  # save original input
  orig_x <- x

  # save attributes
  res <- attributes(x)$details
  coef_name <- attributes(x)$coefficient_name
  sigma <- attributes(x)$sigma
  s_value <- attributes(x)$s_value
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  # minor fix for nested Anovas
  if ("Group" %in% colnames(x) && sum(x$Parameter == "Residuals") > 1) {
    colnames(x)[which(colnames(x) == "Group")] <- "Subgroup"
  }

  if (!is.null(select)) {
    if (all(select == "minimal")) {
      select <- c("Parameter", "Coefficient", "CI", "CI_low", "CI_high", "p")
    } else if (all(select == "short")) {
      select <- c("Parameter", "Coefficient", "SE", "p")
    } else if (is.numeric(select)) {
      select <- colnames(x)[select]
    }
    select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup"))
    # for emmGrid objects, we save specific parameter names as attribute
    parameter_names <- attributes(x)$parameter_names
    if (!is.null(parameter_names)) {
      select <- c(parameter_names, select)
    }
    to_remove <- setdiff(colnames(x), select)
    x[to_remove] <- NULL
  }

  # remove columns that have only NA or Inf
  to_remove <- sapply(x, function(col) all(is.na(col) | is.infinite(col)))
  if (any(to_remove)) x[to_remove] <- NULL

  if (!is.null(attributes(x)$title)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(res)) {
    table_caption <- "Fixed Effects"
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

  # for bayesian meta, remove ROPE_CI
  if (isTRUE(attributes(x)$is_bayes_meta)) {
    x$CI <- NULL
    x$ROPE_CI <- NULL
    x$ROPE_low <- NULL
    x$ROPE_high <- NULL
  }

  split_by <- ""
  split_by <- c(split_by, ifelse("Component" %in% names(x) && .n_unique(x$Component) > 1, "Component", ""))
  split_by <- c(split_by, ifelse("Effects" %in% names(x) && .n_unique(x$Effects) > 1, "Effects", ""))
  split_by <- c(split_by, ifelse("Response" %in% names(x) && .n_unique(x$Response) > 1, "Response", ""))
  split_by <- c(split_by, ifelse("Subgroup" %in% names(x) && .n_unique(x$Subgroup) > 1, "Subgroup", ""))

  split_by <- split_by[nchar(split_by) > 0]

  if (!is.null(coef_name)) {
    colnames(x)[which(colnames(x) == "Coefficient")] <- coef_name
    colnames(x)[which(colnames(x) == "Std_Coefficient")] <- paste0("Std_", coef_name)
  }

  if (isTRUE(s_value) && "p" %in% colnames(x)) {
    colnames(x)[colnames(x) == "p"] <- "s"
    x[["s"]] <- log2(1 / x[["s"]])
  }

  if (split_components && !is.null(split_by) && length(split_by)) {
    .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, ...)
  } else {
    formatted_table <- parameters_table(x, pretty_names = pretty_names, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ...)
    insight::format_table(formatted_table, style = "markdown", caption = table_caption)
  }
}
