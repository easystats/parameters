.add_model_parameters_attributes <- function(parameters, model, ci, exponentiate = FALSE, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  info <- insight::model_info(model)

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  attr(parameters, "exponentiate") <- exponentiate
  attr(parameters, "ordinal_model") <- info$is_ordinal


  if ("digits" %in% names(dot.arguments)) {
    attr(parameters, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(parameters, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(parameters, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(parameters, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(parameters, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(parameters, "p_digits") <- 3
  }

  parameters
}




.exponentiate_parameters <- function(parameters) {
  columns <- grepl(pattern = "^(Coefficient|Std_Coefficient|CI_)", colnames(parameters))
  if (any(columns)) parameters[columns] <- exp(parameters[columns])
  parameters
}




#' @importFrom insight clean_parameters
.add_pretty_names <- function(parameters, model, ...) {
  attr(parameters, "model_class") <- class(model)

  clean_params <- insight::clean_parameters(model)
  elements <- sapply(eval(substitute(alist(...))), deparse)

  if (length(elements)) elements <- gsub("\"", "", elements, fixed = TRUE)

  if (!length(elements)) {
    component <- "conditional"
    effects <- "fixed"
  } else {
    if ("effects" %in% names(elements)) {
      effects <- switch(
        elements["effects"],
        all = c("fixed", "random"),
        effects
      )
    }
    if ("component" %in% names(elements)) {
      component <- switch(
        elements["component"],
        all = c("conditional", "zi", "zero_inflated", "dispersion"),
        component
      )
    }
  }

  clean_params <- subset(clean_params, subset = Component %in% component & Effects %in% effects)
  attr(parameters, "cleaned_parameters") <- clean_params$Cleaned_Parameter

  parameters
}