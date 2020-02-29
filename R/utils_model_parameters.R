#' @keywords internal
.add_model_parameters_attributes <- function(parameters, model, ci, exponentiate = FALSE, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  info <- insight::model_info(model)

  if (is.null(attr(parameters, "pretty_names", exact = TRUE))) {
    attr(parameters, "pretty_names") <- format_parameters(model)
  }
  attr(parameters, "ci") <- ci
  attr(parameters, "exponentiate") <- exponentiate
  attr(parameters, "ordinal_model") <- info$is_ordinal | info$is_multinomial
  attr(parameters, "model_class") <- class(model)


  if (inherits(model, c("rma", "rma.uni"))) {
    attr(parameters, "data") <- insight::get_data(model)
    attr(parameters, "study_weights") <- 1 / model$vi
  }

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



#' @keywords internal
.exponentiate_parameters <- function(parameters) {
  columns <- grepl(pattern = "^(Coefficient|Std_Coefficient|CI_)", colnames(parameters))
  if (any(columns)) parameters[columns] <- exp(parameters[columns])
  parameters
}




#' @importFrom insight clean_parameters
.add_pretty_names <- function(parameters, model, effects = NULL, component = NULL) {
  attr(parameters, "model_class") <- class(model)
  clean_params <- insight::clean_parameters(model)

  if (is.null(effects)) {
    effects <- "fixed"
  } else if (effects == "all") {
    effects <- c("fixed", "random")
  }

  if (is.null(component)) {
    component <- "conditional"
  } else if (component == "all") {
    component <- c("conditional", "zi", "zero_inflated", "dispersion")
  }

  clean_params <- clean_params[clean_params$Component %in% component & clean_params$Effects %in% effects, ]
  attr(parameters, "cleaned_parameters") <- clean_params$Cleaned_Parameter

  parameters
}
