.add_model_parameters_attributes <- function(parameters, model, ci, exponentiate = FALSE, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  attr(parameters, "exponentiate") <- exponentiate

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
  parameters[columns] <- exp(parameters[columns])
  parameters
}
