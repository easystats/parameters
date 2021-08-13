# defining global variables and functions to appease R CMD Check
utils::globalVariables(
  names = c(".", "Parameter"),
  package = "parameters",
  add = FALSE
)