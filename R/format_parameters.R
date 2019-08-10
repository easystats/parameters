#' Parameters Names Formatting
#'
#' @param model A statistical model.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Sepal.Width * Petal.Length, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species / Petal.Length, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species / Petal.Length * Sepal.Width, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species / (Petal.Length * Sepal.Width), data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
#' format_parameters(model)
#'
#'
#' @return The formatted parameter names.
#' @export
format_parameters <- function(model) {
  types <- parameters_type(model)

  names <- types$Parameter
  for (i in 1:nrow(types)) {

    # Factors
    if (types$Type[i] == "factor") {
      names[i] <- .format_factor(name = names[i], variable = types$Term[i])
    }

    # Polynomials
    if (types$Type[i] %in% c("poly", "poly_raw")){
      names[i] <- .format_poly(name = names[i], variable = types$Term[i], type = types$Type[i], degree = types$Parameter2[i])
    }

    # Interactions
    if (types$Type[i] == "interaction") {
      components <- unlist(strsplit(names[i], ":", fixed = TRUE))

      for (j in 1:length(components)) {
        component <- components[j]
        if (component %in% types$Parameter) {
          if (types[types$Parameter == component, "Type"] == "factor") {
            components[j] <- .format_factor(component, types[types$Parameter == component, "Term"])
          }
        }
      }
      if (length(components) > 2) {
        names[i] <- paste0("(", paste0(head(components, -1), collapse = " * "), ") * ", tail(components, 1))
      } else {
        names[i] <- paste0(components, collapse = " * ")
      }
    }
  }

  names
}


#' @keywords internal
.format_factor <- function(name, variable) {
  level <- gsub(variable, "", name)
  paste0(variable, " (", level, ")")
}

#' @keywords internal
.format_poly <- function(name, variable, type, degree) {
  paste0(variable, " (", format_order(as.numeric(degree), textual = FALSE), " degree)")
}


