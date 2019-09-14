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
#' @return The formatted parameter names.
#' @importFrom utils tail head
#' @export
format_parameters <- function(model) {
  UseMethod("format_parameters")
}

#' @export
format_parameters.default <- function(model) {

  names <- insight::find_parameters(model, flatten = TRUE)
  info <- insight::model_info(model)

  # hurdle- and zeroinfl-models
  if (info$is_zero_inflated | info$is_hurdle) {
    names <- gsub("count_", "", names)
    names <- gsub("zero_", "", names)
  }

  # Type-specific changes
  types <- parameters_type(model)
  for (i in 1:nrow(types)) {

    # Factors
    if (types$Type[i] == "factor") {
      names[i] <- .format_factor(name = names[i], variable = types$Variable[i])
    }

    # Polynomials
    if (types$Type[i] %in% c("poly", "poly_raw")) {
      names[i] <- .format_poly(name = names[i], variable = types$Variable[i], type = types$Type[i], degree = types$Level[i])
    }

    # Smooth
    if (types$Type[i] == "smooth") {
      names[i] <- gsub("^smooth_(.*)\\[(.*)\\]", "\\2", names[i])
      names[i] <- gsub("s(", "Smooth term (", names[i], fixed = TRUE)
    }

    # Interactions
    if (types$Type[i] %in% c("interaction", "nested")) {
      sep <- ifelse(types$Type[i] == "interaction", " * ", " / ")

      components <- unlist(strsplit(names[i], ":", fixed = TRUE))

      for (j in 1:length(components)) {
        component <- components[j]
        if (component %in% types$Parameter) {
          if (types[types$Parameter == component, "Type"] == "factor") {
            components[j] <- .format_factor(component, types[types$Parameter == component, "Variable"])
          }
        }
      }
      if (length(components) > 2) {
        names[i] <- paste0("(", paste0(utils::head(components, -1), collapse = " * "), ")", sep, utils::tail(components, 1))
      } else {
        names[i] <- paste0(components, collapse = sep)
      }
    }
  }
  names(names) <- types$Parameter
  names
}



#' @export
format_parameters.parameters_model <- function(model) {
  if (!is.null(attributes(model)$pretty_names)) {
    model$Parameter <- attributes(model)$pretty_names[model$Parameter]
  }
  model
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
