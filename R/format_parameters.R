#' Parameters Names Formatting
#'
#' @param model A statistical model.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species / Petal.Length, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
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

  if (inherits(model, "bracl")) {
    names <- gsub("(.*):(.*)", "\\2", names)
  }


  # remove "as.factor()", "log()" etc. from parameter names
  names <- .clean_parameter_names(names)

  # Type-specific changes
  types <- parameters_type(model)
  types$Parameter <- .clean_parameter_names(types$Parameter, full = TRUE)

  for (i in 1:nrow(types)) {

    name <- types$Parameter[i]

    # No interaction
    if (!types$Type[i] %in% c("interaction", "nested")) {
      type <- types[types$Parameter == name, ]
      names[i] <- .format_parameter(name, variable = type$Variable, type = type$Type, level = type$Level)

    # Interaction or nesting
    } else{
      components <- unlist(strsplit(name, ":", fixed = TRUE))
      for (j in 1:length(components)) {
        if (components[j] %in% types$Parameter) {
          type <- types[types$Parameter == components[j], ]
          components[j] <- .format_parameter(components[j], variable = type$Variable, type = type$Type, level = type$Level)
        } else if (components[j] %in% types$Secondary_Parameter) {
          type <- types[!is.na(types$Secondary_Parameter) & types$Secondary_Parameter == components[j], ]
          components[j] <- .format_parameter(components[j], variable = type[1, ]$Secondary_Variable, type = type[1, ]$Secondary_Type, level = type[1, ]$Secondary_Level)
        }
      }
      names[i] <- .format_interaction(components, type = types[i, "Type"])
    }
  }
  names(names) <- types$Parameter
  names
}


#' @export
format_parameters.rma <- function(model) {
  params <- insight::find_parameters(model, flatten = TRUE)
  names(params) <- params
  params
}


#' @export
format_parameters.parameters_model <- function(model) {
  if (!is.null(attributes(model)$pretty_names)) {
    model$Parameter <- attributes(model)$pretty_names[model$Parameter]
  }
  model
}





# Utilities ---------------------------------------------------------------




#' @keywords internal
.format_parameter <- function(name, variable, type, level){

  # Factors
  if (type == "factor") {
    name <- .format_factor(name = name, variable = variable)
  }

  # Polynomials
  if (type %in% c("poly", "poly_raw")) {
    name <- .format_poly(name = name, variable = variable, type = type, degree = level)
  }

  # Smooth
  if (type == "smooth") {
    name <- gsub("^smooth_(.*)\\[(.*)\\]", "\\2", name)
    name <- gsub("s(", "Smooth term (", name, fixed = TRUE)
  }

  name
}


#' @keywords internal
.format_interaction <- function(components, type) {
  # sep <- ifelse(type == "interaction", " * ", " / ")

  if (length(components) > 2) {
    if (type == "interaction") {
      components <- paste0("(", paste0(utils::head(components, -1), collapse = " * "), ")", " * ", utils::tail(components, 1))
    } else{
      components <- paste0(components, collapse = " * ")
    }
  } else {
    components <- paste0(components, collapse = " * ")
  }
  components
}


#' @keywords internal
.format_factor <- function(name, variable) {
  level <- gsub(variable, "", name)
  paste0(variable, " [", level, "]")
}

#' @keywords internal
.format_poly <- function(name, variable, type, degree) {
  paste0(variable, " [", format_order(as.numeric(degree), textual = FALSE), " degree]")
}
