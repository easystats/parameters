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
#' @return The formatted parameter names.
#' @export
format_parameters <- function(model) {
  types <- parameters_type(model)

  names <- types$Parameter
  for(i in 1:nrow(types)){
    if(types$Type[i] == "factor"){
      names[i] <- .format_factor(names[i], types$Term[i])
    }

    if(types$Type[i] == "interaction"){
      components <- unlist(strsplit(names[i], ":", fixed = TRUE))

      for(j in 1:length(components)){
        component <- components[j]
        if(component %in% types$Parameter){
          if(types[types$Parameter == component, "Type"] == "factor"){
            components[j] <- .format_factor(component, types[types$Parameter == component, "Term"])
          }
        }
      }
      if(length(components) > 2){
        names[i] <- paste0("(", paste0(head(components, -1), collapse = " * "), ") * ", tail(components, 1))
      } else{
        names[i] <- paste0(components, collapse = " * ")
      }

    }
  }

  names
}


#' @keywords internal
.format_factor <- function(param, term){
  level <- gsub(term, "", param)
  paste0(term, " (", level, ")")
}