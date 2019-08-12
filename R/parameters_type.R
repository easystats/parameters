#' Type of Model Parameters
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Sepal.Width * Petal.Length, data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
#' parameters_type(model)
#' @return A data.frame.
#' @export
parameters_type <- function(model, ...) {
  params_table <- data.frame(
    Parameter = insight::find_parameters(model)$conditional,
    stringsAsFactors = FALSE
  )

  types <- lapply(params_table$Parameter, .parameters_type, data = insight::get_data(model))
  types <- as.data.frame(do.call(rbind, types), stringsAsFactors = FALSE)

  names(types) <- c("Type", "Term", "Parameter2")

  # find secondary type
  secondary <- lapply(as.character(types$Secondary_Term), .parameters_type, insight::get_data(model))
  types$Type2 <- do.call(rbind, secondary)[, 1]

  cbind(params_table, types)
}



#' @importFrom utils tail head
#' @keywords internal
.parameters_type <- function(name, data) {
  if (is.na(name)) {
    return(c(NA, NA, NA))

  # Interactions
  } else if (grepl(":", name, fixed = TRUE)) {
    var <- unlist(strsplit(name, ":", fixed = TRUE))
    if (length(var) > 2) {
      var <- c(utils::tail(var, 1), paste0(utils::head(var, -1), collapse = ":"))
    } else {
      var <- rev(var)
    }
    return(c("interaction", var))

  # Intercept
  } else if (name == "(Intercept)") {
    return(c("intercept", NA, NA))

  # Numeric
  } else if (name %in% names(data)) {
    return(c("numeric", name, NA))

  # Polynomials
  } else if(grepl("poly(", name, fixed = TRUE)) {
    if(grepl(", raw = TRUE", name, fixed = TRUE)){
      name <- gsub(", raw = TRUE", "", name, fixed = TRUE)
      type <- "poly_raw"
    } else{
      type <- "poly"
    }
    vars <- gsub("poly(", "", name, fixed = TRUE)
    vars <- unlist(strsplit(vars, ", ", fixed = TRUE))
    var <- vars[[1]]
    degree <- vars[[2]]
    degree <- substr(vars[[2]], nchar(vars[[2]]), nchar(vars[[2]]))
    return(c(type, var, degree))

  # Factors
  } else {
    facs <- data[sapply(data, is.factor)]
    facs_names <- c()
    for (fac in names(facs)) {
      facs_names <- c(facs_names, paste0(fac, unique(data[[fac]])))
      if (name %in% facs_names) {
        return(c("factor", fac, NA))
      }
    }

    return(c("unknown", NA, NA))
  }
}
