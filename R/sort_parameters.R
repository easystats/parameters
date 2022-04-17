#' Sort parameters by coefficient values
#'
#' @param x A data frame or a `parameters_model` object.
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param style Naming conventions the given object follows. This can either be
#'   based on the naming conventions from the
#'   [easystats-project](https://easystats.github.io/easystats/), or on
#'   \pkg{broom}'s naming conventions.
#'
#' @examples
#' # creating object to sort (can also be a regular data frame)
#' mod <- model_parameters(stats::lm(wt ~ am * cyl, data = mtcars))
#'
#' # original output
#' mod
#'
#' # sorted outputs
#' sort_parameters(mod, sort = "ascending")
#' sort_parameters(mod, sort = "descending")
#'
#' @return A sorted data frame or original object.
#'
#' @export
sort_parameters <- function(x, ...) {
  UseMethod("sort_parameters")
}

#' @rdname sort_parameters
#' @export
sort_parameters.default <- function(x, sort = "none", style = "easystats") {
  sort <- match.arg(tolower(sort), choices = c("none", "ascending", "descending"))
  style <- match.arg(tolower(style), choices = c("easystats", "broom"))

  if (sort == "none") {
    return(x)
  }

  # new row indices to use for sorting
  if (style == "easystats") {
    new_row_order <- switch(sort,
      "ascending" = order(x$Coefficient, decreasing = FALSE),
      "descending" = order(x$Coefficient, decreasing = TRUE)
    )
  } else {
    new_row_order <- switch(sort,
      "ascending" = order(x$estimate, decreasing = FALSE),
      "descending" = order(x$estimate, decreasing = TRUE)
    )
  }

  x[new_row_order, ]
}

#' @export
sort_parameters.data.frame <- sort_parameters.default
