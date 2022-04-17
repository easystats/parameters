#' Sort parameters by coefficient values
#'
#' @param x A data frame or a `parameters_model` object.
#' @param ... Arguments passed to or from other methods.
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
#'
#' @param sort If `"none"` (default) do not sort, `"ascending"` sort by
#'   increasing coefficient value, or `"descending"` sort by decreasing
#'   coefficient value.
#' @param column The column containing model parameter estimates. This will be
#'   `"Coefficient"` (default) in *easystats* packages, `"estimate"` in *broom*
#'   package, etc.
#'
#' @export
sort_parameters.default <- function(x, sort = "none", column = "Coefficient", ...) {
  sort <- match.arg(tolower(sort), choices = c("none", "ascending", "descending"))

  if (sort == "none") {
    return(x)
  }

  # new row indices to use for sorting
    new_row_order <- switch(sort,
      "ascending" = order(x[[column]], decreasing = FALSE),
      "descending" = order(x[[column]], decreasing = TRUE)
    )

  x[new_row_order, ]
}

#' @export
sort_parameters.data.frame <- sort_parameters.default
