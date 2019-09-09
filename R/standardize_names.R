#' Standardize column names
#'
#' Standardize column names from data frames, in particular objects returned from
#' \code{\link[=model_parameters]{model_parameters()}}, so column names are
#' consistent and the same for any model object.
#'
#' @param data A data frame. Currently, only objects from \code{\link[=model_parameters]{model_parameters()}} are accepted.
#' @param style Standardization can either be based on the naming conventions from the easystats project, or on \pkg{broom}'s naming scheme.
#'
#' @return A data frame, with standardized column names.
#'
#' @examples
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#'
#' as.data.frame(mp)
#' standardize_names(mp)
#' standardize_names(mp, style = "broom")
#' @export
standardize_names <- function(data, ...) {
  UseMethod("standardize_names")
}


#' @export
standardize_names.default <- function(data, ...) {
  insight::print_color(sprintf("Objects of class '%s' are currently not supported.\n", class(data)[1]), "red")
}


#' @rdname standardize_names
#' @export
standardize_names.parameters_model <- function(data, style = c("easystats", "broom"), ...) {
  style <- match.arg(style)
  cn <- colnames(data)

  if (style == "easystats") {
    cn[cn %in% c("t", "z", "F", "chisq", "t / F")] <- "Statistic"
  } else {
    cn[cn == "Parameter"] <- "term"
    cn[cn == "Coefficient"] <- "estimate"
    cn[cn == "SE"] <- "std.error"
    cn[cn == "p"] <- "p.value"
    cn[cn == "df_residual"] <- "df.residual"
    cn <- gsub("^CI_low", "conf.low", cn)
    cn <- gsub("^CI_high", "conf.high", cn)
    cn[cn %in% c("t", "z", "F", "chisq", "t / F")] <- "statistic"
  }

  colnames(data) <- cn
  as.data.frame(data)
}
