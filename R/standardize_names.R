#' Standardize column names
#'
#' Standardize column names from data frames, in particular objects returned from
#' \code{\link[=model_parameters]{model_parameters()}}, so column names are
#' consistent and the same for any model object.
#'
#' @param data A data frame. Currently, only objects from \code{\link[=model_parameters]{model_parameters()}} are accepted.
#' @param style Standardization can either be based on the naming conventions from the easystats project, or on \pkg{broom}'s naming scheme.
#' @param ... Currently not used.
#'
#' @return A data frame, with standardized column names.
#'
#' @details This method is in particular useful for package developers or users
#'   who use \code{\link[=model_parameters]{model_parameters()}} in their own
#'   code or functions to retrieve model parameters for further processing. As
#'   \code{model_parameters()} returns a data frame with varying column names
#'   (depending on the input), accessing the required information is probably
#'   not quite straightforward. In such cases, \code{standardize_names()} can
#'   be used to get consistent, i.e. always the same column names, no matter
#'   what kind of model was used in \code{model_parameters()}.
#'   \cr \cr
#'   For \code{style = "broom"}, column names are renamed to match \pkg{broom}'s
#'   naming scheme, i.e. \code{Parameter} is renamed to \code{term}, \code{Coefficient}
#'   becomes \code{estimate} and so on.
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
  .standardize_names(data, style, ...)
}





.standardize_names <- function(data, style, ...) {
  cn <- colnames(data)

  if (style == "easystats") {
    cn[cn %in% c("t", "z", "F", "chisq", "t / F", "z / Chisq")] <- "Statistic"
    cn[cn %in% c("Median", "Mean", "MAP")] <- "Coefficient"
    cn[cn %in% c("df_residual", "df_error")] <- "df"
  } else {
    # easy replacements
    cn[cn == "Parameter"] <- "term"
    cn[cn == "SE"] <- "std.error"
    cn[cn == "SD"] <- "std.dev"
    cn[cn == "p"] <- "p.value"
    cn[cn == "BF"] <- "bayes.factor"
    cn[cn == "Component"] <- "component"
    cn[cn == "Effects"] <- "effects"
    # more sophisticated replacements
    cn[cn %in% c("df_residual", "df_error")] <- "df.error"
    cn[cn %in% c("Coefficient", "Std_Coefficient", "Median", "Mean", "MAP")] <- "estimate"
    cn[cn %in% c("t", "z", "F", "chisq", "t / F", "z / Chisq")] <- "statistic"
    # fancy regex replacements
    cn <- gsub("^CI_low", "conf.low", cn)
    cn <- gsub("^CI_high", "conf.high", cn)
    # lowercase for everything
    cn <- tolower(cn)
  }

  colnames(data) <- cn
  as.data.frame(data)
}
