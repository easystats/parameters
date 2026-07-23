#' @title Forest plot of model parameters using `tinyplot`
#' @name tinyplot.parameters_model
#'
#' @description Draws a forest plot (coefficients as points, confidence
#' intervals as ranges) from a `parameters_model` object, using the
#' lightweight base-graphics package `tinyplot` instead of `ggplot2`. The
#' method is registered for [`tinyplot::tinyplot()`] and its shorthand alias
#' `tinyplot::plt()`.
#'
#' @param x An object returned by [`model_parameters()`].
#' @param flip Logical, if `TRUE` (default), coefficients are plotted
#' horizontally, with parameter names on the vertical axis.
#' @param zero Logical, if `TRUE` (default), a dashed reference line is drawn
#' at zero (resp. at one for exponentiated coefficients), and the estimate
#' axis is extended to include that value.
#' @param sort Logical, if `TRUE`, coefficients are sorted by size, largest
#' on top. If `FALSE` (default), parameters appear in model order.
#' @param ... Other arguments passed to [`tinyplot::tinyplot()`], e.g. `theme`
#' or `palette`. User-supplied `xlim`, `ylim` or `ylab` override the defaults
#' set by this method. The `type`, `data`, `ymin` and `ymax` arguments are
#' fixed by this method and will be ignored.
#'
#' @details Random effects parameters are dropped from the plot, and for
#' models with a zero-inflation component, only the conditional (count)
#' component is shown. A message is printed in these cases.
#'
#' This method requires *tinyplot* version 0.7.0 or later, which supports
#' extending axis limits to cover the reference value.
#'
#' @return The input `x`, invisibly. The function is called for its side
#' effect of drawing a plot.
#'
#' @seealso The `plot` method for `parameters_model` objects, provided by the
#' *see* package, for `ggplot2` based plots.
#'
#' @examplesIf insight::check_if_installed("tinyplot", minimum_version = "0.7.0", quietly = TRUE)
#' \donttest{
#' library(tinyplot)
#' data(mtcars)
#'
#' model <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' result <- model_parameters(model)
#' plt(result)
#'
#' # sorted by coefficient size, using a theme
#' plt(result, sort = TRUE, theme = "classic")
#'
#' # exponentiated coefficients place the reference line at 1
#' model <- glm(am ~ wt + hp, data = mtcars, family = "binomial")
#' result <- model_parameters(model, exponentiate = TRUE)
#' plt(result)
#' }
#' @exportS3Method tinyplot::tinyplot
tinyplot.parameters_model <- function(x, flip = TRUE, zero = TRUE, sort = FALSE, ...) {
  insight::check_if_installed("tinyplot", minimum_version = "0.7.0")

  out <- as.data.frame(x)
  dots <- list(...)

  # these arguments are fixed by this method, remove them from dots
  reserved <- intersect(c("type", "data", "ymin", "ymax"), names(dots))
  if (length(reserved)) {
    dots[reserved] <- NULL
    insight::format_alert(paste0(
      "Following arguments are set by this method and were ignored: ",
      toString(paste0("`", reserved, "`")), "."
    ))
  }

  # keep only fixed effects and the conditional component. these subsets are
  # what a forest plot of the coefficients usually shows; inform the user.
  if ("Effects" %in% colnames(out) && length(unique(out$Effects)) > 1L) {
    out <- out[out$Effects == "fixed", , drop = FALSE]
    insight::format_alert("Only fixed effects are plotted.")
  }
  if ("Component" %in% colnames(out) && length(unique(out$Component)) > 1L) {
    keep <- intersect(c("conditional", "count"), unique(out$Component))
    if (length(keep)) {
      out <- out[out$Component == keep[1], , drop = FALSE]
      insight::format_alert("Only the conditional component is plotted.")
    }
  }

  required_columns <- c("Parameter", "Coefficient", "CI_low", "CI_high")
  missing_columns <- setdiff(required_columns, colnames(out))
  if (length(missing_columns)) {
    insight::format_error(paste0(
      "Cannot plot this object. The following columns are missing: ",
      toString(missing_columns), "."
    ))
  }
  if (all(is.na(out$Coefficient)) || all(is.na(out$CI_low)) || all(is.na(out$CI_high))) {
    insight::format_error(
      "Cannot plot this object. All coefficient or confidence interval values are missing."
    )
  }

  # human readable labels, one entry per row, keyed by the raw parameter name
  pretty_labels <- attr(x, "pretty_labels")
  matched <- match(out$Parameter, names(pretty_labels))
  out$Parameter[!is.na(matched)] <- pretty_labels[matched[!is.na(matched)]]

  # equal parameter names, e.g. for different response levels in multinomial
  # models, would overlap - disambiguate labels in that case
  if ("Response" %in% colnames(out) && length(unique(out$Response)) > 1L) {
    out$Parameter <- paste0(out$Parameter, " (", out$Response, ")")
  }
  if (anyDuplicated(out$Parameter)) {
    out$Parameter <- make.unique(out$Parameter, sep = " ")
  }

  # for interval types, tinyplot places categories in row order, with the
  # first row at the bottom of a flipped plot. sort rows if requested, and
  # reverse the categorical axis so the first row appears on top.
  if (isTRUE(sort)) {
    out <- out[order(out$Coefficient, decreasing = TRUE), , drop = FALSE]
  }
  if (is.null(dots$xlim)) {
    dots$xlim <- "rev"
  }
  out$Parameter <- factor(out$Parameter, levels = unique(out$Parameter))

  # exponentiated coefficients (odds ratios etc.) have their null value at 1.
  # "nongaussian" only exponentiates when the model is not linear
  exponentiate <- attr(x, "exponentiate")
  is_exponentiated <- isTRUE(exponentiate) ||
    (identical(exponentiate, "nongaussian") && !isTRUE(attr(x, "linear_model")))
  reference <- if (is_exponentiated) 1 else 0

  # make sure the reference value is covered by the estimate axis
  if (isTRUE(zero) && is.null(dots$ylim)) {
    dots$ylim <- reference
  }

  # label the estimate axis by what the coefficients are ("Odds Ratio" etc.)
  if (is.null(dots$ylab) && !is.null(attr(x, "coefficient_name"))) {
    dots$ylab <- attr(x, "coefficient_name")
  }

  plot_args <- c(
    list(
      Coefficient ~ Parameter,
      data = out,
      type = "pointrange",
      flip = flip,
      ymin = as.symbol("CI_low"),
      ymax = as.symbol("CI_high")
    ),
    dots
  )
  do.call(tinyplot::tinyplot, plot_args)

  if (isTRUE(zero)) {
    if (isTRUE(flip)) {
      graphics::abline(v = reference, lty = 2)
    } else {
      graphics::abline(h = reference, lty = 2)
    }
  }

  invisible(x)
}
