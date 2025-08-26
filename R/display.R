#' @title Print tables in different output formats
#' @name display.parameters_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#' `print_md()` is an alias for `display(format = "markdown")` and
#' `print_html()` is an alias for `display(format = "html")`. A third option is
#' `display(format = "tt")`, which returns a `tinytable` object, which is either
#' printed as markdown or HTML table, depending on the environment.
#'
#' @param object An object returned by one of the package's function, for example
#' [`model_parameters()`], [`simulate_parameters()`], [`equivalence_test()`] or
#' [`principal_components()`].
#' @param format String, indicating the output format. Can be `"markdown"`
#' `"html"`, or `"tt"`. `format = "tt"` creates a `tinytable` object, which is
#' either printed as markdown or HTML table, depending on the environment. See
#' [`insight::export_table()`] for details.
#' @param ... Arguments passed to the underlying functions, such as `print_md()`
#' or `print_html()`.
#'
#' @return If `format = "markdown"`, the return value will be a character
#' vector in markdown-table format. If `format = "html"`, an object of
#' class `gt_tbl`. If `format = "tt"`, an object of class `tinytable`.
#'
#' @details `display()` is useful when the table-output from functions,
#' which is usually printed as formatted text-table to console, should
#' be formatted for pretty table-rendering in markdown documents, or if
#' knitted from rmarkdown to PDF or Word files. See
#' [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#' for examples.
#'
#' @seealso [`print.parameters_model()`] and [`print.compare_parameters()`]
#'
#' @examplesIf require("gt", quietly = TRUE)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#' display(mp)
#'
#' \donttest{
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' out <- compare_parameters(lm1, lm2, lm3)
#'
#' print_html(
#'   out,
#'   select = "{coef}{stars}|({ci})",
#'   column_labels = c("Estimate", "95% CI")
#' )
#'
#' # line break, unicode minus-sign
#' print_html(
#'   out,
#'   select = "{estimate}{stars}<br>({ci_low} \u2212 {ci_high})",
#'   column_labels = c("Est. (95% CI)")
#' )
#' }
#'
#' @examplesIf all(insight::check_if_installed(c("glmmTMB", "lme4", "tinytable"), quietly = TRUE))
#' \donttest{
#' data(iris)
#' data(Salamanders, package = "glmmTMB")
#' m1 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' m2 <- lme4::lmer(
#'   Sepal.Length ~ Petal.Length + Petal.Width + (1 | Species),
#'   data = iris
#' )
#' m3 <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' out <- compare_parameters(m1, m2, m3, effects = "all", component = "all")
#'
#' display(out, format = "tt")
#'
#' display(out, select = "{estimate}|{ci}", format = "tt")
#' }
#' @export
display.parameters_model <- function(object, format = "markdown", ...) {
  format <- .display_default_format(format)

  if (format %in% c("html", "tt")) {
    print_html(x = object, backend = ifelse(format == "tt", "tt", "html"), ...)
  } else {
    print_md(x = object, ...)
  }
}

#' @export
display.parameters_simulate <- display.parameters_model

#' @export
display.parameters_brms_meta <- display.parameters_model

#' @export
display.compare_parameters <- display.parameters_model

#' @export
display.parameters_sem <- display.parameters_model

#' @export
display.parameters_efa_summary <- display.parameters_model

#' @export
display.parameters_pca_summary <- display.parameters_model

#' @export
display.parameters_omega_summary <- display.parameters_model

#' @export
display.parameters_efa <- display.parameters_model

#' @export
display.parameters_pca <- display.parameters_model

#' @export
display.parameters_omega <- display.parameters_model

#' @export
display.equivalence_test_lm <- display.parameters_model

#' @export
display.parameters_p_function <- display.parameters_model

.display_default_format <- function(format) {
  format <- getOption("easystats_display_format", "markdown")
  insight::validate_argument(format, c("markdown", "html", "md", "tt"))
}
