% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display.R, R/print_table.R
\name{display.parameters_model}
\alias{display.parameters_model}
\alias{display.parameters_sem}
\alias{display.parameters_efa_summary}
\alias{display.parameters_efa}
\alias{display.equivalence_test_lm}
\alias{print_table}
\title{Print tables in different output formats}
\usage{
\method{display}{parameters_model}(
  object,
  format = "markdown",
  pretty_names = TRUE,
  split_components = TRUE,
  select = NULL,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  align = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  footer_digits = 3,
  ci_brackets = c("(", ")"),
  show_sigma = FALSE,
  show_formula = FALSE,
  zap_small = FALSE,
  font_size = "100\%",
  line_padding = 4,
  column_labels = NULL,
  include_reference = FALSE,
  verbose = TRUE,
  ...
)

\method{display}{parameters_sem}(
  object,
  format = "markdown",
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  ci_brackets = c("(", ")"),
  ...
)

\method{display}{parameters_efa_summary}(object, format = "markdown", digits = 3, ...)

\method{display}{parameters_efa}(
  object,
  format = "markdown",
  digits = 2,
  sort = FALSE,
  threshold = NULL,
  labels = NULL,
  ...
)

\method{display}{equivalence_test_lm}(object, format = "markdown", digits = 2, ...)

print_table(x, digits = 2, p_digits = 3, theme = "default", ...)
}
\arguments{
\item{object}{An object returned by \code{\link[=model_parameters]{model_parameters()}},\code{\link[=simulate_parameters]{simulate_parameters()}},
\code{\link[=equivalence_test]{equivalence_test()}} or \code{\link[=principal_components]{principal_components()}}.}

\item{format}{String, indicating the output format. Can be \code{"markdown"}
or \code{"html"}.}

\item{pretty_names}{Can be \code{TRUE}, which will return "pretty" (i.e. more human
readable) parameter names. Or \code{"labels"}, in which case value and variable
labels will be used as parameters names. The latter only works for "labelled"
data, i.e. if the data used to fit the model had \code{"label"} and \code{"labels"}
attributes. See also section \emph{Global Options to Customize Messages when Printing}.}

\item{split_components}{Logical, if \code{TRUE} (default), For models with
multiple components (zero-inflation, smooth terms, ...), each component is
printed in a separate table. If \code{FALSE}, model parameters are printed
in a single table and a \code{Component} column is added to the output.}

\item{select}{Determines which columns and and which layout columns are
printed. There are three options for this argument:
\itemize{
\item \strong{Selecting columns by name or index}

\code{select} can be a character vector (or numeric index) of column names that
should be printed, where columns are extracted from the data frame returned
by \code{model_parameters()} and related functions.

There are two pre-defined options for selecting columns:
\code{select = "minimal"} prints coefficients, confidence intervals and
p-values, while \code{select = "short"} prints coefficients, standard errors and
p-values.
\item \strong{A string expression with layout pattern}

\code{select} is a string with "tokens" enclosed in braces. These tokens will be
replaced by their associated columns, where the selected columns will be
collapsed into one column. Following tokens are replaced by the related
coefficients or statistics: \code{{estimate}}, \code{{se}}, \code{{ci}} (or \code{{ci_low}} and
\code{{ci_high}}), \code{{p}} and \code{{stars}}. The token \code{{ci}} will be replaced by
\verb{\{ci_low\}, \{ci_high\}}. Example: \code{select = "{estimate}{stars} ({ci})"}

It is possible to create multiple columns as well. A \code{|} separates values
into new cells/columns. Example: \code{select = "{estimate} ({ci})|{p}"}.

If \code{format = "html"}, a \verb{<br>} inserts a line break inside a cell. See
'Examples'.
\item \strong{A string indicating a pre-defined layout}

\code{select} can be one of the following string values, to create one of the
following pre-defined column layouts:
\itemize{
\item \code{"ci"}: Estimates and confidence intervals, no asterisks for p-values.
This is equivalent to \code{select = "{estimate} ({ci})"}.
\item \code{"se"}: Estimates and standard errors, no asterisks for p-values. This is
equivalent to \code{select = "{estimate} ({se})"}.
\item \code{"ci_p"}: Estimates, confidence intervals and asterisks for p-values. This
is equivalent to \code{select = "{estimate}{stars} ({ci})"}.
\item \code{"se_p"}: Estimates, standard errors and asterisks for p-values. This is
equivalent to \code{select = "{estimate}{stars} ({se})"}..
\item \code{"ci_p2"}: Estimates, confidence intervals and numeric p-values, in two
columns. This is equivalent to \code{select = "{estimate} ({ci})|{p}"}.
\item \code{"se_p2"}: Estimate, standard errors and numeric p-values, in two columns.
This is equivalent to \code{select = "{estimate} ({se})|{p}"}.
}
}

For \code{model_parameters()}, glue-like syntax is still experimental in the
case of more complex models (like mixed models) and may not return expected
results.}

\item{caption}{Table caption as string. If \code{NULL}, depending on the model,
either a default caption or no table caption is printed. Use \code{caption = ""}
to suppress the table caption.}

\item{subtitle}{Table title (same as caption) and subtitle, as strings. If \code{NULL},
no title or subtitle is printed, unless it is stored as attributes (\code{table_title},
or its alias \code{table_caption}, and \code{table_subtitle}). If \code{x} is a list of
data frames, \code{caption} may be a list of table captions, one for each table.}

\item{footer}{Can either be \code{FALSE} or an empty string (i.e. \code{""}) to
suppress the footer, \code{NULL} to print the default footer, or a string. The
latter will combine the string value with the default footer.}

\item{align}{Only applies to HTML tables. May be one of \code{"left"},
\code{"right"} or \code{"center"}.}

\item{digits, ci_digits, p_digits}{Number of digits for rounding or
significant figures. May also be \code{"signif"} to return significant
figures or \code{"scientific"} to return scientific notation. Control the
number of digits by adding the value as suffix, e.g. \code{digits = "scientific4"}
to have scientific notation with 4 decimal places, or \code{digits = "signif5"}
for 5 significant figures (see also \code{\link[=signif]{signif()}}).}

\item{footer_digits}{Number of decimal places for values in the footer summary.}

\item{ci_brackets}{Logical, if \code{TRUE} (default), CI-values are
encompassed in square brackets (else in parentheses).}

\item{show_sigma}{Logical, if \code{TRUE}, adds information about the residual
standard deviation.}

\item{show_formula}{Logical, if \code{TRUE}, adds the model formula to the output.}

\item{zap_small}{Logical, if \code{TRUE}, small values are rounded after
\code{digits} decimal places. If \code{FALSE}, values with more decimal
places than \code{digits} are printed in scientific notation.}

\item{font_size}{For HTML tables, the font size.}

\item{line_padding}{For HTML tables, the distance (in pixel) between lines.}

\item{column_labels}{Labels of columns for HTML tables. If \code{NULL}, automatic
column names are generated. See 'Examples'.}

\item{include_reference}{Logical, if \code{TRUE}, the reference level of factors will
be added to the parameters table. This is only relevant for models with
categorical predictors. The coefficient for the reference level is always
\code{0} (except when \code{exponentiate = TRUE}, then the coefficient will be \code{1}),
so this is just for completeness.}

\item{verbose}{Toggle messages and warnings.}

\item{...}{Arguments passed down to \code{\link[=format.parameters_model]{format.parameters_model()}},
\code{\link[insight:format_table]{insight::format_table()}} and \code{\link[insight:export_table]{insight::export_table()}}}

\item{sort}{Sort the loadings.}

\item{threshold}{A value between 0 and 1 indicates which (absolute) values
from the loadings should be removed. An integer higher than 1 indicates the
n strongest loadings to retain. Can also be \code{"max"}, in which case it will
only display the maximum loading per variable (the most simple structure).}

\item{labels}{A character vector containing labels to be added to the
loadings data. Usually, the question related to the item.}

\item{x}{An object returned by \code{\link[=model_parameters]{model_parameters()}}.}

\item{theme}{String, indicating the table theme. Can be one of \code{"default"},
\code{"grid"}, \code{"striped"}, \code{"bootstrap"} or \code{"darklines"}.}
}
\value{
If \code{format = "markdown"}, the return value will be a character
vector in markdown-table format. If \code{format = "html"}, an object of
class \code{gt_tbl}. For \code{print_table()}, an object of class \code{tinytable} is
returned.
}
\description{
Prints tables (i.e. data frame) in different output formats.
\code{print_md()} is an alias for \code{display(format = "markdown")}, \code{print_html()}
is an alias for \code{display(format = "html")}. \code{print_table()} is for specific
use cases only, and currently only works for \code{compare_parameters()} objects.
}
\details{
\code{display()} is useful when the table-output from functions,
which is usually printed as formatted text-table to console, should
be formatted for pretty table-rendering in markdown documents, or if
knitted from rmarkdown to PDF or Word files. See
\href{https://easystats.github.io/parameters/articles/model_parameters_formatting.html}{vignette}
for examples.

\code{print_table()} is a special function for \code{compare_parameters()} objects,
which prints the output as a formatted HTML table. It is still somewhat
experimental, thus, only a fixed layout-style is available at the moment
(columns for estimates, confidence intervals and p-values). However, it
is possible to include other model components, like zero-inflation, or random
effects in the table. See 'Examples'. An alternative is to set \code{engine = "tt"}
in \code{print_html()} to use the \emph{tinytable} package for creating HTML tables.
}
\examples{
\dontshow{if (require("gt", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_parameters(model)
display(mp)

\donttest{
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
out <- compare_parameters(lm1, lm2, lm3)

print_html(
  out,
  select = "{coef}{stars}|({ci})",
  column_labels = c("Estimate", "95\% CI")
)

# line break, unicode minus-sign
print_html(
  out,
  select = "{estimate}{stars}<br>({ci_low} \u2212 {ci_high})",
  column_labels = c("Est. (95\% CI)")
)
}
\dontshow{\}) # examplesIf}
\dontshow{if (require("tinytable") && require("lme4") && require("glmmTMB")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
data(iris)
data(Salamanders, package = "glmmTMB")
m1 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
m2 <- lme4::lmer(
  Sepal.Length ~ Petal.Length + Petal.Width + (1 | Species),
  data = iris
)
m3 <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
out <- compare_parameters(m1, m2, m3, effects = "all", component = "all")
print_table(out)
}
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=print.parameters_model]{print.parameters_model()}} and \code{\link[=print.compare_parameters]{print.compare_parameters()}}
}
