% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_parameters.R
\name{compare_parameters}
\alias{compare_parameters}
\alias{compare_models}
\title{Compare model parameters of multiple models}
\usage{
compare_parameters(
  ...,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  standardize = NULL,
  exponentiate = FALSE,
  ci_method = "wald",
  p_adjust = NULL,
  select = NULL,
  column_names = NULL,
  pretty_names = TRUE,
  coefficient_names = NULL,
  keep = NULL,
  drop = NULL,
  include_reference = FALSE,
  groups = NULL,
  verbose = TRUE
)

compare_models(
  ...,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  standardize = NULL,
  exponentiate = FALSE,
  ci_method = "wald",
  p_adjust = NULL,
  select = NULL,
  column_names = NULL,
  pretty_names = TRUE,
  coefficient_names = NULL,
  keep = NULL,
  drop = NULL,
  include_reference = FALSE,
  groups = NULL,
  verbose = TRUE
)
}
\arguments{
\item{...}{One or more regression model objects, or objects returned by
\code{model_parameters()}. Regression models may be of different model
types. Model objects may be passed comma separated, or as a list.
If model objects are passed with names or the list has named elements,
these names will be used as column names.}

\item{ci}{Confidence Interval (CI) level. Default to \code{0.95} (\verb{95\%}).}

\item{effects}{Should parameters for fixed effects (\code{"fixed"}), random effects
(\code{"random"}), or both fixed and random effects (\code{"all"}) be returned? By
default, the variance components for random effects are returned. If
group-level effects are requested, \code{"grouplevel"} returns the group-level
random effects (BLUPs), while \code{"random_total"} return the overall (sum of
fixed and random) effects (similar to what \code{coef()} returns). Using
\code{"grouplevel"} is equivalent to setting \code{group_level = TRUE}. The \code{effects}
argument only applies to mixed models. If the calculation of random effects
parameters takes too long, you may use \code{effects = "fixed"}.}

\item{component}{Model component for which parameters should be shown. See
documentation for related model class in \code{\link[=model_parameters]{model_parameters()}}.}

\item{standardize}{The method used for standardizing the parameters. Can be
\code{NULL} (default; no standardization), \code{"refit"} (for re-fitting the model
on standardized data) or one of \code{"basic"}, \code{"posthoc"}, \code{"smart"},
\code{"pseudo"}. See 'Details' in \code{\link[=standardize_parameters]{standardize_parameters()}}.
\strong{Importantly}:
\itemize{
\item The \code{"refit"} method does \emph{not} standardize categorical predictors (i.e.
factors), which may be a different behaviour compared to other R packages
(such as \strong{lm.beta}) or other software packages (like SPSS). to mimic
such behaviours, either use \code{standardize="basic"} or standardize the data
with \code{datawizard::standardize(force=TRUE)} \emph{before} fitting the model.
\item By default, the response (dependent) variable is also standardized, \emph{if
applicable}. Set \code{include_response = FALSE} to avoid standardization of
the response variable. See details in \code{\link[datawizard:standardize.default]{datawizard::standardize.default()}}.
\item For mixed models, when using methods other than \code{"refit"}, only the fixed
effects will be standardized.
\item Robust estimation (i.e., \code{vcov} set to a value other than \code{NULL}) of
standardized parameters only works when \code{standardize="refit"}.
}}

\item{exponentiate}{Logical, indicating whether or not to exponentiate the
coefficients (and related confidence intervals). This is typical for
logistic regression, or more generally speaking, for models with log or
logit links. It is also recommended to use \code{exponentiate = TRUE} for models
with log-transformed response values. For models with a log-transformed
response variable, when \code{exponentiate = TRUE}, a one-unit increase in the
predictor is associated with multiplying the outcome by that predictor's
coefficient. \strong{Note:} Delta-method standard errors are also computed (by
multiplying the standard errors by the transformed coefficients). This is
to mimic behaviour of other software packages, such as Stata, but these
standard errors poorly estimate uncertainty for the transformed
coefficient. The transformed confidence interval more clearly captures this
uncertainty. For \code{compare_parameters()}, \code{exponentiate = "nongaussian"}
will only exponentiate coefficients from non-Gaussian families.}

\item{ci_method}{Method for computing degrees of freedom for p-values
and confidence intervals (CI). See documentation for related model class
in \code{\link[=model_parameters]{model_parameters()}}.}

\item{p_adjust}{String value, if not \code{NULL}, indicates the method to adjust
p-values. See \code{\link[stats:p.adjust]{stats::p.adjust()}} for details. Further possible
adjustment methods are \code{"tukey"}, \code{"scheffe"}, \code{"sidak"}, \code{"sup-t"}, and
\code{"none"} to explicitly disable adjustment for \code{emmGrid} objects (from
\strong{emmeans}). \code{"sup-t"} computes simultaneous confidence bands, also called
sup-t confidence band (Montiel Olea & Plagborg-Møller, 2019).}

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

\item{column_names}{Character vector with strings that should be used as
column headers. Must be of same length as number of models in \code{...}.}

\item{pretty_names}{Can be \code{TRUE}, which will return "pretty" (i.e. more human
readable) parameter names. Or \code{"labels"}, in which case value and variable
labels will be used as parameters names. The latter only works for "labelled"
data, i.e. if the data used to fit the model had \code{"label"} and \code{"labels"}
attributes. See also section \emph{Global Options to Customize Messages when Printing}.}

\item{coefficient_names}{Character vector with strings that should be used
as column headers for the coefficient column. Must be of same length as
number of models in \code{...}, or length 1. If length 1, this name will be
used for all coefficient columns. If \code{NULL}, the name for the coefficient
column will detected automatically (as in \code{model_parameters()}).}

\item{keep}{Character containing a regular expression pattern that
describes the parameters that should be included (for \code{keep}) or excluded
(for \code{drop}) in the returned data frame. \code{keep} may also be a
named list of regular expressions. All non-matching parameters will be
removed from the output. If \code{keep} is a character vector, every parameter
name in the \emph{"Parameter"} column that matches the regular expression in
\code{keep} will be selected from the returned data frame (and vice versa,
all parameter names matching \code{drop} will be excluded). Furthermore, if
\code{keep} has more than one element, these will be merged with an \code{OR}
operator into a regular expression pattern like this: \code{"(one|two|three)"}.
If \code{keep} is a named list of regular expression patterns, the names of the
list-element should equal the column name where selection should be
applied. This is useful for model objects where \code{model_parameters()}
returns multiple columns with parameter components, like in
\code{\link[=model_parameters.lavaan]{model_parameters.lavaan()}}. Note that the regular expression pattern
should match the parameter names as they are stored in the returned data
frame, which can be different from how they are printed. Inspect the
\verb{$Parameter} column of the parameters table to get the exact parameter
names.}

\item{drop}{See \code{keep}.}

\item{include_reference}{Logical, if \code{TRUE}, the reference level of factors will
be added to the parameters table. This is only relevant for models with
categorical predictors. The coefficient for the reference level is always
\code{0} (except when \code{exponentiate = TRUE}, then the coefficient will be \code{1}),
so this is just for completeness.}

\item{groups}{Named list, can be used to group parameters in the printed output.
List elements may either be character vectors that match the name of those
parameters that belong to one group, or list elements can be row numbers
of those parameter rows that should belong to one group. The names of the
list elements will be used as group names, which will be inserted as "header
row". A possible use case might be to emphasize focal predictors and control
variables, see 'Examples'. Parameters will be re-ordered according to the
order used in \code{groups}, while all non-matching parameters will be added
to the end.}

\item{verbose}{Toggle warnings and messages.}
}
\value{
A data frame of indices related to the model's parameters.
}
\description{
Compute and extract model parameters of multiple regression
models. See \code{\link[=model_parameters]{model_parameters()}} for further details.
}
\details{
This function is in an early stage and does not yet cope with more complex
models, and probably does not yet properly render all model components. It
should also be noted that when including models with interaction terms, not
only do the values of the parameters change, but so does their meaning (from
main effects, to simple slopes), thereby making such comparisons hard.
Therefore, you should not use this function to compare models with
interaction terms with models without interaction terms.
}
\examples{
\dontshow{if (require("gt", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
compare_parameters(lm1, lm2)

# custom style
compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")

\donttest{
# custom style, in HTML
result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
print_html(result)
}

data(mtcars)
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
compare_parameters(m1, m2)
\donttest{
# exponentiate coefficients, but not for lm
compare_parameters(m1, m2, exponentiate = "nongaussian")

# change column names
compare_parameters("linear model" = m1, "logistic reg." = m2)
compare_parameters(m1, m2, column_names = c("linear model", "logistic reg."))

# or as list
compare_parameters(list(m1, m2))
compare_parameters(list("linear model" = m1, "logistic reg." = m2))
}
\dontshow{\}) # examplesIf}
}
