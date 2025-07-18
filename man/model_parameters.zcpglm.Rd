% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_cplm.R
\name{model_parameters.zcpglm}
\alias{model_parameters.zcpglm}
\title{Parameters from Zero-Inflated Models}
\usage{
\method{model_parameters}{zcpglm}(
  model,
  ci = 0.95,
  bootstrap = FALSE,
  iterations = 1000,
  component = "all",
  standardize = NULL,
  exponentiate = FALSE,
  p_adjust = NULL,
  include_info = getOption("parameters_info", FALSE),
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{model}{A model with zero-inflation component.}

\item{ci}{Confidence Interval (CI) level. Default to \code{0.95} (\verb{95\%}).}

\item{bootstrap}{Should estimates be based on bootstrapped model? If \code{TRUE},
then arguments of \link[=model_parameters.brmsfit]{Bayesian regressions} apply
(see also \code{\link[=bootstrap_parameters]{bootstrap_parameters()}}).}

\item{iterations}{The number of bootstrap replicates. This only apply in the
case of bootstrapped frequentist models.}

\item{component}{Should all parameters, parameters for the conditional model,
for the zero-inflation part of the model, or the dispersion model be returned?
Applies to models with zero-inflation and/or dispersion component. \code{component}
may be one of \code{"conditional"}, \code{"zi"}, \code{"zero-inflated"}, \code{"dispersion"} or
\code{"all"} (default). May be abbreviated.}

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

\item{p_adjust}{String value, if not \code{NULL}, indicates the method to adjust
p-values. See \code{\link[stats:p.adjust]{stats::p.adjust()}} for details. Further possible
adjustment methods are \code{"tukey"}, \code{"scheffe"}, \code{"sidak"}, \code{"sup-t"}, and
\code{"none"} to explicitly disable adjustment for \code{emmGrid} objects (from
\strong{emmeans}). \code{"sup-t"} computes simultaneous confidence bands, also called
sup-t confidence band (Montiel Olea & Plagborg-Møller, 2019).}

\item{include_info}{Logical, if \code{TRUE}, prints summary information about the
model (model formula, number of observations, residual standard deviation
and more).}

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

\item{verbose}{Toggle warnings and messages.}

\item{...}{Arguments passed to or from other methods. For instance, when
\code{bootstrap = TRUE}, arguments like \code{type} or \code{parallel} are passed down to
\code{bootstrap_model()}.

Further non-documented arguments are:
\itemize{
\item \code{digits}, \code{p_digits}, \code{ci_digits} and \code{footer_digits} to set the number of
digits for the output. \code{groups} can be used to group coefficients. These
arguments will be passed to the print-method, or can directly be used in
\code{print()}, see documentation in \code{\link[=print.parameters_model]{print.parameters_model()}}.
\item If \code{s_value = TRUE}, the p-value will be replaced by the S-value in the
output (cf. \emph{Rafi and Greenland 2020}).
\item \code{pd} adds an additional column with the \emph{probability of direction} (see
\code{\link[bayestestR:p_direction]{bayestestR::p_direction()}} for details). Furthermore, see 'Examples' for
this function.
\item For developers, whose interest mainly is to get a "tidy" data frame of
model summaries, it is recommended to set \code{pretty_names = FALSE} to speed
up computation of the summary table.
}}
}
\value{
A data frame of indices related to the model's parameters.
}
\description{
Parameters from zero-inflated models (from packages like \strong{pscl},
\strong{cplm} or \strong{countreg}).
}
\section{Model components}{

Possible values for the \code{component} argument depend on the model class.
Following are valid options:
\itemize{
\item \code{"all"}: returns all model components, applies to all models, but will only
have an effect for models with more than just the conditional model component.
\item \code{"conditional"}: only returns the conditional component, i.e. "fixed effects"
terms from the model. Will only have an effect for models with more than
just the conditional model component.
\item \code{"smooth_terms"}: returns smooth terms, only applies to GAMs (or similar
models that may contain smooth terms).
\item \code{"zero_inflated"} (or \code{"zi"}): returns the zero-inflation component.
\item \code{"dispersion"}: returns the dispersion model component. This is common
for models with zero-inflation or that can model the dispersion parameter.
\item \code{"instruments"}: for instrumental-variable or some fixed effects regression,
returns the instruments.
\item \code{"nonlinear"}: for non-linear models (like models of class \code{nlmerMod} or
\code{nls}), returns staring estimates for the nonlinear parameters.
\item \code{"correlation"}: for models with correlation-component, like \code{gls}, the
variables used to describe the correlation structure are returned.
}

\strong{Special models}

Some model classes also allow rather uncommon options. These are:
\itemize{
\item \strong{mhurdle}: \code{"infrequent_purchase"}, \code{"ip"}, and \code{"auxiliary"}
\item \strong{BGGM}: \code{"correlation"} and \code{"intercept"}
\item \strong{BFBayesFactor}, \strong{glmx}: \code{"extra"}
\item \strong{averaging}:\code{"conditional"} and \code{"full"}
\item \strong{mjoint}: \code{"survival"}
\item \strong{mfx}: \code{"precision"}, \code{"marginal"}
\item \strong{betareg}, \strong{DirichletRegModel}: \code{"precision"}
\item \strong{mvord}: \code{"thresholds"} and \code{"correlation"}
\item \strong{clm2}: \code{"scale"}
\item \strong{selection}: \code{"selection"}, \code{"outcome"}, and \code{"auxiliary"}
\item \strong{lavaan}: One or more of \code{"regression"}, \code{"correlation"}, \code{"loading"},
\code{"variance"}, \code{"defined"}, or \code{"mean"}. Can also be \code{"all"} to include
all components.
}

For models of class \code{brmsfit} (package \strong{brms}), even more options are
possible for the \code{component} argument, which are not all documented in detail
here.
}

\examples{
\dontshow{if (require("pscl")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data("bioChemists", package = "pscl")
model <- pscl::zeroinfl(
  art ~ fem + mar + kid5 + ment | kid5 + phd,
  data = bioChemists
)
model_parameters(model)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[insight:standardize_names]{insight::standardize_names()}} to rename
columns into a consistent, standardized naming scheme.
}
