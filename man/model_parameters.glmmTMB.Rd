% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_glmmTMB.R
\name{model_parameters.glmmTMB}
\alias{model_parameters.glmmTMB}
\title{Parameters from Mixed Models}
\usage{
\method{model_parameters}{glmmTMB}(
  model,
  ci = 0.95,
  ci_method = "wald",
  ci_random = NULL,
  bootstrap = FALSE,
  iterations = 1000,
  standardize = NULL,
  effects = "all",
  component = "all",
  group_level = FALSE,
  exponentiate = FALSE,
  p_adjust = NULL,
  wb_component = FALSE,
  include_info = getOption("parameters_mixed_info", FALSE),
  include_sigma = FALSE,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{model}{A mixed model.}

\item{ci}{Confidence Interval (CI) level. Default to \code{0.95} (\verb{95\%}).}

\item{ci_method}{Method for computing degrees of freedom for
confidence intervals (CI) and the related p-values. Allowed are following
options (which vary depending on the model class): \code{"residual"},
\code{"normal"}, \code{"likelihood"}, \code{"satterthwaite"}, \code{"kenward"}, \code{"wald"},
\code{"profile"}, \code{"boot"}, \code{"uniroot"}, \code{"ml1"}, \code{"betwithin"}, \code{"hdi"},
\code{"quantile"}, \code{"ci"}, \code{"eti"}, \code{"si"}, \code{"bci"}, or \code{"bcai"}. See section
\emph{Confidence intervals and approximation of degrees of freedom} in
\code{\link[=model_parameters]{model_parameters()}} for further details. When \code{ci_method=NULL}, in most
cases \code{"wald"} is used then.}

\item{ci_random}{Logical, if \code{TRUE}, includes the confidence intervals for
random effects parameters. Only applies if \code{effects} is not \code{"fixed"} and
if \code{ci} is not \code{NULL}. Set \code{ci_random = FALSE} if computation of the model
summary is too much time consuming. By default, \code{ci_random = NULL}, which
uses a heuristic to guess if computation of confidence intervals for random
effects is fast enough or not. For models with larger sample size and/or
more complex random effects structures, confidence intervals will not be
computed by default, for simpler models or fewer observations, confidence
intervals will be included. Set explicitly to \code{TRUE} or \code{FALSE} to enforce
or omit calculation of confidence intervals.}

\item{bootstrap}{Should estimates be based on bootstrapped model? If \code{TRUE},
then arguments of \link[=model_parameters.brmsfit]{Bayesian regressions} apply
(see also \code{\link[=bootstrap_parameters]{bootstrap_parameters()}}).}

\item{iterations}{The number of bootstrap replicates. This only apply in the
case of bootstrapped frequentist models.}

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

\item{effects}{Should parameters for fixed effects (\code{"fixed"}), random effects
(\code{"random"}), or both fixed and random effects (\code{"all"}) be returned? By
default, the variance components for random effects are returned. If
group-level effects are requested, \code{"grouplevel"} returns the group-level
random effects (BLUPs), while \code{"random_total"} return the overall (sum of
fixed and random) effects (similar to what \code{coef()} returns). Using
\code{"grouplevel"} is equivalent to setting \code{group_level = TRUE}. The \code{effects}
argument only applies to mixed models. If the calculation of random effects
parameters takes too long, you may use \code{effects = "fixed"}.}

\item{component}{Which type of parameters to return, such as parameters for the
conditional model, the zero-inflation part of the model, the dispersion
term, or other auxiliary parameters be returned? Applies to models with
zero-inflation and/or dispersion formula, or if parameters such as \code{sigma}
should be included. May be abbreviated. Note that the \emph{conditional}
component is also called \emph{count} or \emph{mean} component, depending on the
model. There are three convenient shortcuts: \code{component = "all"} returns
all possible parameters. If \code{component = "location"}, location parameters
such as \code{conditional}, \code{zero_inflated}, or \code{smooth_terms}, are returned
(everything that are fixed or random effects - depending on the \code{effects}
argument - but no auxiliary parameters). For \code{component = "distributional"}
(or \code{"auxiliary"}), components like \code{sigma}, \code{dispersion}, or \code{beta}
(and other auxiliary parameters) are returned.}

\item{group_level}{Logical, for multilevel models (i.e. models with random
effects) and when \code{effects = "random"}, include the parameters for each
group level from random effects. If \code{group_level = FALSE} (the default),
only information on SD and COR are shown.}

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

\item{wb_component}{Logical, if \code{TRUE} and models contains within- and
between-effects (see \code{datawizard::demean()}), the \code{Component} column
will indicate which variables belong to the within-effects,
between-effects, and cross-level interactions. By default, the
\code{Component} column indicates, which parameters belong to the
conditional or zero-inflation component of the model.}

\item{include_info}{Logical, if \code{TRUE}, prints summary information about the
model (model formula, number of observations, residual standard deviation
and more).}

\item{include_sigma}{Logical, if \code{TRUE}, includes the residual standard
deviation. For mixed models, this is defined as the sum of the distribution-specific
variance and the variance for the additive overdispersion term (see
\code{\link[insight:get_variance]{insight::get_variance()}} for details). Defaults to \code{FALSE} for mixed models
due to the longer computation time.}

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
Parameters from (linear) mixed models.
}
\note{
If the calculation of random effects parameters takes too long, you may
use \code{effects = "fixed"}. There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method}
implemented in the \href{https://easystats.github.io/see/}{\strong{see}-package}.
}
\section{Confidence intervals for random effects variances}{

For models of class \code{merMod} and \code{glmmTMB}, confidence intervals for random
effect variances can be calculated.
\itemize{
\item For models of from package \strong{lme4}, when \code{ci_method} is either \code{"profile"}
or \code{"boot"}, and \code{effects} is either \code{"random"} or \code{"all"}, profiled resp.
bootstrapped confidence intervals are computed for the random effects.
\item For all other options of \code{ci_method}, and only when the \strong{merDeriv}
package is installed, confidence intervals for random effects are based on
normal-distribution approximation, using the delta-method to transform
standard errors for constructing the intervals around the log-transformed
SD parameters. These are than back-transformed, so that random effect
variances, standard errors and confidence intervals are shown on the original
scale. Due to the transformation, the intervals are asymmetrical, however,
they are within the correct bounds (i.e. no negative interval for the SD,
and the interval for the correlations is within the range from -1 to +1).
\item For models of class \code{glmmTMB}, confidence intervals for random effect
variances always use a Wald t-distribution approximation.
}
}

\section{Singular fits (random effects variances near zero)}{

If a model is "singular", this means that some dimensions of the
variance-covariance matrix have been estimated as exactly zero. This
often occurs for mixed models with complex random effects structures.

There is no gold-standard about how to deal with singularity and which
random-effects specification to choose. One way is to fully go Bayesian
(with informative priors). Other proposals are listed in the documentation
of \code{\link[performance:check_singularity]{performance::check_singularity()}}. However, since version 1.1.9, the
\strong{glmmTMB} package allows to use priors in a frequentist framework, too. One
recommendation is to use a Gamma prior (\emph{Chung et al. 2013}). The mean may
vary from 1 to very large values (like \code{1e8}), and the shape parameter should
be set to a value of 2.5. You can then \code{update()} your model with the specified
prior. In \strong{glmmTMB}, the code would look like this:

\if{html}{\out{<div class="sourceCode">}}\preformatted{# "model" is an object of class gmmmTMB
prior <- data.frame(
  prior = "gamma(1, 2.5)",  # mean can be 1, but even 1e8
  class = "ranef"           # for random effects
)
model_with_priors <- update(model, priors = prior)
}\if{html}{\out{</div>}}

Large values for the mean parameter of the Gamma prior have no large impact
on the random effects variances in terms of a "bias". Thus, if \code{1} doesn't
fix the singular fit, you can safely try larger values.
}

\section{Dispersion parameters in \emph{glmmTMB}}{

For some models from package \strong{glmmTMB}, both the dispersion parameter and
the residual variance from the random effects parameters are shown. Usually,
these are the same but presented on different scales, e.g.

\if{html}{\out{<div class="sourceCode">}}\preformatted{model <- glmmTMB(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
exp(fixef(model)$disp) # 0.09902987
sigma(model)^2         # 0.09902987
}\if{html}{\out{</div>}}

For models where the dispersion parameter and the residual variance are
the same, only the residual variance is shown in the output.
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

\section{Confidence intervals and approximation of degrees of freedom}{

There are different ways of approximating the degrees of freedom depending
on different assumptions about the nature of the model and its sampling
distribution. The \code{ci_method} argument modulates the method for computing degrees
of freedom (df) that are used to calculate confidence intervals (CI) and the
related p-values. Following options are allowed, depending on the model
class:

\strong{Classical methods:}

Classical inference is generally based on the \strong{Wald method}.
The Wald approach to inference computes a test statistic by dividing the
parameter estimate by its standard error (Coefficient / SE),
then comparing this statistic against a t- or normal distribution.
This approach can be used to compute CIs and p-values.

\code{"wald"}:
\itemize{
\item Applies to \emph{non-Bayesian models}. For \emph{linear models}, CIs
computed using the Wald method (SE and a \emph{t-distribution with residual df});
p-values computed using the Wald method with a \emph{t-distribution with residual df}.
For other models, CIs computed using the Wald method (SE and a \emph{normal distribution});
p-values computed using the Wald method with a \emph{normal distribution}.
}

\code{"normal"}
\itemize{
\item Applies to \emph{non-Bayesian models}. Compute Wald CIs and p-values,
but always use a normal distribution.
}

\code{"residual"}
\itemize{
\item Applies to \emph{non-Bayesian models}. Compute Wald CIs and p-values,
but always use a \emph{t-distribution with residual df} when possible. If the
residual df for a model cannot be determined, a normal distribution is
used instead.
}

\strong{Methods for mixed models:}

Compared to fixed effects (or single-level) models, determining appropriate
df for Wald-based inference in mixed models is more difficult.
See \href{https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable}{the R GLMM FAQ}
for a discussion.

Several approximate methods for computing df are available, but you should
also consider instead using profile likelihood (\code{"profile"}) or bootstrap ("\verb{boot"})
CIs and p-values instead.

\code{"satterthwaite"}
\itemize{
\item Applies to \emph{linear mixed models}. CIs computed using the
Wald method (SE and a \emph{t-distribution with Satterthwaite df}); p-values
computed using the Wald method with a \emph{t-distribution with Satterthwaite df}.
}

\code{"kenward"}
\itemize{
\item Applies to \emph{linear mixed models}. CIs computed using the Wald
method (\emph{Kenward-Roger SE} and a \emph{t-distribution with Kenward-Roger df});
p-values computed using the Wald method with \emph{Kenward-Roger SE and t-distribution with Kenward-Roger df}.
}

\code{"ml1"}
\itemize{
\item Applies to \emph{linear mixed models}. CIs computed using the Wald
method (SE and a \emph{t-distribution with m-l-1 approximated df}); p-values
computed using the Wald method with a \emph{t-distribution with m-l-1 approximated df}.
See \code{\link[=ci_ml1]{ci_ml1()}}.
}

\code{"betwithin"}
\itemize{
\item Applies to \emph{linear mixed models} and \emph{generalized linear mixed models}.
CIs computed using the Wald method (SE and a \emph{t-distribution with between-within df});
p-values computed using the Wald method with a \emph{t-distribution with between-within df}.
See \code{\link[=ci_betwithin]{ci_betwithin()}}.
}

\strong{Likelihood-based methods:}

Likelihood-based inference is based on comparing the likelihood for the
maximum-likelihood estimate to the the likelihood for models with one or more
parameter values changed (e.g., set to zero or a range of alternative values).
Likelihood ratios for the maximum-likelihood and alternative models are compared
to a \eqn{\chi}-squared distribution to compute CIs and p-values.

\code{"profile"}
\itemize{
\item Applies to \emph{non-Bayesian models} of class \code{glm}, \code{polr}, \code{merMod} or \code{glmmTMB}.
CIs computed by \emph{profiling the likelihood curve for a parameter}, using
linear interpolation to find where likelihood ratio equals a critical value;
p-values computed using the Wald method with a \emph{normal-distribution} (note:
this might change in a future update!)
}

\code{"uniroot"}
\itemize{
\item Applies to \emph{non-Bayesian models} of class \code{glmmTMB}. CIs
computed by \emph{profiling the likelihood curve for a parameter}, using root
finding to find where likelihood ratio equals a critical value; p-values
computed using the Wald method with a \emph{normal-distribution} (note: this
might change in a future update!)
}

\strong{Methods for bootstrapped or Bayesian models:}

Bootstrap-based inference is based on \strong{resampling} and refitting the model
to the resampled datasets. The distribution of parameter estimates across
resampled datasets is used to approximate the parameter's sampling
distribution. Depending on the type of model, several different methods for
bootstrapping and constructing CIs and p-values from the bootstrap
distribution are available.

For Bayesian models, inference is based on drawing samples from the model
posterior distribution.

\code{"quantile"} (or \code{"eti"})
\itemize{
\item Applies to \emph{all models (including Bayesian models)}.
For non-Bayesian models, only applies if \code{bootstrap = TRUE}. CIs computed
as \emph{equal tailed intervals} using the quantiles of the bootstrap or
posterior samples; p-values are based on the \emph{probability of direction}.
See \code{\link[bayestestR:eti]{bayestestR::eti()}}.
}

\code{"hdi"}
\itemize{
\item Applies to \emph{all models (including Bayesian models)}. For non-Bayesian
models, only applies if \code{bootstrap = TRUE}. CIs computed as \emph{highest density intervals}
for the bootstrap or posterior samples; p-values are based on the \emph{probability of direction}.
See \code{\link[bayestestR:hdi]{bayestestR::hdi()}}.
}

\code{"bci"} (or \code{"bcai"})
\itemize{
\item Applies to \emph{all models (including Bayesian models)}.
For non-Bayesian models, only applies if \code{bootstrap = TRUE}. CIs computed
as \emph{bias corrected and accelerated intervals} for the bootstrap or
posterior samples; p-values are based on the \emph{probability of direction}.
See \code{\link[bayestestR:bci]{bayestestR::bci()}}.
}

\code{"si"}
\itemize{
\item Applies to \emph{Bayesian models} with proper priors. CIs computed as
\emph{support intervals} comparing the posterior samples against the prior samples;
p-values are based on the \emph{probability of direction}. See \code{\link[bayestestR:si]{bayestestR::si()}}.
}

\code{"boot"}
\itemize{
\item Applies to \emph{non-Bayesian models} of class \code{merMod}. CIs computed
using \emph{parametric bootstrapping} (simulating data from the fitted model);
p-values computed using the Wald method with a \emph{normal-distribution)}
(note: this might change in a future update!).
}

For all iteration-based methods other than \code{"boot"}
(\code{"hdi"}, \code{"quantile"}, \code{"ci"}, \code{"eti"}, \code{"si"}, \code{"bci"}, \code{"bcai"}),
p-values are based on the probability of direction (\code{\link[bayestestR:p_direction]{bayestestR::p_direction()}}),
which is converted into a p-value using \code{\link[bayestestR:pd_to_p]{bayestestR::pd_to_p()}}.
}

\examples{
\dontshow{if (require("lme4") && require("glmmTMB")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(parameters)
data(mtcars)
model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
model_parameters(model)

\donttest{
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
model_parameters(model, effects = "all")

model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
model_parameters(model, bootstrap = TRUE, iterations = 50, verbose = FALSE)
}
\dontshow{\}) # examplesIf}
}
\references{
Chung Y, Rabe-Hesketh S, Dorie V, Gelman A, and Liu J. 2013. "A Nondegenerate
Penalized Likelihood Estimator for Variance Parameters in Multilevel Models."
Psychometrika 78 (4): 685–709. \doi{10.1007/s11336-013-9328-2}
}
\seealso{
\code{\link[insight:standardize_names]{insight::standardize_names()}} to
rename columns into a consistent, standardized naming scheme.
}
