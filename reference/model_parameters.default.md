# Parameters from (General) Linear Models

Extract and compute indices and measures to describe parameters of
(generalized) linear models (GLMs).

## Usage

``` r
# Default S3 method
model_parameters(
  model,
  ci = 0.95,
  ci_method = NULL,
  bootstrap = FALSE,
  iterations = 1000,
  standardize = NULL,
  exponentiate = FALSE,
  p_adjust = NULL,
  vcov = NULL,
  vcov_args = NULL,
  include_info = getOption("parameters_info", FALSE),
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Model object.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- ci_method:

  Method for computing degrees of freedom for confidence intervals (CI)
  and the related p-values. Allowed are following options (which vary
  depending on the model class): `"residual"`, `"normal"`,
  `"likelihood"`, `"satterthwaite"`, `"kenward"`, `"wald"`, `"profile"`,
  `"boot"`, `"uniroot"`, `"ml1"`, `"betwithin"`, `"hdi"`, `"quantile"`,
  `"ci"`, `"eti"`, `"si"`, `"bci"`, or `"bcai"`. See section *Confidence
  intervals and approximation of degrees of freedom* in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for further details. When `ci_method=NULL`, in most cases `"wald"` is
  used then.

- bootstrap:

  Should estimates be based on bootstrapped model? If `TRUE`, then
  arguments of [Bayesian
  regressions](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.md)
  apply (see also
  [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)).

- iterations:

  The number of bootstrap replicates. This only apply in the case of
  bootstrapped frequentist models.

- standardize:

  The method used for standardizing the parameters. Can be `NULL`
  (default; no standardization), `"refit"` (for re-fitting the model on
  standardized data) or one of `"basic"`, `"posthoc"`, `"smart"`,
  `"pseudo"`. See 'Details' in
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md).
  **Importantly**:

  - The `"refit"` method does *not* standardize categorical predictors
    (i.e. factors), which may be a different behaviour compared to other
    R packages (such as **lm.beta**) or other software packages (like
    SPSS). to mimic such behaviours, either use `standardize="basic"` or
    standardize the data with `datawizard::standardize(force=TRUE)`
    *before* fitting the model.

  - By default, the response (dependent) variable is also standardized,
    *if applicable*. Set `include_response = FALSE` to avoid
    standardization of the response variable. See details in
    [`datawizard::standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.html).

  - For mixed models, when using methods other than `"refit"`, only the
    fixed effects will be standardized.

  - Robust estimation (i.e., `vcov` set to a value other than `NULL`) of
    standardized parameters only works when `standardize="refit"`.

- exponentiate:

  Logical, indicating whether or not to exponentiate the coefficients
  (and related confidence intervals). This is typical for logistic
  regression, or more generally speaking, for models with log or logit
  links. It is also recommended to use `exponentiate = TRUE` for models
  with log-transformed response values. For models with a
  log-transformed response variable, when `exponentiate = TRUE`, a
  one-unit increase in the predictor is associated with multiplying the
  outcome by that predictor's coefficient. **Note:** Delta-method
  standard errors are also computed (by multiplying the standard errors
  by the transformed coefficients). This is to mimic behaviour of other
  software packages, such as Stata, but these standard errors poorly
  estimate uncertainty for the transformed coefficient. The transformed
  confidence interval more clearly captures this uncertainty. For
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  `exponentiate = "nongaussian"` will only exponentiate coefficients
  from non-Gaussian families.

- p_adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"`,
  `"scheffe"`, `"sidak"`, `"sup-t"`, and `"none"` to explicitly disable
  adjustment for `emmGrid` objects (from **emmeans**). `"sup-t"`
  computes simultaneous confidence bands, also called sup-t confidence
  band (Montiel Olea & Plagborg-Møller, 2019).

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`.

- include_info:

  Logical, if `TRUE`, prints summary information about the model (model
  formula, number of observations, residual standard deviation and
  more).

- keep:

  Character containing a regular expression pattern that describes the
  parameters that should be included (for `keep`) or excluded (for
  `drop`) in the returned data frame. `keep` may also be a named list of
  regular expressions. All non-matching parameters will be removed from
  the output. If `keep` is a character vector, every parameter name in
  the *"Parameter"* column that matches the regular expression in `keep`
  will be selected from the returned data frame (and vice versa, all
  parameter names matching `drop` will be excluded). Furthermore, if
  `keep` has more than one element, these will be merged with an `OR`
  operator into a regular expression pattern like this:
  `"(one|two|three)"`. If `keep` is a named list of regular expression
  patterns, the names of the list-element should equal the column name
  where selection should be applied. This is useful for model objects
  where
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  returns multiple columns with parameter components, like in
  [`model_parameters.lavaan()`](https://easystats.github.io/parameters/reference/model_parameters.principal.md).
  Note that the regular expression pattern should match the parameter
  names as they are stored in the returned data frame, which can be
  different from how they are printed. Inspect the `$Parameter` column
  of the parameters table to get the exact parameter names.

- drop:

  See `keep`.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to or from other methods. For instance, when
  `bootstrap = TRUE`, arguments like `type` or `parallel` are passed
  down to
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md).

  Further non-documented arguments are:

  - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the
    number of digits for the output. `groups` can be used to group
    coefficients. These arguments will be passed to the print-method, or
    can directly be used in
    [`print()`](https://rdrr.io/r/base/print.html), see documentation in
    [`print.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.md).

  - If `s_value = TRUE`, the p-value will be replaced by the S-value in
    the output (cf. *Rafi and Greenland 2020*).

  - `pd` adds an additional column with the *probability of direction*
    (see
    [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
    for details). Furthermore, see 'Examples' for this function.

  - For developers, whose interest mainly is to get a "tidy" data frame
    of model summaries, it is recommended to set `pretty_names = FALSE`
    to speed up computation of the summary table.

## Value

A data frame of indices related to the model's parameters.

## Confidence intervals and approximation of degrees of freedom

There are different ways of approximating the degrees of freedom
depending on different assumptions about the nature of the model and its
sampling distribution. The `ci_method` argument modulates the method for
computing degrees of freedom (df) that are used to calculate confidence
intervals (CI) and the related p-values. Following options are allowed,
depending on the model class:

**Classical methods:**

Classical inference is generally based on the **Wald method**. The Wald
approach to inference computes a test statistic by dividing the
parameter estimate by its standard error (Coefficient / SE), then
comparing this statistic against a t- or normal distribution. This
approach can be used to compute CIs and p-values.

`"wald"`:

- Applies to *non-Bayesian models*. For *linear models*, CIs computed
  using the Wald method (SE and a *t-distribution with residual df*);
  p-values computed using the Wald method with a *t-distribution with
  residual df*. For other models, CIs computed using the Wald method (SE
  and a *normal distribution*); p-values computed using the Wald method
  with a *normal distribution*.

`"normal"`

- Applies to *non-Bayesian models*. Compute Wald CIs and p-values, but
  always use a normal distribution.

`"residual"`

- Applies to *non-Bayesian models*. Compute Wald CIs and p-values, but
  always use a *t-distribution with residual df* when possible. If the
  residual df for a model cannot be determined, a normal distribution is
  used instead.

**Methods for mixed models:**

Compared to fixed effects (or single-level) models, determining
appropriate df for Wald-based inference in mixed models is more
difficult. See [the R GLMM
FAQ](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable)
for a discussion.

Several approximate methods for computing df are available, but you
should also consider instead using profile likelihood (`"profile"`) or
bootstrap ("`boot"`) CIs and p-values instead.

`"satterthwaite"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (SE and a *t-distribution with Satterthwaite df*); p-values computed
  using the Wald method with a *t-distribution with Satterthwaite df*.

`"kenward"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (*Kenward-Roger SE* and a *t-distribution with Kenward-Roger df*);
  p-values computed using the Wald method with *Kenward-Roger SE and
  t-distribution with Kenward-Roger df*.

`"ml1"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (SE and a *t-distribution with m-l-1 approximated df*); p-values
  computed using the Wald method with a *t-distribution with m-l-1
  approximated df*. See
  [`ci_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md).

`"betwithin"`

- Applies to *linear mixed models* and *generalized linear mixed
  models*. CIs computed using the Wald method (SE and a *t-distribution
  with between-within df*); p-values computed using the Wald method with
  a *t-distribution with between-within df*. See
  [`ci_betwithin()`](https://easystats.github.io/parameters/reference/p_value_betwithin.md).

**Likelihood-based methods:**

Likelihood-based inference is based on comparing the likelihood for the
maximum-likelihood estimate to the the likelihood for models with one or
more parameter values changed (e.g., set to zero or a range of
alternative values). Likelihood ratios for the maximum-likelihood and
alternative models are compared to a \\\chi\\-squared distribution to
compute CIs and p-values.

`"profile"`

- Applies to *non-Bayesian models* of class `glm`, `polr`, `merMod` or
  `glmmTMB`. CIs computed by *profiling the likelihood curve for a
  parameter*, using linear interpolation to find where likelihood ratio
  equals a critical value; p-values computed using the Wald method with
  a *normal-distribution* (note: this might change in a future update!)

`"uniroot"`

- Applies to *non-Bayesian models* of class `glmmTMB`. CIs computed by
  *profiling the likelihood curve for a parameter*, using root finding
  to find where likelihood ratio equals a critical value; p-values
  computed using the Wald method with a *normal-distribution* (note:
  this might change in a future update!)

**Methods for bootstrapped or Bayesian models:**

Bootstrap-based inference is based on **resampling** and refitting the
model to the resampled datasets. The distribution of parameter estimates
across resampled datasets is used to approximate the parameter's
sampling distribution. Depending on the type of model, several different
methods for bootstrapping and constructing CIs and p-values from the
bootstrap distribution are available.

For Bayesian models, inference is based on drawing samples from the
model posterior distribution.

`"quantile"` (or `"eti"`)

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *equal
  tailed intervals* using the quantiles of the bootstrap or posterior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::eti()`](https://easystats.github.io/bayestestR/reference/eti.html).

`"hdi"`

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *highest
  density intervals* for the bootstrap or posterior samples; p-values
  are based on the *probability of direction*. See
  [`bayestestR::hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html).

`"bci"` (or `"bcai"`)

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *bias
  corrected and accelerated intervals* for the bootstrap or posterior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::bci()`](https://easystats.github.io/bayestestR/reference/bci.html).

`"si"`

- Applies to *Bayesian models* with proper priors. CIs computed as
  *support intervals* comparing the posterior samples against the prior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::si()`](https://easystats.github.io/bayestestR/reference/si.html).

`"boot"`

- Applies to *non-Bayesian models* of class `merMod`. CIs computed using
  *parametric bootstrapping* (simulating data from the fitted model);
  p-values computed using the Wald method with a *normal-distribution)*
  (note: this might change in a future update!).

For all iteration-based methods other than `"boot"` (`"hdi"`,
`"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, `"bcai"`), p-values are
based on the probability of direction
([`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)),
which is converted into a p-value using
[`bayestestR::pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.html).

## See also

[`insight::standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
to rename columns into a consistent, standardized naming scheme.

## Examples

``` r
library(parameters)
model <- lm(mpg ~ wt + cyl, data = mtcars)

model_parameters(model)
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |      p
#> ------------------------------------------------------------------
#> (Intercept) |       39.69 | 1.71 | [36.18, 43.19] | 23.14 | < .001
#> wt          |       -3.19 | 0.76 | [-4.74, -1.64] | -4.22 | < .001
#> cyl         |       -1.51 | 0.41 | [-2.36, -0.66] | -3.64 | 0.001 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# bootstrapped parameters
model_parameters(model, bootstrap = TRUE)
#> Parameter   | Coefficient |         95% CI |      p
#> ---------------------------------------------------
#> (Intercept) |       39.58 | [35.51, 43.99] | < .001
#> wt          |       -3.23 | [-4.94, -2.01] | < .001
#> cyl         |       -1.46 | [-2.19, -0.73] | 0.002 
#> 
#> Uncertainty intervals (equal-tailed) are naıve bootstrap
#>   intervals.

# standardized parameters
model_parameters(model, standardize = "refit")
#> Parameter   | Coefficient |   SE |         95% CI |    t(29) |      p
#> ---------------------------------------------------------------------
#> (Intercept) |    6.14e-17 | 0.08 | [-0.15,  0.15] | 8.15e-16 | > .999
#> wt          |       -0.52 | 0.12 | [-0.77, -0.27] |    -4.22 | < .001
#> cyl         |       -0.45 | 0.12 | [-0.70, -0.20] |    -3.64 | 0.001 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# robust, heteroskedasticity-consistent standard errors
model_parameters(model, vcov = "HC3")
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |      p
#> ------------------------------------------------------------------
#> (Intercept) |       39.69 | 2.30 | [34.97, 44.40] | 17.22 | < .001
#> wt          |       -3.19 | 0.78 | [-4.78, -1.60] | -4.10 | < .001
#> cyl         |       -1.51 | 0.39 | [-2.30, -0.72] | -3.90 | < .001
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

model_parameters(model,
  vcov = "vcovCL",
  vcov_args = list(cluster = mtcars$cyl)
)
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |      p
#> ------------------------------------------------------------------
#> (Intercept) |       39.69 | 1.50 | [36.61, 42.76] | 26.43 | < .001
#> wt          |       -3.19 | 1.20 | [-5.65, -0.73] | -2.65 | 0.013 
#> cyl         |       -1.51 | 0.40 | [-2.32, -0.70] | -3.82 | < .001
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# different p-value style in output
model_parameters(model, p_digits = 5)
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |           p
#> -----------------------------------------------------------------------
#> (Intercept) |       39.69 | 1.71 | [36.18, 43.19] | 23.14 | 3.04318e-20
#> wt          |       -3.19 | 0.76 | [-4.74, -1.64] | -4.22 | 0.00022    
#> cyl         |       -1.51 | 0.41 | [-2.36, -0.66] | -3.64 | 0.00106    
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")
#> Parameter   | Coefficient |    SE |             95% CI |  t(29) |           p
#> -----------------------------------------------------------------------------
#> (Intercept) |      39.686 | 1.715 | [36.1787, 43.1938] | 23.141 | 3.04318e-20
#> wt          |      -3.191 | 0.757 | [-4.7390, -1.6429] | -4.216 | 2.22020e-04
#> cyl         |      -1.508 | 0.415 | [-2.3559, -0.6597] | -3.636 | 1.06428e-03
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# report S-value or probability of direction for parameters
model_parameters(model, s_value = TRUE)
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |     s
#> -----------------------------------------------------------------
#> (Intercept) |       39.69 | 1.71 | [36.18, 43.19] | 23.14 | 64.83
#> wt          |       -3.19 | 0.76 | [-4.74, -1.64] | -4.22 | 12.14
#> cyl         |       -1.51 | 0.41 | [-2.36, -0.66] | -3.64 |  9.88
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
model_parameters(model, pd = TRUE)
#> Parameter   | Coefficient |   SE |         95% CI | t(29) |      p |     pd
#> ---------------------------------------------------------------------------
#> (Intercept) |       39.69 | 1.71 | [36.18, 43.19] | 23.14 | < .001 |   100%
#> wt          |       -3.19 | 0.76 | [-4.74, -1.64] | -4.22 | < .001 | 99.99%
#> cyl         |       -1.51 | 0.41 | [-2.36, -0.66] | -3.64 | 0.001  | 99.95%
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# \donttest{
# logistic regression model
model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
model_parameters(model)
#> Parameter   | Log-Odds |   SE |         95% CI |     z |     p
#> --------------------------------------------------------------
#> (Intercept) |    10.62 | 4.17 | [ 4.79, 22.66] |  2.55 | 0.011
#> wt          |     2.10 | 1.55 | [-0.53,  6.24] |  1.36 | 0.174
#> cyl         |    -2.93 | 1.38 | [-6.92, -1.07] | -2.12 | 0.034
#> 
#> Uncertainty intervals (profile-likelihood) and p-values
#>   (two-tailed) computed using a Wald z-distribution approximation.
#> 
#> The model has a log- or logit-link. Consider using `exponentiate =
#>   TRUE` to interpret coefficients as ratios.
#>   
#> Some coefficients seem to be rather large, which may indicate issues
#>   with (quasi) complete separation. Consider using bias-corrected or
#>   penalized regression models.

# show odds ratio / exponentiated coefficients
model_parameters(model, exponentiate = TRUE)
#> Parameter   | Odds Ratio |       SE |             95% CI |     z |     p
#> ------------------------------------------------------------------------
#> (Intercept) |   40911.34 | 1.71e+05 | [120.16, 6.95e+09] |  2.55 | 0.011
#> wt          |       8.17 |    12.63 | [  0.59,   514.10] |  1.36 | 0.174
#> cyl         |       0.05 |     0.07 | [  0.00,     0.34] | -2.12 | 0.034
#> 
#> Uncertainty intervals (profile-likelihood) and p-values
#>   (two-tailed) computed using a Wald z-distribution approximation.

# bias-corrected logistic regression with penalized maximum likelihood
model <- glm(
  vs ~ wt + cyl,
  data = mtcars,
  family = "binomial",
  method = "brglmFit"
)
model_parameters(model)
#> Parameter   | Log-Odds |   SE |         95% CI |     z |     p
#> --------------------------------------------------------------
#> (Intercept) |     7.71 | 2.66 | [ 2.49, 12.93] |  2.89 | 0.004
#> wt          |     1.46 | 1.08 | [-0.65,  3.57] |  1.35 | 0.176
#> cyl         |    -2.09 | 0.85 | [-3.76, -0.41] | -2.44 | 0.015
#> 
#> Uncertainty intervals (profile-likelihood) and p-values
#>   (two-tailed) computed using a Wald z-distribution approximation.
# }
```
