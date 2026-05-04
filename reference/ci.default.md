# Confidence Intervals (CI)

[`ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
attempts to return confidence intervals of model parameters.

## Usage

``` r
# Default S3 method
ci(
  x,
  ci = 0.95,
  dof = NULL,
  method = NULL,
  iterations = 500,
  component = "all",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A statistical model.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- dof:

  Number of degrees of freedom to be used when calculating confidence
  intervals. If `NULL` (default), the degrees of freedom are retrieved
  by calling
  [`insight::get_df()`](https://easystats.github.io/insight/reference/get_df.html)
  with approximation method defined in `method`. If not `NULL`, use this
  argument to override the default degrees of freedom used to compute
  confidence intervals.

- method:

  Method for computing degrees of freedom for confidence intervals (CI)
  and the related p-values. Allowed are following options (which vary
  depending on the model class): `"residual"`, `"normal"`,
  `"likelihood"`, `"satterthwaite"`, `"kenward"`, `"wald"`, `"profile"`,
  `"boot"`, `"uniroot"`, `"ml1"`, `"betwithin"`, `"hdi"`, `"quantile"`,
  `"ci"`, `"eti"`, `"si"`, `"bci"`, or `"bcai"`. See section *Confidence
  intervals and approximation of degrees of freedom* in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for further details.

- iterations:

  The number of bootstrap replicates. Only applies to models of class
  `merMod` when `method=boot`.

- component:

  Model component for which parameters should be shown. See the
  documentation for your object's class in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  or
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  for further details, or see section *Model components*.

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

- verbose:

  Toggle warnings and messages.

- ...:

  Additional arguments passed down to the underlying functions. E.g.,
  arguments like `vcov` or `vcov_args` can be used to compute confidence
  intervals using a specific variance-covariance matrix for the standard
  errors.

## Value

A data frame containing the CI bounds.

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

## Model components

Possible values for the `component` argument depend on the model class.
Following are valid options:

- `"all"`: returns all model components, applies to all models, but will
  only have an effect for models with more than just the conditional
  model component.

- `"conditional"`: only returns the conditional component, i.e. "fixed
  effects" terms from the model. Will only have an effect for models
  with more than just the conditional model component.

- `"smooth_terms"`: returns smooth terms, only applies to GAMs (or
  similar models that may contain smooth terms).

- `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.

- `"dispersion"`: returns the dispersion model component. This is common
  for models with zero-inflation or that can model the dispersion
  parameter.

- `"instruments"`: for instrumental-variable or some fixed effects
  regression, returns the instruments.

- `"nonlinear"`: for non-linear models (like models of class `nlmerMod`
  or `nls`), returns staring estimates for the nonlinear parameters.

- `"correlation"`: for models with correlation-component, like `gls`,
  the variables used to describe the correlation structure are returned.

**Special models**

Some model classes also allow rather uncommon options. These are:

- **mhurdle**: `"infrequent_purchase"`, `"ip"`, and `"auxiliary"`

- **BGGM**: `"correlation"` and `"intercept"`

- **BFBayesFactor**, **glmx**: `"extra"`

- **averaging**:`"conditional"` and `"full"`

- **mjoint**: `"survival"`

- **mfx**: `"precision"`, `"marginal"`

- **betareg**, **DirichletRegModel**: `"precision"`

- **mvord**: `"thresholds"` and `"correlation"`

- **clm2**: `"scale"`

- **selection**: `"selection"`, `"outcome"`, and `"auxiliary"`

- **lavaan**: One or more of `"regression"`, `"correlation"`,
  `"loading"`, `"variance"`, `"defined"`, or `"mean"`. Can also be
  `"all"` to include all components.

For models of class `brmsfit` (package **brms**), even more options are
possible for the `component` argument, which are not all documented in
detail here.

## Examples

``` r
data(qol_cancer)
model <- lm(QoL ~ time + age + education, data = qol_cancer)

# regular confidence intervals
ci(model)
#>       Parameter   CI    CI_low    CI_high
#> 1   (Intercept) 0.95 58.455237 69.2799382
#> 2          time 0.95 -1.073378  2.8464276
#> 3           age 0.95 -0.320625  0.3709297
#> 4  educationmid 0.95  4.434914 13.0868340
#> 5 educationhigh 0.95  9.326662 19.3815413

# using heteroscedasticity-robust standard errors
ci(model, vcov = "HC3")
#>       Parameter   CI     CI_low    CI_high
#> 1   (Intercept) 0.95 58.3265330 69.4086418
#> 2          time 0.95 -1.1252812  2.8983308
#> 3           age 0.95 -0.3293582  0.3796629
#> 4  educationmid 0.95  4.2133861 13.3083623
#> 5 educationhigh 0.95  9.3661349 19.3420686

# \donttest{
library(parameters)
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)

ci(model)
#>      Parameter   CI       CI_low    CI_high     Component
#> 1  (Intercept) 0.95 -0.904868203  0.1834728   conditional
#> 2        sppPR 0.95 -1.741720718 -0.7977701   conditional
#> 3        sppDM 0.95 -0.001515455  0.5395142   conditional
#> 4      sppEC-A 0.95 -0.967367323 -0.1630136   conditional
#> 5      sppEC-L 0.95  0.414693327  0.9167312   conditional
#> 6     sppDES-L 0.95  0.378400449  0.8723050   conditional
#> 7        sppDF 0.95 -0.173790668  0.4038560   conditional
#> 8      minedno 0.95  0.742998151  1.7998569   conditional
#> 9  (Intercept) 0.95  0.255245254  1.3247538 zero_inflated
#> 10     minedno 0.95 -2.460449166 -1.2293688 zero_inflated
ci(model, component = "zi")
#>     Parameter   CI     CI_low   CI_high     Component
#> 1 (Intercept) 0.95  0.2552453  1.324754 zero_inflated
#> 2     minedno 0.95 -2.4604492 -1.229369 zero_inflated
# }
```
