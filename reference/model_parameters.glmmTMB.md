# Parameters from Mixed Models

Parameters from (linear) mixed models.

## Usage

``` r
# S3 method for class 'glmmTMB'
model_parameters(
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
  vcov = NULL,
  vcov_args = NULL,
  wb_component = FALSE,
  include_info = getOption("parameters_mixed_info", FALSE),
  include_sigma = FALSE,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A mixed model.

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

- ci_random:

  Logical, if `TRUE`, includes the confidence intervals for random
  effects parameters. Only applies if `effects` is not `"fixed"` and if
  `ci` is not `NULL`. Set `ci_random = FALSE` if computation of the
  model summary is too much time consuming. By default,
  `ci_random = NULL`, which uses a heuristic to guess if computation of
  confidence intervals for random effects is fast enough or not. For
  models with larger sample size and/or more complex random effects
  structures, confidence intervals will not be computed by default, for
  simpler models or fewer observations, confidence intervals will be
  included. Set explicitly to `TRUE` or `FALSE` to enforce or omit
  calculation of confidence intervals.

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

- effects:

  Should parameters for fixed effects (`"fixed"`), random effects
  (`"random"`), or both fixed and random effects (`"all"`) be returned?
  By default, the variance components for random effects are returned.
  If group-level effects are requested, `"grouplevel"` returns the
  group-level random effects (BLUPs), while `"random_total"` return the
  overall (sum of fixed and random) effects (similar to what
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns). Using
  `"grouplevel"` is equivalent to setting `group_level = TRUE`. The
  `effects` argument only applies to mixed models. If the calculation of
  random effects parameters takes too long, you may use
  `effects = "fixed"`.

- component:

  Which type of parameters to return, such as parameters for the
  conditional model, the zero-inflation part of the model, the
  dispersion term, or other auxiliary parameters be returned? Applies to
  models with zero-inflation and/or dispersion formula, or if parameters
  such as `sigma` should be included. May be abbreviated. Note that the
  *conditional* component is also called *count* or *mean* component,
  depending on the model. There are three convenient shortcuts:
  `component = "all"` returns all possible parameters. If
  `component = "location"`, location parameters such as `conditional`,
  `zero_inflated`, or `smooth_terms`, are returned (everything that are
  fixed or random effects - depending on the `effects` argument - but no
  auxiliary parameters). For `component = "distributional"` (or
  `"auxiliary"`), components like `sigma`, `dispersion`, or `beta` (and
  other auxiliary parameters) are returned.

- group_level:

  Logical, for multilevel models (i.e. models with random effects) and
  when `effects = "random"`, include the parameters for each group level
  from random effects. If `group_level = FALSE` (the default), only
  information on SD and COR are shown.

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

- wb_component:

  Logical, if `TRUE` and models contains within- and between-effects
  (see
  [`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html)),
  the `Component` column will indicate which variables belong to the
  within-effects, between-effects, and cross-level interactions. By
  default, the `Component` column indicates, which parameters belong to
  the conditional or zero-inflation component of the model.

- include_info:

  Logical, if `TRUE`, prints summary information about the model (model
  formula, number of observations, residual standard deviation and
  more).

- include_sigma:

  Logical, if `TRUE`, includes the residual standard deviation. For
  mixed models, this is defined as the sum of the distribution-specific
  variance and the variance for the additive overdispersion term (see
  [`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html)
  for details). Defaults to `FALSE` for mixed models due to the longer
  computation time.

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

## Note

If the calculation of random effects parameters takes too long, you may
use `effects = "fixed"`. There is also a
[`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/).

## Confidence intervals for random effects variances

For models of class `merMod` and `glmmTMB`, confidence intervals for
random effect variances can be calculated.

- For models of from package **lme4**, when `ci_method` is either
  `"profile"` or `"boot"`, and `effects` is either `"random"` or
  `"all"`, profiled resp. bootstrapped confidence intervals are computed
  for the random effects.

- For all other options of `ci_method`, and only when the **merDeriv**
  package is installed, confidence intervals for random effects are
  based on normal-distribution approximation, using the delta-method to
  transform standard errors for constructing the intervals around the
  log-transformed SD parameters. These are than back-transformed, so
  that random effect variances, standard errors and confidence intervals
  are shown on the original scale. Due to the transformation, the
  intervals are asymmetrical, however, they are within the correct
  bounds (i.e. no negative interval for the SD, and the interval for the
  correlations is within the range from -1 to +1).

- For models of class `glmmTMB`, confidence intervals for random effect
  variances always use a Wald t-distribution approximation.

## Singular fits (random effects variances near zero)

If a model is "singular", this means that some dimensions of the
variance-covariance matrix have been estimated as exactly zero. This
often occurs for mixed models with complex random effects structures.

There is no gold-standard about how to deal with singularity and which
random-effects specification to choose. One way is to fully go Bayesian
(with informative priors). Other proposals are listed in the
documentation of
[`performance::check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.html).
However, since version 1.1.9, the **glmmTMB** package allows to use
priors in a frequentist framework, too. One recommendation is to use a
Gamma prior (*Chung et al. 2013*). The mean may vary from 1 to very
large values (like `1e8`), and the shape parameter should be set to a
value of 2.5. You can then
[`update()`](https://rdrr.io/r/stats/update.html) your model with the
specified prior. In **glmmTMB**, the code would look like this:

    # "model" is an object of class gmmmTMB
    prior <- data.frame(
      prior = "gamma(1, 2.5)",  # mean can be 1, but even 1e8
      class = "ranef"           # for random effects
    )
    model_with_priors <- update(model, priors = prior)

Large values for the mean parameter of the Gamma prior have no large
impact on the random effects variances in terms of a "bias". Thus, if
`1` doesn't fix the singular fit, you can safely try larger values.

## Dispersion parameters in *glmmTMB*

For some models from package **glmmTMB**, both the dispersion parameter
and the residual variance from the random effects parameters are shown.
Usually, these are the same but presented on different scales, e.g.

    model <- glmmTMB(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
    exp(fixef(model)$disp) # 0.09902987
    sigma(model)^2         # 0.09902987

For models where the dispersion parameter and the residual variance are
the same, only the residual variance is shown in the output.

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

## References

Chung Y, Rabe-Hesketh S, Dorie V, Gelman A, and Liu J. 2013. "A
Nondegenerate Penalized Likelihood Estimator for Variance Parameters in
Multilevel Models." Psychometrika 78 (4): 685–709.
[doi:10.1007/s11336-013-9328-2](https://doi.org/10.1007/s11336-013-9328-2)

## See also

[`insight::standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
to rename columns into a consistent, standardized naming scheme.

## Examples

``` r
library(parameters)
data(mtcars)
model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
model_parameters(model)
#> # Fixed Effects 
#> 
#> Parameter   | Coefficient |   SE |         95% CI | t(28) |      p
#> ------------------------------------------------------------------
#> (Intercept) |       36.19 | 2.19 | [31.70, 40.68] | 16.52 | < .001
#> wt          |       -5.05 | 0.64 | [-6.36, -3.73] | -7.89 | < .001
#> 
#> # Random Effects 
#> 
#> Parameter            | Coefficient |   SE |       95% CI
#> --------------------------------------------------------
#> SD (Intercept: gear) |        1.26 | 1.12 | [0.22, 7.17]
#> SD (Residual)        |        2.91 | 0.39 | [2.24, 3.78]
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation. Uncertainty
#>   intervals for random effect variances computed using a Wald
#>   z-distribution approximation.

# \donttest{
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
model_parameters(model, effects = "all")
#> # Fixed Effects (Count Model) 
#> 
#> Parameter   | Log-Mean |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |    -0.36 | 0.28 | [-0.90,  0.18] | -1.30 | 0.194 
#> spp [PR]    |    -1.27 | 0.24 | [-1.74, -0.80] | -5.27 | < .001
#> spp [DM]    |     0.27 | 0.14 | [ 0.00,  0.54] |  1.95 | 0.051 
#> spp [EC-A]  |    -0.57 | 0.21 | [-0.97, -0.16] | -2.75 | 0.006 
#> spp [EC-L]  |     0.67 | 0.13 | [ 0.41,  0.92] |  5.20 | < .001
#> spp [DES-L] |     0.63 | 0.13 | [ 0.38,  0.87] |  4.96 | < .001
#> spp [DF]    |     0.12 | 0.15 | [-0.17,  0.40] |  0.78 | 0.435 
#> mined [no]  |     1.27 | 0.27 | [ 0.74,  1.80] |  4.72 | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component) 
#> 
#> Parameter   | Log-Odds |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |     0.79 | 0.27 | [ 0.26,  1.32] |  2.90 | 0.004 
#> mined [no]  |    -1.84 | 0.31 | [-2.46, -1.23] | -5.87 | < .001
#> 
#> # Random Effects Variances 
#> 
#> Parameter            | Coefficient |       95% CI
#> -------------------------------------------------
#> SD (Intercept: site) |        0.33 | [0.18, 0.63]
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.

model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
model_parameters(model, bootstrap = TRUE, iterations = 50, verbose = FALSE)
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |         95% CI |      p
#> ---------------------------------------------------
#> (Intercept) |       36.34 | [31.64, 40.03] | < .001
#> wt          |       -5.07 | [-6.39, -3.39] | < .001
# }
```
