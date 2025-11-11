# Parameters from Meta-Analysis

Extract and compute indices and measures to describe parameters of
meta-analysis models.

## Usage

``` r
# S3 method for class 'rma'
model_parameters(
  model,
  ci = 0.95,
  bootstrap = FALSE,
  iterations = 1000,
  standardize = NULL,
  exponentiate = FALSE,
  include_studies = TRUE,
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

- include_studies:

  Logical, if `TRUE` (default), includes parameters for all studies.
  Else, only parameters for overall-effects are shown.

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

## Examples

``` r
library(parameters)
mydat <<- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  stderr = c(0.317, 0.317, 0.13, 0.36)
)
if (require("metafor", quietly = TRUE)) {
  model <- rma(yi = effectsize, sei = stderr, method = "REML", data = mydat)
  model_parameters(model)
}
#> 
#> Loading the 'metafor' package (version 4.8-0). For an
#> introduction to the package please type: help(metafor)
#> 
#> Attaching package: ‘metafor’
#> The following object is masked from ‘package:rstanarm’:
#> 
#>     se
#> The following object is masked from ‘package:mclust’:
#> 
#>     hc
#> Meta-analysis using 'metafor'
#> 
#> Parameter | Coefficient |   SE |         95% CI |     z |      p | Weight
#> -------------------------------------------------------------------------
#> Study 1   |       -0.39 | 0.32 | [-1.01,  0.23] | -1.24 | 0.215  |   9.95
#> Study 2   |        0.68 | 0.32 | [ 0.05,  1.30] |  2.13 | 0.033  |   9.95
#> Study 3   |        0.28 | 0.13 | [ 0.03,  0.54] |  2.17 | 0.030  |  59.17
#> Study 4   |       -1.40 | 0.36 | [-2.10, -0.69] | -3.88 | < .001 |   7.72
#> Overall   |       -0.18 | 0.44 | [-1.05,  0.68] | -0.42 | 0.676  |       
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.
# \donttest{
# with subgroups
if (require("metafor", quietly = TRUE)) {
  data(dat.bcg)
  dat <- escalc(
    measure = "RR",
    ai = tpos,
    bi = tneg,
    ci = cpos,
    di = cneg,
    data = dat.bcg
  )
  dat$alloc <- ifelse(dat$alloc == "random", "random", "other")
  d <<- dat
  model <- rma(yi, vi, mods = ~alloc, data = d, digits = 3, slab = author)
  model_parameters(model)
}
#> # Random Effects 
#> 
#> Parameter         | Coefficient |   SE |         95% CI |      z |      p | Weight
#> ----------------------------------------------------------------------------------
#> Aronson           |       -0.89 | 0.57 | [-2.01,  0.23] |  -1.56 | 0.119  |   3.07
#> Ferguson & Simes  |       -1.59 | 0.44 | [-2.45, -0.72] |  -3.59 | < .001 |   5.14
#> Rosenthal et al.1 |       -1.35 | 0.64 | [-2.61, -0.08] |  -2.09 | 0.036  |   2.41
#> Hart & Sutherland |       -1.44 | 0.14 | [-1.72, -1.16] | -10.19 | < .001 |  49.97
#> Vandiviere et al  |       -1.62 | 0.47 | [-2.55, -0.70] |  -3.43 | < .001 |   4.48
#> TPT Madras        |        0.01 | 0.06 | [-0.11,  0.14] |   0.19 | 0.849  | 252.42
#> Coetzee & Berjak  |       -0.47 | 0.24 | [-0.94,  0.00] |  -1.98 | 0.048  |  17.72
#> Overall           |       -0.49 | 0.36 | [-1.20,  0.22] |  -1.35 | 0.176  |       
#> 
#> # other 
#> 
#> Parameter            | Coefficient |   SE |         95% CI |     z |      p | Weight
#> ------------------------------------------------------------------------------------
#> Frimodt-Moller et al |       -0.22 | 0.23 | [-0.66,  0.23] | -0.96 | 0.336  |  19.53
#> Stein & Aronson      |       -0.79 | 0.08 | [-0.95, -0.62] | -9.46 | < .001 | 144.81
#> Rosenthal et al.2    |       -1.37 | 0.27 | [-1.90, -0.84] | -5.07 | < .001 |  13.69
#> Comstock et al.1     |       -0.34 | 0.11 | [-0.56, -0.12] | -3.05 | 0.002  |  80.57
#> Comstock & Webster   |        0.45 | 0.73 | [-0.98,  1.88] |  0.61 | 0.541  |   1.88
#> Comstock et al.2     |       -0.02 | 0.27 | [-0.54,  0.51] | -0.06 | 0.948  |  14.00
#> Overall              |       -0.47 | 0.26 | [-0.97,  0.04] | -1.82 | 0.069  |       
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.

if (require("metaBMA", quietly = TRUE)) {
  data(towels)
  m <- suppressWarnings(meta_random(logOR, SE, study, data = towels))
  model_parameters(m)
}
#> This is metaBMA version 0.6.9
#> - Default priors were changed in version 0.6.6.
#> - Since default priors may change again, it is safest to specify priors (even when using the defaults).
#> 
#> Attaching package: ‘metaBMA’
#> The following object is masked from ‘package:brms’:
#> 
#>     prior
#> # Studies 
#> 
#> Parameter                                           | Coefficient |   SE
#> ------------------------------------------------------------------------
#> Goldstein, Cialdini, & Griskevicius (2008), Exp. 1  |        0.38 | 0.20
#> Goldstein, Cialdini, & Griskevicius  (2008), Exp. 2 |        0.30 | 0.14
#> Schultz, Khazian, & Zaleski (2008), Exp. 2          |        0.21 | 0.19
#> Schultz, Khazian, & Zaleski (2008), Exp. 3          |        0.25 | 0.17
#> Mair & Bergin-Seers (2010), Exp. 1                  |        0.29 | 0.82
#> Bohner & Schluter (2014), Exp. 1                    |       -0.12 | 0.25
#> Bohner & Schluter (2014), Exp. 2                    |       -1.46 | 0.76
#> 
#> Parameter                                           |        95% CI | Weight |                                 Method
#> ---------------------------------------------------------------------------------------------------------------------
#> Goldstein, Cialdini, & Griskevicius (2008), Exp. 1  | [-0.01, 0.77] |  25.59 | Bayesian meta-analysis using 'metaBMA'
#> Goldstein, Cialdini, & Griskevicius  (2008), Exp. 2 | [ 0.04, 0.57] |  53.97 | Bayesian meta-analysis using 'metaBMA'
#> Schultz, Khazian, & Zaleski (2008), Exp. 2          | [-0.17, 0.58] |  27.24 | Bayesian meta-analysis using 'metaBMA'
#> Schultz, Khazian, & Zaleski (2008), Exp. 3          | [-0.08, 0.58] |  34.57 | Bayesian meta-analysis using 'metaBMA'
#> Mair & Bergin-Seers (2010), Exp. 1                  | [-1.33, 1.90] |   1.47 | Bayesian meta-analysis using 'metaBMA'
#> Bohner & Schluter (2014), Exp. 1                    | [-0.61, 0.36] |  16.25 | Bayesian meta-analysis using 'metaBMA'
#> Bohner & Schluter (2014), Exp. 2                    | [-2.95, 0.03] |   1.73 | Bayesian meta-analysis using 'metaBMA'
#> 
#> # Meta-Parameters 
#> 
#> Parameter | Coefficient |   SE |        95% CI |    BF |  Rhat |  ESS
#> ---------------------------------------------------------------------
#> Overall   |        0.20 | 0.11 | [-0.02, 0.39] | 0.804 | 1.001 | 4714
#> tau       |        0.13 | 0.10 | [ 0.03, 0.39] |       | 1.000 | 3696
#> 
#> Parameter |                     Prior |                                 Method
#> ------------------------------------------------------------------------------
#> Overall   |   Student's t (0 +- 0.71) | Bayesian meta-analysis using 'metaBMA'
#> tau       | Inverse gamma (1 +- 0.15) | Bayesian meta-analysis using 'metaBMA'
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a MCMC distribution approximation.
# }
```
