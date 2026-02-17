# Parameters standardization

Compute standardized model parameters (coefficients).

## Usage

``` r
standardize_parameters(
  model,
  method = "refit",
  ci = 0.95,
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE,
  ...
)

standardize_posteriors(
  model,
  method = "refit",
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A statistical model.

- method:

  The method used for standardizing the parameters. Can be `"refit"`
  (default), `"posthoc"`, `"smart"`, `"basic"`, `"pseudo"` or `"sdy"`.
  See Details'.

- ci:

  Confidence Interval (CI) level

- robust:

  Logical, if `TRUE`, centering is done by subtracting the median from
  the variables and dividing it by the median absolute deviation (MAD).
  If `FALSE`, variables are standardized by subtracting the mean and
  dividing it by the standard deviation (SD).

- two_sd:

  If `TRUE`, the variables are scaled by two times the deviation (SD or
  MAD depending on `robust`). This method can be useful to obtain model
  coefficients of continuous parameters comparable to coefficients
  related to binary predictors, when applied to **the predictors** (not
  the outcome) (Gelman, 2008).

- include_response:

  If `TRUE` (default), the response value will also be standardized. If
  `FALSE`, only the predictors will be standardized. For GLMs the
  response value will never be standardized (see *Generalized Linear
  Models* section).

- verbose:

  Toggle warnings and messages on or off.

- ...:

  For `standardize_parameters()`, arguments passed to
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  such as:

  - `ci_method`, `centrality` for Mixed models and Bayesian models...

  - `exponentiate`, ...

  - etc.

## Value

A data frame with the standardized parameters (`Std_*`, depending on the
model type) and their CIs (`CI_low` and `CI_high`). Where applicable,
standard errors (SEs) are returned as an attribute
(`attr(x, "standard_error")`).

## Details

### Standardization Methods

- **refit**: This method is based on a complete model re-fit with a
  standardized version of the data. Hence, this method is equal to
  standardizing the variables before fitting the model. It is the
  "purest" and the most accurate (Neter et al., 1989), but it is also
  the most computationally costly and long (especially for heavy models
  such as Bayesian models). This method is particularly recommended for
  complex models that include interactions or transformations (e.g.,
  polynomial or spline terms). The `robust` (default to `FALSE`)
  argument enables a robust standardization of data, i.e., based on the
  `median` and `MAD` instead of the `mean` and `SD`. **See
  [`datawizard::standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for more details.**

  - **Note** that `standardize_parameters(method = "refit")` may not
    return the same results as fitting a model on data that has been
    standardized with
    [`standardize()`](https://rdrr.io/pkg/mvtnorm/man/ltMatrices.html);
    `standardize_parameters()` used the data used by the model fitting
    function, which might not be same data if there are missing values.
    see the `remove_na` argument in
    [`standardize()`](https://rdrr.io/pkg/mvtnorm/man/ltMatrices.html).

- **posthoc**: Post-hoc standardization of the parameters, aiming at
  emulating the results obtained by "refit" without refitting the model.
  The coefficients are divided by the standard deviation (or MAD if
  `robust`) of the outcome (which becomes their expression 'unit').
  Then, the coefficients related to numeric variables are additionally
  multiplied by the standard deviation (or MAD if `robust`) of the
  related terms, so that they correspond to changes of 1 SD of the
  predictor (e.g., "A change in 1 SD of `x` is related to a change of
  0.24 of the SD of `y`). This does not apply to binary variables or
  factors, so the coefficients are still related to changes in levels.
  This method is not accurate and tend to give aberrant results when
  interactions are specified.

- **basic**: This method is similar to `method = "posthoc"`, but treats
  all variables as continuous: it also scales the coefficient by the
  standard deviation of model's matrix' parameter of factors levels
  (transformed to integers) or binary predictors. Although being
  inappropriate for these cases, this method is the one implemented by
  default in other software packages, such as
  [`lm.beta::lm.beta()`](https://rdrr.io/pkg/lm.beta/man/lm.beta.html).

- **smart** (Standardization of Model's parameters with Adjustment,
  Reconnaissance and Transformation - *experimental*): Similar to
  `method = "posthoc"` in that it does not involve model refitting. The
  difference is that the SD (or MAD if `robust`) of the response is
  computed on the relevant section of the data. For instance, if a
  factor with 3 levels A (the intercept), B and C is entered as a
  predictor, the effect corresponding to B vs. A will be scaled by the
  variance of the response at the intercept only. As a results, the
  coefficients for effects of factors are similar to a Glass' delta.

- **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method,
  the response and the predictor are standardized based on the level of
  prediction (levels are detected with
  [`performance::check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.html)):
  Predictors are standardized based on their SD at level of prediction
  (see also
  [`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html));
  The outcome (in linear LMMs) is standardized based on a fitted
  random-intercept-model, where `sqrt(random-intercept-variance)` is
  used for level 2 predictors, and `sqrt(residual-variance)` is used for
  level 1 predictors (Hoffman 2015, page 342). A warning is given when a
  within-group variable is found to have access between-group variance.

- **sdy** (*for logistic regression models only*): This
  y-standardization is useful when comparing coefficients of logistic
  regression models across models for the same sample. Unobserved
  heterogeneity varies across models with different independent
  variables, and thus, odds ratios from the same predictor of different
  models cannot be compared directly. The y-standardization makes
  coefficients "comparable across models by dividing them with the
  estimated standard deviation of the latent variable for each model"
  (Mood 2010). Thus, whenever one has multiple logistic regression
  models that are fit to the same data and share certain predictors
  (e.g. nested models), it can be useful to use this standardization
  approach to make log-odds or odds ratios comparable.

### Transformed Variables

When the model's formula contains transformations (e.g. `y ~ exp(X)`)
`method = "refit"` will give different results compared to
`method = "basic"` (`"posthoc"` and `"smart"` do not support such
transformations): While `"refit"` standardizes the data *prior* to the
transformation (e.g. equivalent to `exp(scale(X))`), the `"basic"`
method standardizes the transformed data (e.g. equivalent to
`scale(exp(X))`).\
\
See the *Transformed Variables* section in
[`datawizard::standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.html)
for more details on how different transformations are dealt with when
`method = "refit"`.

### Confidence Intervals

The returned confidence intervals are re-scaled versions of the
unstandardized confidence intervals, and not "true" confidence intervals
of the standardized coefficients (cf. Jones & Waller, 2015).

### Generalized Linear Models

Standardization for generalized linear models (GLM, GLMM, etc) is done
only with respect to the predictors (while the outcome remains as-is,
unstandardized) - maintaining the interpretability of the coefficients
(e.g., in a binomial model: the exponent of the standardized parameter
is the OR of a change of 1 SD in the predictor, etc.)

### Dealing with Factors

`standardize(model)` or
`standardize_parameters(model, method = "refit")` do *not* standardize
categorical predictors (i.e. factors) / their dummy-variables, which may
be a different behaviour compared to other R packages (such as lm.beta)
or other software packages (like SPSS). To mimic such behaviours, either
use `standardize_parameters(model, method = "basic")` to obtain post-hoc
standardized parameters, or standardize the data with
`datawizard::standardize(data, force = TRUE)` *before* fitting the
model.

## References

- Hoffman, L. (2015). Longitudinal analysis: Modeling within-person
  fluctuation and change. Routledge.

- Jones, J. A., & Waller, N. G. (2015). The normal-theory and asymptotic
  distribution-free (ADF) covariance matrix of standardized regression
  coefficients: theoretical extensions and finite sample behavior.
  Psychometrika, 80(2), 365-378.

- Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear
  regression models.

- Gelman, A. (2008). Scaling regression inputs by dividing by two
  standard deviations. Statistics in medicine, 27(15), 2865-2873.

- Mood C. Logistic Regression: Why We Cannot Do What We Think We Can Do,
  and What We Can Do About It. European Sociological Review (2010)
  26:67–82.

## See also

See also [package
vignette](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).

Other standardize:
[`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.md)

## Examples

``` r
model <- lm(len ~ supp * dose, data = ToothGrowth)
standardize_parameters(model, method = "refit")
#> # Standardization method: refit
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.24 | [ 0.05,  0.44]
#> supp [VC]        |      -0.48 | [-0.76, -0.21]
#> dose             |       0.64 | [ 0.45,  0.84]
#> supp [VC] × dose |       0.32 | [ 0.04,  0.60]
# \donttest{
standardize_parameters(model, method = "posthoc")
#> # Standardization method: posthoc
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.00 | [ 0.00,  0.00]
#> supp [VC]        |      -1.08 | [-1.66, -0.49]
#> dose             |       0.64 | [ 0.45,  0.84]
#> supp [VC] × dose |       0.32 | [ 0.04,  0.60]
standardize_parameters(model, method = "smart")
#> # Standardization method: smart
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.00 | [ 0.00,  0.00]
#> supp [VC]        |      -1.00 | [-1.54, -0.46]
#> dose             |       0.64 | [ 0.45,  0.84]
#> supp [VC] × dose |       0.55 | [ 0.07,  1.02]
standardize_parameters(model, method = "basic")
#> # Standardization method: basic
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.00 | [ 0.00,  0.00]
#> supp [VC]        |      -0.54 | [-0.84, -0.25]
#> dose             |       0.64 | [ 0.45,  0.84]
#> supp [VC] × dose |       0.38 | [ 0.05,  0.70]

# Robust and 2 SD
standardize_parameters(model, robust = TRUE)
#> # Standardization method: refit
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.01 | [-0.16,  0.18]
#> supp [VC]        |      -0.48 | [-0.72, -0.24]
#> dose             |       0.64 | [ 0.44,  0.84]
#> supp [VC] × dose |       0.32 | [ 0.04,  0.60]
#> 
#> - Scaled by one MAD from the median.
standardize_parameters(model, two_sd = TRUE)
#> # Standardization method: refit
#> 
#> Parameter        | Std. Coef. |         95% CI
#> ----------------------------------------------
#> (Intercept)      |       0.24 | [ 0.05,  0.44]
#> supp [VC]        |      -0.48 | [-0.76, -0.21]
#> dose             |       1.28 | [ 0.89,  1.68]
#> supp [VC] × dose |       0.64 | [ 0.09,  1.20]
#> 
#> - Scaled by two SDs from the mean.

model <- glm(am ~ cyl * mpg, data = mtcars, family = "binomial")
standardize_parameters(model, method = "refit")
#> # Standardization method: refit
#> 
#> Parameter   | Std. Coef. |        95% CI
#> ----------------------------------------
#> (Intercept) |      -0.58 | [-1.98, 0.70]
#> cyl         |       0.25 | [-1.54, 2.10]
#> mpg         |       2.10 | [-0.19, 5.28]
#> cyl × mpg   |      -0.36 | [-2.57, 1.54]
#> 
#> - Response is unstandardized.
standardize_parameters(model, method = "posthoc")
#> # Standardization method: posthoc
#> 
#> Parameter   | Std. Coef. |         95% CI
#> -----------------------------------------
#> (Intercept) |            |               
#> cyl         |       1.46 | [-4.63,  9.37]
#> mpg         |       3.36 | [-2.31, 12.59]
#> cyl × mpg   |      -0.20 | [-1.44,  0.86]
#> 
#> - Response is unstandardized.
standardize_parameters(model, method = "basic", exponentiate = TRUE)
#> # Standardization method: basic
#> 
#> Parameter   | Std_Odds_Ratio |           95% CI
#> -----------------------------------------------
#> (Intercept) |                |                 
#> cyl         |           4.32 | [0.01, 11681.98]
#> mpg         |          28.80 | [0.10, 2.92e+05]
#> cyl × mpg   |           0.54 | [0.01,    13.94]
#> 
#> - Response is unstandardized.
# }

# \donttest{
m <- lme4::lmer(mpg ~ cyl + am + vs + (1 | cyl), mtcars)
#> boundary (singular) fit: see help('isSingular')
standardize_parameters(m, method = "pseudo", ci_method = "satterthwaite")
#> The following within-group terms have between-group variance:
#>   am, vs
#>   This can inflate standardized within-group parameters associated with
#>   these terms.
#>   See `help("demean", package = "datawizard")` for modeling between- and
#>   within-subject effects.
#> # Standardization method: pseudo
#> 
#> Parameter   | Std. Coef. |         95% CI
#> -----------------------------------------
#> (Intercept) |       0.00 | [ 0.00,  0.00]
#> cyl         |      -0.74 | [-1.25, -0.23]
#> am          |       0.47 | [-0.01,  0.95]
#> vs          |       0.20 | [-0.50,  0.90]
# }
# \donttest{
model <- rstanarm::stan_glm(rating ~ critical + privileges, data = attitude, refresh = 0)
head(standardize_posteriors(model, method = "refit", verbose = FALSE))
#> # Standardization method: refit
#> 
#> (Intercept) | critical | privileges
#> -----------------------------------
#> -0.15       |     0.05 |       0.45
#> 0.10        |     0.29 |       0.35
#> -0.10       |    -0.05 |       0.50
#> 0.04        |     0.26 |       0.36
#> -0.11       |     0.19 |       0.43
#> -0.15       |     0.11 |       0.54
head(standardize_posteriors(model, method = "basic", verbose = FALSE))
#> # Standardization method: basic
#> 
#> (Intercept) | critical | privileges
#> -----------------------------------
#> 0           |     0.17 |       0.10
#> 0           |     0.31 |       0.10
#> 0           |     0.28 |       0.45
#> 0           |    -0.19 |       0.37
#> 0           |     0.17 |       0.31
#> 0           |    -0.14 |       0.52
# }
```
