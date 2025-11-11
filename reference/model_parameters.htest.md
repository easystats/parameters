# Parameters from hypothesis tests

Parameters of h-tests (correlations, t-tests, chi-squared, ...).

## Usage

``` r
# S3 method for class 'htest'
model_parameters(
  model,
  ci = 0.95,
  alternative = NULL,
  bootstrap = FALSE,
  es_type = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'coeftest'
model_parameters(
  model,
  ci = 0.95,
  ci_method = "wald",
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Object of class `htest` or `pairwise.htest`.

- ci:

  Level of confidence intervals for effect size statistic. Currently
  only applies to objects from
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
  [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html).

- alternative:

  A character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See section *One-Sided CIs* in the
  [effectsize_CIs vignette](https://easystats.github.io/effectsize/).

- bootstrap:

  Should estimates be bootstrapped?

- es_type:

  The effect size of interest. Not that possibly not all effect sizes
  are applicable to the model object. See 'Details'. For Anova models,
  can also be a character vector with multiple effect size names.

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

## Value

A data frame of indices related to the model's parameters.

## Details

- For an object of class `htest`, data is extracted via
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html),
  and passed to the relevant function according to:

  - A **t-test** depending on `type`: `"cohens_d"` (default),
    `"hedges_g"`, or one of `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`,
    `"overlap"`.

    - For a **Paired t-test**: depending on `type`: `"rm_rm"`,
      `"rm_av"`, `"rm_b"`, `"rm_d"`, `"rm_z"`.

  - A **Chi-squared tests of independence** or **Fisher's Exact Test**,
    depending on `type`: `"cramers_v"` (default), `"tschuprows_t"`,
    `"phi"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`,
    `"riskratio"`, `"arr"`, or `"nnt"`.

  - A **Chi-squared tests of goodness-of-fit**, depending on `type`:
    `"fei"` (default) `"cohens_w"`, `"pearsons_c"`

  - A **One-way ANOVA test**, depending on `type`: `"eta"` (default),
    `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.

  - A **McNemar test** returns *Cohen's g*.

  - A **Wilcoxon test** depending on `type`: returns "`rank_biserial`"
    correlation (default) or one of `"p_superiority"`, `"vda"`, `"u2"`,
    `"u3"`, `"overlap"`.

  - A **Kruskal-Wallis test** depending on `type`: `"epsilon"` (default)
    or `"eta"`.

  - A **Friedman test** returns *Kendall's W*. (Where applicable, `ci`
    and `alternative` are taken from the `htest` if not otherwise
    provided.)

- For an object of class `BFBayesFactor`, using
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html),

  - A **t-test** depending on `type`: `"cohens_d"` (default) or one of
    `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`, `"overlap"`.

  - A **correlation test** returns *r*.

  - A **contingency table test**, depending on `type`: `"cramers_v"`
    (default), `"phi"`, `"tschuprows_t"`, `"cohens_w"`, `"pearsons_c"`,
    `"cohens_h"`, `"oddsratio"`, or `"riskratio"`, `"arr"`, or `"nnt"`.

  - A **proportion test** returns *p*.

- Objects of class `anova`, `aov`, `aovlist` or `afex_aov`, depending on
  `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`,
  or `"f2"`.

- Objects of class `datawizard_crosstab(s)` / `datawizard_table(s)`
  built with
  [`datawizard::data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.html) -
  same as Chi-squared tests of independence / goodness-of-fit,
  respectively.

- Other objects are passed to
  [`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md).

**For statistical models it is recommended to directly use the listed
functions, for the full range of options they provide.**

## Examples

``` r

model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
model_parameters(model)
#> Pearson's product-moment correlation
#> 
#> Parameter1 | Parameter2 |     r |         95% CI | t(30) |      p
#> -----------------------------------------------------------------
#> mtcars$mpg | mtcars$cyl | -0.85 | [-0.93, -0.72] | -8.92 | < .001
#> 
#> Alternative hypothesis: true correlation is not equal to 0

model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
model_parameters(model, es_type = "hedges_g")
#> Welch Two Sample t-test
#> 
#> Parameter1       |        Parameter2 | Mean_Parameter1 | Mean_Parameter2
#> ------------------------------------------------------------------------
#> iris$Sepal.Width | iris$Sepal.Length |            3.06 |            5.84
#> 
#> Parameter1       | Difference |         95% CI | Hedges' g |       g 95% CI
#> ---------------------------------------------------------------------------
#> iris$Sepal.Width |      -2.79 | [-2.94, -2.64] |     -4.20 | [-4.64, -3.75]
#> 
#> Parameter1       | t(225.68) |      p
#> -------------------------------------
#> iris$Sepal.Width |    -36.46 | < .001
#> 
#> Alternative hypothesis: true difference in means is not equal to 0

model <- t.test(mtcars$mpg ~ mtcars$vs)
model_parameters(model, es_type = "hedges_g")
#> Welch Two Sample t-test
#> 
#> Parameter  |     Group | Mean_Group1 | Mean_Group2 | Difference
#> ---------------------------------------------------------------
#> mtcars$mpg | mtcars$vs |       16.62 |       24.56 |      -7.94
#> 
#> Parameter  |          95% CI | Hedges' g |       g 95% CI | t(22.72) |      p
#> -----------------------------------------------------------------------------
#> mtcars$mpg | [-11.46, -4.42] |     -1.64 | [-2.46, -0.79] |    -4.67 | < .001
#> 
#> Alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0

model <- t.test(iris$Sepal.Width, mu = 1)
model_parameters(model, es_type = "cohens_d")
#> One Sample t-test
#> 
#> Parameter        | mu | Difference |       95% CI | Cohen's d |     d 95% CI
#> ----------------------------------------------------------------------------
#> iris$Sepal.Width |  1 |       2.06 | [2.99, 3.13] |      4.72 | [4.15, 5.27]
#> 
#> Parameter        | t(149) |      p
#> ----------------------------------
#> iris$Sepal.Width |  57.81 | < .001
#> 
#> Alternative hypothesis: true mean is not equal to 1

data(airquality)
airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
model <- pairwise.t.test(airquality$Ozone, airquality$Month)
model_parameters(model)
#> # Fixed Effects
#> 
#> Group1 | Group2 |      p
#> ------------------------
#> Jun    |    May | > .999
#> Jul    |    May | < .001
#> Jul    |    Jun | 0.051 
#> Aug    |    May | < .001
#> Aug    |    Jun | 0.050 
#> Aug    |    Jul | > .999
#> Sep    |    May | > .999
#> Sep    |    Jun | > .999
#> Sep    |    Jul | 0.005 
#> Sep    |    Aug | 0.004 
#> 
#> p-value adjustment method: Holm (1979)

smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
model <- suppressWarnings(pairwise.prop.test(smokers, patients))
model_parameters(model)
#> # Fixed Effects
#> 
#> Group1 | Group2 |      p
#> ------------------------
#> 2      |      1 | > .999
#> 3      |      1 | > .999
#> 3      |      2 | > .999
#> 4      |      1 | 0.119 
#> 4      |      2 | 0.093 
#> 4      |      3 | 0.124 
#> 
#> p-value adjustment method: Holm (1979)

model <- suppressWarnings(chisq.test(table(mtcars$am, mtcars$cyl)))
model_parameters(model, es_type = "cramers_v")
#> Pearson's Chi-squared test
#> 
#> Chi2(2) | Cramer's V (adj.) | Cramers 95% CI |     p
#> ----------------------------------------------------
#> 8.74    |              0.46 |   [0.00, 1.00] | 0.013
```
