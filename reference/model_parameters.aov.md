# Parameters from ANOVAs

Parameters from ANOVAs

## Usage

``` r
# S3 method for class 'aov'
model_parameters(
  model,
  type = NULL,
  df_error = NULL,
  ci = NULL,
  alternative = NULL,
  p_adjust = NULL,
  test = NULL,
  power = FALSE,
  es_type = NULL,
  keep = NULL,
  drop = NULL,
  include_intercept = FALSE,
  table_wide = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Object of class [`aov()`](https://rdrr.io/r/stats/aov.html),
  [`anova()`](https://rdrr.io/r/stats/anova.html), `aovlist`, `Gam`,
  [`manova()`](https://rdrr.io/r/stats/manova.html), `Anova.mlm`,
  `afex_aov` or `maov`.

- type:

  Numeric, type of sums of squares. May be 1, 2 or 3. If 2 or 3,
  ANOVA-tables using
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) will be
  returned. (Ignored for `afex_aov`.)

- df_error:

  Denominator degrees of freedom (or degrees of freedom of the error
  estimate, i.e., the residuals). This is used to compute effect sizes
  for ANOVA-tables from mixed models. See 'Examples'. (Ignored for
  `afex_aov`.)

- ci:

  Confidence Interval (CI) level for effect sizes specified in
  `es_type`. The default, `NULL`, will compute no confidence intervals.
  `ci` should be a scalar between 0 and 1.

- alternative:

  A character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See section *One-Sided CIs* in the
  [effectsize_CIs vignette](https://easystats.github.io/effectsize/).

- p_adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"`,
  `"scheffe"`, `"sidak"`, `"sup-t"`, and `"none"` to explicitly disable
  adjustment for `emmGrid` objects (from **emmeans**). `"sup-t"`
  computes simultaneous confidence bands, also called sup-t confidence
  band (Montiel Olea & Plagborg-Møller, 2019).

- test:

  String, indicating the type of test for `Anova.mlm` to be returned. If
  `"multivariate"` (or `NULL`), returns the summary of the multivariate
  test (that is also given by the `print`-method). If
  `test = "univariate"`, returns the summary of the univariate test.

- power:

  Logical, if `TRUE`, adds a column with power for each parameter.

- es_type:

  The effect size of interest. Not that possibly not all effect sizes
  are applicable to the model object. See 'Details'. For Anova models,
  can also be a character vector with multiple effect size names.

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

- include_intercept:

  Logical, if `TRUE`, includes the intercept (`(Intercept)`) in the
  anova table.

- table_wide:

  Logical that decides whether the ANOVA table should be in wide format,
  i.e. should the numerator and denominator degrees of freedom be in the
  same row. Default: `FALSE`.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to
  [`effectsize::effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.html).
  For example, to calculate *partial* effect sizes types, use
  `partial = TRUE`. For objects of class `htest` or `BFBayesFactor`,
  `adjust = TRUE` can be used to return bias-corrected effect sizes,
  which is advisable for small samples and large tables. See also
  [`?effectsize::eta_squared`](https://easystats.github.io/effectsize/reference/eta_squared.html)
  for arguments `partial` and `generalized`;
  [`?effectsize::phi`](https://easystats.github.io/effectsize/reference/phi.html)
  for `adjust`; and
  [`?effectsize::oddratio`](https://easystats.github.io/effectsize/reference/oddsratio.html)
  for `log`.

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

## Note

For ANOVA-tables from mixed models (i.e. `anova(lmer())`), only partial
or adjusted effect sizes can be computed. Note that type 3 ANOVAs with
interactions involved only give sensible and informative results when
covariates are mean-centred and factors are coded with orthogonal
contrasts (such as those produced by `contr.sum`, `contr.poly`, or
`contr.helmert`, but *not* by the default `contr.treatment`).

## Examples

``` r
df <- iris
df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

model <- aov(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model)
#> Parameter | Sum_Squares |  df | Mean_Square |    F |     p
#> ----------------------------------------------------------
#> Sepal.Big |        1.10 |   1 |        1.10 | 1.61 | 0.207
#> Residuals |      101.07 | 148 |        0.68 |      |      
#> 
#> Anova Table (Type 1 tests)

model_parameters(model, es_type = c("omega", "eta"), ci = 0.9)
#> Parameter | Sum_Squares |  df | Mean_Square |    F |     p |   Omega2
#> ---------------------------------------------------------------------
#> Sepal.Big |        1.10 |   1 |        1.10 | 1.61 | 0.207 | 4.04e-03
#> Residuals |      101.07 | 148 |        0.68 |      |       |         
#> 
#> Parameter | Omega2 90% CI | Eta2 |  Eta2 90% CI
#> -----------------------------------------------
#> Sepal.Big |  [0.00, 1.00] | 0.01 | [0.00, 1.00]
#> Residuals |               |      |             
#> 
#> Anova Table (Type 1 tests)

model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
model_parameters(model)
#> Parameter | Sum_Squares |  df | Mean_Square |    F |     p
#> ----------------------------------------------------------
#> Sepal.Big |        1.10 |   1 |        1.10 | 1.61 | 0.207
#> Residuals |      101.07 | 148 |        0.68 |      |      
#> 
#> Anova Table (Type 1 tests)
model_parameters(
  model,
  es_type = c("omega", "eta", "epsilon"),
  alternative = "greater"
)
#> Parameter | Sum_Squares |  df | Mean_Square |    F |     p |   Omega2 | Eta2 | Epsilon2
#> ---------------------------------------------------------------------------------------
#> Sepal.Big |        1.10 |   1 |        1.10 | 1.61 | 0.207 | 4.04e-03 | 0.01 | 4.07e-03
#> Residuals |      101.07 | 148 |        0.68 |      |       |          |      |         
#> 
#> Anova Table (Type 1 tests)

model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
model_parameters(model)
#> # Species 
#> 
#> Parameter | Sum_Squares | df | Mean_Square |    F |     p
#> ---------------------------------------------------------
#> Sepal.Big |       28.27 |  1 |       28.27 | 0.81 | 0.534
#> Residuals |       34.94 |  1 |       34.94 |      |      
#> 
#> # Within 
#> 
#> Parameter | Sum_Squares |  df | Mean_Square |     F |      p
#> ------------------------------------------------------------
#> Sepal.Big |        4.74 |   1 |        4.74 | 20.24 | < .001
#> Residuals |       34.21 | 146 |        0.23 |       |       
#> 
#> Anova Table (Type 1 tests)
# \donttest{
df <- iris
df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
mm <- lme4::lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species), data = df)
#> boundary (singular) fit: see help('isSingular')
model <- anova(mm)

# simple parameters table
model_parameters(model)
#> Parameter   | Sum_Squares | df | Mean_Square |      F
#> -----------------------------------------------------
#> Sepal.Big   |        1.10 |  1 |        1.10 |   4.96
#> Petal.Width |       68.50 |  1 |       68.50 | 309.23
#> 
#> Anova Table (Type 1 tests)

# parameters table including effect sizes
model_parameters(
  model,
  es_type = "eta",
  ci = 0.9,
  df_error = dof_satterthwaite(mm)[2:3]
)
#> Parameter   | Sum_Squares | df | Mean_Square |      F | Eta2 (partial) |  Eta2 90% CI
#> -------------------------------------------------------------------------------------
#> Sepal.Big   |        1.10 |  1 |        1.10 |   4.96 |           0.03 | [0.01, 1.00]
#> Petal.Width |       68.50 |  1 |       68.50 | 309.23 |           0.68 | [0.63, 1.00]
#> 
#> Anova Table (Type 1 tests)
# }
```
