# Printing Model Parameters

[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
and
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
are functions that return a data frame of model summaries in a
consistent way. The printed table of those summaries is formatted to
make the output more readable and removes or collapses redundant
columns, to get a compact and yet comprehensive summary table. *(N.B.
for developers: the function
[`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
standardizes the column names, so column names are consistent and the
same for any model object, also in **broom** style, which makes it easy
to build your packages on top of the **parameters** package.)*

The default
[print-methods](https://easystats.github.io/parameters/reference/print.parameters_model.html)
for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
and
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
allows the user to modify the layout and style of the output.

## Summaries for a single model

In the following examples for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
which returns tabular output for single models, are shown.

### Pretty parameter names formatting

By default, the argument `pretty_names` is `TRUE`, meaning that
parameter names are formatted to make them more “human readable”,
i.e. factor levels are separated from the variable names, interactions
are denoted by `*` etc.

``` r

library(parameters)
data(iris)
model <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
model_parameters(model)
#> Parameter                           | Coefficient |   SE |         95% CI
#> -------------------------------------------------------------------------
#> (Intercept)                         |        4.21 | 0.41 | [ 3.41,  5.02]
#> Species [versicolor]                |       -1.81 | 0.60 | [-2.99, -0.62]
#> Species [virginica]                 |       -3.15 | 0.63 | [-4.41, -1.90]
#> Petal Length                        |        0.54 | 0.28 | [ 0.00,  1.09]
#> Species [versicolor] × Petal Length |        0.29 | 0.30 | [-0.30,  0.87]
#> Species [virginica] × Petal Length  |        0.45 | 0.29 | [-0.12,  1.03]
#> 
#> Parameter                           | t(144) |      p
#> -----------------------------------------------------
#> (Intercept)                         |  10.34 | < .001
#> Species [versicolor]                |  -3.02 | 0.003 
#> Species [virginica]                 |  -4.97 | < .001
#> Petal Length                        |   1.96 | 0.052 
#> Species [versicolor] × Petal Length |   0.97 | 0.334 
#> Species [virginica] × Petal Length  |   1.56 | 0.120

mp <- model_parameters(model)
print(mp, pretty_names = FALSE)
#> Parameter                      | Coefficient |   SE |         95% CI | t(144) |      p
#> --------------------------------------------------------------------------------------
#> (Intercept)                    |        4.21 | 0.41 | [ 3.41,  5.02] |  10.34 | < .001
#> Speciesversicolor              |       -1.81 | 0.60 | [-2.99, -0.62] |  -3.02 | 0.003 
#> Speciesvirginica               |       -3.15 | 0.63 | [-4.41, -1.90] |  -4.97 | < .001
#> Petal.Length                   |        0.54 | 0.28 | [ 0.00,  1.09] |   1.96 | 0.052 
#> Speciesversicolor:Petal.Length |        0.29 | 0.30 | [-0.30,  0.87] |   0.97 | 0.334 
#> Speciesvirginica:Petal.Length  |        0.45 | 0.29 | [-0.12,  1.03] |   1.56 | 0.120
```

If data is
[labelled](https://strengejacke.github.io/sjlabelled/articles/intro_sjlabelled.html),
`pretty_names = "labels"` will use variable and value labels as pretty
names. If data is not labelled, default pretty names will be used.

``` r

data(efc, package = "datawizard")
model <- lm(neg_c_7 ~ e42dep + c172code, data = efc)

# default printing
model_parameters(model)
#> Parameter   | Coefficient |   SE |         95% CI | t(80) |     p
#> -----------------------------------------------------------------
#> (Intercept) |        8.72 | 3.56 | [ 1.63, 15.80] |  2.45 | 0.017
#> e42dep [2]  |       -1.00 | 3.72 | [-8.41,  6.41] | -0.27 | 0.789
#> e42dep [3]  |        2.68 | 3.16 | [-3.60,  8.96] |  0.85 | 0.398
#> e42dep [4]  |        3.88 | 3.10 | [-2.29, 10.04] |  1.25 | 0.214
#> c172code    |        1.14 | 0.93 | [-0.70,  2.99] |  1.23 | 0.221

# using value and variable labels
mp <- model_parameters(model)
print(mp, pretty_names = "labels")
#> Parameter                                 | Coefficient |   SE |         95% CI
#> -------------------------------------------------------------------------------
#> (Intercept)                               |        8.72 | 3.56 | [ 1.63, 15.80]
#> elder's dependency [slightly dependent]   |       -1.00 | 3.72 | [-8.41,  6.41]
#> elder's dependency [moderately dependent] |        2.68 | 3.16 | [-3.60,  8.96]
#> elder's dependency [severely dependent]   |        3.88 | 3.10 | [-2.29, 10.04]
#> carer's level of education                |        1.14 | 0.93 | [-0.70,  2.99]
#> 
#> Parameter                                 | t(80) |     p
#> ---------------------------------------------------------
#> (Intercept)                               |  2.45 | 0.017
#> elder's dependency [slightly dependent]   | -0.27 | 0.789
#> elder's dependency [moderately dependent] |  0.85 | 0.398
#> elder's dependency [severely dependent]   |  1.25 | 0.214
#> carer's level of education                |  1.23 | 0.221
```

### More compact output

Using `summary`, or the `select` argument via the
[`print()`](https://rdrr.io/r/base/print.html) method allows for a more
compact table, in case not all information is required.
[`summary()`](https://rdrr.io/r/base/summary.html) will return the
coefficient, confidence intervals and p-values. `select` allows to
select specific columns only.

``` r

data(iris)
model <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)

result <- model_parameters(model)

# Coefficients, CI and p
summary(result)
#> Parameter                           | Coefficient |         95% CI |      p
#> ---------------------------------------------------------------------------
#> (Intercept)                         |        4.21 | [ 3.41,  5.02] | < .001
#> Species [versicolor]                |       -1.81 | [-2.99, -0.62] | 0.003 
#> Species [virginica]                 |       -3.15 | [-4.41, -1.90] | < .001
#> Petal Length                        |        0.54 | [ 0.00,  1.09] | 0.052 
#> Species [versicolor] × Petal Length |        0.29 | [-0.30,  0.87] | 0.334 
#> Species [virginica] × Petal Length  |        0.45 | [-0.12,  1.03] | 0.120 
#> 
#> Model: Sepal.Length ~ Species * Petal.Length (150 Observations)
#> Sigma: 0.336 (df = 144)

# Parameter name, SE and p
print(result, select = c("Parameter", "SE", "p"))
#> Parameter                           |   SE |      p
#> ---------------------------------------------------
#> (Intercept)                         | 0.41 | < .001
#> Species [versicolor]                | 0.60 | 0.003 
#> Species [virginica]                 | 0.63 | < .001
#> Petal Length                        | 0.28 | 0.052 
#> Species [versicolor] × Petal Length | 0.30 | 0.334 
#> Species [virginica] × Petal Length  | 0.29 | 0.120
```

The `select` argument can also be used for a more customized output. See
examples below for
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md).

### Splitting model components

Again by default, the argument `split_components` is `TRUE`, which means
that models with multiple components like fixed and random effects,
count and zero-inflated part etc. are split into separate tables in the
output.

``` r

library(glmmTMB)
data("Salamanders")
model <- glmmTMB(count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined,
  family = nbinom2(),
  data = Salamanders
)
model_parameters(model)
#> # Fixed Effects (Count Model)
#> 
#> Parameter   | Log-Mean |   SE |        95% CI |     z |      p
#> --------------------------------------------------------------
#> (Intercept) |    -0.61 | 0.41 | [-1.40, 0.18] | -1.51 | 0.132 
#> spp [PR]    |    -0.96 | 0.64 | [-2.23, 0.30] | -1.50 | 0.134 
#> spp [DM]    |     0.17 | 0.24 | [-0.29, 0.63] |  0.73 | 0.468 
#> spp [EC-A]  |    -0.39 | 0.34 | [-1.06, 0.28] | -1.13 | 0.258 
#> spp [EC-L]  |     0.49 | 0.24 | [ 0.02, 0.96] |  2.05 | 0.041 
#> spp [DES-L] |     0.59 | 0.23 | [ 0.14, 1.04] |  2.59 | 0.010 
#> spp [DF]    |    -0.11 | 0.24 | [-0.59, 0.36] | -0.46 | 0.642 
#> mined [no]  |     1.43 | 0.37 | [ 0.71, 2.15] |  3.90 | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter   | Log-Odds |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |     0.91 | 0.63 | [-0.32,  2.14] |  1.45 | 0.147 
#> spp [PR]    |     1.16 | 1.33 | [-1.45,  3.78] |  0.87 | 0.384 
#> spp [DM]    |    -0.94 | 0.80 | [-2.51,  0.63] | -1.17 | 0.241 
#> spp [EC-A]  |     1.04 | 0.71 | [-0.36,  2.44] |  1.46 | 0.144 
#> spp [EC-L]  |    -0.56 | 0.73 | [-1.99,  0.86] | -0.77 | 0.439 
#> spp [DES-L] |    -0.89 | 0.75 | [-2.37,  0.58] | -1.19 | 0.236 
#> spp [DF]    |    -2.54 | 2.18 | [-6.82,  1.74] | -1.16 | 0.244 
#> mined [no]  |    -2.56 | 0.60 | [-3.75, -1.38] | -4.24 | < .001
#> 
#> # Dispersion
#> 
#> Parameter   | Coefficient |       95% CI
#> ----------------------------------------
#> (Intercept) |        1.51 | [0.93, 2.46]
#> 
#> # Random Effects Variances
#> 
#> Parameter            | Coefficient |       95% CI
#> -------------------------------------------------
#> SD (Intercept: site) |        0.38 | [0.17, 0.87]
```

Redundant columns are removed. The related model component is shown as
table header. However, you can also return a single table:

``` r

mp <- model_parameters(model)
# We use `table_width` here to print a wider table,
# which is not split into multiple tables
print(mp, split_component = FALSE, table_width = Inf)
#> # Fixed Effects
#> 
#> Parameter            | Coefficient |   SE |         95% CI |     z |      p | Effects |     Component
#> -----------------------------------------------------------------------------------------------------
#> (Intercept)          |       -0.61 | 0.41 | [-1.40,  0.18] | -1.51 | 0.132  |   fixed |   conditional
#> spp [PR]             |       -0.96 | 0.64 | [-2.23,  0.30] | -1.50 | 0.134  |   fixed |   conditional
#> spp [DM]             |        0.17 | 0.24 | [-0.29,  0.63] |  0.73 | 0.468  |   fixed |   conditional
#> spp [EC-A]           |       -0.39 | 0.34 | [-1.06,  0.28] | -1.13 | 0.258  |   fixed |   conditional
#> spp [EC-L]           |        0.49 | 0.24 | [ 0.02,  0.96] |  2.05 | 0.041  |   fixed |   conditional
#> spp [DES-L]          |        0.59 | 0.23 | [ 0.14,  1.04] |  2.59 | 0.010  |   fixed |   conditional
#> spp [DF]             |       -0.11 | 0.24 | [-0.59,  0.36] | -0.46 | 0.642  |   fixed |   conditional
#> mined [no]           |        1.43 | 0.37 | [ 0.71,  2.15] |  3.90 | < .001 |   fixed |   conditional
#> (Intercept)          |        0.91 | 0.63 | [-0.32,  2.14] |  1.45 | 0.147  |   fixed | zero_inflated
#> sppPR                |        1.16 | 1.33 | [-1.45,  3.78] |  0.87 | 0.384  |   fixed | zero_inflated
#> sppDM                |       -0.94 | 0.80 | [-2.51,  0.63] | -1.17 | 0.241  |   fixed | zero_inflated
#> sppEC-A              |        1.04 | 0.71 | [-0.36,  2.44] |  1.46 | 0.144  |   fixed | zero_inflated
#> sppEC-L              |       -0.56 | 0.73 | [-1.99,  0.86] | -0.77 | 0.439  |   fixed | zero_inflated
#> sppDES-L             |       -0.89 | 0.75 | [-2.37,  0.58] | -1.19 | 0.236  |   fixed | zero_inflated
#> sppDF                |       -2.54 | 2.18 | [-6.82,  1.74] | -1.16 | 0.244  |   fixed | zero_inflated
#> minedno              |       -2.56 | 0.60 | [-3.75, -1.38] | -4.24 | < .001 |   fixed | zero_inflated
#> (Intercept)          |        1.51 |      | [ 0.93,  2.46] |       |        |   fixed |    dispersion
#> SD (Intercept: site) |        0.38 |      | [ 0.17,  0.87] |       |        |  random |   conditional
```

### Adding model information

A model summary can be added to the table when `include_info = TRUE` in
the call to
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md):

``` r

model <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
model_parameters(model, include_info = TRUE)
#> Parameter                           | Coefficient |   SE |         95% CI
#> -------------------------------------------------------------------------
#> (Intercept)                         |        4.21 | 0.41 | [ 3.41,  5.02]
#> Species [versicolor]                |       -1.81 | 0.60 | [-2.99, -0.62]
#> Species [virginica]                 |       -3.15 | 0.63 | [-4.41, -1.90]
#> Petal Length                        |        0.54 | 0.28 | [ 0.00,  1.09]
#> Species [versicolor] × Petal Length |        0.29 | 0.30 | [-0.30,  0.87]
#> Species [virginica] × Petal Length  |        0.45 | 0.29 | [-0.12,  1.03]
#> 
#> Parameter                           | t(144) |      p
#> -----------------------------------------------------
#> (Intercept)                         |  10.34 | < .001
#> Species [versicolor]                |  -3.02 | 0.003 
#> Species [virginica]                 |  -4.97 | < .001
#> Petal Length                        |   1.96 | 0.052 
#> Species [versicolor] × Petal Length |   0.97 | 0.334 
#> Species [virginica] × Petal Length  |   1.56 | 0.120 
#> 
#> Model: Sepal.Length ~ Species * Petal.Length (150 Observations)
#> Sigma: 0.336 (df = 144)
#> RMSE : 0.330
#> R2: 0.840; adjusted R2: 0.835
```

### Including the reference level of categorical variables

Sometimes, it can be helpful to include the reference level of
categorical predictors in the table. This can be done by setting
`include_reference = TRUE` (either directly in
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
or in the [`print()`](https://rdrr.io/r/base/print.html) method). Since
the reference level is not a parameter, it is shown in a separate row,
with `0` for the coefficient and blank cells for the remaining columns.

``` r

model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
model_parameters(model, include_reference = TRUE)
#> Parameter            | Coefficient |   SE |         95% CI | t(146) |      p
#> ----------------------------------------------------------------------------
#> (Intercept)          |        3.68 | 0.11 | [ 3.47,  3.89] |  34.72 | < .001
#> Petal Length         |        0.90 | 0.06 | [ 0.78,  1.03] |  13.96 | < .001
#> Species [setosa]     |        0.00 |      |                |        |       
#> Species [versicolor] |       -1.60 | 0.19 | [-1.98, -1.22] |  -8.28 | < .001
#> Species [virginica]  |       -2.12 | 0.27 | [-2.66, -1.58] |  -7.74 | < .001
```

### Changing number of digits

`digits` changes the digits for coefficients, standard errors and
statistics. `ci_digits` and `p_digits` are especially for the confidence
intervals and p-values.

``` r

model <- lm(Sepal.Length ~ Species, data = iris)
model_parameters(model, digits = 4)
#> Parameter            | Coefficient |     SE |           95% CI |  t(147) |      p
#> ---------------------------------------------------------------------------------
#> (Intercept)          |      5.0060 | 0.0728 | [4.8621, 5.1499] | 68.7616 | < .001
#> Species [versicolor] |      0.9300 | 0.1030 | [0.7265, 1.1335] |  9.0328 | < .001
#> Species [virginica]  |      1.5820 | 0.1030 | [1.3785, 1.7855] | 15.3655 | < .001
```

p-values can be displayed in exact, scientific notation if required.

``` r

model_parameters(model, p_digits = "scientific")
#> Parameter            | Coefficient |   SE |       95% CI | t(147) |            p
#> --------------------------------------------------------------------------------
#> (Intercept)          |        5.01 | 0.07 | [4.86, 5.15] |  68.76 | 1.13429e-113
#> Species [versicolor] |        0.93 | 0.10 | [0.73, 1.13] |   9.03 | 8.77019e-16 
#> Species [virginica]  |        1.58 | 0.10 | [1.38, 1.79] |  15.37 | 2.21482e-32
```

### Fixing column widths

By default, the width of table columns is set to the minimum required
width. This works well for models that produce just one table. However,
for models with multiple components, where each component is shown as
separate table, columns are possibly no longer aligned across tables.
See the following example from a zero-inflated mixed model that has
three components (fixed count, fixed zero-inflated, random effects):

``` r

data("Salamanders")
# we create very long parameter names for this predictor here
levels(Salamanders$spp) <- paste("long", levels(Salamanders$spp))

model <- glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)

# default printing
model_parameters(model)
#> # Fixed Effects (Count Model)
#> 
#> Parameter        | Log-Mean |   SE |         95% CI |     z |      p
#> --------------------------------------------------------------------
#> (Intercept)      |    -0.36 | 0.28 | [-0.90,  0.18] | -1.30 | 0.194 
#> spp [long PR]    |    -1.27 | 0.24 | [-1.74, -0.80] | -5.27 | < .001
#> spp [long DM]    |     0.27 | 0.14 | [ 0.00,  0.54] |  1.95 | 0.051 
#> spp [long EC-A]  |    -0.57 | 0.21 | [-0.97, -0.16] | -2.75 | 0.006 
#> spp [long EC-L]  |     0.67 | 0.13 | [ 0.41,  0.92] |  5.20 | < .001
#> spp [long DES-L] |     0.63 | 0.13 | [ 0.38,  0.87] |  4.96 | < .001
#> spp [long DF]    |     0.12 | 0.15 | [-0.17,  0.40] |  0.78 | 0.435 
#> mined [no]       |     1.27 | 0.27 | [ 0.74,  1.80] |  4.72 | < .001
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
```

The `column_width` argument can be used to either define the width of
specific columns, or to fix column widths of the same columns across
tables to have the same width. In the latter case, use
`column_width = "fixed"` in the
[`print()`](https://rdrr.io/r/base/print.html) method.

``` r

mp <- model_parameters(model)
print(mp, column_width = "fixed")
#> # Fixed Effects (Count Model)
#> 
#> Parameter            | Log-Mean |   SE |         95% CI |     z |      p
#> ------------------------------------------------------------------------
#> (Intercept)          |    -0.36 | 0.28 | [-0.90,  0.18] | -1.30 | 0.194 
#> spp [long PR]        |    -1.27 | 0.24 | [-1.74, -0.80] | -5.27 | < .001
#> spp [long DM]        |     0.27 | 0.14 | [ 0.00,  0.54] |  1.95 | 0.051 
#> spp [long EC-A]      |    -0.57 | 0.21 | [-0.97, -0.16] | -2.75 | 0.006 
#> spp [long EC-L]      |     0.67 | 0.13 | [ 0.41,  0.92] |  5.20 | < .001
#> spp [long DES-L]     |     0.63 | 0.13 | [ 0.38,  0.87] |  4.96 | < .001
#> spp [long DF]        |     0.12 | 0.15 | [-0.17,  0.40] |  0.78 | 0.435 
#> mined [no]           |     1.27 | 0.27 | [ 0.74,  1.80] |  4.72 | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter            | Log-Odds |   SE |         95% CI |     z |      p
#> ------------------------------------------------------------------------
#> (Intercept)          |     0.79 | 0.27 | [ 0.26,  1.32] |  2.90 | 0.004 
#> mined [no]           |    -1.84 | 0.31 | [-2.46, -1.23] | -5.87 | < .001
#> 
#> # Random Effects Variances
#> 
#> Parameter            | Coefficient |         95% CI
#> ---------------------------------------------------
#> SD (Intercept: site) |        0.33 |   [0.18, 0.63]
```

If `column_width` is a named vector, names are matched against column
names, and those columns gain the specified minimum width.

``` r

print(mp, column_width = c(SE = 8, `95% CI` = 12, p = 7))
#> # Fixed Effects (Count Model)
#> 
#> Parameter        | Log-Mean |       SE |         95% CI |     z |       p
#> -------------------------------------------------------------------------
#> (Intercept)      |    -0.36 |     0.28 | [-0.90,  0.18] | -1.30 |   0.194
#> spp [long PR]    |    -1.27 |     0.24 | [-1.74, -0.80] | -5.27 |  < .001
#> spp [long DM]    |     0.27 |     0.14 | [ 0.00,  0.54] |  1.95 |   0.051
#> spp [long EC-A]  |    -0.57 |     0.21 | [-0.97, -0.16] | -2.75 |   0.006
#> spp [long EC-L]  |     0.67 |     0.13 | [ 0.41,  0.92] |  5.20 |  < .001
#> spp [long DES-L] |     0.63 |     0.13 | [ 0.38,  0.87] |  4.96 |  < .001
#> spp [long DF]    |     0.12 |     0.15 | [-0.17,  0.40] |  0.78 |   0.435
#> mined [no]       |     1.27 |     0.27 | [ 0.74,  1.80] |  4.72 |  < .001
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter   | Log-Odds |       SE |         95% CI |     z |       p
#> --------------------------------------------------------------------
#> (Intercept) |     0.79 |     0.27 | [ 0.26,  1.32] |  2.90 |   0.004
#> mined [no]  |    -1.84 |     0.31 | [-2.46, -1.23] | -5.87 |  < .001
#> 
#> # Random Effects Variances
#> 
#> Parameter            | Coefficient |       95% CI
#> -------------------------------------------------
#> SD (Intercept: site) |        0.33 | [0.18, 0.63]
```

### Group parameters

The `groups` argument can be used to group parameters in the table.
`groups` must be a named list, where the names of the list elements
equal the header of each group, while the values of the list elements
equal the parameter names, or the position of the parameters in the
table (data frame). Usually, indexing by position is easier, since the
parameter names can be modified during formatting the output.

In the following example, we see the names of the parameters in the
`Parameter` column, while the rownumbers indicate their position.

``` r

data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

# don't select "Intercept" parameter
mp <- model_parameters(model, drop = "^\\(Intercept")

# inspect data frame
as.data.frame(mp)
#>   Parameter Coefficient    SE   CI CI_low CI_high     t df_error      p
#> 1        hp      -0.062 0.021 0.95  -0.11  -0.018 -2.91       22 0.0081
#> 2     gear4       3.100 4.339 0.95  -5.90  12.098  0.71       22 0.4825
#> 3     gear5       4.798 3.478 0.95  -2.42  12.011  1.38       22 0.1816
#> 4        vs       3.183 3.790 0.95  -4.68  11.042  0.84       22 0.4100
#> 5      cyl6      -2.466 2.210 0.95  -7.05   2.116 -1.12       22 0.2764
#> 6      cyl8       1.975 5.111 0.95  -8.63  12.575  0.39       22 0.7029
#> 7      drat       2.697 2.033 0.95  -1.52   6.913  1.33       22 0.1983
#> 8  gear4:vs      -2.897 4.665 0.95 -12.57   6.778 -0.62       22 0.5410
#> 9  gear5:vs       2.588 4.537 0.95  -6.82  11.998  0.57       22 0.5741
```

Now we create a group named `"Engine"`, which encompasses the parameters
`"cyl6"`, `"cyl8"`, `"vs"` and `"hp"` (rows 5, 6, 4 and 1). The
`"Interactions"` group includes `"gear4:vs"` and `"gear5:vs"` (rows 8
and 9). The group `"controls"` has the parameters from rows 2, 3 and 7.

Note that the parameters in the table summary are re-ordered according
to the order specified in `groups`.

``` r

# group parameters, either by parameter name or position
print(mp, groups = list(
  Engine = c(5, 6, 4, 1),
  Interactions = c(8, 9),
  Controls = c(2, 3, 7)
))
#> Parameter       | Coefficient |   SE |          95% CI | t(22) |     p
#> ----------------------------------------------------------------------
#> Engine          |             |      |                 |       |      
#>   cyl [6]       |       -2.47 | 2.21 | [ -7.05,  2.12] | -1.12 | 0.276
#>   cyl [8]       |        1.97 | 5.11 | [ -8.63, 12.58] |  0.39 | 0.703
#>   vs            |        3.18 | 3.79 | [ -4.68, 11.04] |  0.84 | 0.410
#>   hp            |       -0.06 | 0.02 | [ -0.11, -0.02] | -2.91 | 0.008
#> Interactions    |             |      |                 |       |      
#>   gear [4] × vs |       -2.90 | 4.67 | [-12.57,  6.78] | -0.62 | 0.541
#>   gear [5] × vs |        2.59 | 4.54 | [ -6.82, 12.00] |  0.57 | 0.574
#> Controls        |             |      |                 |       |      
#>   gear [4]      |        3.10 | 4.34 | [ -5.90, 12.10] |  0.71 | 0.482
#>   gear [5]      |        4.80 | 3.48 | [ -2.42, 12.01] |  1.38 | 0.182
#>   drat          |        2.70 | 2.03 | [ -1.52,  6.91] |  1.33 | 0.198
```

If you prefer tables without vertical borders, use the `sep` argument to
define the string that is used as border-separator. This argument is
passed down to
[`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

``` r

# group parameters, either by parameter name or position
print(mp,
  sep = "  ",
  groups = list(
    Engine = c(5, 6, 4, 1),
    Interactions = c(8, 9),
    Controls = c(2, 3, 7)
  )
)
#> Parameter        Coefficient    SE           95% CI  t(22)      p
#> -----------------------------------------------------------------
#> Engine                                                           
#>   cyl [6]              -2.47  2.21  [ -7.05,  2.12]  -1.12  0.276
#>   cyl [8]               1.97  5.11  [ -8.63, 12.58]   0.39  0.703
#>   vs                    3.18  3.79  [ -4.68, 11.04]   0.84  0.410
#>   hp                   -0.06  0.02  [ -0.11, -0.02]  -2.91  0.008
#> Interactions                                                     
#>   gear [4] × vs        -2.90  4.67  [-12.57,  6.78]  -0.62  0.541
#>   gear [5] × vs         2.59  4.54  [ -6.82, 12.00]   0.57  0.574
#> Controls                                                         
#>   gear [4]              3.10  4.34  [ -5.90, 12.10]   0.71  0.482
#>   gear [5]              4.80  3.48  [ -2.42, 12.01]   1.38  0.182
#>   drat                  2.70  2.03  [ -1.52,  6.91]   1.33  0.198
```

## Summaries for multiple models

[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
(or its alias
[`compare_models()`](https://easystats.github.io/parameters/reference/compare_parameters.md))
allows to create tables for multiple models, aligned side by side.

By default, estimates and confidence intervals are shown.

``` r

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
compare_parameters(lm1, lm2, lm3)
#> Parameter                           |               lm1 |                  lm2 |                  lm3
#> -----------------------------------------------------------------------------------------------------
#> (Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89) |  4.21 ( 3.41,  5.02)
#> Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22) | -1.81 (-2.99, -0.62)
#> Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58) | -3.15 (-4.41, -1.90)
#> Petal Length                        |                   |  0.90 ( 0.78,  1.03) |  0.54 ( 0.00,  1.09)
#> Species [versicolor] × Petal Length |                   |                      |  0.29 (-0.30,  0.87)
#> Species [virginica] × Petal Length  |                   |                      |  0.45 (-0.12,  1.03)
#> -----------------------------------------------------------------------------------------------------
#> Observations                        |               150 |                  150 |                  150
```

### Changing style of column output

By default, estimates and confidence intervals are shown. Using `select`
allows us to create different output, e.g. standard errors instead of
confidence intervals, or including p-values.

``` r

compare_parameters(lm1, lm2, lm3, select = "se_p")
#> Parameter                           |            lm1 |             lm2 |             lm3
#> ----------------------------------------------------------------------------------------
#> (Intercept)                         | 5.01*** (0.07) |  3.68*** (0.11) |  4.21*** (0.41)
#> Species [versicolor]                | 0.93*** (0.10) | -1.60*** (0.19) | -1.81 ** (0.60)
#> Species [virginica]                 | 1.58*** (0.10) | -2.12*** (0.27) | -3.15*** (0.63)
#> Petal Length                        |                |  0.90*** (0.06) |     0.54 (0.28)
#> Species [versicolor] × Petal Length |                |                 |     0.29 (0.30)
#> Species [virginica] × Petal Length  |                |                 |     0.45 (0.29)
#> ----------------------------------------------------------------------------------------
#> Observations                        |            150 |             150 |             150
```

The `select` argument also has a basic support for glue-like syntax,
which lets you layout model elements in a very flexible way. Following
tokens are replaced by the related summary coefficients or statistics:

- `{estimate}` (or `{coefficient}` or `{coef}`): coefficient
- `{se}` (or `{std.error}` or `{standard error}`): standard error
- `{ci_low}` and `{ci_high}`: lower/upper confidence interval limits
- `{p}` (or `{pval}` or `{p.value}`): p-values
- [stars](https://r-spatial.github.io/stars/): significant stars for
  p-values

Note that you have to add parentheses manually, e.g. around confidence
intervals.

``` r

# estimates, p-stars and standard error in parentheses
compare_parameters(lm1, lm2, lm3, select = "{estimate}{stars} ({se})")
#> Parameter                           |            lm1 |             lm2 |             lm3
#> ----------------------------------------------------------------------------------------
#> (Intercept)                         | 5.01*** (0.07) |  3.68*** (0.11) |  4.21*** (0.41)
#> Species [versicolor]                | 0.93*** (0.10) | -1.60*** (0.19) | -1.81 ** (0.60)
#> Species [virginica]                 | 1.58*** (0.10) | -2.12*** (0.27) | -3.15*** (0.63)
#> Petal Length                        |                |  0.90*** (0.06) |     0.54 (0.28)
#> Species [versicolor] × Petal Length |                |                 |     0.29 (0.30)
#> Species [virginica] × Petal Length  |                |                 |     0.45 (0.29)
#> ----------------------------------------------------------------------------------------
#> Observations                        |            150 |             150 |             150

# estimates, CI, p and stars
compare_parameters(lm1, lm2, lm3, select = "{estimate} ({ci_low}, {ci_high}), p={p}{stars}")
#> Parameter                           |                           lm1
#> -------------------------------------------------------------------
#> (Intercept)                         | 5.01 (4.86, 5.15), p<0.001***
#> Species [versicolor]                | 0.93 (0.73, 1.13), p<0.001***
#> Species [virginica]                 | 1.58 (1.38, 1.79), p<0.001***
#> Petal Length                        |                              
#> Species [versicolor] × Petal Length |                              
#> Species [virginica] × Petal Length  |                              
#> -------------------------------------------------------------------
#> Observations                        |                           150
#> 
#> Parameter                           |                              lm2 |                              lm3
#> ---------------------------------------------------------------------------------------------------------
#> (Intercept)                         |  3.68 ( 3.47,  3.89), p<0.001*** |  4.21 ( 3.41,  5.02), p<0.001***
#> Species [versicolor]                | -1.60 (-1.98, -1.22), p<0.001*** | -1.81 (-2.99, -0.62), p=0.003 **
#> Species [virginica]                 | -2.12 (-2.66, -1.58), p<0.001*** | -3.15 (-4.41, -1.90), p<0.001***
#> Petal Length                        |  0.90 ( 0.78,  1.03), p<0.001*** |     0.54 ( 0.00,  1.09), p=0.052
#> Species [versicolor] × Petal Length |                                  |     0.29 (-0.30,  0.87), p=0.334
#> Species [virginica] × Petal Length  |                                  |     0.45 (-0.12,  1.03), p=0.120
#> ---------------------------------------------------------------------------------------------------------
#> Observations                        |                              150 |                              150
```

`select` also works for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
however, it’s necessary to call this argument via the
[`print()`](https://rdrr.io/r/base/print.html) method:

``` r

# estimates, p-stars and CI in parentheses
result <- model_parameters(lm3)
print(result, select = "{estimate}{stars} ({ci})")
#> Parameter                           |        Coefficient (CI)
#> -------------------------------------------------------------
#> (Intercept)                         |  4.21*** ( 3.41,  5.02)
#> Species [versicolor]                | -1.81 ** (-2.99, -0.62)
#> Species [virginica]                 | -3.15*** (-4.41, -1.90)
#> Petal Length                        |     0.54 ( 0.00,  1.09)
#> Species [versicolor] × Petal Length |     0.29 (-0.30,  0.87)
#> Species [virginica] × Petal Length  |     0.45 (-0.12,  1.03)
```

### Defining column names

The column names for the models are by default the objects’ names. You
can define own names using the `column_names` argument.

``` r

compare_parameters(
  lm1, lm2, lm3,
  column_names = c("First Model", "Second Model", "Third Model")
)
#> Parameter                           |       First Model |         Second Model |          Third Model
#> -----------------------------------------------------------------------------------------------------
#> (Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89) |  4.21 ( 3.41,  5.02)
#> Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22) | -1.81 (-2.99, -0.62)
#> Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58) | -3.15 (-4.41, -1.90)
#> Petal Length                        |                   |  0.90 ( 0.78,  1.03) |  0.54 ( 0.00,  1.09)
#> Species [versicolor] × Petal Length |                   |                      |  0.29 (-0.30,  0.87)
#> Species [virginica] × Petal Length  |                   |                      |  0.45 (-0.12,  1.03)
#> -----------------------------------------------------------------------------------------------------
#> Observations                        |               150 |                  150 |                  150
```

### Models with multiple components

For models with multiple components, like mixed models with fixed and
random effects, or models with count- and zero-inflation parts, using
arguments `effects = "all"` and/or `component = "all"` prints separate
tables for each model component.

``` r

library(glmmTMB)
data("fish")

m0 <- glm(count ~ child + camper, data = fish, family = poisson())

m1 <- glmmTMB(
  count ~ child + camper + (1 | persons) + (1 | ID),
  data = fish,
  family = poisson()
)

m2 <- glmmTMB(
  count ~ child + camper + zg + (1 | ID),
  ziformula = ~ child + (1 | persons),
  data = fish,
  family = truncated_poisson()
)

compare_parameters(m0, m1, m2, effects = "all", component = "all")
#> # Fixed Effects
#> 
#> Parameter   |                   m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------------
#> (Intercept) |  0.91 ( 0.75,  1.07) |  0.68 (-0.54,  1.91) |  1.41 ( 1.06,  1.75)
#> child       | -1.23 (-1.39, -1.08) | -1.67 (-1.84, -1.51) | -0.53 (-0.77, -0.29)
#> camper [1]  |  1.05 ( 0.88,  1.23) |  0.94 ( 0.77,  1.12) |  0.58 ( 0.39,  0.78)
#> zg          |                      |                      |  0.13 ( 0.05,  0.21)
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter   | m0 | m1 |                   m2
#> --------------------------------------------
#> (Intercept) |    |    | -0.92 (-2.07,  0.22)
#> child       |    |    |  1.96 ( 1.38,  2.54)
#> 
#> # Random Effects
#> 
#> Parameter               | m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------
#> SD (Intercept: ID)      |    |  0.27 ( 0.11,  0.63) |  0.28 ( 0.13,  0.60)
#> SD (Intercept: persons) |    |  1.21 ( 0.60,  2.43) |                     
#> 
#> # Random Effects (Zero-Inflation Component)
#> 
#> Parameter               | m0 | m1 |                   m2
#> --------------------------------------------------------
#> SD (Intercept: persons) |    |    |  1.08 ( 0.49,  2.37)
```

For such tables, they usually become clearer when the columns per model
are aligned to a fixed width. You can do this using
`column_width = "fixed"`.

``` r

cp <- compare_parameters(m0, m1, m2, effects = "all", component = "all")
print(cp, column_width = "fixed")
#> # Fixed Effects
#> 
#> Parameter               |                   m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------------------------
#> (Intercept)             |  0.91 ( 0.75,  1.07) |  0.68 (-0.54,  1.91) |  1.41 ( 1.06,  1.75)
#> child                   | -1.23 (-1.39, -1.08) | -1.67 (-1.84, -1.51) | -0.53 (-0.77, -0.29)
#> camper [1]              |  1.05 ( 0.88,  1.23) |  0.94 ( 0.77,  1.12) |  0.58 ( 0.39,  0.78)
#> zg                      |                      |                      |  0.13 ( 0.05,  0.21)
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter               |                   m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------------------------
#> (Intercept)             |                      |                      | -0.92 (-2.07,  0.22)
#> child                   |                      |                      |  1.96 ( 1.38,  2.54)
#> 
#> # Random Effects
#> 
#> Parameter               |                   m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------------------------
#> SD (Intercept: ID)      |                      |  0.27 ( 0.11,  0.63) |  0.28 ( 0.13,  0.60)
#> SD (Intercept: persons) |                      |  1.21 ( 0.60,  2.43) |                     
#> 
#> # Random Effects (Zero-Inflation Component)
#> 
#> Parameter               |                   m0 |                   m1 |                   m2
#> --------------------------------------------------------------------------------------------
#> SD (Intercept: persons) |                      |                      |  1.08 ( 0.49,  2.37)
```

### Group parameters of multiple model tables

Grouping parameters works for
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
in the same way as shown above for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

**Note:** By default, the interaction mark is `×`, not `*` (see also
section on global options [in this
vignette](https://easystats.github.io/parameters/reference/print.parameters_model.html)).
Since parameter names in
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
are already formatted before printing, `as.data.frame(cp)$Parameter`
will probably return special unicode characters that you need to take
care of in the `groups` argument (unless you use numeric indices).

``` r

lm1 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm2 <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

# remove intercept
cp <- compare_parameters(lm1, lm2, drop = "^\\(Intercept")

# look at parameters names, to know their names for "groups" argument
as.data.frame(cp)$Parameter # note the unicode char as interaction mark
#> [1] "Species [versicolor]"                "Species [virginica]"                
#> [3] "Petal Length"                        "Species [versicolor] × Petal Length"
#> [5] "Species [virginica] × Petal Length"

# create groups. Interactions only present in 2nd model
print(cp, groups = list(
  Species = c(
    "Species [versicolor]",
    "Species [virginica]"
  ),
  Interactions = c(
    "Species [versicolor] × Petal Length", # note the unicode char!
    "Species [virginica] × Petal Length"
  ),
  Controls = "Petal Length"
))
#> Parameter                             |                  lm1 |                  lm2
#> -----------------------------------------------------------------------------------
#> Species                               |                      |                     
#>   Species [versicolor]                | -1.60 (-1.98, -1.22) | -1.69 (-2.80, -0.57)
#>   Species [virginica]                 | -2.12 (-2.66, -1.58) | -1.19 (-2.37, -0.01)
#> Interactions                          |                      |                     
#>   Species [versicolor] × Petal Length |                      | -0.01 (-0.56,  0.53)
#>   Species [virginica] × Petal Length  |                      | -0.15 (-0.69,  0.39)
#> Controls                              |                      |                     
#>   Petal Length                        |  0.90 ( 0.78,  1.03) |  0.39 (-0.13,  0.90)
#> -----------------------------------------------------------------------------------
#> Observations                          |                  150 |                  150
```

## Splitting wide tables into multiple table parts

For very wide tables that cannot be displayed properly, you can use the
`table_width` argument in the
[`print()`](https://rdrr.io/r/base/print.html) method to split tables
into multiple parts. `table_width` can be a numeric value, or `"auto"`,
indicating the width of the complete table. If `table_width = "auto"`
and the table is wider than the current available width (i.e. line
length) of the console (or any other source for textual output, like
markdown files), the table is split into multiple parts. Else, if
`table_width` is numeric and table rows are wider than `table_width`,
the table is split into multiple parts.

``` r

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
lm4 <- lm(Sepal.Length ~ Species * Petal.Length + Petal.Width, data = iris)

# very wide table
compare_parameters(lm1, lm2, lm3, lm4)
#> Parameter                           |               lm1 |                  lm2
#> ------------------------------------------------------------------------------
#> (Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89)
#> Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22)
#> Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58)
#> Petal Length                        |                   |  0.90 ( 0.78,  1.03)
#> Species [versicolor] × Petal Length |                   |                     
#> Species [virginica] × Petal Length  |                   |                     
#> Petal Width                         |                   |                     
#> ------------------------------------------------------------------------------
#> Observations                        |               150 |                  150
#> 
#> Parameter                           |                  lm3 |                  lm4
#> ---------------------------------------------------------------------------------
#> (Intercept)                         |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02)
#> Species [versicolor]                | -1.81 (-2.99, -0.62) | -1.80 (-2.99, -0.62)
#> Species [virginica]                 | -3.15 (-4.41, -1.90) | -3.19 (-4.50, -1.88)
#> Petal Length                        |  0.54 ( 0.00,  1.09) |  0.54 (-0.02,  1.09)
#> Species [versicolor] × Petal Length |  0.29 (-0.30,  0.87) |  0.28 (-0.30,  0.87)
#> Species [virginica] × Petal Length  |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03)
#> Petal Width                         |                      |  0.03 (-0.28,  0.34)
#> ---------------------------------------------------------------------------------
#> Observations                        |                  150 |                  150

# table split into two parts
tab <- compare_parameters(lm1, lm2, lm3, lm4)
print(tab, table_width = 80)
#> Parameter                           |               lm1 |                  lm2
#> ------------------------------------------------------------------------------
#> (Intercept)                         | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89)
#> Species [versicolor]                | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22)
#> Species [virginica]                 | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58)
#> Petal Length                        |                   |  0.90 ( 0.78,  1.03)
#> Species [versicolor] × Petal Length |                   |                     
#> Species [virginica] × Petal Length  |                   |                     
#> Petal Width                         |                   |                     
#> ------------------------------------------------------------------------------
#> Observations                        |               150 |                  150
#> 
#> Parameter                           |                  lm3 |                  lm4
#> ---------------------------------------------------------------------------------
#> (Intercept)                         |  4.21 ( 3.41,  5.02) |  4.21 ( 3.41,  5.02)
#> Species [versicolor]                | -1.81 (-2.99, -0.62) | -1.80 (-2.99, -0.62)
#> Species [virginica]                 | -3.15 (-4.41, -1.90) | -3.19 (-4.50, -1.88)
#> Petal Length                        |  0.54 ( 0.00,  1.09) |  0.54 (-0.02,  1.09)
#> Species [versicolor] × Petal Length |  0.29 (-0.30,  0.87) |  0.28 (-0.30,  0.87)
#> Species [virginica] × Petal Length  |  0.45 (-0.12,  1.03) |  0.45 (-0.12,  1.03)
#> Petal Width                         |                      |  0.03 (-0.28,  0.34)
#> ---------------------------------------------------------------------------------
#> Observations                        |                  150 |                  150
```

## More advances tables and markdown / HTML formatting

The
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
as well as
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
functions can be used to create markdown (for knitting to PDF or Word)
and HTML tables, where HTML tables are created using the
[**gt**](https://gt.rstudio.com/) package. There is also a
[`display()`](https://easystats.github.io/insight/reference/display.html)
function, which internally calls
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
or
[`print_html()`](https://easystats.github.io/insight/reference/display.html),
depending on the `format` argument. However, for
[`display()`](https://easystats.github.io/insight/reference/display.html),
the `format` argument can also be `"tt"` to use the
[**tinytable**](https://vincentarelbundock.github.io/tinytable/) package
as engine to produce tables. This will create tables in different output
formats, depending on the environment where the code is run (e.g. R
Markdown, Jupyter Notebook, etc.).

Meanwhile, there are a lot of additional packages that allow users to
have even more flexibility regarding table layouts. One package we can
recommend is the [*modelsummary*
package](https://vincentarelbundock.github.io/modelsummary/).
