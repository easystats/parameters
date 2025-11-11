# Formatting Model Parameters

The *parameters* package, together with the [*insight*
package](https://easystats.github.io/insight/), provides tools to format
the layout and style of tables from model parameters. When you use the
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
function, you usually don’t have to take care about formatting and
layout, at least not for simple purposes like printing to the console or
inside rmarkdown documents. However, sometime you may want to do the
formatting steps manually. This vignette introduces the various
functions that are used for parameters table formatting.

## An Example Model

We start with a model that does not make much sense, but it is useful
for demonstrating the formatting functions.

``` r

data(iris)
iris$Petlen <- cut(iris$Petal.Length, breaks = c(0, 3, 7))
model <- lm(Sepal.Width ~ poly(Sepal.Length, 2) + Species + Petlen, data = iris)

summary(model)
#> 
#> Call:
#> lm(formula = Sepal.Width ~ poly(Sepal.Length, 2) + Species + 
#>     Petlen, data = iris)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.7742 -0.1490 -0.0056  0.1666  0.6973 
#> 
#> Coefficients:
#>                        Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)              3.8127     0.0582   65.50  < 2e-16 ***
#> poly(Sepal.Length, 2)1   4.0602     0.4668    8.70    7e-15 ***
#> poly(Sepal.Length, 2)2  -1.3024     0.3149   -4.14    6e-05 ***
#> Speciesversicolor       -1.0056     0.2781   -3.62  0.00041 ***
#> Speciesvirginica        -0.9913     0.2851   -3.48  0.00067 ***
#> Petlen(3,7]             -0.1360     0.2818   -0.48  0.63019    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.28 on 144 degrees of freedom
#> Multiple R-squared:  0.615,  Adjusted R-squared:  0.602 
#> F-statistic:   46 on 5 and 144 DF,  p-value: <2e-16
```

## Formatting Parameter Names

As we can see, in such cases, the standard R output looks a bit cryptic,
although all necessary and important information is included in the
summary. The formatting of coefficients for polynomial transformation is
difficult to read, factors grouped with
[`cut()`](https://rdrr.io/r/base/cut.html) always require a short time
of thinking to find out which of the bound (in this case, `Petlen(3,7]`,
3 and 7) is included in the range, and names of factor levels are
directly concatenated to the name of the factor variable.

Thus, the first step would be to format the parameter names, which can
be done with
[`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.md)
from the *parameters* package:

``` r

library(parameters)
format_parameters(model)
#>                 (Intercept)      poly(Sepal.Length, 2)1 
#>               "(Intercept)" "Sepal Length [1st degree]" 
#>      poly(Sepal.Length, 2)2           Speciesversicolor 
#> "Sepal Length [2nd degree]"      "Species [versicolor]" 
#>            Speciesvirginica                 Petlen(3,7] 
#>       "Species [virginica]"             "Petlen [>3-7]"
```

[`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.md)
returns a (named) character vector with the original coefficients as
*names* of each character element, and the formatted names of the
coefficients as values of the character vector. Let’s look at the
results again:

``` r

cat(format_parameters(model), sep = "\n")
#> (Intercept)
#> Sepal Length [1st degree]
#> Sepal Length [2nd degree]
#> Species [versicolor]
#> Species [virginica]
#> Petlen [>3-7]
```

Now variable names and factor levels, but also polynomial terms or even
factors grouped with [`cut()`](https://rdrr.io/r/base/cut.html) are much
more readable. Factor levels are separated from the variable name,
inside brackets. Same for the coefficients of the different polynomial
degrees. And the exact range for
[`cut()`](https://rdrr.io/r/base/cut.html)-factors is also clearer now.

## Standardizing Column Names of Parameter Tables

As seen above, the [`summary()`](https://rdrr.io/r/base/summary.html)
returns columns named `Estimate`, `t value` or `Pr(>|t|)`. While
`Estimate` is not specific for certain models, `t value` is. For
logistic regression models, you would get `z value`. Some packages alter
the names, so you get just `t` or `t-value` etc.

[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
also uses context-specific column names, where applicable:

``` r

colnames(model_parameters(model))
#> [1] "Parameter"   "Coefficient" "SE"          "CI"          "CI_low"     
#> [6] "CI_high"     "t"           "df_error"    "p"
```

For Bayesian models, `Coefficient` is usually named `Median` etc. While
this makes sense from a user perspective, because you instantly know
which type of statistic or coefficient you have, it becomes difficult
when you need a generic naming scheme to access model parameters when
the input model is unknown. This is the typical approach from the
*broom* package, where you get “standardized” column names:

``` r

library(broom)
colnames(tidy(model))
#> [1] "term"      "estimate"  "std.error" "statistic" "p.value"
```

To deal with such situations, the *insight* package provides a
[`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
function, which exactly does that: standardizing the column names of the
input. In the following example, you see that the statistic-column is no
longer named `t`, but `statistic`. `df_error` or `df_residuals` will be
renamed to `df`.

``` r

library(insight)
model |>
  model_parameters() |>
  standardize_names() |>
  colnames()
#> [1] "Parameter"   "Coefficient" "SE"          "CI"          "CI_low"     
#> [6] "CI_high"     "Statistic"   "df"          "p"
```

Furthermore, you can request “broom”-style for column names:

``` r

model |>
  model_parameters() |>
  standardize_names(style = "broom") |>
  colnames()
#> [1] "term"       "estimate"   "std.error"  "conf.level" "conf.low"  
#> [6] "conf.high"  "statistic"  "df.error"   "p.value"
```

## Formatting Column Names and Columns

Beside formatting parameter names (coefficient names) using
[`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.md),
we can do even more to make the output more readable. Let’s look at an
example that includes confidence intervals.

``` r

cbind(summary(model)$coefficients, confint(model))
#>                        Estimate Std. Error t value Pr(>|t|) 2.5 % 97.5 %
#> (Intercept)                3.81      0.058   65.50 4.6e-109  3.70   3.93
#> poly(Sepal.Length, 2)1     4.06      0.467    8.70  7.0e-15  3.14   4.98
#> poly(Sepal.Length, 2)2    -1.30      0.315   -4.14  6.0e-05 -1.92  -0.68
#> Speciesversicolor         -1.01      0.278   -3.62  4.1e-04 -1.56  -0.46
#> Speciesvirginica          -0.99      0.285   -3.48  6.7e-04 -1.55  -0.43
#> Petlen(3,7]               -0.14      0.282   -0.48  6.3e-01 -0.69   0.42
```

We can get a similar tabular output using *broom*.

``` r

tidy(model, conf.int = TRUE)
#> # A tibble: 6 × 7
#>   term                 estimate std.error statistic   p.value conf.low conf.high
#>   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 (Intercept)             3.81     0.0582    65.5   4.61e-109    3.70      3.93 
#> 2 poly(Sepal.Length, …    4.06     0.467      8.70  7.00e- 15    3.14      4.98 
#> 3 poly(Sepal.Length, …   -1.30     0.315     -4.14  5.98e-  5   -1.92     -0.680
#> 4 Speciesversicolor      -1.01     0.278     -3.62  4.12e-  4   -1.56     -0.456
#> 5 Speciesvirginica       -0.991    0.285     -3.48  6.72e-  4   -1.55     -0.428
#> 6 Petlen(3,7]            -0.136    0.282     -0.482 6.30e-  1   -0.693     0.421
```

Some improvements according to readability could be collapsing and
formatting the confidence intervals, and maybe the p-values. This would
require some effort, for instance, to format the values of the lower and
upper confidence intervals and collapsing them into one column. However,
the
[`format_table()`](https://easystats.github.io/insight/reference/format_table.html)
function is a convenient function that does all the work for you.

[`format_table()`](https://easystats.github.io/insight/reference/format_table.html)
requires a data frame with model parameters as input, however, there are
some requirements to make
[`format_table()`](https://easystats.github.io/insight/reference/format_table.html)
work. In particular, the column names must follow a certain pattern to
be recognized, and this pattern may either be the naming convention from
*broom* or the [*easystats*
packages](https://easystats.github.io/easystats/).

``` r

model |>
  tidy(conf.int = TRUE) |>
  format_table()
#>                     term estimate std.error statistic p.value       conf.int
#> 1            (Intercept)     3.81      0.06     65.50  < .001 [ 3.70,  3.93]
#> 2 poly(Sepal.Length, 2)1     4.06      0.47      8.70  < .001 [ 3.14,  4.98]
#> 3 poly(Sepal.Length, 2)2    -1.30      0.31     -4.14  < .001 [-1.92, -0.68]
#> 4      Speciesversicolor    -1.01      0.28     -3.62  < .001 [-1.56, -0.46]
#> 5       Speciesvirginica    -0.99      0.29     -3.48  < .001 [-1.55, -0.43]
#> 6            Petlen(3,7]    -0.14      0.28     -0.48  0.630  [-0.69,  0.42]
```

When the parameters table also includes degrees of freedom, and the
degrees of freedom are the same for each parameter, then this
information is included in the statistic-column. This is usually the
default for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md):

``` r

model |>
  model_parameters() |>
  format_table()
#>                   Parameter Coefficient   SE         95% CI t(144)      p
#> 1               (Intercept)        3.81 0.06 [ 3.70,  3.93]  65.50 < .001
#> 2 Sepal Length [1st degree]        4.06 0.47 [ 3.14,  4.98]   8.70 < .001
#> 3 Sepal Length [2nd degree]       -1.30 0.31 [-1.92, -0.68]  -4.14 < .001
#> 4      Species [versicolor]       -1.01 0.28 [-1.56, -0.46]  -3.62 < .001
#> 5       Species [virginica]       -0.99 0.29 [-1.55, -0.43]  -3.48 < .001
#> 6             Petlen [>3-7]       -0.14 0.28 [-0.69,  0.42]  -0.48 0.630
```

## Exporting the Parameters Table

Finally,
[`export_table()`](https://easystats.github.io/insight/reference/export_table.html)
from *insight* formats the data frame and returns a character vector
that can be printed to the console or inside rmarkdown documents. The
data frame then looks more “table-like”.

``` r

data(mtcars)
export_table(mtcars[1:8, 1:5])
#>   mpg | cyl |   disp |  hp | drat
#> ---------------------------------
#> 21.00 |   6 | 160.00 | 110 | 3.90
#> 21.00 |   6 | 160.00 | 110 | 3.90
#> 22.80 |   4 | 108.00 |  93 | 3.85
#> 21.40 |   6 | 258.00 | 110 | 3.08
#> 18.70 |   8 | 360.00 | 175 | 3.15
#> 18.10 |   6 | 225.00 | 105 | 2.76
#> 14.30 |   8 | 360.00 | 245 | 3.21
#> 24.40 |   4 | 146.70 |  62 | 3.69
```

Putting all this together allows us to create nice tabular outputs of
parameters tables. This can be done using *broom*:

``` r

model |>
  tidy(conf.int = TRUE) |>
  format_table() |>
  export_table()
#> term                   | estimate | std.error | statistic | p.value |       conf.int
#> ------------------------------------------------------------------------------------
#> (Intercept)            |     3.81 |      0.06 |     65.50 |  < .001 | [ 3.70,  3.93]
#> poly(Sepal.Length, 2)1 |     4.06 |      0.47 |      8.70 |  < .001 | [ 3.14,  4.98]
#> poly(Sepal.Length, 2)2 |    -1.30 |      0.31 |     -4.14 |  < .001 | [-1.92, -0.68]
#> Speciesversicolor      |    -1.01 |      0.28 |     -3.62 |  < .001 | [-1.56, -0.46]
#> Speciesvirginica       |    -0.99 |      0.29 |     -3.48 |  < .001 | [-1.55, -0.43]
#> Petlen(3,7]            |    -0.14 |      0.28 |     -0.48 |  0.630  | [-0.69,  0.42]
```

Or, in a simpler way and with much more options (like standardizing,
robust standard errors, bootstrapping, …) using
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
which [`print()`](https://rdrr.io/r/base/print.html)-method does all
these steps automatically:

``` r

model_parameters(model)
#> Parameter                 | Coefficient |   SE |         95% CI | t(144) |      p
#> ---------------------------------------------------------------------------------
#> (Intercept)               |        3.81 | 0.06 | [ 3.70,  3.93] |  65.50 | < .001
#> Sepal Length [1st degree] |        4.06 | 0.47 | [ 3.14,  4.98] |   8.70 | < .001
#> Sepal Length [2nd degree] |       -1.30 | 0.31 | [-1.92, -0.68] |  -4.14 | < .001
#> Species [versicolor]      |       -1.01 | 0.28 | [-1.56, -0.46] |  -3.62 | < .001
#> Species [virginica]       |       -0.99 | 0.29 | [-1.55, -0.43] |  -3.48 | < .001
#> Petlen [>3-7]             |       -0.14 | 0.28 | [-0.69,  0.42] |  -0.48 | 0.630
```

## Formatting the Parameters Table in Markdown

[`export_table()`](https://easystats.github.io/insight/reference/export_table.html)
provides a few options to generate tables in markdown-format. This
allows to easily render nice-looking tables inside markdown-documents.
First of all, use `format = "markdown"` to activate the
markdown-formatting. `caption` can be used to add a table caption.
Furthermore, `align` allows to choose an alignment for all table
columns, or to specify the alignment for each column individually.

The following table has six columns. Using `align = "lcccrr"` would
left-align the first column, center columns two to four, and right-align
the last two columns.

``` r

model |>
  tidy(conf.int = TRUE) |>
  # parenthesis look better in markdown-tables, so we use "brackets" here
  format_table(ci_brackets = c("(", ")")) |>
  export_table(format = "markdown", caption = "My Table", align = "lcccrr")
```

| term                   | estimate | std.error | statistic | p.value |       conf.int |
|:-----------------------|:--------:|:---------:|:---------:|--------:|---------------:|
| (Intercept)            |   3.81   |   0.06    |   65.50   | \< .001 |  ( 3.70, 3.93) |
| poly(Sepal.Length, 2)1 |   4.06   |   0.47    |   8.70    | \< .001 |  ( 3.14, 4.98) |
| poly(Sepal.Length, 2)2 |  -1.30   |   0.31    |   -4.14   | \< .001 | (-1.92, -0.68) |
| Speciesversicolor      |  -1.01   |   0.28    |   -3.62   | \< .001 | (-1.56, -0.46) |
| Speciesvirginica       |  -0.99   |   0.29    |   -3.48   | \< .001 | (-1.55, -0.43) |
| Petlen(3,7\]           |  -0.14   |   0.28    |   -0.48   |   0.630 |  (-0.69, 0.42) |

My Table {.table style="width:100%;"}

[`print_md()`](https://easystats.github.io/insight/reference/display.html)
is a convenient wrapper around
[`format_table()`](https://easystats.github.io/insight/reference/format_table.html)
and `export_table(format = "markdown")`, and allows to directly format
the output of functions like
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
[`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md)
or other *parameters* functions in markdown-format.

These tables are also nicely formatted when knitting markdown-documents
into Word or PDF.
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
applies some default settings that have proven to work well for
markdown, PDF or Word tables.

``` r

model_parameters(model) |> print_md()
```

| Parameter                 | Coefficient |  SE  |     95% CI     | t(144) |    p    |
|:--------------------------|:-----------:|:----:|:--------------:|:------:|:-------:|
| (Intercept)               |    3.81     | 0.06 |  (3.70, 3.93)  | 65.50  | \< .001 |
| Sepal Length (1st degree) |    4.06     | 0.47 |  (3.14, 4.98)  |  8.70  | \< .001 |
| Sepal Length (2nd degree) |    -1.30    | 0.31 | (-1.92, -0.68) | -4.14  | \< .001 |
| Species (versicolor)      |    -1.01    | 0.28 | (-1.56, -0.46) | -3.62  | \< .001 |
| Species (virginica)       |    -0.99    | 0.29 | (-1.55, -0.43) | -3.48  | \< .001 |
| Petlen (\>3-7)            |    -0.14    | 0.28 | (-0.69, 0.42)  | -0.48  |  0.630  |

A similar option is
[`print_html()`](https://easystats.github.io/insight/reference/display.html),
which is a convenient wrapper for
[`format_table()`](https://easystats.github.io/insight/reference/format_table.html)
and `export_table(format = "html")`. Using HTML in markdown has the
advantage that it will be properly rendered when exporting to PDF.

``` r

model_parameters(model) |> print_html()
```

| Parameter                 | Coefficient | SE   | 95% CI         | t(144) | p       |
|---------------------------|-------------|------|----------------|--------|---------|
| (Intercept)               | 3.81        | 0.06 | (3.70, 3.93)   | 65.50  | \< .001 |
| Sepal Length (1st degree) | 4.06        | 0.47 | (3.14, 4.98)   | 8.70   | \< .001 |
| Sepal Length (2nd degree) | -1.30       | 0.31 | (-1.92, -0.68) | -4.14  | \< .001 |
| Species (versicolor)      | -1.01       | 0.28 | (-1.56, -0.46) | -3.62  | \< .001 |
| Species (virginica)       | -0.99       | 0.29 | (-1.55, -0.43) | -3.48  | \< .001 |
| Petlen (\>3-7)            | -0.14       | 0.28 | (-0.69, 0.42)  | -0.48  | 0.630   |

[`print_md()`](https://easystats.github.io/insight/reference/display.html)
and
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
are considered as main functions for users who want to generate nicely
rendered tables inside markdown-documents. A wrapper around these both
is
[`display()`](https://easystats.github.io/insight/reference/display.html),
which either calls
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
or
[`print_html()`](https://easystats.github.io/insight/reference/display.html).

``` r

model_parameters(model) |> display(format = "html")
```

| Parameter                 | Coefficient | SE   | 95% CI         | t(144) | p       |
|---------------------------|-------------|------|----------------|--------|---------|
| (Intercept)               | 3.81        | 0.06 | (3.70, 3.93)   | 65.50  | \< .001 |
| Sepal Length (1st degree) | 4.06        | 0.47 | (3.14, 4.98)   | 8.70   | \< .001 |
| Sepal Length (2nd degree) | -1.30       | 0.31 | (-1.92, -0.68) | -4.14  | \< .001 |
| Species (versicolor)      | -1.01       | 0.28 | (-1.56, -0.46) | -3.62  | \< .001 |
| Species (virginica)       | -0.99       | 0.29 | (-1.55, -0.43) | -3.48  | \< .001 |
| Petlen (\>3-7)            | -0.14       | 0.28 | (-0.69, 0.42)  | -0.48  | 0.630   |
