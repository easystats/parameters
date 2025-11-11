# Print tables in different output formats

Prints tables (i.e. data frame) in different output formats.
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
is an alias for `display(format = "markdown")` and
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
is an alias for `display(format = "html")`. A third option is
`display(format = "tt")`, which returns a `tinytable` object, which is
either printed as markdown or HTML table, depending on the environment.

## Usage

``` r
# S3 method for class 'parameters_model'
display(object, format = "markdown", ...)
```

## Arguments

- object:

  An object returned by one of the package's function, for example
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  [`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md),
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  or
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md).

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "tt"` creates a `tinytable` object, which is either
  printed as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

- ...:

  Arguments passed to the underlying functions, such as
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  or
  [`print_html()`](https://easystats.github.io/insight/reference/display.html).

## Value

If `format = "markdown"`, the return value will be a character vector in
markdown-table format. If `format = "html"`, an object of class
`gt_tbl`. If `format = "tt"`, an object of class `tinytable`.

## Details

[`display()`](https://easystats.github.io/insight/reference/display.html)
is useful when the table-output from functions, which is usually printed
as formatted text-table to console, should be formatted for pretty
table-rendering in markdown documents, or if knitted from rmarkdown to
PDF or Word files. See
[vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
for examples.

## See also

[`print.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
and
[`print.compare_parameters()`](https://easystats.github.io/parameters/reference/print.compare_parameters.md)

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_parameters(model)
display(mp)
#> 
#> 
#> |Parameter   | Coefficient |   SE |         95% CI | t(29) |      p |
#> |:-----------|:-----------:|:----:|:--------------:|:-----:|:------:|
#> |(Intercept) |       39.69 | 1.71 | (36.18, 43.19) | 23.14 | < .001 |
#> |wt          |       -3.19 | 0.76 | (-4.74, -1.64) | -4.22 | < .001 |
#> |cyl         |       -1.51 | 0.41 | (-2.36, -0.66) | -3.64 | 0.001  |

# \donttest{
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
out <- compare_parameters(lm1, lm2, lm3)

print_html(
  out,
  select = "{coef}{stars}|({ci})",
  column_labels = c("Estimate", "95% CI")
)


  


Parameter
```
