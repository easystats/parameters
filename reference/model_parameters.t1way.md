# Parameters from robust statistical objects in `WRS2`

Parameters from robust statistical objects in `WRS2`

## Usage

``` r
# S3 method for class 't1way'
model_parameters(model, keep = NULL, verbose = TRUE, ...)
```

## Arguments

- model:

  Object from `WRS2` package.

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

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame of indices related to the model's parameters.

## Examples

``` r
if (require("WRS2") && packageVersion("WRS2") >= "1.1.3") {
  model <- t1way(libido ~ dose, data = viagra)
  model_parameters(model)
}
#> Loading required package: WRS2
#> A heteroscedastic one-way ANOVA for trimmed means
#> 
#> F | df | df (error) |     p | Estimate |       95% CI |                         Effectsize
#> ------------------------------------------------------------------------------------------
#> 3 |  2 |          4 | 0.160 |     0.79 | [0.28, 1.54] | Explanatory measure of effect size
```
