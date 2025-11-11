# Parameter names formatting

This functions formats the names of model parameters (coefficients) to
make them more human-readable.

## Usage

``` r
format_parameters(model, ...)

# Default S3 method
format_parameters(model, brackets = c("[", "]"), ...)
```

## Arguments

- model:

  A statistical model.

- ...:

  Currently not used.

- brackets:

  A character vector of length two, indicating the opening and closing
  brackets.

## Value

A (names) character vector with formatted parameter names. The value
names refer to the original names of the coefficients.

## Interpretation of Interaction Terms

Note that the *interpretation* of interaction terms depends on many
characteristics of the model. The number of parameters, and overall
performance of the model, can differ *or not* between `a * b`, `a : b`,
and `a / b`, suggesting that sometimes interaction terms give different
parameterizations of the same model, but other times it gives completely
different models (depending on `a` or `b` being factors of covariates,
included as main effects or not, etc.). Their interpretation depends of
the full context of the model, which should not be inferred from the
parameters table alone - rather, we recommend to use packages that
calculate estimated marginal means or marginal effects, such as
[modelbased](https://CRAN.R-project.org/package=modelbased),
[emmeans](https://CRAN.R-project.org/package=emmeans),
[ggeffects](https://CRAN.R-project.org/package=ggeffects), or
[marginaleffects](https://CRAN.R-project.org/package=marginaleffects).
To raise awareness for this issue, you may use
`print(...,show_formula=TRUE)` to add the model-specification to the
output of the
[`print()`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
method for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

## Examples

``` r
model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
format_parameters(model)
#>                          (Intercept)                    Speciesversicolor 
#>                        "(Intercept)"               "Species [versicolor]" 
#>                     Speciesvirginica                          Sepal.Width 
#>                "Species [virginica]"                        "Sepal Width" 
#>        Speciesversicolor:Sepal.Width         Speciesvirginica:Sepal.Width 
#> "Species [versicolor] × Sepal Width"  "Species [virginica] × Sepal Width" 

model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
format_parameters(model)
#>                          (Intercept)                         Petal.Length 
#>                        "(Intercept)"                       "Petal Length" 
#>                    Speciesversicolor                     Speciesvirginica 
#>               "Species [versicolor]"                "Species [virginica]" 
#>            Speciessetosa:Sepal.Width        Speciesversicolor:Sepal.Width 
#>     "Species [setosa] × Sepal Width" "Species [versicolor] × Sepal Width" 
#>         Speciesvirginica:Sepal.Width 
#>  "Species [virginica] × Sepal Width" 

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
format_parameters(model)
#>                (Intercept)          Speciesversicolor 
#>              "(Intercept)"     "Species [versicolor]" 
#>           Speciesvirginica      poly(Sepal.Width, 2)1 
#>      "Species [virginica]" "Sepal Width [1st degree]" 
#>      poly(Sepal.Width, 2)2 
#> "Sepal Width [2nd degree]" 

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
format_parameters(model)
#>                       (Intercept)                 Speciesversicolor 
#>                     "(Intercept)"            "Species [versicolor]" 
#>                  Speciesvirginica poly(Sepal.Width, 2, raw = TRUE)1 
#>             "Species [virginica]"        "Sepal Width [1st degree]" 
#> poly(Sepal.Width, 2, raw = TRUE)2 
#>        "Sepal Width [2nd degree]" 
```
