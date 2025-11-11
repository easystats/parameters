# Conversion between EFA results and CFA structure

Enables a conversion between Exploratory Factor Analysis (EFA) and
Confirmatory Factor Analysis (CFA) `lavaan`-ready structure.

## Usage

``` r
convert_efa_to_cfa(model, ...)

# S3 method for class 'fa'
convert_efa_to_cfa(
  model,
  threshold = "max",
  names = NULL,
  max_per_dimension = NULL,
  ...
)

efa_to_cfa(model, ...)
```

## Arguments

- model:

  An EFA model (e.g., a
  [`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html) object).

- ...:

  Arguments passed to or from other methods.

- threshold:

  A value between 0 and 1 indicates which (absolute) values from the
  loadings should be removed. An integer higher than 1 indicates the n
  strongest loadings to retain. Can also be `"max"`, in which case it
  will only display the maximum loading per variable (the most simple
  structure).

- names:

  Vector containing dimension names.

- max_per_dimension:

  Maximum number of variables to keep per dimension.

## Value

Converted index.

## Examples

``` r
# \donttest{
library(parameters)
data(attitude)
efa <- psych::fa(attitude, nfactors = 3)
#> Loading required namespace: GPArotation

model1 <- efa_to_cfa(efa)
model2 <- efa_to_cfa(efa, threshold = 0.3)
model3 <- efa_to_cfa(efa, max_per_dimension = 2)

suppressWarnings(anova(
  lavaan::cfa(model1, data = attitude),
  lavaan::cfa(model2, data = attitude),
  lavaan::cfa(model3, data = attitude)
))
#> 
#> Chi-Squared Difference Test
#> 
#>                                      Df    AIC    BIC   Chisq Chisq diff  RMSEA
#> lavaan::cfa(model3, data = attitude)  3 1111.9 1128.7  3.2673                  
#> lavaan::cfa(model2, data = attitude) 10 1540.5 1565.7  9.1827     5.9155 0.0000
#> lavaan::cfa(model1, data = attitude) 12 1549.8 1572.2 22.4374    13.2547 0.4331
#>                                      Df diff Pr(>Chisq)   
#> lavaan::cfa(model3, data = attitude)                      
#> lavaan::cfa(model2, data = attitude)       7   0.549655   
#> lavaan::cfa(model1, data = attitude)       2   0.001324 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# }
```
