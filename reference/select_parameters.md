# Automated selection of model parameters

This function performs an automated selection of the 'best' parameters,
updating and returning the "best" model.

## Usage

``` r
select_parameters(model, ...)

# S3 method for class 'lm'
select_parameters(model, direction = "both", steps = 1000, k = 2, ...)

# S3 method for class 'merMod'
select_parameters(model, direction = "backward", steps = 1000, ...)
```

## Arguments

- model:

  A statistical model (of class `lm`, `glm`, or `merMod`).

- ...:

  Arguments passed to or from other methods.

- direction:

  the mode of stepwise search, can be one of `"both"`, `"backward"`, or
  `"forward"`, with a default of `"both"`. If the `scope` argument is
  missing the default for `direction` is `"backward"`. Values can be
  abbreviated.

- steps:

  the maximum number of steps to be considered. The default is 1000
  (essentially as many as required). It is typically used to stop the
  process early.

- k:

  The multiple of the number of degrees of freedom used for the penalty.
  Only `k = 2` gives the genuine AIC: `k = log(n)` is sometimes referred
  to as BIC or SBC.

## Value

The model refitted with optimal number of parameters.

## Classical lm and glm

For frequentist GLMs, `select_parameters()` performs an AIC-based
stepwise selection.

## Mixed models

For mixed-effects models of class `merMod`, stepwise selection is based
on [`cAIC4::stepcAIC()`](https://rdrr.io/pkg/cAIC4/man/stepcAIC.html).
This step function only searches the "best" model based on the
random-effects structure, i.e. `select_parameters()` adds or excludes
random-effects until the cAIC can't be improved further.

## Examples

``` r
model <- lm(mpg ~ ., data = mtcars)
select_parameters(model)
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec + am, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec           am  
#>       9.618       -3.917        1.226        2.936  
#> 

model <- lm(mpg ~ cyl * disp * hp * wt, data = mtcars)
select_parameters(model)
#> 
#> Call:
#> lm(formula = mpg ~ cyl + disp + hp + wt + cyl:disp + cyl:hp + 
#>     disp:hp + cyl:wt + disp:wt + hp:wt + cyl:disp:hp + cyl:hp:wt, 
#>     data = mtcars)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp           hp           wt     cyl:disp  
#>  49.1436077   -3.6167276   -1.2955318   -0.0004854   58.8328841    0.1704703  
#>      cyl:hp      disp:hp       cyl:wt      disp:wt        hp:wt  cyl:disp:hp  
#>  -0.0134573    0.0132124   -7.4915051   -0.0167172   -0.6524341   -0.0016542  
#>   cyl:hp:wt  
#>   0.0850798  
#> 
# \donttest{
# lme4 -------------------------------------------
model <- lme4::lmer(
  Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species),
  data = iris
)
select_parameters(model)
#> Registered S3 method overwritten by 'cAIC4':
#>   method     from 
#>   family.lme MuMIn
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 |  
#>     Species)
#>    Data: iris
#> REML criterion at convergence: 50.9896
#> Random effects:
#>  Groups   Name        Std.Dev.
#>  Species  (Intercept) 0.8259  
#>  Residual             0.2536  
#> Number of obs: 150, groups:  Species, 3
#> Fixed Effects:
#>                           (Intercept)                           Sepal.Length  
#>                             -2.000229                               0.936730  
#>                           Petal.Width                           Petal.Length  
#>                              1.575526                               0.265556  
#>              Sepal.Length:Petal.Width              Sepal.Length:Petal.Length  
#>                             -0.282960                              -0.088409  
#>              Petal.Width:Petal.Length  Sepal.Length:Petal.Width:Petal.Length  
#>                              0.001866                               0.023319  
# }
```
