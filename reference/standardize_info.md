# Get Standardization Information

This function extracts information, such as the deviations (SD or MAD)
from parent variables, that are necessary for post-hoc standardization
of parameters. This function gives a window on how standardized are
obtained, i.e., by what they are divided. The "basic" method of
standardization uses.

## Usage

``` r
standardize_info(model, ...)

# Default S3 method
standardize_info(
  model,
  robust = FALSE,
  two_sd = FALSE,
  include_pseudo = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A statistical model.

- ...:

  Arguments passed to or from other methods.

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

- include_pseudo:

  (For (G)LMMs) Should Pseudo-standardized information be included?

- verbose:

  Toggle warnings and messages on or off.

## Value

A data frame with information on each parameter (see
[`parameters_type()`](https://easystats.github.io/parameters/reference/parameters_type.md)),
and various standardization coefficients for the post-hoc methods (see
[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md))
for the predictor and the response.

## See also

Other standardize:
[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)

## Examples

``` r
model <- lm(mpg ~ ., data = mtcars)
standardize_info(model)
#>      Parameter      Type        Link Secondary_Parameter EffectSize_Type
#> 1  (Intercept) intercept        Mean                <NA>            <NA>
#> 2          cyl   numeric Association                <NA>               r
#> 3         disp   numeric Association                <NA>               r
#> 4           hp   numeric Association                <NA>               r
#> 5         drat   numeric Association                <NA>               r
#> 6           wt   numeric Association                <NA>               r
#> 7         qsec   numeric Association                <NA>               r
#> 8           vs   numeric Association                <NA>               r
#> 9           am   numeric Association                <NA>               r
#> 10        gear   numeric Association                <NA>               r
#> 11        carb   numeric Association                <NA>               r
#>    Deviation_Response_Basic Deviation_Response_Smart Deviation_Basic
#> 1                  6.026948                 6.026948       0.0000000
#> 2                  6.026948                 6.026948       1.7859216
#> 3                  6.026948                 6.026948     123.9386938
#> 4                  6.026948                 6.026948      68.5628685
#> 5                  6.026948                 6.026948       0.5346787
#> 6                  6.026948                 6.026948       0.9784574
#> 7                  6.026948                 6.026948       1.7869432
#> 8                  6.026948                 6.026948       0.5040161
#> 9                  6.026948                 6.026948       0.4989909
#> 10                 6.026948                 6.026948       0.7378041
#> 11                 6.026948                 6.026948       1.6152000
#>    Deviation_Smart Deviation_SDy
#> 1        0.0000000       0.13455
#> 2        1.7859216       0.13455
#> 3      123.9386938       0.13455
#> 4       68.5628685       0.13455
#> 5        0.5346787       0.13455
#> 6        0.9784574       0.13455
#> 7        1.7869432       0.13455
#> 8        0.5040161       0.13455
#> 9        0.4989909       0.13455
#> 10       0.7378041       0.13455
#> 11       1.6152000       0.13455
standardize_info(model, robust = TRUE)
#>      Parameter      Type        Link Secondary_Parameter EffectSize_Type
#> 1  (Intercept) intercept        Mean                <NA>            <NA>
#> 2          cyl   numeric Association                <NA>               r
#> 3         disp   numeric Association                <NA>               r
#> 4           hp   numeric Association                <NA>               r
#> 5         drat   numeric Association                <NA>               r
#> 6           wt   numeric Association                <NA>               r
#> 7         qsec   numeric Association                <NA>               r
#> 8           vs   numeric Association                <NA>               r
#> 9           am   numeric Association                <NA>               r
#> 10        gear   numeric Association                <NA>               r
#> 11        carb   numeric Association                <NA>               r
#>    Deviation_Response_Basic Deviation_Response_Smart Deviation_Basic
#> 1                   5.41149                  5.41149       0.0000000
#> 2                   5.41149                  5.41149       2.9652000
#> 3                   5.41149                  5.41149     140.4763500
#> 4                   5.41149                  5.41149      77.0952000
#> 5                   5.41149                  5.41149       0.7042350
#> 6                   5.41149                  5.41149       0.7672455
#> 7                   5.41149                  5.41149       1.4158830
#> 8                   5.41149                  5.41149       0.0000000
#> 9                   5.41149                  5.41149       0.0000000
#> 10                  5.41149                  5.41149       1.4826000
#> 11                  5.41149                  5.41149       1.4826000
#>    Deviation_Smart Deviation_SDy
#> 1        0.0000000       0.13455
#> 2        2.9652000       0.13455
#> 3      140.4763500       0.13455
#> 4       77.0952000       0.13455
#> 5        0.7042350       0.13455
#> 6        0.7672455       0.13455
#> 7        1.4158830       0.13455
#> 8        0.0000000       0.13455
#> 9        0.0000000       0.13455
#> 10       1.4826000       0.13455
#> 11       1.4826000       0.13455
standardize_info(model, two_sd = TRUE)
#>      Parameter      Type        Link Secondary_Parameter EffectSize_Type
#> 1  (Intercept) intercept        Mean                <NA>            <NA>
#> 2          cyl   numeric Association                <NA>               r
#> 3         disp   numeric Association                <NA>               r
#> 4           hp   numeric Association                <NA>               r
#> 5         drat   numeric Association                <NA>               r
#> 6           wt   numeric Association                <NA>               r
#> 7         qsec   numeric Association                <NA>               r
#> 8           vs   numeric Association                <NA>               r
#> 9           am   numeric Association                <NA>               r
#> 10        gear   numeric Association                <NA>               r
#> 11        carb   numeric Association                <NA>               r
#>    Deviation_Response_Basic Deviation_Response_Smart Deviation_Basic
#> 1                  6.026948                 6.026948       0.0000000
#> 2                  6.026948                 6.026948       3.5718433
#> 3                  6.026948                 6.026948     247.8773877
#> 4                  6.026948                 6.026948     137.1257370
#> 5                  6.026948                 6.026948       1.0693575
#> 6                  6.026948                 6.026948       1.9569149
#> 7                  6.026948                 6.026948       3.5738865
#> 8                  6.026948                 6.026948       1.0080323
#> 9                  6.026948                 6.026948       0.9979818
#> 10                 6.026948                 6.026948       1.4756081
#> 11                 6.026948                 6.026948       3.2304000
#>    Deviation_Smart Deviation_SDy
#> 1        0.0000000       0.13455
#> 2        3.5718433       0.13455
#> 3      247.8773877       0.13455
#> 4      137.1257370       0.13455
#> 5        1.0693575       0.13455
#> 6        1.9569149       0.13455
#> 7        3.5738865       0.13455
#> 8        1.0080323       0.13455
#> 9        0.9979818       0.13455
#> 10       1.4756081       0.13455
#> 11       3.2304000       0.13455
```
