# Format the name of the p-value adjustment methods

Format the name of the p-value adjustment methods.

## Usage

``` r
format_p_adjust(method)
```

## Arguments

- method:

  Name of the method.

## Value

A string with the full surname(s) of the author(s), including year of
publication, for the adjustment-method.

## Examples

``` r
library(parameters)

format_p_adjust("holm")
#> [1] "Holm (1979)"
format_p_adjust("bonferroni")
#> [1] "Bonferroni"
```
