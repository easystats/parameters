# Format the name of the degrees-of-freedom adjustment methods

Format the name of the degrees-of-freedom adjustment methods.

## Usage

``` r
format_df_adjust(
  method,
  approx_string = "-approximated",
  dof_string = " degrees of freedom"
)
```

## Arguments

- method:

  Name of the method.

- approx_string, dof_string:

  Suffix added to the name of the method in the returned string.

## Value

A formatted string.

## Examples

``` r
library(parameters)

format_df_adjust("kenward")
#> [1] "Kenward-Roger-approximated degrees of freedom"
format_df_adjust("kenward", approx_string = "", dof_string = " DoF")
#> [1] "Kenward-Roger DoF"
```
