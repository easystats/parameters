# Order (first, second, ...) formatting

Format order.

## Usage

``` r
format_order(order, textual = TRUE, ...)
```

## Arguments

- order:

  value or vector of orders.

- textual:

  Return number as words. If `FALSE`, will run
  [`insight::format_value()`](https://easystats.github.io/insight/reference/format_value.html).

- ...:

  Arguments to be passed to
  [`insight::format_value()`](https://easystats.github.io/insight/reference/format_value.html)
  if `textual` is `FALSE`.

## Value

A formatted string.

## Examples

``` r
format_order(2)
#> [1] "second"
format_order(8)
#> [1] "eigth"
format_order(25, textual = FALSE)
#> [1] "25th"
```
