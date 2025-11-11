# Get Scores from Principal Component or Factor Analysis (PCA/FA)

`get_scores()` takes `n_items` amount of items that load the most
(either by loading cutoff or number) on a component, and then computes
their average. This results in a sum score for each component from the
PCA/FA, which is on the same scale as the original, single items that
were used to compute the PCA/FA.

## Usage

``` r
get_scores(x, n_items = NULL)
```

## Arguments

- x:

  An object returned by
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  or
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md).

- n_items:

  Number of required (i.e. non-missing) items to build the sum score for
  an observation. If an observation has more missing values than
  `n_items` in all items of a (sub) scale, `NA` is returned for that
  observation, else, the sum score of all (sub) items is calculated. If
  `NULL`, the value is chosen to match half of the number of columns in
  a data frame, i.e. no more than 50% missing values are allowed.

## Value

A data frame with subscales, which are average sum scores for all items
from each component or factor.

## Details

`get_scores()` takes the results from
[`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
or
[`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
and extracts the variables for each component found by the PCA/FA. Then,
for each of these "subscales", row means are calculated (which equals
adding up the single items and dividing by the number of items). This
results in a sum score for each component from the PCA/FA, which is on
the same scale as the original, single items that were used to compute
the PCA/FA.

## See also

Functions to carry out a PCA
([`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md))
or a FA
([`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)).
[`factor_scores()`](https://easystats.github.io/parameters/reference/factor_scores.md)
extracts factor scores from an FA object.

## Examples

``` r
pca <- principal_components(mtcars[, 1:7], n = 2, rotation = "varimax")

# PCA extracted two components
pca
#> # Rotated loadings from Principal Component Analysis (varimax-rotation)
#> 
#> Variable |   RC1 |   RC2 | Complexity | Uniqueness |  MSA
#> ---------------------------------------------------------
#> mpg      | -0.84 | -0.42 |       1.47 |       0.13 | 0.87
#> cyl      |  0.77 |  0.58 |       1.86 |       0.08 | 0.87
#> disp     |  0.85 |  0.43 |       1.48 |       0.08 | 0.85
#> hp       |  0.55 |  0.77 |       1.81 |       0.10 | 0.90
#> drat     | -0.89 |  0.01 |       1.00 |       0.21 | 0.85
#> wt       |  0.93 |  0.17 |       1.07 |       0.10 | 0.77
#> qsec     | -0.03 | -0.97 |       1.00 |       0.06 | 0.61
#> 
#> The 2 principal components (varimax rotation) accounted for 89.18% of the total variance of the original data (RC1 = 56.82%, RC2 = 32.36%).
#> 

# assignment of items to each component
closest_component(pca)
#>  mpg  cyl disp   hp drat   wt qsec 
#>    1    1    1    2    1    1    2 

# now we want to have sum scores for each component
get_scores(pca)
#>                     Component_1 Component_2
#> Mazda RX4               38.7040      63.230
#> Mazda RX4 Wag           38.7550      63.510
#> Datsun 710              28.1940      55.805
#> Hornet 4 Drive          58.3390      64.720
#> Hornet Sportabout       78.6580      96.010
#> Valiant                 51.0640      62.610
#> Duster 360              77.8160     130.420
#> Merc 240D               36.3960      41.000
#> Merc 230                34.9340      58.950
#> Merc 280                40.0320      70.650
#> Merc 280C               39.7520      70.950
#> Merc 450SE              61.4680      98.700
#> Merc 450SL              61.5800      98.800
#> Merc 450SLC             61.1700      99.000
#> Cadillac Fleetwood      99.7160     111.490
#> Lincoln Continental     97.3648     116.410
#> Chrysler Imperial       94.2550     123.710
#> Fiat 128                24.2760      42.735
#> Honda Civic             23.3290      35.260
#> Toyota Corolla          23.0110      42.450
#> Toyota Corona           30.3530      58.505
#> Dodge Challenger        69.5560      83.435
#> AMC Javelin             66.7570      83.650
#> Camaro Z28              75.7740     130.205
#> Pontiac Firebird        86.8250      96.025
#> Fiat X1-9               23.2630      42.450
#> Porsche 914-2           31.3740      53.850
#> Lotus Europa            26.9566      64.950
#> Ford Pantera L          76.4380     139.250
#> Ferrari Dino            35.4180      95.250
#> Maserati Bora           66.2220     174.800
#> Volvo 142E              30.6580      63.800

# compare to manually computed sum score for 2nd component, which
# consists of items "hp" and "qsec"
(mtcars$hp + mtcars$qsec) / 2
#>  [1]  63.230  63.510  55.805  64.720  96.010  62.610 130.420  41.000  58.950
#> [10]  70.650  70.950  98.700  98.800  99.000 111.490 116.410 123.710  42.735
#> [19]  35.260  42.450  58.505  83.435  83.650 130.205  96.025  42.450  53.850
#> [28]  64.950 139.250  95.250 174.800  63.800
```
