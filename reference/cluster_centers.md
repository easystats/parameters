# Find the cluster centers in your data

For each cluster, computes the mean (or other indices) of the variables.
Can be used to retrieve the centers of clusters. Also returns the within
Sum of Squares.

## Usage

``` r
cluster_centers(data, clusters, fun = mean, ...)
```

## Arguments

- data:

  A data.frame.

- clusters:

  A vector with clusters assignments (must be same length as rows in
  data).

- fun:

  What function to use, `mean` by default.

- ...:

  Other arguments to be passed to or from other functions.

## Value

A dataframe containing the cluster centers. Attributes include
performance statistics and distance between each observation and its
respective cluster centre.

## Examples

``` r
k <- kmeans(iris[1:4], 3)
cluster_centers(iris[1:4], clusters = k$cluster)
#>   Cluster n_Obs Sum_Squares Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1       1    38    23.87947     6.850000    3.073684     5.742105    2.071053
#> 2       2    50    15.15100     5.006000    3.428000     1.462000    0.246000
#> 3       3    62    39.82097     5.901613    2.748387     4.393548    1.433871
cluster_centers(iris[1:4], clusters = k$cluster, fun = median)
#>   Cluster n_Obs Sum_Squares Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1       1    38    23.87947          6.7         3.0         5.65         2.1
#> 2       2    50    15.15100          5.0         3.4         1.50         0.2
#> 3       3    62    39.82097          5.9         2.8         4.50         1.4
```
