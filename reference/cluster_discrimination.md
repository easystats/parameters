# Compute a linear discriminant analysis on classified cluster groups

Computes linear discriminant analysis (LDA) on classified cluster
groups, and determines the goodness of classification for each cluster
group. See [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) for
details.

## Usage

``` r
cluster_discrimination(x, cluster_groups = NULL, ...)
```

## Arguments

- x:

  A data frame

- cluster_groups:

  Group classification of the cluster analysis, which can be retrieved
  from the
  [`cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.md)
  function.

- ...:

  Other arguments to be passed to or from.

## See also

[`n_clusters()`](https://easystats.github.io/parameters/reference/n_clusters.md)
to determine the number of clusters to extract,
[`cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.md)
to compute a cluster analysis and
[`performance::check_clusterstructure()`](https://easystats.github.io/performance/reference/check_clusterstructure.html)
to check suitability of data for clustering.

## Examples

``` r
# Retrieve group classification from hierarchical cluster analysis
clustering <- cluster_analysis(iris[, 1:4], n = 3)

# Goodness of group classification
cluster_discrimination(clustering)
#> # Accuracy of Cluster Group Classification via Linear Discriminant Analysis (LDA)
#> 
#>  Group Accuracy
#>      1  100.00%
#>      2   82.98%
#>      3   94.34%
#> 
#> Overall accuracy of classification: 92.67%
#> 
```
