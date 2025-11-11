# Cluster Analysis

Compute hierarchical or kmeans cluster analysis and return the group
assignment for each observation as vector.

## Usage

``` r
cluster_analysis(
  x,
  n = NULL,
  method = "kmeans",
  include_factors = FALSE,
  standardize = TRUE,
  verbose = TRUE,
  distance_method = "euclidean",
  hclust_method = "complete",
  kmeans_method = "Hartigan-Wong",
  dbscan_eps = 15,
  iterations = 100,
  ...
)
```

## Arguments

- x:

  A data frame (with at least two variables), or a matrix (with at least
  two columns).

- n:

  Number of clusters used for supervised cluster methods. If `NULL`, the
  number of clusters to extract is determined by calling
  [`n_clusters()`](https://easystats.github.io/parameters/reference/n_clusters.md).
  Note that this argument does not apply for unsupervised clustering
  methods like `dbscan`, `hdbscan`, `mixture`, `pvclust`, or `pamk`.

- method:

  Method for computing the cluster analysis. Can be `"kmeans"` (default;
  k-means using [`kmeans()`](https://rdrr.io/r/stats/kmeans.html)),
  `"hkmeans"` (hierarchical k-means using
  [`factoextra::hkmeans()`](https://rdrr.io/pkg/factoextra/man/hkmeans.html)),
  `pam` (K-Medoids using
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)), `pamk`
  (K-Medoids that finds out the number of clusters), `"hclust"`
  (hierarchical clustering using
  [`hclust()`](https://rdrr.io/r/stats/hclust.html) or
  [`pvclust::pvclust()`](https://rdrr.io/pkg/pvclust/man/pvclust.html)),
  `dbscan` (DBSCAN using
  [`dbscan::dbscan()`](https://rdrr.io/pkg/dbscan/man/dbscan.html)),
  `hdbscan` (Hierarchical DBSCAN using
  [`dbscan::hdbscan()`](https://rdrr.io/pkg/dbscan/man/hdbscan.html)),
  or `mixture` (Mixture modeling using
  [`mclust::Mclust()`](https://mclust-org.github.io/mclust/reference/Mclust.html),
  which requires the user to run
  [`library(mclust)`](https://mclust-org.github.io/mclust/) before).

- include_factors:

  Logical, if `TRUE`, factors are converted to numerical values in order
  to be included in the data for determining the number of clusters. By
  default, factors are removed, because most methods that determine the
  number of clusters need numeric input only.

- standardize:

  Standardize the dataframe before clustering (default).

- verbose:

  Toggle warnings and messages.

- distance_method:

  Distance measure to be used for methods based on distances (e.g., when
  `method = "hclust"` for hierarchical clustering. For other methods,
  such as `"kmeans"`, this argument will be ignored). Must be one of
  `"euclidean"`, `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"` or
  `"minkowski"`. See
  [`dist()`](https://rdrr.io/pkg/factoextra/man/dist.html) and
  [`pvclust::pvclust()`](https://rdrr.io/pkg/pvclust/man/pvclust.html)
  for more information.

- hclust_method:

  Agglomeration method to be used when `method = "hclust"` or
  `method = "hkmeans"` (for hierarchical clustering). This should be one
  of `"ward"`, `"ward.D2"`, `"single"`, `"complete"`, `"average"`,
  `"mcquitty"`, `"median"` or `"centroid"`. Default is `"complete"` (see
  [`hclust()`](https://rdrr.io/r/stats/hclust.html)).

- kmeans_method:

  Algorithm used for calculating kmeans cluster. Only applies, if
  `method = "kmeans"`. May be one of `"Hartigan-Wong"` (default),
  `"Lloyd"` (used by SPSS), or `"MacQueen"`. See
  [`kmeans()`](https://rdrr.io/r/stats/kmeans.html) for details on this
  argument.

- dbscan_eps:

  The `eps` argument for DBSCAN method. See
  [`n_clusters_dbscan()`](https://easystats.github.io/parameters/reference/n_clusters.md).

- iterations:

  The number of replications.

- ...:

  Arguments passed to or from other methods.

## Value

The group classification for each observation as vector. The returned
vector includes missing values, so it has the same length as `nrow(x)`.

## Details

The [`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods show
the (standardized) mean value for each variable within each cluster.
Thus, a higher absolute value indicates that a certain variable
characteristic is more pronounced within that specific cluster (as
compared to other cluster groups with lower absolute mean values).

Clusters classification can be obtained via
`print(x, newdata = NULL, ...)`.

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/).

## References

- Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster:
  Cluster Analysis Basics and Extensions. R package.

## See also

- [`n_clusters()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  to determine the number of clusters to extract.

- [`cluster_discrimination()`](https://easystats.github.io/parameters/reference/cluster_discrimination.md)
  to determine the accuracy of cluster group classification via linear
  discriminant analysis (LDA).

- [`performance::check_clusterstructure()`](https://easystats.github.io/performance/reference/check_clusterstructure.html)
  to check suitability of data for clustering.

- https://www.datanovia.com/en/lessons/

## Examples

``` r
set.seed(33)
# K-Means ====================================================
rez <- cluster_analysis(iris[1:4], n = 3, method = "kmeans")
rez # Show results
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 68.16% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    21 |       23.16 |        -1.32 |       -0.37 |        -1.13 |       -1.11
#> 2       |    33 |       17.33 |        -0.81 |        1.31 |        -1.28 |       -1.22
#> 3       |    96 |      149.26 |         0.57 |       -0.37 |         0.69 |        0.66
#> 
#>   Sum_Squares_Total Sum_Squares_Between Sum_Squares_Within        R2
#> 1               596            406.2488           189.7512 0.6816254
#> 
#> # You can access the predicted clusters via `predict()`.
#> 
predict(rez) # Get clusters
#>   [1] 2 1 1 1 2 2 2 2 1 1 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 1 2 2 2 1 1 2
#>  [38] 2 1 2 2 1 1 2 2 1 2 1 2 2 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3
#>  [75] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3
#> [112] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#> [149] 3 3
summary(rez) # Extract the centers values (can use 'plot()' on that)
#>   Cluster Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1       1   -1.3232208  -0.3718921   -1.1334386  -1.1111395
#> 2       2   -0.8135055   1.3145538   -1.2825372  -1.2156393
#> 3       3    0.5690971  -0.3705265    0.6888118   0.6609378
if (requireNamespace("MASS", quietly = TRUE)) {
  cluster_discrimination(rez) # Perform LDA
}
#> # Accuracy of Cluster Group Classification via Linear Discriminant Analysis (LDA)
#> 
#>  Group Accuracy
#>      1  100.00%
#>      2   71.43%
#>      3  100.00%
#> 
#> Overall accuracy of classification: 96.00%
#> 

# Hierarchical k-means (more robust k-means)
if (require("factoextra", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans")
  rez # Show results
  predict(rez) # Get clusters
}
#> Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 2
#>  [75] 2 3 3 3 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 3 3 3 2 3 3 3 3
#> [112] 3 3 2 2 3 3 3 3 2 3 2 3 2 3 3 2 3 3 3 3 3 3 2 2 3 3 3 2 3 3 3 2 3 3 3 2 3
#> [149] 3 2

# Hierarchical Clustering (hclust) ===========================
rez <- cluster_analysis(iris[1:4], n = 3, method = "hclust")
rez # Show results
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 74.35% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    49 |       40.12 |        -1.00 |        0.90 |        -1.30 |       -1.25
#> 2       |    24 |       18.65 |        -0.40 |       -1.36 |         0.06 |       -0.04
#> 3       |    77 |       94.08 |         0.76 |       -0.15 |         0.81 |        0.81
#> 
#>   Sum_Squares_Total Sum_Squares_Between Sum_Squares_Within        R2
#> 1               596            443.1431           152.8569 0.7435286
#> 
#> # You can access the predicted clusters via `predict()`.
#> 
predict(rez) # Get clusters
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 2 1 1 1 1 1 1 1 1 3 3 3 2 3 2 3 2 3 2 2 3 2 3 3 3 3 2 2 2 3 3 3 3
#>  [75] 3 3 3 3 3 2 2 2 2 3 3 3 3 2 3 2 2 3 2 2 2 3 3 3 2 2 3 3 3 3 3 3 2 3 3 3 3
#> [112] 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#> [149] 3 3

# K-Medoids (pam) ============================================
if (require("cluster", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], n = 3, method = "pam")
  rez # Show results
  predict(rez) # Get clusters
}
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 3 3 3 2 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3
#>  [75] 3 2 2 2 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 2 2 2 2 3 2 2 2 2
#> [112] 2 2 3 2 2 2 2 2 3 2 3 2 3 2 2 3 3 2 2 2 2 2 3 3 2 2 2 3 2 2 2 3 2 2 2 3 2
#> [149] 2 3

# PAM with automated number of clusters
if (require("fpc", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], method = "pamk")
  rez # Show results
  predict(rez) # Get clusters
}
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [149] 2 2

# DBSCAN ====================================================
if (require("dbscan", quietly = TRUE)) {
  # Note that you can assimilate more outliers (cluster 0) to neighbouring
  # clusters by setting borderPoints = TRUE.
  rez <- cluster_analysis(iris[1:4], method = "dbscan", dbscan_eps = 1.45)
  rez # Show results
  predict(rez) # Get clusters
}
#> 
#> Attaching package: ‘dbscan’
#> The following object is masked from ‘package:fpc’:
#> 
#>     dbscan
#> The following object is masked from ‘package:stats’:
#> 
#>     as.dendrogram
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [112] 2 2 2 2 2 2 0 0 2 2 2 2 2 2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [149] 2 2

# Mixture ====================================================
if (require("mclust", quietly = TRUE)) {
  library(mclust) # Needs the package to be loaded
  rez <- cluster_analysis(iris[1:4], method = "mixture")
  rez # Show results
  predict(rez) # Get clusters
}
#> Package 'mclust' version 6.1.2
#> Type 'citation("mclust")' for citing this R package in publications.
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [149] 2 2
```
