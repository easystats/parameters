# Parameters from Cluster Models (k-means, ...)

Format cluster models obtained for example by
[`kmeans()`](https://rdrr.io/r/stats/kmeans.html).

## Usage

``` r
# S3 method for class 'hclust'
model_parameters(model, data = NULL, clusters = NULL, ...)
```

## Arguments

- model:

  Cluster model.

- data:

  A data frame.

- clusters:

  A vector with clusters assignments (must be same length as rows in
  data).

- ...:

  Arguments passed to or from other methods.

## Examples

``` r
# \donttest{
#
# K-means -------------------------------
model <- kmeans(iris[1:4], centers = 3)
rez <- model_parameters(model)
rez
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 79.05% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    21 |       17.67 |         4.74 |        2.90 |         1.79 |        0.35
#> 2       |    33 |        6.43 |         5.18 |        3.62 |         1.47 |        0.27
#> 3       |    96 |      118.65 |         6.31 |        2.90 |         4.97 |        1.70

# Get clusters
predict(rez)
#>   [1] 2 1 1 1 2 2 2 2 1 1 2 2 1 1 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 1 1 2 2 2 1 2 2
#>  [38] 2 1 2 2 1 1 2 2 1 2 1 2 2 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3
#>  [75] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3
#> [112] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#> [149] 3 3

# Clusters centers in long form
attributes(rez)$means
#>    Cluster n_Obs Sum_Squares     Variable      Mean
#> 1        1    21   17.669524 Sepal.Length 4.7380952
#> 2        1    21   17.669524  Sepal.Width 2.9047619
#> 3        1    21   17.669524 Petal.Length 1.7904762
#> 4        1    21   17.669524  Petal.Width 0.3523810
#> 5        2    33    6.432121 Sepal.Length 5.1757576
#> 6        2    33    6.432121  Sepal.Width 3.6242424
#> 7        2    33    6.432121 Petal.Length 1.4727273
#> 8        2    33    6.432121  Petal.Width 0.2727273
#> 9        3    96  118.651875 Sepal.Length 6.3145833
#> 10       3    96  118.651875  Sepal.Width 2.8958333
#> 11       3    96  118.651875 Petal.Length 4.9739583
#> 12       3    96  118.651875  Petal.Width 1.7031250

# Between and Total Sum of Squares
attributes(rez)$Sum_Squares_Total
#> [1] 681.3706
attributes(rez)$Sum_Squares_Between
#> [1] 538.6171

#
# Hierarchical clustering (hclust) ---------------------------
data <- iris[1:4]
model <- hclust(dist(data))
clusters <- cutree(model, 3)

rez <- model_parameters(model, data, clusters)
rez
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 86.86% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    50 |       15.15 |         5.01 |        3.43 |         1.46 |        0.25
#> 2       |    72 |       64.62 |         6.55 |        2.96 |         5.27 |        1.85
#> 3       |    28 |        9.75 |         5.53 |        2.64 |         3.96 |        1.23

# Get clusters
predict(rez)
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 3 2 3 2 3 2 3 3 3 3 2 3 2 3 3 2 3 2 3 2 2
#>  [75] 2 2 2 2 2 3 3 3 3 2 3 2 2 2 3 3 3 2 3 3 3 3 3 2 3 3 2 2 2 2 2 2 3 2 2 2 2
#> [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [149] 2 2

# Clusters centers in long form
attributes(rez)$means
#>    Cluster n_Obs Sum_Squares     Variable     Mean
#> 1        1    50   15.151000 Sepal.Length 5.006000
#> 2        1    50   15.151000  Sepal.Width 3.428000
#> 3        1    50   15.151000 Petal.Length 1.462000
#> 4        1    50   15.151000  Petal.Width 0.246000
#> 5        2    72   64.624722 Sepal.Length 6.545833
#> 6        2    72   64.624722  Sepal.Width 2.963889
#> 7        2    72   64.624722 Petal.Length 5.273611
#> 8        2    72   64.624722  Petal.Width 1.850000
#> 9        3    28    9.749286 Sepal.Length 5.532143
#> 10       3    28    9.749286  Sepal.Width 2.635714
#> 11       3    28    9.749286 Petal.Length 3.960714
#> 12       3    28    9.749286  Petal.Width 1.228571

# Between and Total Sum of Squares
attributes(rez)$Total_Sum_Squares
#> NULL
attributes(rez)$Between_Sum_Squares
#> NULL

#
# K-Medoids (PAM and HPAM) ==============
model <- cluster::pam(iris[1:4], k = 3)
model_parameters(model)
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 88.43% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    50 |       15.15 |         5.01 |        3.43 |         1.46 |        0.25
#> 2       |    62 |       39.82 |         5.90 |        2.75 |         4.39 |        1.43
#> 3       |    38 |       23.88 |         6.85 |        3.07 |         5.74 |        2.07

model <- fpc::pamk(iris[1:4], criterion = "ch")
model_parameters(model)
#> # Clustering Solution
#> 
#> The 3 clusters accounted for 88.43% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    50 |       15.15 |         5.01 |        3.43 |         1.46 |        0.25
#> 2       |    62 |       39.82 |         5.90 |        2.75 |         4.39 |        1.43
#> 3       |    38 |       23.88 |         6.85 |        3.07 |         5.74 |        2.07

# DBSCAN ---------------------------
model <- dbscan::dbscan(iris[1:4], eps = 1.45, minPts = 10)

rez <- model_parameters(model, iris[1:4])
rez
#> # Clustering Solution
#> 
#> The 2 clusters accounted for 77.26% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |    50 |       15.15 |         5.01 |        3.43 |         1.46 |        0.25
#> 2       |   100 |      139.80 |         6.26 |        2.87 |         4.91 |        1.68

# Get clusters
predict(rez)
#> NULL

# Clusters centers in long form
attributes(rez)$means
#>   Cluster n_Obs Sum_Squares     Variable  Mean
#> 1       1    50      15.151 Sepal.Length 5.006
#> 2       1    50      15.151  Sepal.Width 3.428
#> 3       1    50      15.151 Petal.Length 1.462
#> 4       1    50      15.151  Petal.Width 0.246
#> 5       2   100     139.796 Sepal.Length 6.262
#> 6       2   100     139.796  Sepal.Width 2.872
#> 7       2   100     139.796 Petal.Length 4.906
#> 8       2   100     139.796  Petal.Width 1.676

# Between and Total Sum of Squares
attributes(rez)$Sum_Squares_Total
#> [1] 681.3706
attributes(rez)$Sum_Squares_Between
#> [1] 526.4236

# HDBSCAN
model <- dbscan::hdbscan(iris[1:4], minPts = 10)
model_parameters(model, iris[1:4])
#> # Clustering Solution
#> 
#> The 2 clusters accounted for 77.26% of the total variance of the original data.
#> 
#> Cluster | n_Obs | Sum_Squares | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
#> ---------------------------------------------------------------------------------------
#> 1       |   100 |      139.80 |         6.26 |        2.87 |         4.91 |        1.68
#> 2       |    50 |       15.15 |         5.01 |        3.43 |         1.46 |        0.25
# }
```
