# Performance of clustering models

Compute performance indices for clustering solutions.

## Usage

``` r
cluster_performance(model, ...)

# S3 method for class 'hclust'
cluster_performance(model, data, clusters, ...)
```

## Arguments

- model:

  Cluster model.

- ...:

  Arguments passed to or from other methods.

- data:

  A data frame.

- clusters:

  A vector with clusters assignments (must be same length as rows in
  data).

## Examples

``` r
# kmeans
model <- kmeans(iris[1:4], 3)
cluster_performance(model)
#> # Indices of model performance
#> 
#> Sum_Squares_Total | Sum_Squares_Between | Sum_Squares_Within |    R2
#> --------------------------------------------------------------------
#> 681.371           |             602.519 |             78.851 | 0.884

# hclust
data <- iris[1:4]
model <- hclust(dist(data))
clusters <- cutree(model, 3)
cluster_performance(model, data, clusters)
#> # Indices of model performance
#> 
#> Sum_Squares_Total | Sum_Squares_Between | Sum_Squares_Within |    R2
#> --------------------------------------------------------------------
#> 681.371           |             591.846 |             89.525 | 0.869

# Retrieve performance from parameters
params <- model_parameters(kmeans(iris[1:4], 3))
cluster_performance(params)
#> # Indices of model performance
#> 
#> Sum_Squares_Total | Sum_Squares_Between | Sum_Squares_Within |    R2
#> --------------------------------------------------------------------
#> 681.371           |             538.617 |            142.754 | 0.790
```
