#' @rdname n_clusters
#' @examples
#' #
#' # Specific Methods =========================
#' # Elbow method --------------------
#' if (require("openxlsx") && require("see")) {
#'   x <- n_clusters_elbow(iris[1:4])
#'   x
#'   as.data.frame(x)
#'   plot(x)
#' }
#' @export
n_clusters_elbow <- function(x,
                             standardize = TRUE,
                             include_factors = FALSE,
                             clustering_function = stats::kmeans,
                             n_max = 10,
                             ...) {
  out <- .n_clusters_factoextra(
    x,
    method = "wss",
    standardize = standardize,
    include_factors = include_factors,
    clustering_function = clustering_function,
    n_max = n_max,
    ...
  )
  names(out) <- c("n_Clusters", "WSS")

  gradient <- c(0, diff(out$WSS))
  optim <- out$n_Clusters[which.min(gradient)]

  attr(out, "n") <- optim
  attr(out, "gradient") <- gradient
  class(out) <- c("n_clusters_elbow", class(out))
  out
}




#' @rdname n_clusters
#' @examples
#' # Gap method --------------------
#' if (require("see", quietly = TRUE)) {
#'   x <- n_clusters_gap(iris[1:4])
#'   x
#'   as.data.frame(x)
#'   plot(x)
#' }
#' @export
n_clusters_gap <- function(x,
                           standardize = TRUE,
                           include_factors = FALSE,
                           clustering_function = stats::kmeans,
                           n_max = 10,
                           gap_method = "firstSEmax",
                           ...) {
  insight::check_if_installed("cluster")
  rez <- .n_clusters_factoextra(
    x,
    method = "gap_stat",
    standardize = standardize,
    include_factors = include_factors,
    clustering_function = clustering_function,
    n_max = n_max,
    ...
  )
  out <- rez[c("clusters", "gap", "SE.sim")]
  names(out) <- c("n_Clusters", "Gap", "SE")

  optim <- cluster::maxSE(f = out$Gap, SE.f = out$SE, method = gap_method)

  attr(out, "n") <- optim
  attr(out, "ymin") <- rez$ymin
  attr(out, "ymax") <- rez$ymax
  class(out) <- c("n_clusters_gap", class(out))
  out
}



#' @rdname n_clusters
#' @examples
#' #
#' # Silhouette method --------------------------
#' x <- n_clusters_silhouette(iris[1:4])
#' x
#' as.data.frame(x)
#' plot(x)
#' @export
n_clusters_silhouette <- function(x,
                                  standardize = TRUE,
                                  include_factors = FALSE,
                                  clustering_function = stats::kmeans,
                                  n_max = 10,
                                  ...) {
  out <- .n_clusters_factoextra(
    x,
    method = "silhouette",
    standardize = standardize,
    include_factors = include_factors,
    clustering_function = clustering_function,
    n_max = n_max,
    ...
  )
  names(out) <- c("n_Clusters", "Silhouette")

  optim <- which.max(out$Silhouette)

  attr(out, "n") <- optim
  class(out) <- c("n_clusters_silhouette", class(out))
  out
}



#' @rdname n_clusters
#' @examples
#' #
#' if (require("dbscan", quietly = TRUE)) {
#'   # DBSCAN method -------------------------
#'   # NOTE: This actually primarily estimates the 'eps' parameter, the number of
#'   # clusters is a side effect (it's the number of clusters corresponding to
#'   # this 'optimal' EPS parameter).
#'   x <- n_clusters_dbscan(iris[1:4], method = "kNN", min_size = 0.05) # 5 percent
#'   x
#'   head(as.data.frame(x))
#'   plot(x)
#'
#'   x <- n_clusters_dbscan(iris[1:4], method = "SS", eps_n = 100, eps_range = c(0.1, 2))
#'   x
#'   head(as.data.frame(x))
#'   plot(x)
#' }
#' @export
n_clusters_dbscan <- function(x, standardize = TRUE, include_factors = FALSE, method = c("kNN", "SS"), min_size = 0.1, eps_n = 50, eps_range = c(0.1, 3), ...) {
  method <- match.arg(method)
  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  if (method == "SS") {
    out <- data.frame()
    for (eps in seq(eps_range[1], eps_range[2], length.out = eps_n)) {
      rez <- .cluster_analysis_dbscan(x, dbscan_eps = eps, min_size = min_size)
      out <- rbind(out, data.frame(
        eps = eps,
        n_Clusters = length(unique(rez$clusters)) - 1,
        total_SS = sum(.cluster_centers_SS(x, rez$clusters)$WSS)
      ))
    }
    attr(out, "min_size") <- rez$model$MinPts
    attr(out, "eps") <- out$eps[which.min(out$total_SS)]
    attr(out, "n") <- out$n_Clusters[which.min(out$total_SS)]
  } else {
    insight::check_if_installed("dbscan")
    if (min_size < 1) min_size <- round(min_size * nrow(x))
    out <- data.frame(n_Obs = 1:nrow(x), eps = sort(dbscan::kNNdist(x, k = min_size)))
    row.names(out) <- NULL

    gradient <- c(0, diff(out$eps))
    eps <- out$eps[which.max(gradient)]

    rez <- .cluster_analysis_dbscan(x, dbscan_eps = eps, min_size = min_size)

    attr(out, "gradient") <- gradient
    attr(out, "min_size") <- min_size
    attr(out, "eps") <- eps
    attr(out, "n") <- length(unique(rez$clusters)) - 1
  }

  class(out) <- c("n_clusters_dbscan", class(out))
  out
}






#' @rdname n_clusters
#' @examples
#' #
#' # hclust method -------------------------------
#' # iterations should be higher for real analyses
#' x <- n_clusters_hclust(iris[1:4], iterations = 50, ci = 0.90)
#' x
#' head(as.data.frame(x), n = 10) # Print 10 first rows
#' plot(x)
#' @export
n_clusters_hclust <- function(x, standardize = TRUE, include_factors = FALSE, distance_method = "correlation", hclust_method = "average", ci = 0.95, iterations = 100, ...) {
  insight::check_if_installed("pvclust")
  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  # pvclust works on columns, so we need to pivot the dataframe
  model <- pvclust::pvclust(datawizard::data_transpose(x), method.hclust = hclust_method, method.dist = distance_method, nboot = iterations, quiet = TRUE)
  out <- .model_parameters_pvclust_clusters(model, x, ci)

  attr(out, "model") <- model
  attr(out, "ci") <- ci
  attr(out, "n") <- length(unique(out$Cluster)[unique(out$Cluster) != 0])
  class(out) <- c("n_clusters_hclust", class(out))
  out
}




# Utils -------------------------------------------------------------------



#' @keywords internal
.n_clusters_factoextra <- function(x, method = "wss", standardize = TRUE, include_factors = FALSE, clustering_function = stats::kmeans, n_max = 10, ...) {
  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  insight::check_if_installed("factoextra")

  factoextra::fviz_nbclust(x, clustering_function, method = method, k.max = n_max, verbose = FALSE)$data
}




# Printing ----------------------------------------------------------------

#' @export
print.n_clusters_elbow <- function(x, ...) {
  insight::print_color(paste0("The Elbow method, that aims at minimizing the total intra-cluster variation (i.e., the total within-cluster sum of square), suggests that the optimal number of clusters is ", attributes(x)$n, "."), "green")
  invisible(x)
}

#' @export
print.n_clusters_gap <- function(x, ...) {
  insight::print_color(paste0("The Gap method, that compares the total intracluster variation of k clusters with their expected values under null reference distribution of the data, suggests that the optimal number of clusters is ", attributes(x)$n, "."), "green")
  invisible(x)
}

#' @export
print.n_clusters_silhouette <- function(x, ...) {
  insight::print_color(paste0("The Silhouette method, based on the average quality of clustering, suggests that the optimal number of clusters is ", attributes(x)$n, "."), "green")
  invisible(x)
}

#' @export
print.n_clusters_dbscan <- function(x, ...) {
  insight::print_color(paste0("The DBSCAN method, based on the total clusters sum of squares, suggests that the optimal eps = ", attributes(x)$eps, " (with min. cluster size set to ", attributes(x)$min_size, "), which corresponds to ", attributes(x)$n, " clusters."), "green")
  invisible(x)
}

#' @export
print.n_clusters_hclust <- function(x, ...) {
  insight::print_color(paste0("The bootstrap analysis of hierachical clustering highlighted ", attributes(x)$n, " significant clusters."), "green")
  invisible(x)
}

# Plotting ----------------------------------------------------------------

#' @export
visualisation_recipe.n_clusters_elbow <- function(x, ...) {
  data <- as.data.frame(x)
  data$Gradient <- datawizard::change_scale(attributes(x)$gradient, c(min(data$WSS, max(data$WSS))))
  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(
    geom = "line",
    data = data,
    aes = list(x = "n_Clusters", y = "WSS", group = 1),
    size = 1
  )
  layers[["l2"]] <- list(
    geom = "point",
    data = data,
    aes = list(x = "n_Clusters", y = "WSS")
  )
  layers[["l3"]] <- list(
    geom = "line",
    data = data,
    aes = list(x = "n_Clusters", y = "Gradient", group = 1),
    size = 0.5,
    color = "red",
    linetype = "dashed"
  )
  layers[["l4"]] <- list(
    geom = "vline",
    data = data,
    xintercept = attributes(x)$n,
    linetype = "dotted"
  )
  layers[["l5"]] <- list(
    geom = "labs",
    x = "Number of Clusters",
    y = "Total Within-Clusters Sum of Squares",
    title = "Elbow Method"
  )

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
visualisation_recipe.n_clusters_gap <- function(x, ...) {
  data <- as.data.frame(x)
  data$ymin <- attributes(x)$ymin
  data$ymax <- attributes(x)$ymax
  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(
    geom = "line",
    data = data,
    aes = list(x = "n_Clusters", y = "Gap", group = 1)
  )
  layers[["l2"]] <- list(
    geom = "pointrange",
    data = data,
    aes = list(x = "n_Clusters", y = "Gap", ymin = "ymin", ymax = "ymax")
  )
  layers[["l4"]] <- list(
    geom = "vline",
    data = data,
    xintercept = attributes(x)$n,
    linetype = "dotted"
  )
  layers[["l5"]] <- list(
    geom = "labs",
    x = "Number of Clusters",
    y = "Gap statistic",
    title = "Gap Method"
  )

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
visualisation_recipe.n_clusters_silhouette <- function(x, ...) {
  data <- as.data.frame(x)
  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(
    geom = "line",
    data = data,
    aes = list(x = "n_Clusters", y = "Silhouette", group = 1)
  )
  layers[["l2"]] <- list(
    geom = "point",
    data = data,
    aes = list(x = "n_Clusters", y = "Silhouette")
  )
  layers[["l4"]] <- list(
    geom = "vline",
    data = data,
    xintercept = attributes(x)$n,
    linetype = "dotted"
  )
  layers[["l5"]] <- list(
    geom = "labs",
    x = "Number of Clusters",
    y = "Average Silhouette Width",
    title = "Silhouette Method"
  )

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
visualisation_recipe.n_clusters_dbscan <- function(x, ...) {
  data <- as.data.frame(x)

  layers <- list()


  # Layers -----------------------
  if ("gradient" %in% names(attributes(x))) {
    data$gradient <- datawizard::change_scale(attributes(x)$gradient, c(min(data$eps), max(data$eps)))

    layers[["l1"]] <- list(
      geom = "line",
      data = data,
      aes = list(x = "n_Obs", y = "eps"),
      size = 1
    )
    layers[["l2"]] <- list(
      geom = "line",
      data = data,
      aes = list(x = "n_Obs", y = "gradient"),
      color = "red",
      linetype = "dashed"
    )
    layers[["l3"]] <- list(
      geom = "hline",
      data = data,
      yintercept = attributes(x)$eps,
      linetype = "dotted"
    )
    layers[["l4"]] <- list(
      geom = "labs",
      x = "Observations",
      y = paste0("EPS Value (min. size = ", attributes(x)$min_size, ")"),
      title = "DBSCAN Method"
    )
  } else {
    data$y <- datawizard::change_scale(data$total_SS, c(min(data$n_Clusters), max(data$n_Clusters)))

    layers[["l1"]] <- list(
      geom = "line",
      data = data,
      aes = list(x = "eps", y = "n_Clusters"),
      size = 1
    )
    layers[["l2"]] <- list(
      geom = "line",
      data = data,
      aes = list(x = "eps", y = "y"),
      color = "red",
      linetype = "dashed"
    )
    layers[["l3"]] <- list(
      geom = "vline",
      data = data,
      xintercept = attributes(x)$eps,
      linetype = "dotted"
    )
    layers[["l4"]] <- list(
      geom = "labs",
      x = paste0("EPS Value (min. size = ", attributes(x)$min_size, ")"),
      y = paste0("Number of CLusters"),
      title = "DBSCAN Method"
    )
  }

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
plot.n_clusters_elbow <- function(x, ...) {
  plot(visualisation_recipe(x, ...))
}

#' @export
plot.n_clusters_gap <- plot.n_clusters_elbow

#' @export
plot.n_clusters_silhouette <- plot.n_clusters_elbow

#' @export
plot.n_clusters_dbscan <- plot.n_clusters_elbow

#' @export
plot.n_clusters_hclust <- function(x, ...) {
  insight::check_if_installed("pvclust")
  plot(attributes(x)$model)
  pvclust::pvrect(attributes(x)$model, alpha = attributes(x)$ci, pv = "si")
}
