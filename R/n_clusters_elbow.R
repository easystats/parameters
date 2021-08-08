#' @rdname n_clusters
#' @examples
#' # Elbow method --------------------
#' x <- n_clusters_elbow(iris[1:4])
#' x
#' as.data.frame(x)
#' plot(x)
#' @importFrom stats kmeans
#' @export
n_clusters_elbow <-  function(x, standardize = TRUE, include_factors = FALSE, clustering_function = stats::kmeans, n_max = 15, ...) {
  out <- .n_clusters_factoextra(x, method = "wss", standardize = standardize, include_factors = include_factors, clustering_function = clustering_function, n_max = n_max, ...)
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
#' x <- n_clusters_gap(iris[1:4])
#' x
#' as.data.frame(x)
#' plot(x)
#' @export
n_clusters_gap <-  function(x, standardize = TRUE, include_factors = FALSE, clustering_function = stats::kmeans, n_max = 15, ...) {
  rez <- .n_clusters_factoextra(x, method = "gap_stat", standardize = standardize, include_factors = include_factors, clustering_function = clustering_function, n_max = n_max, ...)
  out <- rez[c("clusters", "gap", "SE.sim")]
  names(out) <- c("n_Clusters", "Gap", "SE")

  optim <- .firstSEmax(out$Gap, out$SE)

  attr(out, "n") <- optim
  attr(out, "ymin") <- rez$ymin
  attr(out, "ymax") <- rez$ymax
  class(out) <- c("n_clusters_gap", class(out))
  out

}



#' @rdname n_clusters
#' @examples
#' # Silhouette method --------------------
#' x <- n_clusters_silhouette(iris[1:4])
#' x
#' as.data.frame(x)
#' plot(x)
#' @export
n_clusters_silhouette <-  function(x, standardize = TRUE, include_factors = FALSE, clustering_function = kmeans, n_max = 15, ...) {
  out <- .n_clusters_factoextra(x, method = "silhouette", standardize = standardize, include_factors = include_factors, clustering_function = clustering_function, n_max = n_max, ...)
  names(out) <- c("n_Clusters", "Silhouette")

  optim <- which.max(out$Silhouette)

  attr(out, "n") <- optim
  class(out) <- c("n_clusters_silhouette", class(out))
  out

}


# Utils -------------------------------------------------------------------



#' @keywords internal
.n_clusters_factoextra <- function(x, method = "wss", standardize = TRUE, include_factors = FALSE, clustering_function = stats::kmeans, n_max = 15, ...) {
  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  insight::check_if_installed("factoextra")

  factoextra::fviz_nbclust(x, clustering_function, method = method, k.max = n_max, verbose = FALSE, ...)$data
}



#' @keywords internal
.firstSEmax <- function(x, se) {
  # look at the first *local* maximum and then to the left
  k <- length(x)

  decr <- diff(x) <= 0 # length K-1
  if(any(decr)) {
    nc <- which.max(decr)
  } else {
    nc <- k # the first TRUE, or K
  }

  mp <- x[seq_len(nc - 1)] >= (x[nc] - se[nc])
  if(any(mp)) {
    which(mp)[1]
  } else {
    nc
  }
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

# Plotting ----------------------------------------------------------------

#' @export
visualisation_recipe.n_clusters_elbow <- function(x, ...) {
  data <- as.data.frame(x)
  data$Gradient <- datawizard::change_scale(attributes(x)$gradient, c(min(data$WSS, max(data$WSS))))
  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(geom = "line",
                         data = data,
                         aes = list(x = "n_Clusters", y = "WSS", group = 1),
                         size = 1)
  layers[["l2"]] <- list(geom = "point",
                         data = data,
                         aes = list(x = "n_Clusters", y = "WSS"))
  layers[["l3"]] <- list(geom = "line",
                         data = data,
                         aes = list(x = "n_Clusters", y = "Gradient", group = 1),
                         size = 0.5,
                         color = "red",
                         linetype = "dashed")
  layers[["l4"]] <- list(geom = "vline",
                         data = data,
                         xintercept = attributes(x)$n,
                         linetype = "dotted")
  layers[["l5"]] <- list(geom = "labs",
                         x = "Number of Clusters",
                         y = "Total Within-Clusters Sum of Squares",
                         title = "Elbow Method")

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
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
  layers[["l1"]] <- list(geom = "line",
                         data = data,
                         aes = list(x = "n_Clusters", y = "Gap", group = 1))
  layers[["l2"]] <- list(geom = "pointrange",
                         data = data,
                         aes = list(x = "n_Clusters", y = "Gap", ymin = "ymin", ymax = "ymax"))
  layers[["l4"]] <- list(geom = "vline",
                         data = data,
                         xintercept = attributes(x)$n,
                         linetype = "dotted")
  layers[["l5"]] <- list(geom = "labs",
                         x = "Number of Clusters",
                         y = "Gap statistic",
                         title = "Gap Method")

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
visualisation_recipe.n_clusters_silhouette <- function(x, ...) {
  data <- as.data.frame(x)
  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(geom = "line",
                         data = data,
                         aes = list(x = "n_Clusters", y = "Silhouette", group = 1))
  layers[["l2"]] <- list(geom = "point",
                         data = data,
                         aes = list(x = "n_Clusters", y = "Silhouette"))
  layers[["l4"]] <- list(geom = "vline",
                         data = data,
                         xintercept = attributes(x)$n,
                         linetype = "dotted")
  layers[["l5"]] <- list(geom = "labs",
                         x = "Number of Clusters",
                         y = "Average Silhouette Width",
                         title = "Silhouette Method")

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
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