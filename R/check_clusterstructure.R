#' Check suitability of data for Clustering
#'
#' This checks whether the data is appropriate for clustering using the Hopkins' H statistic of given data. If the value of Hopkins statistic is close to 0 (below 0.5), then we can reject the null hypothesis and conclude that the dataset is significantly clusterable. A value for H lower than 0.25 indicates a clustering tendency at the 90\% confidence level. The visual assessment of cluster tendency (VAT) approach (Bezdek and Hathaway, 2002) consists in investigating the heatmap of the ordered dissimilarity matrix. Following this, one can potentially detect the clustering tendency by counting the number of square shaped blocks along the diagonal.
#'
#' @param x A data frame.
#' @param standardize Standardize the dataframe before clustering (default).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' check_clusterstructure(iris[, 1:4])
#' plot(check_clusterstructure(iris[, 1:4]))
#'
#' @return A list of lists of indices related to sphericity and KMO.
#' @seealso check_kmo check_sphericity check_factorstructure
#' @references \itemize{
#'   \item Lawson, R. G., \& Jurs, P. C. (1990). New index for clustering tendency and its application to chemical problems. Journal of chemical information and computer sciences, 30(1), 36-41.
#'   \item Bezdek, J. C., \& Hathaway, R. J. (2002, May). VAT: A tool for visual assessment of (cluster) tendency. In Proceedings of the 2002 International Joint Conference on Neural Networks. IJCNN02 (3), 2225-2230. IEEE.
#' }
#' @export
check_clusterstructure <- function(x, standardize = TRUE, ...) {

  if(standardize){
    x <- standardize(x)
  }

  H <- .clusterstructure_hopkins(x)
  if(H < 0.5){
    text <- paste0("The dataset is suitable for clustering (Hopkins' H = ",
                   insight::format_value(H),
                   ").")
    color <- "green"
  } else{
    text <- paste0("The dataset is not suitable for clustering (Hopkins' H = ",
                   insight::format_value(H),
                   ").")
    color <- "red"
  }

  out <- list(H = H,
              dissimilarity_matrix = .clusterstructure_dm(x))

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Clustering tendency"
  class(out) <- c("see_check_clusterstructure", "check_clusterstructure", "easystats_check", class(out))
  out
}



#' @importFrom stats heatmap
#' @importFrom grDevices colorRampPalette
#' @export
plot.check_clusterstructure <- function(x, ...){
  # Can be reimplemented with ggplot in see
  heatmap(x$dissimilarity_matrix, Rowv = NA, Colv = NA,
          labRow = FALSE, labCol = FALSE,
          col = grDevices::colorRampPalette(c("#2196F3", "#FAFAFA", "#E91E63"))(100))
}



#' @importFrom stats hclust dist
#' @keywords internal
.clusterstructure_dm <- function(x) {
  d <- stats::dist(x, method = "euclidean")
  hc <- stats::hclust(d, method = "ward.D2")
  as.matrix(d)[hc$order, hc$order]
}



#' @importFrom stats runif
#' @keywords internal
.clusterstructure_hopkins <- function(x) {
  # This is based on the hopkins() function from the clustertend package
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  n <- nrow(x) - 1

  c <- apply(x, 2, min) # minimum value per column
  d <- apply(x, 2, max)
  p <- matrix(0, ncol = ncol(x), nrow = n) # n vectors of space
  for (i in 1:ncol(x))
  {
    p[, i] <- runif(n, min = c[i], max = d[i])
  }
  k <- round(runif(n, 1, nrow(x)))
  q <- as.matrix(x[k, ])
  distp <- rep(0, nrow(x))
  # distq=rep(0,nrow(x)-1)
  distq <- 0
  minp <- rep(0, n)
  minq <- rep(0, n)
  for (i in 1:n)
  {
    distp[1] <- dist(rbind(p[i, ], x[1, ]))
    minqi <- dist(rbind(q[i, ], x[1, ]))
    for (j in 2:nrow(x))
    {
      distp[j] <- dist(rbind(p[i, ], x[j, ]))
      error <- q[i, ] - x[j, ]
      if (sum(abs(error)) != 0) {
        # distq[j]<-dist(rbind(q[i,],x[j,]))
        distq <- dist(rbind(q[i, ], x[j, ]))
        if (distq < minqi) {
          minqi <- distq
        }
      }
    }
    minp[i] <- min(distp)
    # minq[i]<-apply(distq,1,min)
    minq[i] <- minqi
  }
  H <- (sum(minq) / (sum(minp) + sum(minq)))
  H
}
