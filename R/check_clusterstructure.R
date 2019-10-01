#' Check suitability of data for Clustering
#'
#' This checks whether the data is appropriate for clustering using the Hopkins' H statistic of given data. If the value of Hopkins statistic is close to 0 (below 0.5), then we can reject the null hypothesis and conclude that the dataset is significantly clusterable. A value for H lower than 0.25 indicates a clustering tendency at the 90\% confidence level.
#'
#' @inheritParams check_sphericity
#' @examples
#' library(parameters)
#'
#' check_clusterstructure(iris[, 1:4])
#'
#' @return A list of lists of indices related to sphericity and KMO.
#' @seealso check_kmo check_sphericity check_factorstructure
#' @references Lawson, R. G., \& Jurs, P. C. (1990). New index for clustering tendency and its application to chemical problems. Journal of chemical information and computer sciences, 30(1), 36-41.
#' @export
check_clusterstructure <- function(x, silent = FALSE, ...) {

  H <- .clustertend_hopkins(x)
  if(H < 0.5){
    text <- paste0("The dataset is suitable for clustering (Hopkins' H = ",
                   insight::format_value(H),
                   ")")
    insight::print_color(text, "green")
  } else{
    text <- paste0("The dataset is not suitable for clustering (Hopkins' H = ",
                   insight::format_value(H),
                   ")")
    insight::print_color("", "red")
  }
  invisible(H)
}


#' @importFrom stats runif
#' @keywords internal
.clustertend_hopkins <- function(data) {
  # This is based on the hopkins() function from the clustertend package
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  n <- nrow(data) - 1

  c <- apply(data, 2, min) # minimum value per column
  d <- apply(data, 2, max)
  p <- matrix(0, ncol = ncol(data), nrow = n) # n vectors of space
  for (i in 1:ncol(data))
  {
    p[, i] <- runif(n, min = c[i], max = d[i])
  }
  k <- round(runif(n, 1, nrow(data)))
  q <- as.matrix(data[k, ])
  distp <- rep(0, nrow(data))
  # distq=rep(0,nrow(data)-1)
  distq <- 0
  minp <- rep(0, n)
  minq <- rep(0, n)
  for (i in 1:n)
  {
    distp[1] <- dist(rbind(p[i, ], data[1, ]))
    minqi <- dist(rbind(q[i, ], data[1, ]))
    for (j in 2:nrow(data))
    {
      distp[j] <- dist(rbind(p[i, ], data[j, ]))
      error <- q[i, ] - data[j, ]
      if (sum(abs(error)) != 0) {
        # distq[j]<-dist(rbind(q[i,],data[j,]))
        distq <- dist(rbind(q[i, ], data[j, ]))
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
