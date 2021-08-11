#' Parameters from Cluster Models (k-means, ...)
#'
#' Format cluster models obtained for example by [kmeans()].
#'
#' @param model Cluster model.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' # K-means -------------------------------
#' model <- kmeans(iris[1:4], centers = 3)
#' rez <- model_parameters(model)
#' rez
#'
#' # Get clusters
#' predict(rez)
#'
#' # Clusters centers in long form
#' attributes(rez)$means
#'
#' # Between and Total Sum of Squares
#' attributes(rez)$Sum_Squares_Total
#' attributes(rez)$Sum_Squares_Between
#' @export
model_parameters.kmeans <- function(model, ...) {
  params <- cbind(
    data.frame(
      Cluster = row.names(model$centers),
      n_Obs = model$size,
      Sum_Squares = model$withinss
    ),
    model$centers
  )

  # Long means
  means <- .long_loadings(params, loadings_columns = 4:ncol(params))
  means <- means[c("Cluster", "Loading", "Component")]
  names(means) <- c("Cluster", "Mean", "Variable")

  # Attributes
  attr(params, "variance") <- model$betweenss / model$totss
  attr(params, "Sum_Squares_Between") <- model$betweenss
  attr(params, "Sum_Squares_Total") <- model$totss
  attr(params, "means") <- means
  attr(params, "model") <- model
  attr(params, "iterations") <- model$iter
  attr(params, "scores") <- model$cluster
  attr(params, "type") <- "kmeans"

  class(params) <- c("parameters_clusters", class(params))
  params
}


#' @export
print.parameters_clusters <- function(x, digits = 2, ...) {
  insight::print_color("# K-means Cluster Means", "blue")

  cat("\n\n")
  insight::print_colour(.text_components_variance(x), "yellow")
  cat("\n\n")

  cat(insight::export_table(x, digits = digits, ...))

  invisible(x)
}


#' @export
summary.parameters_clusters <- function(object, ...) {
  object[1:3]
}




# Predict -----------------------------------------------------------------


#' @export
predict.parameters_clusters <- function(object, newdata = NULL, names = NULL, ...) {
  if (is.null(newdata)) {
    out <- attributes(object)$scores
  } else {
    out <- stats::predict(attributes(object)$model, newdata = newdata, ...)
  }

  # Add labels
  if (!is.null(names)) {

    # List
    if (is.list(names)) {
      out <- as.factor(out)
      for (i in names(names)) {
        levels(out)[levels(out) == i] <- names[[i]]
      }

      # Vector
    } else if (is.character(names)) {
      out <- names[as.numeric(out)]
    } else {
      stop("'names' must be a character vector or a list.")
    }
    out <- as.character(out)
  }
  out
}


#' @export
predict.kmeans <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$cluster)
  }

  # compute squared euclidean distance from each sample to each cluster center
  centers <- object$centers
  sumsquares_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x)^2)
  })
  if (is.null(nrow(sumsquares_by_center))) {
    as.vector(which.min(sumsquares_by_center))
  } else {
    as.vector(apply(as.data.frame(sumsquares_by_center), 1, which.min))
  }
}


