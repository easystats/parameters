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
  means <- datawizard::reshape_longer(params,
    cols = 4:ncol(params),
    values_to = "Mean",
    names_to = "Variable"
  )

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





# factoextra::hkmeans -----------------------------------------------------



#' @rdname model_parameters.kmeans
#' @inheritParams cluster_centers
#'
#' @examples
#' # Hierarchical K-means (factoextra::hkclust) ----------------------
#' if (require("factoextra", quietly = TRUE)) {
#'   data <- iris[1:4]
#'   model <- factoextra::hkmeans(data, k = 3)
#'
#'   rez <- model_parameters(model)
#'   rez
#'
#'   # Get clusters
#'   predict(rez)
#'
#'   # Clusters centers in long form
#'   attributes(rez)$means
#'
#'   # Between and Total Sum of Squares
#'   attributes(rez)$Sum_Squares_Total
#'   attributes(rez)$Sum_Squares_Between
#' }
#' @export
model_parameters.hkmeans <- model_parameters.kmeans



# Methods -------------------------------------------------------------------




#' @export
print.parameters_clusters <- function(x, digits = 2, ...) {
  title <- "# Clustering Solution"
  if ("title" %in% attributes(x)) title <- attributes(x)$title

  insight::print_color(title, "blue")

  cat("\n\n")
  insight::print_colour(.text_components_variance(x), "yellow")
  cat("\n\n")

  cat(insight::export_table(x, digits = digits, ...))

  invisible(x)
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
