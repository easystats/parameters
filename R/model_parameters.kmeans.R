#' Cluster Models (k-means, ...)
#'
#' Format cluster models obtained for example by \code{\link{kmeans}}.
#'
#' @param model Cluster model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' model <- kmeans(iris[1:4], centers = 3)
#' model_parameters(model)
#'
#'
#' @export
model_parameters.kmeans <- function(model, ...){

  params <- cbind(data.frame(Cluster = row.names(model$centers),
                             n_Obs = model$size,
                             Sum_Squares = model$withinss),
                   model$centers)

  # Long means
  means <- .long_loadings(params, loadings_columns = 4:ncol(params))
  means <- means[c("Cluster", "Loading", "Component")]
  names(means) <- c("Cluster", "Mean", "Variable")

  attr(params, "variance") <- model$betweenss / model$totss
  attr(params, "means") <- means
  attr(params, "model") <- model
  attr(params, "iterations") <- model$iter
  attr(params, "scores") <- model$cluster
  attr(params, "type") <- "kmeans"

  class(params) <- c("parameters_clusters", class(params))
  params
}


#' @importFrom insight format_table
#' @export
print.parameters_clusters <- function(x, digits = 2, ...) {
  insight::print_color("# K-means Cluster Means", "blue")

  cat("\n\n")
  insight::print_colour(.text_components_variance(x), "yellow")
  cat("\n\n")

  cat(insight::format_table(x, digits = digits, ...))

  invisible(x)
}


#' @export
summary.parameters_clusters <- function(object, ...){
  object[1:3]
}


#' @export
predict.parameters_clusters <- function(object, newdata = NULL, names = NULL, ...) {
  if (is.null(newdata)) {
    out <- attributes(object)$scores
  } else {
    out <- predict(attributes(object)$model, newdata = newdata, ...)
  }
  out <- as.factor(out)

  # Add labels
  if (!is.null(names)) {

    # List
    if(is.list(names)){
      for(i in names(names)){
        levels(out)[levels(out) == i] <- names[[i]]
      }

    # Vector
    } else if(is.character(names)){
      levels(out) <- names[1:length(levels(out))]
    } else{
      stop("'names' must be a character vector or a list.")
    }

  }
  out
}

