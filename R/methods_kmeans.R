#' @export
model_parameters.kmeans <- function(model, ...) {
  params <- cbind(
    data.frame(
      Cluster = row.names(model$centers),
      n_Obs = model$size,
      Sum_Squares = model$withinss,
      stringsAsFactors = FALSE
    ),
    model$centers
  )

  # Long means
  means <- datawizard::reshape_longer(params,
    select = 4:ncol(params),
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


#' @export
model_parameters.hkmeans <- model_parameters.kmeans



# Methods -------------------------------------------------------------------


#' @export
print.parameters_clusters <- function(x, digits = 2, ...) {
  clusterHeading <- "# Clustering Solution"
  if ("title" %in% attributes(x)) {
    clusterHeading <- attributes(x)$title
  }

  insight::print_color(clusterHeading, "blue")

  cat("\n\n")
  insight::print_colour(.text_components_variance(x), "yellow")
  cat("\n\n")

  cat(insight::export_table(x, digits = digits, ...))

  invisible(x)
}






# Predict -----------------------------------------------------------------


#' Predict method for parameters_clusters objects
#'
#' @export
#' @param names character vector or list
#' @param newdata data.frame
#' @inheritParams stats::predict
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
      insight::format_error("`names` must be a character vector or a list.")
    }
    out <- as.character(out)
  }
  out
}


#' @export
#' @inheritParams stats::predict
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
