#' FactoMineR objects Parameters
#'
#' Parameters of FactoMineR objects.
#'
#' @param model Object of class \code{FactoMineR}.
#' @inheritParams model_parameters.principal
#'
#' @examples
#' library(parameters)
#' library(FactoMineR)
#'
#' model <- FactoMineR::PCA(iris[, 1:4], ncp = 2)
#' model_parameters(model)
#' attributes(model_parameters(model))$scores
#'
#' model <- FactoMineR::FAMD(iris, ncp = 2)
#' model_parameters(model)
#'
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters.PCA <- function(model, sort = FALSE, threshold = NULL, labels = NULL, ...) {

  loadings <- as.data.frame(model$var$coord)
  n <- model$call$ncp


  # Get summary
  eig <- as.data.frame(model$eig[1:n, ])
  data_summary <- data_frame(
    Component = names(loadings),
    Eigenvalues = eig$eigenvalue,
    Variance = eig$`percentage of variance` / 100,
    Variance_Cumulative = eig$`cumulative percentage of variance` / 100
  )
  data_summary$Variance_Proportion <- data_summary$Variance / sum(data_summary$Variance)


  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL

  # Labels
  if(!is.null(labels)){
    loadings$Label <- labels
    loadings <- loadings[c("Variable", "Label", names(loadings)[!names(loadings) %in% c("Variable", "Label")])]
    loading_cols <- 3:(n + 2)
  } else{
    loading_cols <- 2:(n + 1)
  }


  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- "none"
  attr(loadings, "scores") <- as.data.frame(model$ind$coord)
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n
  attr(loadings, "loadings_columns") <- loading_cols

  # Sorting
  if (sort) {
    loadings <- .sort_loadings(loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filter_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold, loadings_columns = loading_cols)


  # add class-attribute for printing
  if("PCA" %in% class(model)){
    attr(loadings, "type") <- "pca"
    class(loadings) <- c("parameters_pca", class(loadings))
  } else if("FAMD" %in% class(model)){
    attr(loadings, "type") <- "fa"
    class(loadings) <- c("parameters_efa", class(loadings))
  }

  loadings
}


#' @export
model_parameters.FAMD <- model_parameters.PCA