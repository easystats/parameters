#' Principal Components Analysis (PCA)
#'
#' This function performs a principal component analysis (PCA) and returns the loadings (of the unrotated matrix) as dataframe.
#'
#' @param x A dataframe.
#' @param n Number of components to extract. If \code{n = NULL}, the number of components is selected through \code{\link{n_factors}}.
#' @param threshold A value between 0 and 1 indicating which (absolute) values from the loadings should be removed.
#' @param standardize A logical value indicating whether the variables should be standardized (centred and scaled) to have unit variance before the analysis takes place (in general, such scaling is advisable).
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(parameters)
#'
#' x <- mtcars[, 1:7]
#' principal_components(x)
#' principal_components(x, n = "all", threshold = 0.2)
#'
#' @importFrom stats prcomp
#' @export
principal_components <- function(x, n = NULL, threshold = NULL, standardize = TRUE, ...) {
  UseMethod("principal_components")
}

#' @rdname principal_components
#' @export
PCA <- principal_components


#' @importFrom stats prcomp na.omit
#' @export
principal_components.data.frame <- function(x, n = NULL, threshold = NULL, standardize = TRUE, ...) {

  # Standardize
  if(standardize){
    x <- standardize(x, ...)
  }

  # PCA
  pca <- stats::prcomp(x, retx = TRUE, center = FALSE, scale. = FALSE, ...)


  # N factors
  if(is.null(n)){
    n <- as.numeric(n_factors(x, type = "PCA", rotation = "none", ...))
  } else if(n == "all"){
    n <- length(pca$sdev)
  } else if(n > length(pca$sdev)){
    n <- length(pca$sdev)
  }



  # Re-add centers and scales
  if(standardize){
    pca$center <- attributes(x)$center
    pca$scale <- attributes(x)$scale
  }

  # Summary
  eigenvalues <- pca$sdev^2
  data_summary <- data_frame(
    Component = sprintf("PC%i", seq_len(length(pca$sdev))),
    SD = pca$sdev,
    Eigenvalues = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues),
    Variance_Cumulative = cumsum(eigenvalues / sum(eigenvalues))
  )

  pca$sdev <-  pca$sdev[1:n]
  pca$rotation <- pca$rotation[, 1:n]
  pca$x <- pca$x[, 1:n]
  data_summary <- data_summary[1:n, ]



  # Compute loadings
  loadings <- as.data.frame(pca$rotation %*% diag(pca$sdev))
  names(loadings) <- data_summary$Component

  # Best representation (max loading)
  rowmax_index <- sapply(as.data.frame(t(loadings)), function(x) which.max(abs(x)))
  rowmax <- sapply(as.data.frame(t(loadings)), function(x) x[which.max(abs(x))])
  loadings_max <- data_frame(Component = names(loadings)[rowmax_index], Loading = rowmax)

  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL
  loadings_max <- cbind(data.frame(Variable = row.names(loadings_max)), loadings_max)
  row.names(loadings_max) <- NULL



  attr(loadings, "summary") <- data_summary
  attr(loadings, "pca") <- pca
  attr(loadings, "scores") <- pca$x
  attr(loadings, "loadings_max") <- attr(loadings, "scores") <- pca$x
  attr(loadings, "n") <- n

  # Replace by NA all cells below threshold
  if(!is.null(threshold)){
    loadings[, sapply(loadings, is.numeric)][abs(loadings[, sapply(loadings, is.numeric)]) < threshold] <- NA
  }

  # add class-attribute for printing
  class(loadings) <- c("PCA", class(loadings))

  loadings
}


#' @export
summary.PCA <- function(object, ...){
  attributes(object)$summary
}

#' @export
model_parameters.PCA <- function(model, ...){
  attributes(model)$summary
}

#' @export
predict.PCA <- function(object, ...){
  predict(attributes(object)$pca, ...)
}

#' @export
print.PCA <- function(x, ...){
  cat(.text_components_variance(x, type = "principal component"))
  cat("\n\n")
  print(format(as.data.frame(x), ...))
}










#' @keywords internal
.text_components_variance <- function(x, type = "principal component") {

  summary <- attributes(x)$summary

  if (nrow(summary) == 1) {
    text <- paste0("The unique ", type)
  } else{
    text <- paste0("The ", nrow(summary), " ", type, "s")
  }

  text <- paste0(text,
                 " accounted for ",
                 sprintf("%.2f", max(summary$Variance_Cumulative) * 100),
                 "% of the total variance")

  if (nrow(summary) == 1) {
    text <- paste0(text, ".")
  } else{
    text <- paste0(text,
                   " (",
                   paste0(summary$Components,
                          " = ",
                          sprintf("%.2f", summary$Variance * 100),
                          "%", collapse = ", "),
                   ").")
  }
  text
}







# #' @keywords internal
# .pca_rotate <- function(x, rotation, n) {
#   if (!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none"))) {
#     stop("`rotation` must be one of \"varimax\", \"quartimax\", \"promax\", \"oblimin\", \"simplimax\", \"cluster\" or \"none\".")
#   }
#
#   if (!inherits(x, c("prcomp", "data.frame"))) {
#     stop("`x` must be of class `prcomp` or a data frame.", call. = FALSE)
#   }
#
#   if (!inherits(x, "data.frame") && rotation != "varimax") {
#     stop(sprintf("`x` must be a data frame for `%s`-rotation.", rotation), call. = FALSE)
#   }
#
#   # rotate loadings
#
#   if (rotation != "varimax") {
#     if (!requireNamespace("psych", quietly = TRUE)) {
#       stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = FALSE)
#     }
#
#     tmp <- psych::principal(r = x, n_compactors = n, rotate = rotation)
#   } else {
#     if (!inherits(x, "pca")) {
#       x <- .pca(x)
#     }
#
#     loadings <- attr(x, "loadings", exact = TRUE)
#     if (is.null(n)) n <- attr(x, "kaiser", exact = TRUE)
#
#     if (n < 2) {
#       stop("Can't rotate loadings, too few components extracted.", call. = FALSE)
#     }
#
#     tmp <- stats::varimax(loadings[, seq_len(n)])
#   }
#
#
#   # tweak column names and class attributes
#
#   tmp <- as.data.frame(unclass(tmp$loadings))
#   colnames(tmp) <- sprintf("PC%i", 1:ncol(tmp))
#   class(tmp) <- c("perf_pca_rotate", "data.frame")
#
#
#   # add explained proportions and proportional and cumulative variance
#
#   .prop.var <- colSums(tmp^2) / nrow(tmp)
#   .cum.var <- cumsum(.prop.var)
#   .prop.exp <- .prop.var / sum(.prop.var)
#   .cum.exp <- cumsum(.prop.exp)
#
#   attr(tmp, "rotation") <- rotation
#
#   attr(tmp, "variance") <- data.frame(
#     prop.var = .prop.var,
#     cum.var = .cum.var,
#     prop.exp = .prop.exp,
#     cum.exp = .cum.exp,
#     stringsAsFactors = FALSE
#   )
#
#   tmp
# }
