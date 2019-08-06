#' Principal Component Analysis (PCA)
#'
#' This function performs a principal component analysis (PCA) and returns the loadings (of the unrotated matrix) as dataframe.
#'
#' @param x A dataframe.
#' @param n Number of components to extract. If \code{n = NULL}, the number of components is selected through \code{\link{n_factors}}.
#' @param sort Sort the loadings.
#' @param threshold A value between 0 and 1 indicates which (absolute) values from the loadings should be removed. Can also be "max", in which case it will only display the maximum loading per veriable (the most simple structure).
#' @param standardize A logical value indicating whether the variables should be standardized (centred and scaled) to have unit variance before the analysis takes place (in general, such scaling is advisable).
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \itemize{
#'    \item \strong{Complexity} (Hoffman's, 1978; Pettersson and Turkheimer, 2010) represents the number of latent components needed to account for the observed variables. Whereas a perfect simple structure solution has a complexity of 1 in that each item would only load on one factor, a solution with evenly distributed items has a complexity greater than 1.
#' }
#'
#'
#' @examples
#' library(parameters)
#'
#' \donttest{
#' principal_components(mtcars[, 1:4])
#' }
#' principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#' principal_components(mtcars[, 1:7], n = 2, threshold = "max", sort = TRUE)
#'
#' pca <- principal_components(mtcars[, 1:5], n = 2)
#' summary(pca)
#' predict(pca)
#'
#' @return A data.frame of loadings.
#' @references \itemize{
#'   \item Pettersson, E., \& Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420.
#' }
#' @importFrom stats prcomp
#' @export
principal_components <- function(x, n = NULL, sort = FALSE, threshold = NULL, standardize = TRUE, ...) {
  UseMethod("principal_components")
}

#' @rdname principal_components
#' @export
PCA <- principal_components


#' @importFrom stats prcomp na.omit
#' @export
principal_components.data.frame <- function(x, n = NULL, sort = FALSE, threshold = NULL, standardize = TRUE, ...) {

  # Standardize
  if (standardize) {
    x <- standardize(x, ...)
  }

  # PCA
  model <- stats::prcomp(x, retx = TRUE, center = TRUE, scale. = TRUE, ...)


  # N factors
  if (is.null(n)) {
    n <- as.numeric(n_factors(x, type = "PCA", rotation = "none", ...))
  } else if (n == "all") {
    n <- length(model$sdev)
  } else if (n > length(model$sdev)) {
    n <- length(model$sdev)
  }



  # Re-add centers and scales
  if (standardize) {
    model$center <- attributes(x)$center
    model$scale <- attributes(x)$scale
  }

  # Summary
  eigenvalues <- model$sdev^2
  data_summary <- data_frame(
    Component = sprintf("PC%i", seq_len(length(model$sdev))),
    Eigenvalues = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues),
    Variance_Cumulative = cumsum(eigenvalues / sum(eigenvalues))
  )

  model$sdev <- model$sdev[1:n]
  model$rotation <- model$rotation[, 1:n, drop = FALSE]
  model$x <- model$x[, 1:n, drop = FALSE]
  data_summary <- data_summary[1:n, , drop = FALSE]



  # Compute loadings
  if (length(model$sdev) > 1) {
    loadings <- as.data.frame(model$rotation %*% diag(model$sdev))
  } else {
    loadings <- as.data.frame(model$rotation %*% model$sdev)
  }
  names(loadings) <- data_summary$Component


  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL

  # Add information
  loading_cols <- 2:(n + 1)
  loadings$Complexity <- (apply(loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^2)))^2 / apply(loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^4))

  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- "none"
  attr(loadings, "scores") <- model$x
  attr(loadings, "standardize") <- standardize
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n
  attr(loadings, "type") <- "prcomp"
  attr(loadings, "loadings_columns") <- loading_cols

  # Sorting
  if (sort) {
    loadings <- .sort_loadings(loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filer_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold)

  # add class-attribute for printing
  class(loadings) <- c("parameters_efa", class(loadings))

  loadings
}







#' @export
sort.parameters_efa <- function(x, ...) {
  .sort_loadings(x)
}


#' @export
summary.parameters_efa <- function(object, ...) {
  attributes(object)$summary
}


#' @export
model_parameters.parameters_efa <- function(model, ...) {
  attributes(model)$summary
}


#' @export
predict.parameters_efa <- function(object, newdata = NULL, names = NULL, ...) {
  if (is.null(newdata)) {
    out <- as.data.frame(attributes(object)$scores)
  } else {
    out <- as.data.frame(predict(attributes(object)$model, newdata = newdata, ...))
  }
  if (!is.null(names)) {
    names(out)[1:length(c(names))] <- names
  }
  row.names(out) <- NULL
  out
}


#' @export
print.parameters_efa <- function(x, digits = 2, ...) {
  if(!is.null(attributes(x)$type)){
    insight::print_colour(.text_components_variance(x), "yellow")
    cat("\n\n")
  }
  cat(format_table(x, digits = digits))
}


#' @export
principal_components.lm <- function(x, ...) {
  principal_components(insight::get_predictors(x, ...), ...)
}

#' @export
principal_components.merMod <- principal_components.lm






#' @keywords internal
.text_components_variance <- function(x) {

  type <- attributes(x)$type
  if (type %in% c("prcomp", "principal")) {
    type <- "principal component"
  } else if (type %in% c("fa")) {
    type <- "latent factor"
  } else {
    type <- paste0(type, " component")
  }


  summary <- attributes(x)$summary

  if (nrow(summary) == 1) {
    text <- paste0("The unique ", type)
  } else {
    text <- paste0("The ", nrow(summary), " ", type, "s")
  }

  # rotation
  if (attributes(x)$rotation != "none") {
    text <- paste0(text, " (", attributes(x)$rotation, " rotation)")
  }

  text <- paste0(
    text,
    " accounted for ",
    sprintf("%.2f", max(summary$Variance_Cumulative) * 100),
    "% of the total variance of the original data"
  )

  if (nrow(summary) == 1) {
    text <- paste0(text, ".")
  } else {
    text <- paste0(
      text,
      " (",
      paste0(summary$Component,
        " = ",
        sprintf("%.2f", summary$Variance * 100),
        "%",
        collapse = ", "
      ),
      ")."
    )
  }
  text
}







#' @keywords internal
.sort_loadings <- function(loadings, cols = NULL) {
  if (is.null(cols)) {
    cols <- attributes(loadings)$loadings_columns
  }

  # Remove variable name column
  x <- loadings[, cols, drop = FALSE]
  row.names(x) <- NULL

  # Initialize clusters
  nitems <- nrow(x)
  loads <- data.frame(item = seq(1:nitems), cluster = rep(0, nitems))

  # first sort them into clusters: Find the maximum for each row and assign it to that cluster
  loads$cluster <- apply(abs(x), 1, which.max)
  ord <- sort(loads$cluster, index.return = TRUE)
  x[1:nitems, ] <- x[ord$ix, ]

  rownames(x)[1:nitems] <- rownames(x)[ord$ix]
  total.ord <- ord$ix

  # now sort column wise so that the loadings that have their highest loading on each cluster
  items <- table(loads$cluster) # how many items are in each cluster?
  first <- 1
  item <- loads$item
  for (i in 1:length(items)) {
    if (items[i] > 0) {
      last <- first + items[i] - 1
      ord <- sort(abs(x[first:last, i]), decreasing = TRUE, index.return = TRUE)
      x[first:last, ] <- x[item[ord$ix + first - 1], ]
      loads[first:last, 1] <- item[ord$ix + first - 1]
      rownames(x)[first:last] <- rownames(x)[ord$ix + first - 1]

      total.ord[first:last] <- total.ord[ord$ix + first - 1 ]
      first <- first + items[i]
    }
  }

  order <- row.names(x)
  loadings <- loadings[as.numeric(as.character(order)), ] # Arrange by max
  row.names(loadings) <- NULL

  loadings
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
