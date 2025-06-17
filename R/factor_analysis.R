#' @rdname principal_components
#' @export
factor_analysis <- function(x,
                            n = "auto",
                            rotation = "none",
                            sort = FALSE,
                            threshold = NULL,
                            standardize = TRUE,
                            correlation_matrix = NULL,
                            ...) {
  UseMethod("factor_analysis")
}


#' @export
factor_analysis.data.frame <- function(x,
                                       n = "auto",
                                       rotation = "none",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = TRUE,
                                       correlation_matrix = NULL,
                                       ...) {
  # Standardize
  if (standardize && is.null(correlation_matrix)) {
    x <- datawizard::standardize(x, ...)
  }

  # N factors
  n <- .get_n_factors(
    x,
    n = n,
    type = "FA",
    rotation = rotation,
    correlation_matrix = correlation_matrix
  )

  out <- .factor_analysis_rotate(
    x,
    n,
    rotation = rotation,
    correlation_matrix = correlation_matrix,
    ...
  )

  attr(out, "dataset") <- x
  attr(out, "type") <- "fa"
  attr(out, "n") <- n
  attr(out, "sort") <- out
  attr(out, "threshold") <- threshold
  class(out) <- c("psych_efa", class(out))

  out
}


# methods -------------------------

#' @export
print.psych_efa <- function(x,
                            sort = FALSE,
                            threshold = NULL,
                            labels = NULL,
                            ...) {
  if (is.null(threshold)) {
    threshold <- attributes(x)$threshold
  }
  if (is.null(sort)) {
    sort <- attributes(x)$sort
  }
  print(model_parameters(x, sort = sort, threshold = threshold, labels = labels, ...))
}


#' @export
summary.psych_efa <- function(object,
                              sort = FALSE,
                              threshold = NULL,
                              labels = NULL,
                              ...) {
  if (is.null(threshold)) {
    threshold <- attributes(object)$threshold
  }
  if (is.null(sort)) {
    sort <- attributes(object)$sort
  }
  out <- model_parameters(object, sort = sort, threshold = threshold, labels = labels, ...)
  summary(out, ...)
}


#' @export
predict.psych_efa <- function(object,
                              newdata = NULL,
                              names = NULL,
                              keep_na = TRUE,
                              sort = FALSE,
                              threshold = NULL,
                              labels = NULL,
                              verbose = TRUE,
                              ...) {
  if (is.null(threshold)) {
    threshold <- attributes(object)$threshold
  }
  if (is.null(sort)) {
    sort <- attributes(object)$sort
  }
  out <- model_parameters(object, sort = sort, threshold = threshold, labels = labels, ...)
  predict(out, newdata = newdata, names = names, keep_na = keep_na, verbose = verbose, ...)
}


#' @export
convert_efa_to_cfa.psych_efa <- function(model,
                                         threshold = NULL,
                                         names = NULL,
                                         max_per_dimension = NULL,
                                         ...) {
  if (is.null(threshold)) {
    threshold <- attributes(object)$threshold
  }
  sort <- attributes(object)$sort
  out <- model_parameters(object, sort = sort, threshold = threshold, ...)
  convert_efa_to_cfa(out, names = names, max_per_dimension = max_per_dimension, ...)
}


# internals -----------------------

#' @keywords internal
.factor_analysis_rotate <- function(x,
                                    n,
                                    rotation,
                                    correlation_matrix = NULL,
                                    ...) {
  if (!inherits(x, "data.frame")) {
    insight::format_error("`x` must be a data frame.")
  }

  # rotate loadings
  if (!requireNamespace("psych", quietly = TRUE)) {
    insight::format_error(sprintf("Package `psych` required for `%s`-rotation.", rotation))
  }

  # Pass correlation_matrix if available
  if (is.null(correlation_matrix)) {
    out <- psych::fa(x, nfactors = n, rotate = rotation, ...)
  } else {
    out <- psych::fa(
      correlation_matrix,
      nfactors = n,
      rotate = rotation,
      n.obs = nrow(x),
      ...
    )
  }
  out
}
