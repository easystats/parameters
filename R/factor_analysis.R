#' @rdname principal_components
#' @export
factor_analysis <- function(x,
                            n = "auto",
                            rotation = "none",
                            sort = FALSE,
                            threshold = NULL,
                            standardize = TRUE,
                            cor = NULL,
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
                                       cor = NULL,
                                       ...) {

  # Standardize
  if (standardize && is.null(cor)) {
    x <- as.data.frame(scale(x))
  }

  # N factors
  n <- .get_n_factors(x, n = n, type = "FA", rotation = rotation, cor = cor)

  .factor_analysis_rotate(
    x,
    n,
    rotation = rotation,
    sort = sort,
    threshold = threshold,
    cor = cor,
    ...
  )
}



#' @keywords internal
.factor_analysis_rotate <- function(x,
                                    n,
                                    rotation,
                                    sort = FALSE,
                                    threshold = NULL,
                                    cor = NULL,
                                    ...) {
  if (!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none"))) {
    stop("`rotation` must be one of \"varimax\", \"quartimax\", \"promax\", \"oblimin\", \"simplimax\", \"cluster\" or \"none\".")
  }

  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  # rotate loadings
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = FALSE)
  }

  # Pass cor if available
  if (!is.null(cor)) {
    out <- model_parameters(
      psych::fa(
        cor,
        nfactors = n,
        rotate = rotation,
        n.obs = nrow(x),
        ...
      ),
      sort = sort,
      threshold = threshold
    )
  } else {
    out <- model_parameters(
      psych::fa(x, nfactors = n, rotate = rotation, ...),
      sort = sort,
      threshold = threshold
    )
  }

  attr(out, "data_set") <- x
  out
}
