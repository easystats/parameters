#' @rdname principal_components
#' @export
factor_analysis <- function(x, ...) {
  UseMethod("factor_analysis")
}


#' @rdname principal_components
#' @export
factor_analysis.data.frame <- function(x,
                                       n = "auto",
                                       rotation = "oblimin",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = FALSE,
                                       ...) {
  insight::check_if_installed("psych")

  # Standardize
  if (standardize) {
    x <- datawizard::standardize(x, ...)
  }

  # N factors
  n <- .get_n_factors(
    x,
    n = n,
    type = "FA",
    rotation = rotation
  )

  # FA
  out <- model_parameters(
    psych::fa(x, nfactors = n, rotate = rotation, ...),
    threshold = threshold,
    sort = sort,
    ...
  )

  attr(out, "dataset") <- x
  out
}


#' @rdname principal_components
#' @export
factor_analysis.matrix <- function(x,
                                   n = "auto",
                                   rotation = "oblimin",
                                   n_obs = NULL,
                                   n_matrix = NULL,
                                   sort = FALSE,
                                   threshold = NULL,
                                   standardize = FALSE,
                                   ...) {
  # check if we have a square matrix. in this case, we assume that
  # the user wants to do a factor analysis on the correlation matrix
  if ((dim(x)[1] == dim(x)[2]) && is.null(n_obs)) {
    insight::format_error(
      "You provided a square matrix, which is assumed to be a correlation matrix. Please specify the number of observations with `n_obs`. If your matrix is not a correlation matrix, please provide a data frame instead."
    )
  }

  # the default n.obs argument in `psych::fa()` is `NA`, so we change
  # our default `NULL` to `NA` to avoid errors
  if (is.null(n_obs)) {
    n_obs <- NA
  }

  factor_analysis.data.frame(
    as.data.frame(x),
    n = n,
    rotation = rotation,
    sort = sort,
    threshold = threshold,
    standardize = standardize,
    n.obs = n_obs,
    np.obs = n_matrix,
    ...
  )
}


.is_oblique_rotation <- function(rotation) {
  !is.null(rotation) && tolower(rotation) %in% c("promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster") # nolint
}
