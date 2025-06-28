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
                                       factor_method = "minres",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = FALSE,
                                       reverse_items = NULL,
                                       verbose = TRUE,
                                       ...) {
  insight::check_if_installed("psych")

  # should some items be reversed?
  if (!is.null(reverse_items)) {
    # only works for data frames, not matrices
    if (!is.data.frame(x)) {
      insight::format_error(
        "The `reverse_items` argument only works with data frames, not matrices."
      )
    }
    # numeric indices should be replaced by their column names
    if (is.numeric(reverse_items)) {
      reverse_items <- colnames(x)[reverse_items]
    }
    if (verbose) {
      insight::format_alert(paste("Reversing items:", toString(reverse_items)))
    }
    # reverse the items
    x <- datawizard::reverse_scale(x, reverse_items, verbose = verbose)
  }

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
    psych::fa(x, nfactors = n, rotate = rotation, fm = factor_method, ...),
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
                                   factor_method = "minres",
                                   n_obs = NULL,
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
  n_matrix <- NULL
  if (is.null(n_obs)) {
    n_obs <- NA
  } else if (is.matrix(n_obs)) {
    n_matrix <- n_obs
    n_obs <- NA
    # check for correct dimensions
    if (dim(n_matrix)[1] != dim(x)[1] || dim(n_matrix)[2] != dim(x)[2]) {
      insight::format_error(
        "The provided `n_obs` matrix must have the same dimensions as the input matrix."
      )
    }
  }

  factor_analysis.data.frame(
    x,
    n = n,
    rotation = rotation,
    factor_method = factor_method,
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
