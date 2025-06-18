#' @rdname principal_components
#' @export
factor_analysis <- function(x, ...) {
  UseMethod("factor_analysis")
}


#' @rdname principal_components
#' @export
factor_analysis.data.frame <- function(x,
                                       n = "auto",
                                       rotation = "none",
                                       sort = FALSE,
                                       threshold = NULL,
                                       standardize = TRUE,
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


.is_oblique_rotation <- function(rotation) {
  !is.null(rotation) && rotation %in% c("Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster") # nolint
}
