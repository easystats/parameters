#' @keywords emmeans_methods
emm_basis.bootstrap_model <- function(object, trms, xlev, grid, ...) {
  insight::check_if_installed("emmeans")

  model <- attr(object, "original_model")
  emb <- emmeans::emm_basis(model, trms, xlev, grid, ...)

  if (ncol(object) != ncol(emb$V) || !all(colnames(object) == colnames(emb$V))) {
    insight::format_error(
      "Oops! Cannot create the reference grid. Please open an issue at {.url https://github.com/easystats/parameters/issues}."
    )
  }

  emb$post.beta <- as.matrix(object)
  emb$misc$is_boot <- TRUE
  emb
}

#' @keywords emmeans_methods
recover_data.bootstrap_model <- function(object, ...) {
  insight::check_if_installed("emmeans")
  model <- attr(object, "original_model")
  emmeans::recover_data(model, ...)
}


#' @keywords emmeans_methods
emm_basis.bootstrap_parameters <- function(object, trms, xlev, grid, ...) {
  insight::check_if_installed("emmeans")
  model <- attr(object, "boot_samples")
  emmeans::emm_basis(model, trms, xlev, grid, ...)
}

#' @keywords emmeans_methods
recover_data.bootstrap_parameters <- function(object, ...) {
  insight::check_if_installed("emmeans")
  model <- attr(object, "boot_samples")
  emmeans::recover_data(model, ...)
}
