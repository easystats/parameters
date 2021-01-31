#' @keywords emmeans_methods
emm_basis.bootstrap_model <- function(object, trms, xlev, grid, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE))
    stop("this function requires the 'emmeans' package to work.")

  model <- attr(object, "original_model")
  emb <- emmeans::emm_basis(model, trms, xlev, grid, ...)

  if (ncol(object) != ncol(emb$V) || !all(colnames(object) == colnames(emb$V)))
    stop("Oops! Cannot create the reference grid. Please open an issue at github.com/easystats/parameters/issues.")

  emb$post.beta <- as.matrix(object)
  emb
}

#' @keywords emmeans_methods
recover_data.bootstrap_model <- function(object, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE))
    stop("this function requires the 'emmeans' package to work.")

  model <- attr(object, "original_model")
  emmeans::recover_data(model, ...)
}


#' @keywords emmeans_methods
emm_basis.bootstrap_parameters <- function(object, trms, xlev, grid, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE))
    stop("this function requires the 'emmeans' package to work.")

  model <- attr(object, "boot_samples")
  emmeans::emm_basis(model, trms, xlev, grid, ...)
}

#' @keywords emmeans_methods
recover_data.bootstrap_parameters <- function(object, ...) {
  if (!requireNamespace("emmeans", quietly = TRUE))
    stop("this function requires the 'emmeans' package to work.")

  model <- attr(object, "boot_samples")
  emmeans::recover_data(model, ...)
}
