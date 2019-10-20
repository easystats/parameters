#' @rdname p_value_kenward
#' @export
se_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  vcov_adj <- pbkrtest::vcovAdj(model)

  params <- insight::get_parameters(model, effects = "fixed")
  le <- nrow(params)
  Lmat <- diag(rep(1, le))

  se <- sapply(1:le, function(i) sqrt(.qform(Lmat[i, ], as.matrix(vcov_adj))))
  names(se) <- params[[1]]

  se
}

.qform <- function(x, A) sum(x * (A %*% x))
