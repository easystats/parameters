#' @importFrom insight find_parameters
#' @importFrom stats vcov
.get_varcov <- function(model, component) {
  if (inherits(model, c("hurdle", "zeroinfl", "zerocount"))) {
    vc <- switch(
      component,
      "conditional" = stats::vcov(object = model, model = "count"),
      "zero_inflated" = stats::vcov(object = model, model = "zero"),
      stats::vcov(object = model)
    )
  } else if (inherits(model, "MixMod")) {
    vc <- switch(
      component,
      "conditional" = stats::vcov(model, parm = "fixed-effects"),
      "zero_inflated" = stats::vcov(model, parm = "zero_part"),
      stats::vcov(model)
    )
  } else if (inherits(model, "feis")) {
    vc <- model$vcov
  } else if (inherits(model, "glimML")) {
    if (!requireNamespace("aod", quietly = TRUE)) {
      stop("Package 'aod' required for this function to work. Please install it.")
    }
    vc <- aod::vcov(model)
  } else if (inherits(model, c("vglm", "vgam"))) {
    if (!requireNamespace("VGAM", quietly = TRUE)) {
      stop("Package 'VGAM' required for this function to work. Please install it.")
    }
    vc <- VGAM::vcov(model)
  } else if (inherits(model, "tobit")) {
    coef_names <- insight::find_parameters(model, flatten = TRUE)
    vc <- stats::vcov(model)[coef_names, coef_names]
  } else if (inherits(model, "geeglm")) {
    vc <- summary(model)$cov.unscaled
  } else if (inherits(model, c("lmRob", "glmRob"))) {
    vc <- model$cov
  } else if (inherits(model, c("gee", "LORgee"))) {
    vc <- model$naive.variance
  } else {
    vc <- suppressWarnings(stats::vcov(model))
    if (is.list(vc)) {
      vc <- switch(
        component,
        "conditional" = vc[["cond"]],
        "zero_inflated" = vc[["zi"]],
        vc[[1]]
      )
    }
  }

  if (.is_negativ_matrix(vc)) {
    vc <- .fix_negative_matrix(vc)
  }
  vc
}



.is_negativ_matrix <- function(x) {
  if (is.matrix(x) && (nrow(x) == ncol(x))) {
    eigenvalues <- eigen(x, only.values = TRUE)$values
    eigenvalues[abs(eigenvalues) < 1e-07] <- 0
    rv <- any(eigenvalues <= 0)
  } else {
    rv <- FALSE
  }

  rv
}



.fix_negative_matrix <- function(m) {
  if (requireNamespace("Matrix", quietly = TRUE)) {
    as.matrix(Matrix::nearPD(m)$mat)
  } else {
    m
  }
}
