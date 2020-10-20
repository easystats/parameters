#' p-values for Models with Zero-Inflation
#'
#' This function attempts to return, or compute, p-values of hurdle and zero-inflated models.
#'
#' @param model A statistical model.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @examples
#' if (require("pscl")) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5| kid5 + phd, data = bioChemists)
#'   p_value(model)
#'   p_value(model, component = "zi")
#' }
#' @importFrom stats coef
#' @importFrom insight find_parameters
#' @export
p_value.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), method = NULL, verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      p = as.vector(stats[, 4]),
      Component = comp
    )
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")

  .filter_component(p, component)
}


#' @export
p_value.hurdle <- p_value.zeroinfl

#' @export
p_value.zerocount <- p_value.zeroinfl


#' @rdname p_value.zeroinfl
#' @importFrom utils capture.output
#' @export
p_value.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    p = as.vector(stats$tweedie[, "Pr(>|z|)"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    p = as.vector(stats$zero[, "Pr(>|z|)"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}
