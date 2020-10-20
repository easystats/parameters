#' p-values for Models with Special Components
#'
#' This function attempts to return, or compute, p-values of models with special model components.
#'
#' @param model A statistical model.
#' @param component Should all parameters, parameters for the conditional model, precision- or scale-component or smooth_terms be returned? \code{component} may be one of \code{"conditional"}, \code{"precision"}, \code{"scale"}, \code{"smooth_terms"}, \code{"full"} or \code{"all"} (default).
#' @inheritParams p_value
#'
#' @return The p-values.
#' @importFrom stats pnorm
#' @importFrom insight get_parameters
#' @export
p_value.DirichletRegModel <- function(model, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$Parameter,
    Response = params$Response,
    p = as.vector(2 * stats::pnorm(-abs(params$Estimate / model$se)))
  )

  if (!is.null(params$Component)) {
    out$Component <- params$Component
  } else {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}



#' @importFrom stats coef
#' @importFrom insight get_parameters
#' @rdname p_value.DirichletRegModel
#' @export
p_value.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))
  p <- cs[, 4]

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}

#' @export
p_value.clmm2 <- p_value.clm2



#' @importFrom insight get_parameters
#' @rdname p_value.DirichletRegModel
#' @export
p_value.cgam <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model, component = "all")
  cs <- summary(model)
  p <- as.vector(cs$coefficients[, 4])
  if (!is.null(cs$coefficients2)) p <- c(p, as.vector(cs$coefficients2[, "p.value"]))

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}



#' @importFrom insight get_parameters
#' @rdname p_value.DirichletRegModel
#' @export
p_value.averaging <- function(model, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = as.vector(s[, 5])
  )
}
