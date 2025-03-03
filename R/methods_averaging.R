# classes: .averaging

#################### .averaging

#' @export
model_parameters.averaging <- function(model,
                                       ci = 0.95,
                                       component = "conditional",
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       include_info = getOption("parameters_info", FALSE),
                                       keep = NULL,
                                       drop = NULL,
                                       verbose = TRUE,
                                       ...) {
  component <- insight::validate_argument(component, c("conditional", "full"))

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    merge_by = "Parameter",
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.averaging <- function(model, component = "conditional", ...) {
  component <- insight::validate_argument(component, c("conditional", "full"))
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = as.vector(s[, 3])
  )
}


#' @export
p_value.averaging <- function(model, component = "conditional", ...) {
  component <- insight::validate_argument(component, c("conditional", "full"))
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }

  # to data frame
  s <- as.data.frame(s)

  # do we have a p-value column based on t?
  pvcn <- which(colnames(s) == "Pr(>|t|)")
  # if not, do we have a p-value column based on z?
  if (length(pvcn) == 0) {
    pvcn <- which(colnames(s) == "Pr(>|z|)")
  }
  # if not, default to ncol
  if (length(pvcn) == 0) {
    if (ncol(s) > 4) {
      pvcn <- 5
    } else {
      pvcn <- 4
    }
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = as.vector(s[, pvcn])
  )
}


#' @export
ci.averaging <- function(x, ci = 0.95, component = "conditional", ...) {
  component <- insight::validate_argument(component, c("conditional", "full"))
  .ci_generic(model = x, ci = ci, dof = Inf, component = component)
}
