# #' @export
# p_value.hglm <- function(model, ...) {
#   stat <- insight::get_statistic(model)
#   .data_frame(
#     Parameter = stat$Parameter,
#     p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
#   )
# }

# #' @export
# ci.hglm <- function(x, ci = 0.95, ...) {
#   .ci_generic(model = x, ci = ci, ...)
# }

#' @export
model_parameters.hglm <- function(model,
                                  ci = 0.95,
                                  ci_method = NULL,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  exponentiate = FALSE,
                                  effects = "all",
                                  component = "all",
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  # which components to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  component <- match.arg(component, choices = c("all", "conditional", "zi", "zero_inflated", "dispersion"))

  mp <- model_parameters.default(
    model, ci = ci, ci_method = ci_method, bootstrap = bootstrap,
    effects = "fixed", component = component, iterations = iterations,
    exponentiate = exponentiate, p_adjust = p_adjust, summary = summary,
    keep = keep, drop = drop, verbose = verbose, ...
  )

  mp
}

#' @export
standard_error.hglm <- function(model,
                                effects = "fixed",
                                component = "conditional",
                                ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", "conditional", "dispersion"))

  f <- insight::find_formula(model)
  if (component == "dispersion" && is.null(f$dispersion)) {
    insight::format_warning("No standard errors found for model's dispersion parameters.")
    return(NULL)
  }
  s <- summary(model)

  if (effects == "fixed") {
    se <- s$FixCoefMat
  } else if (effects == "random") {
    se <- s$RandCoefMat
  } else {
    se <- c(s$FixCoefMat, s$RandCoefMat)
  }
  out <- .data_frame(
    Parameter = row.names(se),
    SE = as.vector(se[, 2])
  )

  if (effects != "random" && component != "conditional") {
    se <- s$SummVC1
    out$Component <- "conditional"
    out <- rbind(out, .data_frame(
      Parameter = row.names(se),
      SE = as.vector(se[, 2]),
      Component = "dispersion"
    ))
  }
  out
}


#' @export
degrees_of_freedom.hglm <- function(model, method = "residual", ...) {
  if (method == "any") {
    method <- "residual"
  }
  insight::get_df(model, type = method, ...)
}
