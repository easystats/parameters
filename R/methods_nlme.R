# Package nlme; .lme, .gls

############### .lme --------------

#' @export
model_parameters.lme <- model_parameters.merMod


#' @rdname ci.merMod
#' @export
ci.lme <- function(x, ci = .95, method = "wald", ...) {
  method <- tolower(method)
  method <- match.arg(method, choices = c("wald", "residual", "betwithin", "ml1", "satterthwaite"))

  if (method %in% c("wald", "residual")) {
    if (!requireNamespace("nlme", quietly = TRUE)) {
      ci_wald(model = x, ci = ci, dof = degrees_of_freedom(model, method = method))
    } else {
      out <- lapply(ci, function(i) {
        ci_list <- tryCatch(
          {
            nlme::intervals(x, level = i, ...)
          },
          error = function(e) {
            nlme::intervals(x, level = i, which = "fixed", ...)
          }
        )
        .data_frame(
          Parameter = rownames(ci_list$fixed),
          CI = i,
          CI_low = as.vector(ci_list$fixed[, "lower"]),
          CI_high = as.vector(ci_list$fixed[, "upper"])
        )
      })
      .remove_backticks_from_parameter_names(do.call(rbind, out))
    }
    # ml1 approx
  } else if (method == "ml1") {
    ci_ml1(x, ci)

    # betwithin approx
  } else if (method == "betwithin") {
    ci_betwithin(x, ci)

    # Satterthwaite
  } else if (method == "satterthwaite") {
    ci_satterthwaite(x, ci)
  }
}


#' @export
p_value.lme <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 5]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}


#' @export
standard_error.lme <- standard_error.default




############### .gls --------------


#' @export
standard_error.gls <- standard_error.default


#' @export
ci.gls <- ci.biglm


#' @export
p_value.gls <- function(model, ...) {
  cs <- summary(model)$tTable
  p <- cs[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}
