# classes: .coxph, .aareg, .survreg, .riskRegression


#################### .coxph ------


#' @export
ci.coxph <- ci.gamlss


#' @rdname standard_error
#' @export
standard_error.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  params <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))
  se <- cs[, 3]

  # check
  if (length(se) > nrow(params)) {
    se <- se[match(params$Parameter, .remove_backticks_from_string(rownames(cs)))]
  }

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se)
  )
}


#' @export
p_value.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }

  cs <- stats::coef(summary(model))
  p_column <- grep("^(Pr\\(>|p)", colnames(cs))
  p <- cs[, p_column]
  params <- insight::get_parameters(model)

  # check
  if (length(p) > nrow(params)) {
    p <- p[match(params$Parameter, .remove_backticks_from_string(rownames(cs)))]
  }

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p)
  )
}




#################### .aareg ------


#' @export
standard_error.aareg <- function(model, ...) {
  s <- summary(model)
  se <- s$table[, "se(coef)"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
p_value.aareg <- function(model, ...) {
  s <- summary(model)
  p <- s$table[, "p"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


#' @export
ci.aareg <- ci.gamlss




#################### .survreg ------


#' @export
standard_error.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  s <- summary(model)
  se <- s$table[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
p_value.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }
  s <- summary(model)
  p <- s$table[, "p"]
  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}




#################### .riskRegression ------


#' @importFrom stats coef
#' @importFrom utils capture.output
#' @export
standard_error.riskRegression <- function(model, ...) {
  junk <- utils::capture.output(cs <- stats::coef(model))
  .data_frame(
    Parameter = .remove_backticks_from_string(as.vector(cs[, 1])),
    SE = as.numeric(cs[, "StandardError"])
  )
}


#' @export
p_value.riskRegression <- function(model, ...) {
  junk <- utils::capture.output(cs <- stats::coef(model))
  .data_frame(
    Parameter = .remove_backticks_from_string(as.vector(cs[, 1])),
    p = as.numeric(cs[, "Pvalue"])
  )
}
