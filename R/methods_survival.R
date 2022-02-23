# classes: .coxph, .aareg, .survreg, .riskRegression


#################### .coxph ------


#' @rdname standard_error
#' @export
standard_error.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error(model, ...))
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
p_value.coxph <- function(model, ...) {
  params <- insight::get_parameters(model)
  stats <- insight::get_statistic(model)

  params <- merge(params, stats, sort = FALSE)
  statistic <- attributes(stats)$statistic

  # convert in case of z
  if (identical(statistic, "z-statistic")) {
    params$Statistic <- params$Statistic^2
  }

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(1 - stats::pchisq(params$Statistic, df = 1))
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





#################### .survreg ------


#' @export
standard_error.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error(model, ...))
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

  if (isTRUE(list(...)$robust) || "vcov" %in% names(list(...))) {
    return(p_value(model, ...))
  }
  s <- summary(model)
  p <- s$table[, "p"]
  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}




#################### .riskRegression ------


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
