#' @export
standard_error.systemfit <- function(model, ...) {
  cf <- stats::coef(summary(model))
  f <- insight::find_formula(model)

  system_names <- names(f)
  parameter_names <- row.names(cf)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, parameter_names)
    data.frame(
      Parameter = gsub(pattern, "\\1", parameter_names[params]),
      SE = as.vector(cf[params, 2]),
      Component = i,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}



#' @export
p_value.systemfit <- function(model, ...) {
  cf <- stats::coef(summary(model))
  f <- insight::find_formula(model)

  system_names <- names(f)
  parameter_names <- row.names(cf)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, parameter_names)
    data.frame(
      Parameter = gsub(pattern, "\\1", parameter_names[params]),
      p = as.vector(cf[params, 4]),
      Component = i,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}
