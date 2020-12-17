

#' @importFrom insight get_statistic
#' @importFrom stats pnorm
#' @export
p_value.sem <- function(model, ...) {
  if (!.is_semLme(model)) {
    return(NULL)
  }

  stat <- insight::get_statistic(model)
  if (is.null(stat)) {
    return(NULL)
  }

  .data_frame(
    Parameter = stat$Parameter,
    p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  )
}



#' @export
p_value.HLfit <- p_value.cpglmm

#' @export
p_value.rlmerMod <- p_value.cpglmm

#' @export
p_value.merModList <- function(model, ...) {
  dof <- degrees_of_freedom(model)
  p_value_wald(model, dof, ...)
}




#' @importFrom insight find_parameters
#' @rdname p_value.lmerMod
#' @export
p_value.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
    return(NULL)
  }

  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
      Component = i
    )
  })

  p <- do.call(rbind, x)
  .filter_component(p, component)
}



#' @rdname p_value.lmerMod
#' @importFrom insight get_parameters
#' @export
p_value.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "P(>|z|)"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}



#' @importFrom insight get_parameters
#' @export
p_value.glmm <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  s <- summary(model)

  out <- insight::get_parameters(model, effects = "all")
  out$p <- c(s$coefmat[, 4], s$nucoefmat[, 4])
  out <- out[, c("Parameter", "p", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  out
}
