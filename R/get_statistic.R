.get_statistic <- function(model, ...) {
  UseMethod(".get_statistic")
}



.get_statistic.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  stat <- .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}



.get_statistic.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  .filter_component(stat, component)
  attr(stat, "statistic") <- "z"

  stat
}



.get_statistic.default <- function(model, statistic_column = 3, ...) {
  cs <- stats::coef(summary(model))
  cs_names <- dimnames(cs)[[2]]

  out <- data_frame(
    Parameter = gsub("`", "", rownames(cs), fixed = TRUE),
    Statistic = as.vector(cs[, statistic_column])
  )

  if (any(c("t val.", "t", "t-value", "t.value") %in% cs_names))
    attr(out, "statistic") <- "t"
  else if (any(c("z val.", "z", "z-value", "z.value") %in% cs_names))
    attr(out, "statistic") <- "z"
  else
    attr(out, "statistic") <- "statistic"

  out
}
