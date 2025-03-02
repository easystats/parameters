#' @keywords internal
group_level_total <- function(model, ...) {
  UseMethod("group_level_total")
}


#' @keywords internal
group_level_total.glmmTMB <- function(model, ...) {
  params <- suppressWarnings(insight::compact_list(stats::coef(model)))
  params_cond <- params$cond
  params_zi <- params$zi

  if (!is.null(params_cond)) {
    group_levels <- insight::compact_list(lapply(
      model$modelInfo$reTrms$cond$flist,
      levels
    ))
    params_cond <- .reshape_group_level_coefficients(
      model,
      params_cond,
      group_levels = group_levels
    )
    params_cond$Component = "conditional"
  }
  if (!is.null(params_zi)) {
    group_levels <- insight::compact_list(lapply(
      model$modelInfo$reTrms$zi$flist,
      levels
    ))
    params_zi <- .reshape_group_level_coefficients(
      model,
      params_zi,
      group_levels = group_levels,
      component = "zero_inflated_random"
    )
    params_zi$Component = "zero_inflated"
  }

  rbind(params_cond, params_zi)
}


# helper ----------------------------------------------------------------------

.reshape_group_level_coefficients <- function(model,
                                              params,
                                              group_levels,
                                              component = "random") {
  group_factors <- insight::find_random(model)
  random_slopes <- insight::find_random_slopes(model)

  # find all columns for which we can add fixed and random effects
  cols <- c(random_slopes[[component]], "(Intercept)")

  # iterate all random effects, add group name and levels
  for (i in group_factors[[component]]) {
    params[[i]] <- params[[i]][cols]
    params[[i]]$Group <- i
    params[[i]]$Level <- group_levels[[i]]
  }

  out <- do.call(rbind, params)
  datawizard::reshape_longer(out, select = seq_along(cols))
}
