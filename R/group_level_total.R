.group_level_total <- function(x, ...) {
  UseMethod(".group_level_total")
}


.group_level_total.glmmTMB <- function(x, ...) {
  params <- suppressWarnings(insight::compact_list(stats::coef(x)))
  params_cond <- params$cond
  params_zi <- params$zi

  # handle random effects in conditional component
  if (!is.null(params_cond)) {
    # extract levels of group factors
    group_levels <- insight::compact_list(lapply(
      x$modelInfo$reTrms$cond$flist,
      levels
    ))
    # extract names of slopes
    slope_names <- insight::compact_list(x$modelInfo$reTrms$cond$cnms)
    # reshape "coef()" data
    params_cond <- .reshape_group_level_coefficients(
      x,
      params = params_cond,
      group_levels = group_levels,
      slope_names = slope_names
    )
    params_cond$Component = "conditional"
  }

  # handle random effects in zero-inflation component
  if (!is.null(params_zi)) {
    # extract levels of group factors
    group_levels <- insight::compact_list(lapply(
      x$modelInfo$reTrms$zi$flist,
      levels
    ))
    # extract names of slopes
    slope_names <- insight::compact_list(x$modelInfo$reTrms$zi$cnms)
    # reshape "coef()" data
    params_zi <- .reshape_group_level_coefficients(
      x,
      params = params_zi,
      group_levels = group_levels,
      slope_names = slope_names,
      component = "zero_inflated_random"
    )
    params_zi$Component = "zero_inflated"
  }

  # create list of data frames
  out <- insight::compact_list(list(params_cond, params_zi))

  if (length(out) == 1) {
    # unlist if only one component
    out <- out[[1]]
  } else {
    # else, join - we can't use rbind() here, because column
    # names do not necessarily match
    out <- datawizard::data_join(out, join = "bind")
  }

  rownames(out) <- NULL
  out
}


# helper ----------------------------------------------------------------------

.reshape_group_level_coefficients <- function(
  x,
  params,
  group_levels,
  slope_names = NULL,
  component = "random"
) {
  group_factors <- insight::find_random(x)
  random_slopes <- insight::find_random_slopes(x)

  # find all columns for which we can add fixed and random effects
  cols <- c(random_slopes[[component]], "(Intercept)")

  # iterate all random effects, add group name and levels
  for (i in group_factors[[component]]) {
    # overwrite cols? if random slopes are factors, the names are
    # not the variable names, but name + factor level, so we need
    # to upate the columns to select here
    if (!is.null(slope_names) && length(slope_names)) {
      cols <- slope_names[[i]]
    }
    # select columns
    params[[i]] <- params[[i]][cols]
    # add information about group factor and levels
    params[[i]]$Group <- i
    params[[i]]$Level <- group_levels[[i]]
  }

  # if only one component, unlist
  if (length(params) == 1) {
    out <- params[[1]]
  } else {
    # else, join - we can't use rbind() here, because column
    # names do not necessarily match
    class(params) <- "list"
    out <- datawizard::data_join(params, join = "bind")
  }

  # make sure first columns are the one to reshape
  out <- datawizard::data_relocate(out, c("Group", "Level"), after = -1)

  # reshape
  to_reshape <- seq_len(ncol(out) - 2)
  out <- datawizard::reshape_longer(out, select = to_reshape)

  # rename
  out <- datawizard::data_rename(
    out,
    select = c(Parameter = "name", Coefficient = "value")
  )
  # remove those without valid values
  out[stats::complete.cases(out), ]
}
