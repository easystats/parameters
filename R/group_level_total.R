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
    params_cond$Component <- "conditional"
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
    params_zi$Component <- "zero_inflated"
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


.group_level_total.merMod <- function(x, ...) {
  params <- suppressWarnings(stats::coef(x))

  # extract levels of group factors
  group_levels <- insight::compact_list(lapply(methods::slot(x, "flist"), levels))
  # extract names of slopes
  slope_names <- insight::compact_list(methods::slot(x, "cnms"))
  # reshape "coef()" data
  params <- .reshape_group_level_coefficients(
    x,
    params = params,
    group_levels = group_levels,
    slope_names = slope_names
  )

  params
}


.group_level_total.stanreg <- function(x, ...) {
  params <- suppressWarnings(stats::coef(x))

  # extract levels of group factors
  group_levels <- insight::compact_list(lapply(x$glmod$reTrms$flist, levels))
  # extract names of slopes
  slope_names <- insight::compact_list(x$glmod$reTrms$cnms)
  # reshape "coef()" data
  params <- .reshape_group_level_coefficients(
    x,
    params = params,
    group_levels = group_levels,
    slope_names = slope_names
  )

  params
}


.group_level_total.brmsfit <- function(x, ...) {
  # extract random effects information
  group_factors <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
  random_slopes <- insight::find_random_slopes(x)
  params <- NULL

  # create full data frame of all random effects retrieved from coef()
  params <- do.call(
    rbind,
    lapply(group_factors, function(i) {
      # we want the posterior distribution from coef(), so we can
      # use bayestestR
      ranef <- stats::coef(x, summary = FALSE)[[i]]
      parameter_names <- dimnames(ranef)[[3]]
      out <- lapply(
        parameter_names,
        function(pn) {
          # summary of posterior
          d <- bayestestR::describe_posterior(as.data.frame(ranef[,, pn]), verbose = FALSE, ...)
          # add information about group factor and levels
          d$Group <- i
          # Parameters in the returned data frame are actually the levels
          # # from the group factors
          d$Level <- d$Parameter
          # the parameter names can be taken from dimnames
          d$Parameter <- pn
          d
        }
      )
      names(out) <- parameter_names
      do.call(rbind, out)
    })
  )

  # select parameters to keep. We want all intercepts, and all random slopes
  components <- c(
    "sigma", "mu", "nu", "shape", "beta", "phi", "hu", "ndt", "zoi",
    "coi", "kappa", "bias", "bs", "zi", "alpha", "xi"
  )
  # standard components
  parameters_to_keep <- params$Parameter %in% c("Intercept", random_slopes$random)
  parameters_to_keep <- parameters_to_keep |
    params$Parameter %in% c("zi_Intercept", random_slopes$zero_inflated_random)

  # auxiliary components
  for (comp in components) {
    parameters_to_keep <- parameters_to_keep |
      params$Parameter %in% c(paste0(comp, "_Intercept"), random_slopes[[paste0(comp, "_random")]])
  }

  # furthermore, categorical random slopes have levels in their name, so we
  # try to find those parameters here, too
  if (!is.null(random_slopes$random)) {
    parameters_to_keep <- parameters_to_keep |
      startsWith(params$Parameter, random_slopes$random)
  }
  if (!is.null(random_slopes$zero_inflated_random)) {
    parameters_to_keep <- parameters_to_keep |
      startsWith(params$Parameter, paste0("zi_", random_slopes$zero_inflated_random))
  }
  # auxiliary components
  for (comp in components) {
    rc <- paste0(comp, "_random")
    if (!is.null(random_slopes[[rc]])) {
      parameters_to_keep <- parameters_to_keep |
        startsWith(params$Parameter, paste0(comp, "_", random_slopes[[rc]]))
    }
  }

  # add Component column
  params$Component <- "conditional"
  params$Component[startsWith(params$Parameter, "zi_")] <- "zero_inflated"
  for (comp in components) {
    params$Component[startsWith(params$Parameter, paste0(comp, "_"))] <- comp
  }

  # clean names
  params$Parameter <- gsub("^zi_", "", params$Parameter)
  for (comp in components) {
    params$Parameter <- gsub(paste0("^", comp, "_"), "", params$Parameter)
  }
  rownames(params) <- NULL

  # make sure first columns are group and level
  datawizard::data_relocate(params[parameters_to_keep, ], c("Group", "Level"))
}


# helper ----------------------------------------------------------------------

.reshape_group_level_coefficients <- function(x,
                                              params,
                                              group_levels,
                                              slope_names = NULL,
                                              component = "random") {
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

  # reshape
  to_reshape <- setdiff(colnames(out), c("Group", "Level"))
  out <- datawizard::reshape_longer(out, select = to_reshape)

  # rename
  out <- datawizard::data_rename(
    out,
    select = c(Parameter = "name", Coefficient = "value")
  )

  # make sure first columns are group and level
  out <- datawizard::data_relocate(out, c("Group", "Level"))

  # remove those without valid values
  out[stats::complete.cases(out), ]
}
