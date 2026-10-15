#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param type Character string specifying the type of bootstrap. For mixed models
#'   of class `merMod` or `glmmTMB`, may be `"parametric"` (default). For
#'   `merMod` models only, may also be `"semiparametric"` (see `?lme4::bootMer`
#'   for details). Semiparametric bootstrapping resamples the residuals and keeps
#'   the random effects at their estimated values (`use.u = TRUE` in
#'   `lme4::bootMer()`). For all other models, see argument `sim` in
#'   `?boot::boot` (defaults to `"ordinary"`). For other mixed models,
#'   `"ordinary"` and `"balanced"` resample whole clusters (see 'Details'), and
#'   `"permutation"` and `"antithetic"` are not available.
#' @param parallel The type of parallel operation to be used (if any).
#' @param n_cpus Number of processes to be used in parallel operation.
#' @param cluster Optional cluster when `parallel = "snow"`. See `?lme4::bootMer`
#' or `?boot::boot` for details.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams p_value
#'
#' @return A data frame of bootstrapped estimates.
#'
#' @details By default, `boot::boot()` is used to generate bootstraps from
#' the model data, which are then used to `update()` the model, i.e. refit
#' the model with the bootstrapped samples. For `merMod` objects (**lme4**)
#' or models from **glmmTMB**, the `lme4::bootMer()` function is used to
#' obtain bootstrapped samples. `bootstrap_parameters()` summarizes the
#' bootstrapped model estimates.
#'
#' Other mixed models (for example from **ordinal**, **cplm** or **nlme**) use
#' a cluster bootstrap when `type` is `"ordinary"` or `"balanced"`: each sample
#' draws whole clusters (the levels of the grouping factor) with replacement,
#' not single rows. A cluster that is drawn twice counts as two clusters. If the
#' model has several grouping factors that are all nested in one factor, the
#' clusters are the levels of that factor. Grouping factors that are not nested
#' in one factor give an error. With few clusters, the cluster bootstrap has few
#' independent units to resample, and its standard errors can be too small.
#'
#' @section Using with **emmeans**:
#' The output can be passed directly to the various functions from the
#' **emmeans** package, to obtain bootstrapped estimates, contrasts, simple
#' slopes, etc. and their confidence intervals. These can then be passed to
#' `model_parameter()` to obtain standard errors, p-values, etc. (see
#' example).
#'
#' Note that that p-values returned here are estimated under the assumption of
#' *translation equivariance*: that shape of the sampling distribution is
#' unaffected by the null being true or not. If this assumption does not hold,
#' p-values can be biased, and it is suggested to use proper permutation tests
#' to obtain non-parametric p-values.
#'
#' @seealso [`bootstrap_parameters()`], [`simulate_model()`], [`simulate_parameters()`]
#'
#' @examplesIf require("boot", quietly = TRUE) && require("emmeans", quietly = TRUE)
#' \donttest{
#' model <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#' b <- bootstrap_model(model)
#' print(head(b))
#'
#' est <- emmeans::emmeans(b, consec ~ cyl)
#' print(model_parameters(est))
#' }
#' @export
bootstrap_model <- function(model, iterations = 1000, ...) {
  UseMethod("bootstrap_model")
}


#' @rdname bootstrap_model
#' @export
bootstrap_model.default <- function(
  model,
  iterations = 1000,
  type = "ordinary",
  parallel = "no",
  n_cpus = 1,
  cluster = NULL,
  verbose = FALSE,
  ...
) {
  # check for valid input
  .is_model_valid(model)

  insight::check_if_installed("boot")

  type <- insight::validate_argument(
    type,
    c("ordinary", "parametric", "balanced", "permutation", "antithetic")
  )
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  # mixed models resample whole clusters, not single rows
  if (type != "parametric" && insight::is_mixed_model(model)) {
    return(.bootstrap_clusters(
      model,
      iterations = iterations,
      type = type,
      parallel = parallel,
      n_cpus = n_cpus,
      cluster = cluster,
      verbose = verbose
    ))
  }

  model_data <- data <- insight::get_data(model, verbose = FALSE) # nolint
  model_response <- insight::find_response(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, , drop = FALSE] # allows boot to select sample

    if (inherits(model, "biglm")) {
      fit <- suppressMessages(stats::update(model, moredata = d))
    } else if (verbose) {
      fit <- stats::update(model, data = d)
    } else {
      fit <- suppressMessages(stats::update(model, data = d))
    }

    params <- insight::get_parameters(fit, verbose = FALSE)
    n_params <- insight::n_parameters(model)

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }

    params
  }

  if (type == "parametric") {
    f <- function(x, mle) {
      out <- model_data
      resp <- stats::simulate(x, nsim = 1)
      out[[model_response]] <- resp
      out
    }
    results <- boot::boot(
      data = data,
      statistic = boot_function,
      R = iterations,
      sim = type,
      parallel = parallel,
      ncpus = n_cpus,
      cl = cluster,
      model = model,
      ran.gen = f
    )
  } else {
    results <- boot::boot(
      data = data,
      statistic = boot_function,
      R = iterations,
      sim = type,
      parallel = parallel,
      ncpus = n_cpus,
      cl = cluster,
      model = model
    )
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::get_parameters(model, verbose = FALSE)$Parameter

  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


# cluster bootstrap for mixed models ------------------------------------------

.bootstrap_clusters <- function(
  model,
  iterations = 1000,
  type = "ordinary",
  parallel = "no",
  n_cpus = 1,
  cluster = NULL,
  verbose = FALSE
) {
  if (type %in% c("permutation", "antithetic")) {
    insight::format_error(paste0(
      "`type = \"",
      type,
      "\"` is not available for mixed models, because their bootstrap resamples",
      " whole clusters. Use `type = \"ordinary\"` or `type = \"balanced\"`."
    ))
  }

  model_data <- insight::get_data(model, verbose = FALSE)
  groups <- .bootstrap_cluster_groups(model, model_data)

  # rows of each cluster of the top grouping factor, in data order
  top <- as.character(model_data[[groups$top]])
  clusters <- unique(top)
  cluster_rows <- split(seq_len(nrow(model_data)), factor(top, levels = clusters))
  n_params <- insight::n_parameters(model)

  # stack the drawn clusters in draw order, and label each by its draw
  # position, so that a cluster drawn twice counts as two clusters
  refit <- function(indices, catch = TRUE) {
    pieces <- lapply(seq_along(indices), function(i) {
      x <- model_data[cluster_rows[[indices[i]]], , drop = FALSE]
      for (g in groups$columns) {
        x[[g]] <- paste(i, x[[g]], sep = "_")
      }
      x
    })
    d <- do.call(rbind, pieces)
    for (g in groups$columns) {
      d[[g]] <- factor(d[[g]], levels = unique(d[[g]]))
    }

    fit <- if (verbose) {
      tryCatch(stats::update(model, data = d), error = function(e) {
        if (catch) NULL else stop(e)
      })
    } else {
      tryCatch(
        suppressWarnings(suppressMessages(stats::update(model, data = d))),
        error = function(e) if (catch) NULL else stop(e)
      )
    }
    if (is.null(fit)) {
      return(rep.int(NA_real_, n_params))
    }

    params <- insight::get_parameters(fit, verbose = FALSE)
    if (nrow(params) != n_params) {
      return(rep.int(NA_real_, n_params))
    }
    params$Estimate
  }

  results <- boot::boot(
    data = seq_along(clusters),
    statistic = function(x, indices) refit(indices),
    R = iterations,
    sim = type,
    parallel = parallel,
    ncpus = n_cpus,
    cl = cluster
  )

  # if the refit on all clusters failed with an error, show that error
  if (all(is.na(results$t0))) {
    refit(seq_along(clusters), catch = FALSE)
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), , drop = FALSE]

  names(out) <- insight::get_parameters(model, verbose = FALSE)$Parameter
  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


# Find the grouping factors of a mixed model, and the top factor that all of
# them nest in. Factors that occur only in `:` terms of the formula (which `/`
# creates) count as nested in the other factors of those terms, because the
# cells of a `:` term nest in each of its factors. Other factors are nested if
# each of their levels occurs within one level of the top factor.
.bootstrap_cluster_groups <- function(model, data) {
  terms <- unique(insight::find_random(model, split_nested = FALSE, flatten = TRUE))
  columns <- unique(insight::find_random(model, split_nested = TRUE, flatten = TRUE))

  not_found <- setdiff(columns, colnames(data))
  if (length(not_found)) {
    insight::format_error(paste0(
      "Cluster bootstrapping needs the grouping factors as columns of the model",
      " data. Not found: ",
      datawizard::text_concatenate(not_found, enclose = "`"),
      "."
    ))
  }

  if (length(columns) == 1L) {
    return(list(top = columns, columns = columns))
  }

  interaction_terms <- strsplit(terms[grepl(":", terms, fixed = TRUE)], ":", fixed = TRUE)

  nests_in <- function(inner, outer) {
    in_formula <- !inner %in% terms &&
      any(vapply(interaction_terms, function(x) all(c(inner, outer) %in% x), logical(1)))
    in_data <- all(
      lengths(lapply(split(data[[outer]], data[[inner]], drop = TRUE), unique)) == 1L
    )
    in_formula || in_data
  }

  is_top <- vapply(
    columns,
    function(outer) {
      all(vapply(setdiff(columns, outer), nests_in, logical(1), outer = outer))
    },
    logical(1)
  )

  if (!any(is_top)) {
    insight::format_error(paste0(
      "Cluster bootstrapping needs grouping factors that are all nested in one",
      " factor. The grouping factors ",
      datawizard::text_concatenate(columns, enclose = "`"),
      " are not."
    ))
  }

  # with more than one candidate, the factor with the fewest levels
  candidates <- columns[is_top]
  n_levels <- vapply(candidates, function(x) length(unique(data[[x]])), integer(1))
  list(top = candidates[which.min(n_levels)], columns = columns)
}


#' @export
bootstrap_model.merMod <- function(
  model,
  iterations = 1000,
  type = "parametric",
  parallel = "no",
  n_cpus = 1,
  cluster = NULL,
  verbose = FALSE,
  ...
) {
  insight::check_if_installed("lme4")

  type <- insight::validate_argument(type, c("parametric", "semiparametric"))
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  # lme4::bootMer() only implements semiparametric bootstrapping with
  # use.u = TRUE, and that path does not support glmmTMB models
  if (type == "semiparametric" && inherits(model, "glmmTMB")) {
    msg <- "Semiparametric bootstrapping is not available for models from package `glmmTMB`. Use `type = \"parametric\"`" # nolint
    if (isTRUE(insight::model_info(model, verbose = FALSE)$is_linear)) {
      msg <- paste0(msg, ", or fit the model with `lme4::lmer()`")
    }
    insight::format_error(paste0(msg, "."))
  }
  use_u <- type == "semiparametric"

  boot_function <- function(model) {
    params <- insight::get_parameters(model, verbose = FALSE)
    n_params <- insight::n_parameters(model)

    # for glmmTMB, remove dispersion paramters, if any
    if (
      inherits(model, "glmmTMB") &&
        "Component" %in% names(params) &&
        "dispersion" %in% params$Component
    ) {
      # find number of dispersion parameters
      n_disp <- sum(params$Component == "dispersion")
      # remove dispersion parameters
      params <- params[params$Component != "dispersion", ]
      # make sure number of parameters is updated
      n_params <- n_params - n_disp
    }

    if (nrow(params) != n_params) {
      params <- stats::setNames(rep.int(NA, n_params), params$Parameter)
    } else {
      params <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
    }
    params
  }

  if (verbose) {
    results <- lme4::bootMer(
      model,
      boot_function,
      nsim = iterations,
      type = type,
      use.u = use_u,
      parallel = parallel,
      ncpus = n_cpus,
      cl = cluster
    )
  } else {
    results <- suppressMessages(lme4::bootMer(
      model,
      boot_function,
      nsim = iterations,
      verbose = FALSE,
      type = type,
      use.u = use_u,
      parallel = parallel,
      ncpus = n_cpus,
      cl = cluster
    ))
  }

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  names(out) <- insight::find_parameters(model, effects = "fixed")$conditional
  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


#' @export
bootstrap_model.glmmTMB <- bootstrap_model.merMod


#' @export
bootstrap_model.nestedLogit <- function(
  model,
  iterations = 1000,
  type = "ordinary",
  parallel = "no",
  n_cpus = 1,
  cluster = NULL,
  verbose = FALSE,
  ...
) {
  insight::check_if_installed("boot")

  type <- insight::validate_argument(
    type,
    c("ordinary", "balanced", "permutation", "antithetic")
  )
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  model_data <- data <- insight::get_data(model, verbose = FALSE) # nolint
  model_response <- insight::find_response(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, , drop = FALSE] # allows boot to select sample

    if (verbose) {
      fit <- stats::update(model, data = d)
    } else {
      fit <- suppressMessages(stats::update(model, data = d))
    }

    params <- insight::get_parameters(fit, verbose = FALSE)
    stats::setNames(params$Estimate, params$Parameter) # Transform to named vector
  }

  results <- boot::boot(
    data = data,
    statistic = boot_function,
    R = iterations,
    sim = type,
    parallel = parallel,
    ncpus = n_cpus,
    cl = cluster,
    model = model
  )

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), ]

  params <- insight::get_parameters(model, verbose = FALSE)
  names(out) <- paste0(params$Parameter, ".", params$Component)

  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}
