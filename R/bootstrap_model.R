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
#' in one factor give an error. A grouping factor that is also a fixed effect
#' gives an error, unless the formula has it only in interactions with the
#' resampled factor (for example `cond` in `random = ~ 1 | subject/cond`, which
#' has the grouping terms `subject` and `subject:cond`). With few clusters, the
#' cluster bootstrap has few independent units to resample, and its standard
#' errors can be too small.
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
  param_names <- insight::get_parameters(model, verbose = FALSE)$Parameter
  n_params <- length(param_names)

  # stack the drawn clusters in draw order, and label each by its draw
  # position, so that a cluster drawn twice counts as two clusters
  refit <- function(indices, catch = TRUE) {
    pieces <- lapply(seq_along(indices), function(i) {
      x <- model_data[cluster_rows[[indices[i]]], , drop = FALSE]
      for (g in groups$relabel) {
        x[[g]] <- paste(i, x[[g]], sep = "_")
      }
      x
    })
    d <- do.call(rbind, pieces)
    for (g in groups$relabel) {
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

    # a draw that misses a factor level can lose or rename parameters
    params <- insight::get_parameters(fit, verbose = FALSE)
    if (!identical(params$Parameter, param_names)) {
      return(rep.int(NA_real_, n_params))
    }
    params$Estimate
  }

  # refit on all clusters once, so that a model that cannot be refit fails
  # before the bootstrap runs
  if (anyNA(refit(seq_along(clusters), catch = FALSE))) {
    insight::format_error(
      "Refitting the model on its clusters does not return the parameters of the model, so the model cannot be bootstrapped by cluster." # nolint
    )
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

  out <- as.data.frame(results$t)
  out <- out[stats::complete.cases(out), , drop = FALSE]

  if (!nrow(out)) {
    insight::format_error(
      "None of the bootstrap samples gave a refit with all parameters of the model. If `parallel = \"snow\"`, the workers may not find the function that fits the model." # nolint
    )
  }

  names(out) <- param_names
  class(out) <- unique(c("bootstrap_model", "see_bootstrap_model", class(out)))
  attr(out, "original_model") <- model
  out
}


# Find the grouping factors of a mixed model, the top factor that all of them
# nest in, and the factors that need new labels for each drawn cluster.
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

  interaction_terms <- strsplit(terms[grepl(":", terms, fixed = TRUE)], ":", fixed = TRUE)
  top <- if (length(columns) == 1L) {
    columns
  } else {
    .bootstrap_top_factor(columns, terms, data)
  }

  # A drawn cluster gets new labels in the top factor. A lower factor needs new
  # labels too, unless it occurs only in `:` terms that also have the top
  # factor, which then keep the copies of a cluster apart.
  needs_relabel <- vapply(
    columns,
    function(g) {
      g == top ||
        g %in% terms ||
        !all(vapply(
          interaction_terms[vapply(interaction_terms, function(x) g %in% x, logical(1))],
          function(x) top %in% x,
          logical(1)
        ))
    },
    logical(1)
  )
  relabel <- columns[needs_relabel]

  fixed <- intersect(
    relabel,
    insight::find_predictors(model, effects = "fixed", flatten = TRUE)
  )
  if (length(fixed)) {
    insight::format_error(paste0(
      "Cluster bootstrapping gives each drawn cluster new labels in ",
      datawizard::text_concatenate(fixed, enclose = "`"),
      ", which the model also uses as fixed effects."
    ))
  }

  list(top = top, columns = columns, relabel = relabel)
}


# Factors that occur only in `:` terms of the formula (which `/` creates) count
# as nested in the other factors of those terms, because the cells of a `:` term
# nest in each of its factors. Other factors are nested if each of their levels
# occurs within one level of the top factor.
.bootstrap_top_factor <- function(columns, terms, data) {
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

  # with more than one candidate, the factor with the most clusters
  candidates <- columns[is_top]
  n_levels <- vapply(candidates, function(x) length(unique(data[[x]])), integer(1))
  candidates[which.max(n_levels)]
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
