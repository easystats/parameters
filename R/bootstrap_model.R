#' Model bootstrapping
#'
#' Bootstrap a statistical model n times to return a data frame of estimates.
#'
#' @param model Statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param type Character string specifying the type of bootstrap. For mixed models
#'   of class `merMod` or `glmmTMB`, may be `"parametric"` (default) or
#'   `"semiparametric"` (see `?lme4::bootMer` for details). For all
#'   other models, see argument `sim` in `?boot::boot` (defaults to
#'   `"ordinary"`).
#' @param parallel The type of parallel operation to be used (if any).
#' @param n_cpus Number of processes to be used in parallel operation.
#' @param cluster Optional cluster when `parallel = "snow"`. See `?lme4::bootMer`
#' for details.
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
bootstrap_model <- function(model,
                            iterations = 1000,
                            ...) {
  UseMethod("bootstrap_model")
}


#' @rdname bootstrap_model
#' @export
bootstrap_model.default <- function(model,
                                    iterations = 1000,
                                    type = "ordinary",
                                    parallel = "no",
                                    n_cpus = 1,
                                    cluster = NULL,
                                    verbose = FALSE,
                                    ...) {
  # check for valid input
  .is_model_valid(model)

  insight::check_if_installed("boot")

  type <- insight::validate_argument(
    type,
    c("ordinary", "parametric", "balanced", "permutation", "antithetic")
  )
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  model_data <- data <- insight::get_data(model, verbose = FALSE) # nolint
  model_response <- insight::find_response(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

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


#' @export
bootstrap_model.merMod <- function(model,
                                   iterations = 1000,
                                   type = "parametric",
                                   parallel = "no",
                                   n_cpus = 1,
                                   cluster = NULL,
                                   verbose = FALSE,
                                   ...) {
  insight::check_if_installed("lme4")

  type <- insight::validate_argument(type, c("parametric", "semiparametric"))
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  boot_function <- function(model) {
    params <- insight::get_parameters(model, verbose = FALSE)
    n_params <- insight::n_parameters(model)

    # for glmmTMB, remove dispersion paramters, if any
    if (inherits(model, "glmmTMB") && "Component" %in% names(params) && "dispersion" %in% params$Component) {
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
bootstrap_model.nestedLogit <- function(model,
                                        iterations = 1000,
                                        type = "ordinary",
                                        parallel = "no",
                                        n_cpus = 1,
                                        verbose = FALSE,
                                        ...) {
  insight::check_if_installed("boot")

  type <- insight::validate_argument(
    type,
    c("ordinary", "balanced", "permutation", "antithetic")
  )
  parallel <- insight::validate_argument(parallel, c("no", "multicore", "snow"))

  model_data <- data <- insight::get_data(model, verbose = FALSE) # nolint
  model_response <- insight::find_response(model)

  boot_function <- function(model, data, indices) {
    d <- data[indices, ] # allows boot to select sample

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
