# merMod -----------------------------------------------------------------

#' @export
model_parameters.merMod <- model_parameters.coxme

#' @export
ci.merMod <- function(x, ci = 0.95, dof = NULL, method = "wald", iterations = 500, ...) {
  method <- tolower(method)
  method <- insight::validate_argument(
    method,
    c(
      "wald",
      "ml1",
      "betwithin",
      "kr",
      "satterthwaite",
      "kenward",
      "boot",
      "profile",
      "residual",
      "normal"
    )
  )

  # bootstrapping
  if (method == "boot") {
    out <- lapply(ci, function(ci, x) .ci_boot_merMod(x, ci, iterations, ...), x = x)
    out <- do.call(rbind, out)
    row.names(out) <- NULL

    # profiled CIs
  } else if (method == "profile") {
    pp <- suppressWarnings(stats::profile(x, which = "beta_"))
    out <- lapply(ci, function(i) .ci_profile_merMod(x, ci = i, profiled = pp, ...))
    out <- do.call(rbind, out)

    # all others
  } else {
    out <- .ci_generic(model = x, ci = ci, dof = dof, method = method, ...)
  }

  out
}


#' @export
standard_error.merMod <- function(
  model,
  effects = "fixed",
  method = NULL,
  vcov = NULL,
  vcov_args = NULL,
  ...
) {
  dots <- list(...)
  effects <- insight::validate_argument(effects, c("fixed", "random"))

  if (effects == "random") {
    out <- .standard_errors_random(model)
    return(out)
  }

  if (is.null(method)) {
    method <- "wald"
  } else if (
    (method == "robust" && is.null(vcov)) ||
      # deprecated argument
      isTRUE(list(...)[["robust"]])
  ) {
    vcov <- "vcovHC"
  }

  if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
    fun_args <- list(model, vcov = vcov, vcov_args = vcov_args)
    fun_args <- c(fun_args, dots)
    out <- do.call("standard_error.default", fun_args)
    return(out)
  }

  # kenward approx
  if (method %in% c("kenward", "kr")) {
    out <- se_kenward(model)
  } else {
    # Classic and Satterthwaite SE
    out <- se_mixed_default(model)
  }
  out
}

#' @export
p_value.merMod <- p_value.cpglmm


# lmerMod and lmerModTest -----------------------------------------------------------------

#' @export
model_parameters.lmerMod <- function(
  model,
  ci = 0.95,
  ci_method = NULL,
  ci_random = NULL,
  bootstrap = FALSE,
  iterations = 1000,
  standardize = NULL,
  effects = "all",
  group_level = FALSE,
  exponentiate = FALSE,
  p_adjust = NULL,
  vcov = NULL,
  vcov_args = NULL,
  wb_component = FALSE,
  include_info = getOption("parameters_mixed_info", FALSE),
  include_sigma = FALSE,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  cl <- match.call()
  cl[[1]] <- model_parameters.merMod
  cl$ci_method <- .attempt_satterthwaite_method(ci_method)
  eval.parent(cl)
}

#' @export
ci.lmerMod <- function(x, ci = 0.95, dof = NULL, method = NULL, iterations = 500, ...) {
  cl <- match.call()
  cl[[1]] <- ci.merMod
  cl$method <- .attempt_satterthwaite_method(method)
  eval.parent(cl)
}

#' @export
standard_error.lmerMod <- function(
  model,
  effects = "fixed",
  method = NULL,
  vcov = NULL,
  vcov_args = NULL,
  ...
) {
  cl <- match.call()
  cl[[1]] <- standard_error.merMod
  cl$method <- .attempt_satterthwaite_method(method)
  eval.parent(cl)
}

#' @export
p_value.lmerMod <- function(model, method = NULL, ...) {
  cl <- match.call()
  cl[[1]] <- p_value.merMod
  cl$method <- .attempt_satterthwaite_method(method)
  eval.parent(cl)
}


# helpers --------------

.add_random_effects_lme4 <- function(
  model,
  params,
  ci,
  ci_method,
  ci_random,
  effects,
  group_level,
  verbose = TRUE,
  ...
) {
  params_random <- params_variance <- NULL

  # only proceed if random effects are requested
  if (effects %in% c("random", "all")) {
    # group level estimates (BLUPs) or random effects variances?
    if (isTRUE(group_level)) {
      params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
    } else {
      params_variance <- .extract_random_variances(
        model,
        ci = ci,
        effects = effects,
        ci_method = ci_method,
        ci_random = ci_random,
        verbose = verbose
      )
    }

    # merge random and fixed effects, if necessary
    if (!is.null(params) && (!is.null(params_random) || !is.null(params_variance))) {
      params$Level <- NA
      params$Group <- ""

      if (is.null(params_random)) {
        params <- params[match(colnames(params_variance), colnames(params))]
      } else {
        params <- params[match(colnames(params_random), colnames(params))]
      }
    }
  }

  rbind(params, params_random, params_variance)
}

.standard_errors_random <- function(model) {
  insight::check_if_installed("lme4")

  rand.se <- lme4::ranef(model, condVar = TRUE)
  n.groupings <- length(rand.se)

  for (m in 1:n.groupings) {
    vars.m <- attr(rand.se[[m]], "postVar")

    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]

    names.full <- dimnames(rand.se[[m]])
    rand.se[[m]] <- array(NA, c(J, K))

    for (j in 1:J) {
      rand.se[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[,, j])))
    }
    dimnames(rand.se[[m]]) <- list(names.full[[1]], names.full[[2]])
  }
  rand.se
}


se_mixed_default <- function(model) {
  params <- insight::find_parameters(
    model,
    effects = "fixed",
    component = "conditional",
    flatten = TRUE
  )
  .data_frame(Parameter = params, SE = .get_se_from_summary(model))
}

.attempt_satterthwaite_method <- function(method, fallback = "wald") {
  if (!is.null(method)) {
    return(method)
  }

  installed <- insight::check_if_installed(
    package = "lmerTest",
    reason = "for Satterthwaite df (default)",
    stop = FALSE,
    prompt = TRUE
  )

  # This is a workaround for the fact that check_if_installed() returns FALSE
  # even if the package is not installed, but the user chooses to install it. In
  # that case, we need to check again if the package is actually installed.
  if (!installed) {
    installed <- requireNamespace("lmerTest", quietly = TRUE)
  }

  if (installed) {
    return("satterthwaite")
  }

  return(fallback)
}
