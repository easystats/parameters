############# .merMod -----------------


#' @export
model_parameters.merMod <- function(model,
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
                                    ...) {
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else {
      ci_method <- switch(insight::find_statistic(model),
        `t-statistic` = "residual",
        "wald"
      )
    }
  }

  # p-values, CI and se might be based of wald, or KR
  ci_method <- tolower(ci_method)

  if (isTRUE(bootstrap)) {
    ci_method <- insight::validate_argument(
      ci_method,
      c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai")
    )
  } else {
    ci_method <- insight::validate_argument(
      ci_method,
      c(
        "wald", "normal", "residual", "ml1", "betwithin", "satterthwaite",
        "kenward", "kr", "boot", "profile", "uniroot"
      )
    )
  }

  # which component to return?
  effects <- insight::validate_argument(
    effects,
    c("fixed", "random", "total", "random_total", "all")
  )
  params <- params_random <- params_variance <- NULL

  # for coef(), we don't need all the attributes and just stop here
  if (effects %in% c("total", "random_total")) {
    params <- .group_level_total(model)
    params$Effects <- "total"
    class(params) <- c("parameters_coef", "see_parameters_coef", class(params))
    return(params)
  }

  # post hoc standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_alert(
        "Standardizing coefficients only works for fixed effects of the mixed model."
      )
    }
    effects <- "fixed"
  }

  # for refit, we completely refit the model, than extract parameters,
  # ci etc. as usual - therefor, we set "standardize" to NULL
  if (!is.null(standardize) && standardize == "refit") {
    model <- datawizard::standardize(model, verbose = FALSE)
    standardize <- NULL
  }

  if (effects %in% c("fixed", "all")) {
    # Processing
    if (bootstrap) {
      params <- bootstrap_parameters(
        model,
        iterations = iterations,
        ci = ci,
        ...
      )
      if (effects != "fixed") {
        effects <- "fixed"
        if (verbose) {
          insight::format_alert("Bootstrapping only returns fixed effects of the mixed model.")
        }
      }
    } else {
      fun_args <- list(
        model,
        ci = ci,
        ci_method = ci_method,
        standardize = standardize,
        p_adjust = p_adjust,
        wb_component = wb_component,
        keep_parameters = keep,
        drop_parameters = drop,
        verbose = verbose,
        include_sigma = include_sigma,
        include_info = include_info,
        vcov = vcov,
        vcov_args = vcov_args
      )
      fun_args <- c(fun_args, dots)
      params <- do.call(".extract_parameters_mixed", fun_args)
    }

    params$Effects <- "fixed"

    # exponentiate coefficients and SE/CI, if requested
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  att <- attributes(params)

  if (effects %in% c("random", "all") && isTRUE(group_level)) {
    params_random <- .extract_random_parameters(model, ci = ci, effects = effects)
  }

  if (effects %in% c("random", "all") && isFALSE(group_level)) {
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

  params <- rbind(params, params_random, params_variance)
  # remove empty column
  if (!is.null(params$Level) && all(is.na(params$Level))) {
    params$Level <- NULL
  }

  # due to rbind(), we lose attributes from "extract_parameters()",
  # so we add those attributes back here...
  if (!is.null(att)) {
    attributes(params) <- utils::modifyList(att, attributes(params))
  }

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci = ci,
    exponentiate,
    bootstrap,
    iterations,
    ci_method = ci_method,
    p_adjust = p_adjust,
    verbose = verbose,
    include_info = include_info,
    group_level = group_level,
    wb_component = wb_component,
    ...
  )


  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
ci.merMod <- function(x,
                      ci = 0.95,
                      dof = NULL,
                      method = "wald",
                      iterations = 500,
                      ...) {
  method <- tolower(method)
  method <- insight::validate_argument(method, c(
    "wald", "ml1", "betwithin", "kr",
    "satterthwaite", "kenward", "boot",
    "profile", "residual", "normal"
  ))

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
standard_error.merMod <- function(model,
                                  effects = "fixed",
                                  method = NULL,
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  ...) {
  dots <- list(...)
  effects <- insight::validate_argument(effects, c("fixed", "random"))

  if (effects == "random") {
    out <- .standard_errors_random(model)
    return(out)
  }

  if (is.null(method)) {
    method <- "wald"
  } else if ((method == "robust" && is.null(vcov)) ||
    # deprecated argument
    isTRUE(list(...)[["robust"]])) {
    vcov <- "vcovHC"
  }

  if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
    fun_args <- list(model,
      vcov = vcov,
      vcov_args = vcov_args
    )
    fun_args <- c(fun_args, dots)
    out <- do.call("standard_error.default", fun_args)
    return(out)
  }

  # kenward approx
  if (method %in% c("kenward", "kr")) {
    out <- se_kenward(model)
    return(out)
  } else {
    # Classic and Satterthwaite SE
    out <- se_mixed_default(model)
    return(out)
  }
}


# helpers --------------

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
      rand.se[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
    }
    dimnames(rand.se[[m]]) <- list(names.full[[1]], names.full[[2]])
  }
  rand.se
}


se_mixed_default <- function(model) {
  params <- insight::find_parameters(model,
    effects = "fixed",
    component = "conditional",
    flatten = TRUE
  )
  .data_frame(Parameter = params, SE = .get_se_from_summary(model))
}


#' @export
p_value.merMod <- p_value.cpglmm
