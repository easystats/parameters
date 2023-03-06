# #' @export
# p_value.hglm <- function(model, ...) {
#   stat <- insight::get_statistic(model)
#   .data_frame(
#     Parameter = stat$Parameter,
#     p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
#   )
# }

# #' @export
# ci.hglm <- function(x, ci = 0.95, ...) {
#   .ci_generic(model = x, ci = ci, ...)
# }

#' @export
model_parameters.hglm <- function(model,
                                  ci = 0.95,
                                  ci_method = NULL,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  exponentiate = FALSE,
                                  effects = "all",
                                  component = "all",
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  # which components to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  component <- match.arg(component, choices = c("all", "conditional", "dispersion"))

  # fixed effects

  mp <- model_parameters.default(
    model, ci = ci, ci_method = ci_method, bootstrap = bootstrap,
    effects = "fixed", component = "conditional", iterations = iterations,
    exponentiate = exponentiate, p_adjust = p_adjust, summary = summary,
    keep = keep, drop = drop, verbose = verbose, ...
  )

  # hglm has a special structure, so we add random effects and dispersion
  # manually here...

  if (effects %in% c("all", "random")) {
    re_params <- insight::get_parameters(model, effects = "random", component = "conditional")
    re_se <- standard_error(model, effects = "random", component = "conditional")
    re_ci <- ci(model, effects = "random", component = "conditional")
    # bind all results
    re_params <- cbind(
      re_params[c("Parameter", "Estimate")],
      re_se["SE"],
      re_ci[c("CI", "CI_low", "CI_high")]
    )
    # no values for statistic, df and p
    re_params$t <- re_params$df_error <- re_params$p <- NA
    # add effects-columns
    mp$Effects <- "fixed"
    re_params$Effects <- "random"
    # renaming
    colnames(re_params)[colnames(re_params) == "Estimate"] <- "Coefficient"
    # bind together
    mp <- rbind(mp, re_params)
  }

  # add dispersion model

  has_dispersion <- !is.null(insight::find_formula(model)$dispersion)
  if (has_dispersion && component %in% c("all", "dispersion")) {
    disp_params <- insight::get_parameters(model, effects = "fixed", component = "dispersion")
    disp_se <- standard_error(model, effects = "fixed", component = "dispersion")
    disp_ci <- ci(model, effects = "fixed", component = "dispersion")
    # bind all results
    disp_params <- cbind(
      disp_params[c("Parameter", "Estimate")],
      disp_se["SE"],
      disp_ci[c("CI", "CI_low", "CI_high")]
    )
    # no values for statistic, df and p
    disp_params$t <- disp_params$df_error <- disp_params$p <- NA
    # add effects-columns
    if (is.null(mp$Effects)) {
      mp$Effects <- "fixed"
    }
    disp_params$Effects <- "fixed"
    # add component-columns
    mp$Component <- "conditional"
    disp_params$Component <- "dispersion"
    # renaming
    colnames(disp_params)[colnames(disp_params) == "Estimate"] <- "Coefficient"
    # bind together
    mp <- rbind(mp, disp_params)
  }

  mp
}


#' @export
standard_error.hglm <- function(model,
                                effects = "fixed",
                                component = "conditional",
                                verbose = TRUE,
                                ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", "conditional", "dispersion"))

  f <- insight::find_formula(model)
  if (component == "dispersion" && is.null(f$dispersion)) {
    if (verbose) {
      insight::format_warning("No standard errors found for model's dispersion parameters.")
    }
    return(NULL)
  }

  # sanity check, make sure we have a dispersion component
  if (component == "all" && is.null(f$dispersion)) {
    compomnent <- "conditional"
  }

  s <- summary(model)

  if (effects == "fixed") {
    se <- s$FixCoefMat
  } else if (effects == "random") {
    se <- s$RandCoefMat
  } else {
    se <- c(s$FixCoefMat, s$RandCoefMat)
  }
  out <- .data_frame(
    Parameter = row.names(se),
    SE = as.vector(se[, 2])
  )

  # dispersion component?
  if (effects != "random" && component %in% c("dispersion", "all")) {
    se <- s$SummVC1
    disp <- .data_frame(
      Parameter = row.names(se),
      SE = as.vector(se[, 2]),
      Component = "dispersion"
    )
    if (component == "dispersion") {
      out <- disp
    } else {
      out$Component <- "conditional"
      out <- rbind(out, disp)
    }
  }

  out
}


#' @export
degrees_of_freedom.hglm <- function(model, method = "residual", ...) {
  if (method == "any") {
    method <- "residual"
  }
  insight::get_df(model, type = method, ...)
}


#' @export
ci.hglm <- function(model,
                    ci = 0.95,
                    method = "wald",
                    dof = NULL,
                    effects = "fixed",
                    component = "conditional",
                    verbose = TRUE,
                    ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  component <- match.arg(component, choices = c("all", "conditional", "dispersion"))

  # fixed effects -----------------

  if (effects %in% c("fixed", "all")) {
    out <- .ci_generic(
      model,
      ci = ci,
      method = method,
      dof = dof,
      effects = "fixed",
      component = component,
      verbose = verbose,
      ...
    )
  }

  # add random effects -----------------

  if (effects %in% c("random", "all")) {
    se <- standard_error(model, effects = "random", component = "conditional")$SE
    .ci_re <- .ci_dof(
      model,
      ci = ci,
      method = method,
      dof = dof,
      effects = "random",
      component = "conditional",
      se = se,
      verbose = verbose,
      ...
    )
    if (effects == "all") {
      out <- rbind(out, .ci_Re)
    } else {
      out <- .ci_re
    }
  }
  out
}


#' @export
p_value.hglm <- function(model,
                         dof = NULL,
                         method = NULL,
                         verbose = TRUE,
                         ...) {
  dots <- list(...)
  dots$component <- NULL
  args <- list(
    model,
    dof = dof,
    component = "conditional",
    method = method,
    verbose = verbose
  )
  args <- c(args, dots)
  do.call("p_value.default", args)
}
