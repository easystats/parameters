# emmeans

# model_parameters ----------------


#' @export
model_parameters.emmGrid <- function(model,
                                     ci = .95,
                                     centrality = "median",
                                     dispersion = TRUE,
                                     ci_method = "hdi",
                                     test = c("pd", "rope"),
                                     rope_range = "default",
                                     rope_ci = 1.0,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     parameters = NULL,
                                     verbose = TRUE,
                                     ...) {

  # set default for p-adjust
  emm_padjust <- tryCatch(
    {
      adj <- model@misc$adjust
    },
    error = function(e) {
      NULL
    }
  )
  if (!is.null(emm_padjust) && is.null(p_adjust)) {
    p_adjust <- emm_padjust
  }

  s <- summary(model, level = ci, adjust = "none")
  params <- as.data.frame(s)

  # Bayesian model?
  is_frq <- isTRUE(all.equal(dim(model@post.beta), c(1, 1))) &&
            isTRUE(is.na(model@post.beta)) && is.null(model@misc$is_boot)

  # we assume frequentist here...
  if (is_frq) {

    # get statistic, se and p
    statistic <- insight::get_statistic(model, ci = ci, adjust = "none")
    SE <- standard_error(model)
    p <- p_value(model, ci = ci, adjust = "none")

    params$Statistic <- statistic$Statistic
    params$SE <- SE$SE
    params$p <- p$p

    # ==== adjust p-values?

    if (!is.null(p_adjust)) {
      params <- .p_adjust(params, p_adjust, model, verbose)
    }

  } else {

    # Bayesian models go here...
    params <- bayestestR::describe_posterior(
      model,
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      bf_prior = NULL,
      diagnostic = NULL,
      priors = NULL,
      verbose = verbose,
      ...
    )

    statistic <- NULL

  }


  # Renaming
  if (!is.null(statistic)) {
    names(params) <- gsub("Statistic", gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE), names(params))
  }
  names(params) <- gsub("Std. Error", "SE", names(params))
  names(params) <- gsub(model@misc$estName, "Estimate", names(params))
  names(params) <- gsub("lower.CL", "CI_low", names(params))
  names(params) <- gsub("upper.CL", "CI_high", names(params))
  names(params) <- gsub("asymp.LCL", "CI_low", names(params))
  names(params) <- gsub("asymp.UCL", "CI_high", names(params))
  names(params) <- gsub("lower.HPD", "CI_low", names(params))
  names(params) <- gsub("upper.HPD", "CI_high", names(params))

  # check if we have CIs
  if (!any(grepl("^CI_", colnames(params)))) {
    df_column <- grep("(df|df_error)", colnames(params))
    if (length(df_column) > 0) {
      df <- params[[df_column[1]]]
    } else {
      df <- Inf
    }
    fac <- stats::qt((1 + ci) / 2, df = df)
    params$CI_low <- params$Estimate - fac * params$SE
    params$CI_high <- params$Estimate + fac * params$SE
  }

  # rename if necessary
  if ("df" %in% colnames(params)) {
    colnames(params)[colnames(params) == "df"] <- "df_error"
  }

  # Reorder
  estimate_pos <- which(colnames(s) == model@misc$estName)
  parameter_names <- colnames(params)[1:(estimate_pos - 1)]
  order <- c(parameter_names, "Estimate", "Median", "Mean", "SE", "CI_low", "CI_high",
             "F", "t", "z", "df", "df_error", "p", "pd", "ROPE_CI", "ROPE_low",
             "ROPE_high", "ROPE_Percentage")
  params <- params[order[order %in% names(params)]]

  # rename
  names(params) <- gsub("Estimate", "Coefficient", names(params))

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }

  # filter parameters
  if (!is.null(parameters)) {
    params <- .filter_parameters(params, parameters, verbose = verbose)
  }

  params <- suppressWarnings(.add_model_parameters_attributes(params, model, ci, exponentiate = FALSE, p_adjust = p_adjust, verbose = verbose, ...))
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(params, "parameter_names") <- parameter_names

  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}


#' @export
model_parameters.emm_list <- function(model,
                                      ci = .95,
                                      exponentiate = FALSE,
                                      p_adjust = NULL,
                                      verbose = TRUE,
                                      ...) {
  s <- summary(model)
  params <- lapply(seq_along(s), function(i) {
    pars <- model_parameters(
      model[[i]],
      ci = ci,
      exponentiate = exponentiate,
      p_adjust = p_adjust,
      verbose = verbose
    )
    estimate_pos <- which(colnames(pars) == "Coefficient")
    pars[1:(estimate_pos - 1)] <- NULL
    cbind(
      Parameter = .pretty_emmeans_Parameter_names(model[[i]]),
      pars
    )
  })
  params <- do.call(rbind, params)
  params$Component <- .pretty_emmeans_Component_names(s)

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, p_adjust = p_adjust, verbose = verbose, ...)

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}




# standard errors -----------------


#' @export
standard_error.emmGrid <- function(model, ...) {
  if (!is.null(model@misc$is_boot) && model@misc$is_boot) {
    return(boot_em_standard_error(model))
  }

  s <- summary(model)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos) && !is.null(s$SE)) {
    out <- .data_frame(
      Parameter = .pretty_emmeans_Parameter_names(model),
      SE = unname(s$SE)
    )
  } else {
    out <- NULL
  }
  out
}


#' @export
standard_error.emm_list <- function(model, ...) {
  if (!is.null(model[[1]]@misc$is_boot) && model[[1]]@misc$is_boot) {
    return(boot_em_standard_error(model))
  }

  params <- insight::get_parameters(model)
  s <- summary(model)
  se <- unlist(lapply(s, function(i) {
    if (is.null(i$SE)) {
      rep(NA, nrow(i))
    } else {
      i$SE
    }
  }))

  .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    SE = unname(se),
    Component = .pretty_emmeans_Component_names(s)
  )
}

boot_em_standard_error <- function(model) {
  est <- insight::get_parameters(model, summary = FALSE)

  Component <- NULL
  if (inherits(s <- summary(model), "list")) {
    Component <- .pretty_emmeans_Component_names(s)
  }

  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    SE = sapply(est, stats::sd)
  )

  if (!is.null(Component)) out$Component <- Component

  out
}




# degrees of freedom --------------------


#' @export
degrees_of_freedom.emmGrid <- function(model, ...) {
  if (!is.null(model@misc$is_boot) && model@misc$is_boot) {
    return(boot_em_df(model))
  }

  summary(model)$df
}


#' @export
degrees_of_freedom.emm_list <- function(model, ...) {
  if (!is.null(model[[1]]@misc$is_boot) && model[[1]]@misc$is_boot) {
    return(boot_em_df(model))
  }

  s <- summary(model)
  unname(unlist(lapply(s, function(i) {
    if (is.null(i$df)) {
      rep(Inf, nrow(i))
    } else {
      i$df
    }
  })))
}

boot_em_df <- function(model) {
  est <- insight::get_parameters(model, summary = FALSE)
  rep(NA, ncol(est))
}


# p values ----------------------


#' @rdname p_value
#' @export
p_value.emmGrid <- function(model, ci = .95, adjust = "none", ...) {
  if (!is.null(model@misc$is_boot) && model@misc$is_boot) {
    return(boot_em_pval(model, adjust))
  }


  s <- summary(model, level = ci, adjust = adjust)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos)) {
    stat <- insight::get_statistic(model, ci = ci, adjust = adjust)
    p <- 2 * stats::pt(abs(stat$Statistic), df = s$df, lower.tail = FALSE)

    .data_frame(
      Parameter = .pretty_emmeans_Parameter_names(model),
      p = as.vector(p)
    )
  } else {
    return(NULL)
  }
}


#' @export
p_value.emm_list <- function(model, adjust = "none", ...) {
  if (!is.null(model[[1]]@misc$is_boot) && model[[1]]@misc$is_boot) {
    return(boot_em_pval(model, adjust))
  }


  params <- insight::get_parameters(model)
  s <- summary(model, adjust = adjust)

  # p-values
  p <- unlist(lapply(s, function(i) {
    if (is.null(i$p)) {
      rep(NA, nrow(i))
    } else {
      i$p
    }
  }))

  # result
  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    p = as.vector(p),
    Component = .pretty_emmeans_Component_names(s)
  )

  # any missing values?
  if (anyNA(out$p)) {

    # standard errors
    se <- unlist(lapply(s, function(i) {
      if (is.null(i$SE)) {
        rep(NA, nrow(i))
      } else {
        i$SE
      }
    }))

    # test statistic and p-values
    stat <- params$Estimate / se
    df <- degrees_of_freedom(model)
    p_val <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
    out$p[is.na(out$p)] <- p_val[is.na(out$p)]
  }

  out
}


boot_em_pval <- function(model, adjust) {
  est <- insight::get_parameters(model, summary = FALSE)

  p <- sapply(est, p_value)

  p <- stats::p.adjust(p, method = adjust)

  Component <- NULL
  if (inherits(s <- summary(model), "list")) {
    Component <- .pretty_emmeans_Component_names(s)
  }

  out <- .data_frame(
    Parameter = .pretty_emmeans_Parameter_names(model),
    p = unname(p)
  )

  if (!is.null(Component)) out$Component <- Component

  out
}


# format parameters -----------------


#' @export
format_parameters.emm_list <- function(model, ...) {
  NULL
}


# Utils -------------------------------------------------------------------

.pretty_emmeans_Parameter_names <- function(model) {
  s <- summary(model)

  if (inherits(s, "list")) {
    parnames <- lapply(seq_along(s), function(i) .pretty_emmeans_Parameter_names(model[[i]]))
    parnames <- unlist(parnames)
  } else {
    estimate_pos <- which(colnames(s) == model@misc$estName)
    params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    if (ncol(params) >= 2) {
      r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
      parnames <- unname(sapply(as.data.frame(r), paste, collapse = ", "))
    } else {
      parnames <- as.vector(params[[1]])
    }
  }
  parnames
}

.pretty_emmeans_Component_names <- function(s) {
  Component <- lapply(seq_along(s), function(i) {
    rep(names(s)[[i]], nrow(s[[i]]))
  })
  Component <- unlist(Component)
}
