
# Special classes and models -----------------------------


#' @export
standard_error.emmGrid <- function(model, ...) {
  s <- summary(model)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos)) {
    params <- s[, 1:(estimate_pos - 1), drop = FALSE]
    if (ncol(params) >= 2) {
      r <- apply(params, 1, function(i) paste0(colnames(params), " [", i, "]"))
      out <- .data_frame(
        Parameter = unname(sapply(as.data.frame(r), paste, collapse = ", ")),
        SE = unname(s$SE)
      )
    } else {
      out <- .data_frame(Parameter = as.vector(params[[1]]), SE = s$SE)
    }
  } else {
    out <- NULL
  }
  out
}


#' @export
standard_error.emm_list <- function(model, ...) {
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
    Parameter = params$Parameter,
    SE = unname(se),
    Component = params$Component
  )
}


#' @export
degrees_of_freedom.emmGrid <- function(model, ...) {
  summary(model)$df
}

#' @export
degrees_of_freedom.emm_list <- function(model, ...) {
  s <- summary(model)
  unname(unlist(lapply(s, function(i) {
    if (is.null(i$df)) {
      Inf
    } else {
      i$df
    }
  })))
}


#' @rdname p_value
#' @export
p_value.emmGrid <- function(model, ci = .95, adjust = "none", ...) {
  s <- summary(model, level = ci, adjust = adjust)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos)) {
    stat <- insight::get_statistic(model, ci = ci, adjust = adjust)
    p <- 2 * stats::pt(abs(stat$Statistic), df = s$df, lower.tail = FALSE)

    .data_frame(
      s[, 1:(estimate_pos - 1), drop = FALSE],
      p = as.vector(p)
    )
  } else {
    return(NULL)
  }
}


#' @export
p_value.emm_list <- function(model, ...) {
  params <- insight::get_parameters(model)
  s <- summary(model)

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
    Parameter = params$Parameter,
    p = as.vector(p),
    Component = params$Component
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


#' @export
model_parameters.emmGrid <- function(model,
                                     ci = .95,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
  if (is.null(p_adjust)) {
    p_adjust <- "none"
  }

  s <- summary(model, level = ci, adjust = p_adjust)
  params <- as.data.frame(s)

  # get statistic and p
  statistic <- insight::get_statistic(model, ci = ci, adjust = p_adjust)
  p <- p_value(model, ci = ci, adjust = p_adjust)

  params$Statistic <- statistic$Statistic
  params$p <- p$p

  # Renaming
  names(params) <- gsub("Statistic", gsub("-statistic", "", attr(statistic, "statistic", exact = TRUE), fixed = TRUE), names(params))
  names(params) <- gsub("Std. Error", "SE", names(params))
  names(params) <- gsub(model@misc$estName, "Estimate", names(params))
  names(params) <- gsub("lower.CL", "CI_low", names(params))
  names(params) <- gsub("upper.CL", "CI_high", names(params))
  names(params) <- gsub("asymp.LCL", "CI_low", names(params))
  names(params) <- gsub("asymp.UCL", "CI_high", names(params))

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

  # Reorder
  estimate_pos <- which(colnames(s) == model@misc$estName)
  parameter_names <- colnames(params)[1:(estimate_pos - 1)]
  order <- c(parameter_names, "Estimate", "SE", "CI_low", "CI_high", "t", "z", "df", "df_error", "p")
  params <- params[order[order %in% names(params)]]

  # rename
  names(params) <- gsub("Estimate", "Coefficient", names(params))

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
  params <-
    suppressMessages(suppressWarnings(
      .extract_parameters_generic(
        model,
        ci = ci,
        component = "conditional",
        merge_by = c("Parameter", "Component"),
        standardize = NULL,
        effects = "fixed",
        robust = FALSE,
        df_method = NULL,
        p_adjust = p_adjust,
        ...
      )
    ))

  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, p_adjust = p_adjust, verbose = verbose, ...)

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
