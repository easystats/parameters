# metaplus


###### .metaplus -------------------


#' @export
model_parameters.metaplus <- function(model,
                                      ci = .95,
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      include_studies = TRUE,
                                      verbose = TRUE,
                                      ...) {
  if (!missing(ci)) {
    if (isTRUE(verbose)) {
      message("'metaplus' models do not support other levels for confidence intervals than 0.95. Argument 'ci' is ignored.")
    }
    ci <- .95
  }

  meta_analysis_overall <- suppressWarnings(.model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  ))

  rma_parameters <- if (!is.null(model$slab) && !is.numeric(model$slab)) {
    sprintf("%s", model$slab)
  } else if (is.null(model$k) && !is.null(model$slab) && is.numeric(model$slab)) {
    sprintf("Study %i", model$slab)
  } else if (!is.null(model$k)) {
    sprintf("Study %i", 1:model[["k"]])
  } else {
    sprintf("Study %i", 1:length(model$yi))
  }

  alpha <- (1 + ci) / 2

  rma_coeffients <- as.vector(model$yi)
  rma_se <- as.vector(model$sei)
  rma_ci_low <- rma_coeffients - rma_se * stats::qt(alpha, df = Inf)
  rma_ci_high <- rma_coeffients + rma_se * stats::qt(alpha, df = Inf)
  rma_statistic <- rma_coeffients / rma_se
  rma_ci_p <- 2 * stats::pt(abs(rma_statistic), df = Inf, lower.tail = FALSE)

  meta_analysis_studies <- data.frame(
    Parameter = rma_parameters,
    Coefficient = rma_coeffients,
    SE = rma_se,
    CI_low = rma_ci_low,
    CI_high = rma_ci_high,
    z = rma_statistic,
    df_error = NA,
    p = rma_ci_p,
    Weight = 1 / as.vector(model$sei),
    stringsAsFactors = FALSE
  )

  original_attributes <- attributes(meta_analysis_overall)
  out <- merge(meta_analysis_studies, meta_analysis_overall, all = TRUE, sort = FALSE)

  # fix intercept name
  out$Parameter[out$Parameter == "(Intercept)"] <- "Overall"
  out <- out[!(out$Parameter %in% c("tau2", "vinv")), ]

  # filter studies?
  if (isFALSE(include_studies)) {
    out <- out[out$Parameter == "Overall", ]
  }

  original_attributes$names <- names(out)
  original_attributes$row.names <- 1:nrow(out)
  original_attributes$pretty_names <- stats::setNames(out$Parameter, out$Parameter)
  attributes(out) <- original_attributes

  # no df
  out$df_error <- NULL
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  attr(out, "measure") <- "Estimate"

  if (!"Method" %in% names(out)) {
    out$Method <- "Robust meta-analysis using 'metaplus'"
  }

  attr(out, "title") <- unique(out$Method)

  out
}


#' @export
standard_error.metaplus <- function(model, ...) {
  ci_low <- as.vector(model$results[, "95% ci.lb"])
  ci_high <- as.vector(model$results[, "95% ci.ub"])
  cis <- apply(cbind(ci_low, ci_high), MARGIN = 1, diff)

  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(model$results)),
    SE = cis / (2 * stats::qnorm(.975))
  )

  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
p_value.metaplus <- function(model, ...) {
  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(model$results)),
    p = as.vector(model$results[, "pvalue"])
  )
  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
ci.metaplus <- function(x, ...) {
  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(x$results)),
    CI_low = as.vector(x$results[, "95% ci.lb"]),
    CI_high = as.vector(x$results[, "95% ci.ub"])
  )

  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}




###### .meta_random -------------------


#' @importFrom insight get_priors
#' @export
model_parameters.meta_random <- function(model,
                                         ci = .95,
                                         ci_method = "hdi",
                                         exponentiate = FALSE,
                                         include_studies = TRUE,
                                         verbose = TRUE,
                                         ...) {

  # process arguments
  params <- as.data.frame(model$estimates)
  ci_method <- match.arg(ci_method, choices = c("hdi", "eti"))

  # parameters of studies included
  study_params <- model$data
  fac <- stats::qnorm((1 + ci) / 2, lower.tail = TRUE)

  out_study <- data.frame(
    Parameter = study_params$labels,
    Coefficient = study_params$y,
    SE = study_params$SE,
    CI_low = study_params$y - fac * study_params$SE,
    CI_high = study_params$y + fac * study_params$SE,
    Weight = 1 / study_params$SE^2,
    BF = NA,
    Rhat = NA,
    ESS = NA,
    Component = "studies",
    Prior_Distribution = NA,
    Prior_Location = NA,
    Prior_Scale = NA,
    stringsAsFactors = FALSE
  )

  # extract ci-level and find ci-columns
  ci <- .meta_bma_extract_ci(params)
  ci_cols <- .metabma_ci_columns(ci_method, ci)

  # parameters of overall / tau
  out <- data.frame(
    Parameter = rownames(params),
    Coefficient = params$mean,
    SE = params$sd,
    CI_low = params[[ci_cols[1]]],
    CI_high = params[[ci_cols[2]]],
    Weight = NA,
    BF = NA,
    Rhat = params$Rhat,
    ESS = params$n_eff,
    Component = "meta",
    stringsAsFactors = FALSE
  )

  # add prior information
  priors <- insight::get_priors(model)

  out$Prior_Distribution <- priors$Distribution
  out$Prior_Location <- priors$Location
  out$Prior_Scale <- priors$Scale

  # fix intercept name
  out$Parameter[out$Parameter == "d"] <- "Overall"

  # add BF
  out$BF[1] <- model$BF[2, 1]

  # merge
  out <- rbind(out_study, out)

  # filter studies?
  if (isFALSE(include_studies)) {
    out <- out[out$Parameter %in% c("Overall", "tau"), ]
  }

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    out <- .exponentiate_parameters(out, model, exponentiate)
  }
  out <- .add_model_parameters_attributes(
    params = out,
    model = model,
    ci = ci,
    exponentiate = exponentiate,
    ci_method = ci_method,
    verbose = verbose,
    ...
  )

  # final atributes
  attr(out, "measure") <- "Estimate"
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  class(out) <- c("parameters_model", "see_parameters_model", class(params))

  if (!"Method" %in% names(out)) {
    out$Method <- "Bayesian meta-analysis using 'metaBMA'"
  }

  attr(out, "title") <- unique(out$Method)

  out
}


#' @export
standard_error.meta_random <- function(model, ...) {
  params <- as.data.frame(model$estimates)
  out <- data.frame(
    Parameter = .remove_backticks_from_string(rownames(params)),
    SE = params$sd,
    stringsAsFactors = FALSE
  )
  out$Parameter[grepl("d", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
ci.meta_random <- function(x, method = "hdi", ...) {
  # process arguments
  params <- as.data.frame(x$estimates)
  ci_method <- match.arg(method, choices = c("hdi", "eti"))

  # extract ci-level and find ci-columns
  ci <- .meta_bma_extract_ci(params)
  ci_cols <- .metabma_ci_columns(ci_method, ci)

  out <- data.frame(
    Parameter = rownames(params),
    CI = .95,
    CI_low = params[[ci_cols[1]]],
    CI_high = params[[ci_cols[2]]],
    stringsAsFactors = FALSE
  )

  out$Parameter[grepl("d", out$Parameter)] <- "(Intercept)"
  out
}




###### .meta_fixed -------------------


#' @export
model_parameters.meta_fixed <- model_parameters.meta_random


#' @export
standard_error.meta_fixed <- standard_error.meta_random


#' @export
ci.meta_fixed <- ci.meta_random




###### .meta_bma -------------------


#' @export
model_parameters.meta_bma <- function(model,
                                      ci = .95,
                                      ci_method = "hdi",
                                      exponentiate = FALSE,
                                      include_studies = TRUE,
                                      verbose = TRUE,
                                      ...) {

  # process arguments
  params <- as.data.frame(model$estimates)
  ci_method <- match.arg(ci_method, choices = c("hdi", "eti"))

  # parameters of studies included
  study_params <- model$meta$fixed$data
  fac <- stats::qnorm((1 + ci) / 2, lower.tail = TRUE)

  out_study <- data.frame(
    Parameter = study_params$labels,
    Coefficient = study_params$y,
    SE = study_params$SE,
    CI_low = study_params$y - fac * study_params$SE,
    CI_high = study_params$y + fac * study_params$SE,
    Weight = 1 / study_params$SE^2,
    BF = NA,
    Rhat = NA,
    ESS = NA,
    Component = "studies",
    stringsAsFactors = FALSE
  )

  # extract ci-level and find ci-columns
  ci <- .meta_bma_extract_ci(params)
  ci_cols <- .metabma_ci_columns(ci_method, ci)

  out <- data.frame(
    Parameter = rownames(params),
    Coefficient = params$mean,
    SE = params$sd,
    CI_low = params[[ci_cols[1]]],
    CI_high = params[[ci_cols[2]]],
    Weight = NA,
    BF = NA,
    Rhat = params$Rhat,
    ESS = params$n_eff,
    Component = "meta",
    stringsAsFactors = FALSE
  )

  # add BF
  out$BF <- c(NA, model$BF[2, 1], model$BF[4, 1])

  # merge
  out <- rbind(out_study, out)

  # filter studies?
  if (isFALSE(include_studies)) {
    out <- out[out$Parameter %in% c("averaged", "fixed", "random"), ]
  }

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    out <- .exponentiate_parameters(out, model, exponentiate)
  }
  out <- .add_model_parameters_attributes(
    params = out,
    model = model,
    ci = ci,
    exponentiate = exponentiate,
    ci_method = ci_method,
    verbose = verbose,
    ...
  )

  # final attributes
  attr(out, "measure") <- "Estimate"
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  class(out) <- c("parameters_model", "see_parameters_model", class(params))

  if (!"Method" %in% names(out)) {
    out$Method <- "Bayesian meta-analysis using 'metaBMA'"
  }

  attr(out, "title") <- unique(out$Method)

  out
}


#' @export
standard_error.meta_bma <- standard_error.meta_random


#' @export
ci.meta_bma <- ci.meta_random




# helper ------


.meta_bma_extract_ci <- function(params) {
  hpd_col <- colnames(params)[grepl("hpd(\\d+)_lower", colnames(params))]
  as.numeric(gsub("hpd(\\d+)_lower", "\\1", hpd_col)) / 100
}


.metabma_ci_columns <- function(ci_method, ci) {
  switch(toupper(ci_method),
    "HDI" = sprintf(c("hpd%i_lower", "hpd%i_upper"), 100 * ci),
    c(sprintf("%g%%", (100 * (1 - ci)) / 2), sprintf("%g%%", 100 - (100 * (1 - ci)) / 2))
  )
}



# format_parameters -----------------------------------

#' @export
format_parameters.meta_random <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  names(params) <- params
  params
}


#' @export
format_parameters.meta_fixed <- format_parameters.meta_random


#' @export
format_parameters.meta_bma <- format_parameters.meta_random
