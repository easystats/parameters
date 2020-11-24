#' Parameters from Meta-Analysis
#'
#' Extract and compute indices and measures to describe parameters of meta-analysis models.
#'
#' @inheritParams model_parameters.default
#'
#' @examples
#' library(parameters)
#' mydat <<- data.frame(
#'   effectsize = c(-0.393, 0.675, 0.282, -1.398),
#'   stderr = c(0.317, 0.317, 0.13, 0.36)
#' )
#' if (require("metafor")) {
#'   model <- rma(yi = effectsize, sei = stderr, method = "REML", data = mydat)
#'   model_parameters(model)
#' }
#' \dontrun{
#' # with subgroups
#' if (require("metafor")) {
#'   data(dat.bcg)
#'   dat <- escalc(
#'     measure = "RR",
#'     ai = tpos,
#'     bi = tneg,
#'     ci = cpos,
#'     di = cneg,
#'     data = dat.bcg
#'   )
#'   dat$alloc <- ifelse(dat$alloc == "random", "random", "other")
#'   model <- rma(yi, vi, mods = ~alloc, data = dat, digits = 3, slab = author)
#'   model_parameters(model)
#' }
#'
#' if (require("metaBMA")) {
#'   data(towels)
#'   m <- meta_random(logOR, SE, study, data = towels)
#'   model_parameters(m)
#' }
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @importFrom stats qt pt setNames
#' @export
model_parameters.rma <- function(model,
                                 ci = .95,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 verbose = TRUE,
                                 ...) {

  meta_analysis_overall <-
    .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  subgroups <- NULL
  group_variable <- NULL

  # subgroup analyses?
  if (!is.null(model$formula.mods)) {
    group_variable <- deparse(model$formula.mods[[2]])[1]
    model_data <- insight::get_data(model)
    if (group_variable %in% colnames(model_data)) {
      subgroups <- sort(unique(model_data[[group_variable]]))
    }
  }

  if (nrow(meta_analysis_overall) > 1 && !is.null(subgroups)) {
    meta_analysis_overall$Subgroup <- subgroups
    meta_analysis_overall$Parameter <- "(Intercept)"
  }

  alpha <- (1 + ci) / 2

  rma_parameters <- if (!is.null(model$slab) && !is.numeric(model$slab)) {
    sprintf("%s", model$slab)
  } else {
    sprintf("Study %i", 1:model[["k"]])
  }

  rma_coeffients <- as.vector(model$yi)
  rma_se <- as.vector(sqrt(model$vi))
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
    Weight = 1 / as.vector(model$vi),
    stringsAsFactors = FALSE
  )

  # subgroup analyses?
  if (!is.null(subgroups)) {
    meta_analysis_studies$Subgroup <- insight::get_data(model)[[group_variable]]
  }

  original_attributes <- attributes(meta_analysis_overall)
  out <- merge(meta_analysis_studies, meta_analysis_overall, all = TRUE, sort = FALSE)

  # fix intercept name
  out$Parameter[out$Parameter == "(Intercept)"] <- "Overall"

  original_attributes$names <- names(out)
  original_attributes$row.names <- 1:nrow(out)
  original_attributes$pretty_names <- stats::setNames(out$Parameter, out$Parameter)
  attributes(out) <- original_attributes

  # no df
  out$df_error <- NULL
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  attr(out, "measure") <- model$measure

  out
}





#' @export
model_parameters.metaplus <- function(model,
                                      ci = .95,
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      verbose = TRUE,
                                      ...) {

  meta_analysis_overall <-
    suppressWarnings(.model_parameters_generic(
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
  out <- out[out$Parameter != "tau2", ]

  original_attributes$names <- names(out)
  original_attributes$row.names <- 1:nrow(out)
  original_attributes$pretty_names <- stats::setNames(out$Parameter, out$Parameter)
  attributes(out) <- original_attributes

  # no df
  out$df_error <- NULL
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  attr(out, "measure") <- "Estimate"

  out
}



#' @importFrom insight get_priors
#' @export
model_parameters.meta_random <- function(model,
                                         ci = .95,
                                         ci_method = "hdi",
                                         exponentiate = FALSE,
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

  if (exponentiate) out <- .exponentiate_parameters(out)
  out <-
    .add_model_parameters_attributes(
      params = out,
      model = model,
      ci = ci,
      exponentiate = exponentiate,
      ci_method = ci_method,
      ...
    )

  # final atributes
  attr(out, "measure") <- "Estimate"
  attr(out, "object_name") <- .safe_deparse(substitute(model))

  ## TODO remove once insight > 0.10.0 is on CRAN
  attr(out, "data") <- model$data$data
  class(out) <- c("parameters_model", "see_parameters_model", class(params))

  out
}


#' @export
model_parameters.meta_fixed <- model_parameters.meta_random


#' @export
model_parameters.meta_bma <- function(model,
                                      ci = .95,
                                      ci_method = "hdi",
                                      exponentiate = FALSE,
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

  if (exponentiate) out <- .exponentiate_parameters(out)
  out <-
    .add_model_parameters_attributes(
      params = out,
      model = model,
      ci = ci,
      exponentiate = exponentiate,
      ci_method = ci_method,
      ...
    )

  # final attributes
  attr(out, "measure") <- "Estimate"
  attr(out, "object_name") <- .safe_deparse(substitute(model))

  ## TODO remove once insight > 0.10.0 is on CRAN
  attr(out, "data") <- model$meta$fixed$data$data
  class(out) <- c("parameters_model", "see_parameters_model", class(params))

  out
}






# helper ------


.meta_bma_extract_ci <- function(params) {
  hpd_col <- colnames(params)[grepl("hpd(\\d+)_lower", colnames(params))]
  as.numeric(gsub("hpd(\\d+)_lower", "\\1", hpd_col)) / 100
}


.metabma_ci_columns <- function(ci_method, ci) {
  switch(
    toupper(ci_method),
    "HDI" = sprintf(c("hpd%i_lower", "hpd%i_upper"), 100 * ci),
    c(sprintf("%g%%", (100 * (1 - ci)) / 2), sprintf("%g%%", 100 - (100 * (1 - ci)) / 2))
  )
}
