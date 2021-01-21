# package metafor


####### .rma -----------------


#' Parameters from Meta-Analysis
#'
#' Extract and compute indices and measures to describe parameters of meta-analysis models.
#'
#' @param include_studies Logical, if \code{TRUE} (default), includes parameters
#'   for all studies. Else, only parameters for overall-effects are shown.
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
                                 include_studies = TRUE,
                                 verbose = TRUE,
                                 ...) {
  # handle ci-level that was defined in function call...
  ci_level <- parse(text = .safe_deparse(model$call))[[1]]$level
  if (!is.null(ci_level) && missing(ci)) {
    ci <- ci_level / 100
  }

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
  attr(out, "measure") <- model$measure

  if (!"Method" %in% names(out)) {
    out$Method <- "Meta-analysis using 'metafor'"
  }

  attr(out, "title") <- unique(out$Method)

  out
}


#' @export
p_value.rma <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = model$pval
  )
}


#' @export
ci.rma <- function(x, ci = .95, ...) {
  params <- insight::get_parameters(x)
  out <- tryCatch(
    {
      tmp <- lapply(ci, function(i) {
        model <- stats::update(x, level = i)
        .data_frame(
          Parameter = params$Parameter,
          CI = i * 100,
          CI_low = as.vector(model$ci.lb),
          CI_high = as.vector(model$ci.ub)
        )
      })
      .remove_backticks_from_parameter_names(do.call(rbind, tmp))
    },
    error = function(e) {
      NULL
    }
  )
  if (is.null(out)) {
    se <- standard_error(x)
    out <- lapply(ci, function(i) {
      alpha <- (1 + i) / 2
      fac <- stats::qnorm(alpha)
      .data_frame(
        Parameter = params$Parameter,
        CI = i * 100,
        CI_low = params$Estimate - as.vector(se$SE) * fac,
        CI_high = params$Estimate + as.vector(se$SE) * fac
      )
    })
    out <- .remove_backticks_from_parameter_names(do.call(rbind, out))
  }
  out
}


#' @export
standard_error.rma <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = model[["se"]]
  )
}


#' @export
format_parameters.rma <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  names(params) <- params
  params
}
