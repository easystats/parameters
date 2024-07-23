#' @export
model_parameters.svy2lme <- function(model,
                                     ci = 0.95,
                                     effects = "all",
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     include_sigma = FALSE,
                                     ...) {
  dots <- list(...)
  # which component to return?
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))
  params <- params_variance <- NULL

  if (effects %in% c("fixed", "all")) {
    # Processing
    fun_args <- list(
      model,
      ci = ci,
      ci_method = "wald",
      standardize = NULL,
      p_adjust = NULL,
      wb_component = FALSE,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      include_sigma = include_sigma,
      summary = FALSE,
      vcov = NULL,
      vcov_args = NULL
    )
    fun_args <- c(fun_args, dots)
    params <- do.call(".extract_parameters_mixed", fun_args)

    params$Effects <- "fixed"
  }

  att <- attributes(params)

  if (effects %in% c("random", "all")) {
    params_variance <- .extract_random_variances(
      model,
      ci = ci,
      effects = effects
    )
  }

  # merge random and fixed effects, if necessary
  if (!is.null(params) && !is.null(params_variance)) {
    params$Level <- NA
    params$Group <- ""
    params <- params[match(colnames(params_variance), colnames(params))]
  }

  params <- rbind(params, params_variance)
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
    exponentiate = FALSE,
    bootstrap = FALSE,
    iterations = 1000,
    ci_method = "wald",
    p_adjust = NULL,
    verbose = verbose,
    summary = FALSE,
    group_level = FALSE,
    wb_component = FALSE,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.svy2lme <- function(model, ...) {
  .data_frame(
    Parameter = .remove_backticks_from_string(colnames(model$Vbeta)),
    SE = as.vector(sqrt(diag(model$Vbeta)))
  )
}


#' @export
p_value.svy2lme <- function(model, ...) {
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  .data_frame(
    Parameter = stat$Parameter,
    p = as.vector(p)
  )
}
