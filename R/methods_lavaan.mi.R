#' @export
model_parameters.lavaan.mi <- function(
  model,
  ci = 0.95,
  standardize = FALSE,
  component = c("regression", "correlation", "loading", "defined"),
  keep_parameters = NULL,
  drop_parameters = NULL,
  verbose = TRUE,
  ...
) {
  params <- .extract_parameters_lavaan_mi(
    model,
    ci = ci,
    standardize = standardize,
    keep_parameters = keep_parameters,
    drop_parameters = drop_parameters,
    verbose = verbose,
    ...
  )

  # Filter
  if (all(component == "all")) {
    component <- c("regression", "correlation", "loading", "variance", "defined", "mean")
  }
  params <- params[tolower(params$Component) %in% component, ]

  # add class-attribute for printing
  class(params) <- c("parameters_sem", "see_parameters_sem", class(params))
  attr(params, "ci") <- ci
  attr(params, "model") <- model
  params
}


.extract_parameters_lavaan_mi <- function(
  model,
  ci = 0.95,
  standardize = FALSE,
  keep_parameters = NULL,
  drop_parameters = NULL,
  verbose = TRUE,
  ...
) {
  insight::check_if_installed("lavaan.mi")

  if (is.null(ci)) {
    ci <- 0.95
  }

  # set proper default
  if (is.null(standardize)) {
    standardize <- FALSE
  }

  # check for valid parameters
  valid_std_options <- c("all", "std.all", "latent", "std.lv", "no_exogenous", "std.nox")
  if (!is.logical(standardize) && !(standardize %in% valid_std_options)) {
    if (verbose) {
      insight::format_alert(
        "`standardize` should be one of `TRUE`, \"all\", \"std.all\", \"latent\", \"std.lv\", \"no_exogenous\" or \"std.nox\".", # nolint
        "Returning unstandardized solution."
      )
    }
    standardize <- FALSE
  }

  # CI
  if (length(ci) > 1L) {
    ci <- ci[1]
    if (verbose) {
      insight::format_alert(paste0(
        "lavaan models only accept one level of CI. Keeping the first one: `ci = ",
        ci,
        "`."
      ))
    }
  }

  # collect dots
  dot_args <- list(...)

  # list all argument names from the `lavaan` function
  # fmt: skip
  dot_args <- dot_args[names(dot_args) %in% c(
    "zstat", "pvalue", "standardized", "fmi", "level", "boot.ci.type", "cov.std",
    "fmi.options", "rsquare", "remove.system.eq", "remove.eq", "remove.ineq",
    "remove.def", "remove.nonfree", "add.attributes", "output", "header"
  )]

  # Get estimates
  sem_data <- do.call(
    lavaan.mi::parameterEstimates.mi,
    c(list(object = model, se = TRUE, ci = TRUE, level = ci), dot_args)
  )

  label <- sem_data$label

  # check if standardized estimates are requested, and if so, which type
  if (isTRUE(standardize) || !is.logical(standardize)) {
    if (is.logical(standardize)) {
      standardize <- "all"
    }

    type <- switch(
      standardize,
      all = ,
      std.all = "std.all",
      latent = ,
      std.lv = "std.lv",
      no_exogenous = ,
      std.nox = "std.nox",
      "std.all"
    )

    # this function errors on unknown arguments
    f <- utils::getFromNamespace("standardizedSolution.mi", "lavaan.mi")
    valid <- names(formals(f))
    dots <- list(...)
    dots <- dots[names(dots) %in% valid]
    fun_args <- c(list(model, se = TRUE, level = ci, type = type), dots)
    sem_data <- do.call("f", fun_args)
    names(sem_data)[names(sem_data) == "est.std"] <- "est"
  }

  # extract statistic column - different to normal lavaan objects
  if (!is.null(sem_data$t)) {
    statistic <- sem_data$t
    stat_col <- "t"
  } else if (!is.null(sem_data$t)) {
    statistic <- sem_data$z
    stat_col <- "z"
  } else {
    statistic <- NULL
  }

  params <- data.frame(
    To = sem_data$lhs,
    Operator = sem_data$op,
    From = sem_data$rhs,
    Coefficient = sem_data$est,
    SE = sem_data$se,
    CI_low = sem_data$ci.lower,
    CI_high = sem_data$ci.upper,
    stringsAsFactors = FALSE
  )

  if (!is.null(statistic)) {
    params[[stat_col]] <- statistic
  }

  params$p <- sem_data$pvalue

  if (!is.null(label)) {
    params$Label <- label
  }

  params$Component <- NA_character_
  params$Component[params$Operator == "=~"] <- "Loading"
  params$Component[params$Operator == "~"] <- "Regression"
  params$Component[params$Operator == "~~"] <- "Correlation"
  params$Component[params$Operator == ":="] <- "Defined"
  params$Component[params$Operator == "~1"] <- "Mean"

  params$Component[as.character(params$From) == as.character(params$To)] <- "Variance"

  if ("p" %in% colnames(params)) {
    params$p[is.na(params$p)] <- 0
  }

  if ("group" %in% names(sem_data)) {
    params$Group <- sem_data$group
  }

  # filter parameters, if requested
  if (!is.null(keep_parameters) || !is.null(drop_parameters)) {
    params <- .filter_parameters(
      params,
      keep = keep_parameters,
      drop = drop_parameters,
      verbose = verbose
    )
  }

  params
}
