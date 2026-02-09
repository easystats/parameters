# classes: .coxph, .aareg, .survreg, .riskRegression, .survfit

#################### .survfit ------

#' @export
model_parameters.survfit <- function(
  model,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  s <- summary(model)
  # extract all elements with same length, which occur most in that list
  # that is the data we need
  uniqv <- unique(lengths(s))
  tab <- tabulate(match(lengths(s), uniqv))
  idx <- which.max(tab)
  most_len <- uniqv[idx]

  # convert list into data frame, only for elements of same length
  params <- as.data.frame(s[lengths(s) == most_len])

  # keep specific columns
  keep_columns <- intersect(
    c("time", "n.risk", "n.event", "surv", "std.err", "strata", "lower", "upper"),
    colnames(params)
  )
  params <- params[keep_columns]

  # rename
  params <- datawizard::data_rename(
    params,
    select = c(
      Time = "time",
      `N Risk` = "n.risk",
      `N Event` = "n.event",
      Survival = "surv",
      SE = "std.err",
      Group = "strata",
      CI_low = "lower",
      CI_high = "upper"
    )
  )

  # fix labels
  params$Group <- gsub("x=", "", params$Group, fixed = TRUE)

  # These are integers, need to be character to display without decimals
  params$Time <- as.character(params$Time)
  params[["N Risk"]] <- as.character(params[["N Risk"]])
  params[["N Event"]] <- as.character(params[["N Event"]])

  attr(params, "ci") <- s$conf.int
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#################### .coxph ------

#' @export
standard_error.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error(model, ...))
  }

  params <- insight::get_parameters(model)
  junk <- utils::capture.output({
    s <- summary(model)
  })
  cs <- stats::coef(s)
  if (isTRUE(s$used.robust)) {
    se <- cs[, 4]
  } else {
    se <- cs[, 3]
  }

  # check
  if (length(se) > nrow(params)) {
    se <- se[match(params$Parameter, .remove_backticks_from_string(rownames(cs)))]
  }

  .data_frame(Parameter = params$Parameter, SE = as.vector(se))
}


#' @export
p_value.coxph <- function(model, ...) {
  params <- insight::get_parameters(model)
  stats <- insight::get_statistic(model)

  params <- merge(params, stats, sort = FALSE)
  statistic <- attributes(stats)$statistic

  # convert in case of z
  if (identical(statistic, "z-statistic")) {
    params$Statistic <- params$Statistic^2
  }

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(1 - stats::pchisq(params$Statistic, df = 1))
  )
}


#################### .aareg ------

#' @export
standard_error.aareg <- function(model, ...) {
  s <- summary(model)
  se <- s$table[, "se(coef)"]

  .data_frame(Parameter = .remove_backticks_from_string(names(se)), SE = as.vector(se))
}


#' @export
p_value.aareg <- function(model, ...) {
  s <- summary(model)
  p <- s$table[, "p"]

  .data_frame(Parameter = .remove_backticks_from_string(names(p)), p = as.vector(p))
}


#################### .survreg ------

#' @export
standard_error.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(standard_error.default(model, ...))
  }

  s <- summary(model)
  se <- s$table[, 2]

  .data_frame(Parameter = .remove_backticks_from_string(names(se)), SE = as.vector(se))
}


#' @export
p_value.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (.check_vcov_args(robust, ...)) {
    return(p_value.default(model, ...))
  }
  s <- summary(model)
  p <- s$table[, "p"]
  .data_frame(Parameter = .remove_backticks_from_string(names(p)), p = as.vector(p))
}


#################### .riskRegression ------

#' @export
standard_error.riskRegression <- function(model, ...) {
  junk <- utils::capture.output(cs <- stats::coef(model))
  .data_frame(
    Parameter = .remove_backticks_from_string(as.vector(cs[, 1])),
    SE = as.numeric(cs[, "StandardError"])
  )
}


#' @export
p_value.riskRegression <- function(model, ...) {
  junk <- utils::capture.output(cs <- stats::coef(model))
  .data_frame(
    Parameter = .remove_backticks_from_string(as.vector(cs[, 1])),
    p = as.numeric(cs[, "Pvalue"])
  )
}
