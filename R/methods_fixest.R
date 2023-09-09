# .fixest -----------------------

#' @export
model_parameters.fixest <- function(model,
                                    ci = 0.95,
                                    ci_method = NULL,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    summary = getOption("parameters_summary", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    ...) {
  # default ci-method, based on statistic
  if (is.null(ci_method)) {
    if (identical(insight::find_statistic(model), "t-statistic")) {
      ci_method <- "residual"
    } else {
      ci_method <- "wald"
    }
  }

  # extract model parameters table, as data frame
  out <- tryCatch(
    {
      .model_parameters_generic(
        model = model,
        ci = ci,
        ci_method = ci_method,
        bootstrap = bootstrap,
        iterations = iterations,
        merge_by = "Parameter",
        standardize = standardize,
        exponentiate = exponentiate,
        p_adjust = p_adjust,
        summary = summary,
        keep_parameters = keep,
        drop_parameters = drop,
        vcov = vcov,
        vcov_args = vcov_args,
        verbose = verbose,
        ...
      )
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    insight::format_error("Something went wrong... :-/")
  }

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.fixest <- function(model, vcov = NULL, vcov_args = NULL, ...) {
  params <- insight::get_parameters(model)

  if (!is.null(vcov)) {
    # we don't want to wrap this in a tryCatch because the `fixest` error is
    # informative when `vcov` is wrong.
    V <- insight::get_varcov(model, vcov = vcov, vcov_args = vcov_args)
    SE <- sqrt(diag(V))
  } else {
    stats <- summary(model)
    SE <- as.vector(stats$se)
  }

  .data_frame(
    Parameter = params$Parameter,
    SE = SE
  )
}


#' @export
degrees_of_freedom.fixest <- function(model, method = "wald", ...) {
  # fixest degrees of freedom can be tricky. best to use the function by the
  # package.
  insight::check_if_installed("fixest")
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(
    tolower(method),
    choices = c("wald", "residual")
  )
  method <- switch(method,
    "wald" = "t",
    "residual" = "resid"
  )
  fixest::degrees_freedom(model, type = method)
}




# .feglm -----------------------

#' @export
model_parameters.feglm <- model_parameters.fixest

#' @export
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. error"])
  )
}

## TODO add ci_method later?

#' @export
p_value.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, 4])
  )
}




# .fixest_multi -----------------------------------

#' @export
model_parameters.fixest_multi <- function(model,
                                          ci = 0.95,
                                          ci_method = NULL,
                                          bootstrap = FALSE,
                                          iterations = 1000,
                                          standardize = NULL,
                                          exponentiate = FALSE,
                                          p_adjust = NULL,
                                          summary = getOption("parameters_summary", FALSE),
                                          keep = NULL,
                                          drop = NULL,
                                          verbose = TRUE,
                                          vcov = NULL,
                                          vcov_args = NULL,
                                          ...) {
  # iterate over responses
  out <- lapply(
    model,
    model_parameters.default,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    summary = summary,
    keep = keep,
    drop = drop,
    verbose = verbose,
    vcov = vcov,
    vcov_args = vcov_args,
    ...
  )

  # bind lists together to one data frame, save attributes
  att <- attributes(out[[1]])
  params <- do.call(rbind, out)

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(model)
  params$Response <- id_columns$Response
  params$Group <- id_columns$Group

  attributes(params) <- utils::modifyList(att, attributes(params))
  attr(params, "model_class") <- "fixest_multi"
  params
}


#' @export
ci.fixest_multi <- function(x, ...) {
  out <- do.call(rbind, lapply(x, ci, ...))

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(x)

  # add response column
  out$Response <- id_columns$Response
  out$Group <- id_columns$Group

  row.names(out) <- NULL
  out
}


#' @export
standard_error.fixest_multi <- function(model, ...) {
  out <- do.call(rbind, lapply(model, standard_error, ...))

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(model)

  # add response column
  out$Response <- id_columns$Response
  out$Group <- id_columns$Group

  row.names(out) <- NULL
  out
}


#' @export
degrees_of_freedom.fixest_multi <- function(model, ...) {
  out <- do.call(rbind, lapply(model, degrees_of_freedom, ...))

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(model)

  # add response column
  out$Response <- id_columns$Response
  out$Group <- id_columns$Group

  row.names(out) <- NULL
  out
}


#' @export
p_value.fixest_multi <- function(model, ...) {
  out <- do.call(rbind, lapply(model, p_value, ...))

  # add response and group columns
  id_columns <- .get_fixest_multi_columns(model)

  # add response column
  out$Response <- id_columns$Response
  out$Group <- id_columns$Group

  row.names(out) <- NULL
  out
}


#' @export
simulate_model.fixest_multi <- function(model, ...) {
  lapply(model, simulate_model, ...)
}



# helper ---------------------------------

.get_fixest_multi_columns <- function(model) {
  # add response and group columns
  s <- summary(model)
  l <- lengths(lapply(s, stats::coef))
  parts <- strsplit(names(l), ";", fixed = TRUE)

  id_columns <- Map(function(i, j) {
    if (length(j) == 1 && startsWith(j, "rhs")) {
      data.frame(
        Group = rep(insight::trim_ws(sub("rhs:", "", j, fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    } else if (length(j) == 1 && startsWith(j, "lhs")) {
      data.frame(
        Response = rep(insight::trim_ws(sub("lhs:", "", j, fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Response = rep(insight::trim_ws(sub("lhs:", "", j[1], fixed = TRUE)), i),
        Group = rep(insight::trim_ws(sub("rhs:", "", j[2], fixed = TRUE)), i),
        stringsAsFactors = FALSE
      )
    }
  }, unname(l), parts)

  do.call(rbind, id_columns)
}
