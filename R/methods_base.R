#' @rdname model_parameters.brmsfit
#' @export
model_parameters.data.frame <- function(model,
                                        as_draws = FALSE,
                                        exponentiate = FALSE,
                                        verbose = TRUE,
                                        ...) {
  # treat data frame as bootstraps/posteriors?
  if (isTRUE(as_draws)) {
    return(model_parameters.draws(model, exponentiate = exponentiate, verbose = verbose, ...))
  }
  if (isTRUE(verbose)) {
    insight::format_warning(
      "A `data.frame` object is no valid regression model object and cannot be used with `model_parameters()`."
    )
  }
  NULL
}




# Standard Errors from standard classes ---------------------------------------------


#' @rdname standard_error
#' @export
standard_error.factor <- function(model, force = FALSE, verbose = TRUE, ...) {
  if (!force) {
    if (verbose) {
      insight::format_warning("Can't compute standard error of non-numeric variables.")
    }
    return(NA)
  }
  standard_error(as.numeric(model), ...)
}


#' @export
standard_error.character <- standard_error.factor


#' @export
standard_error.numeric <- function(model, ...) {
  sqrt(stats::var(model, na.rm = TRUE) / length(stats::na.omit(model)))
}


#' @export
standard_error.data.frame <- function(model, verbose = TRUE, ...) {
  unlist(sapply(model, standard_error, verbose = verbose))
}


#' @export
standard_error.list <- function(model, verbose = TRUE, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    standard_error(model)
  } else if (isTRUE(verbose)) {
    insight::print_color("\nCould not extract standard errors from model object.\n", "red")
  }
}


#' @export
standard_error.table <- function(model, ...) {
  # compute standard error of proportions
  if (length(dim(model)) == 1) {
    total.n <- as.vector(sum(model))
    rel.frq <- as.vector(model) / total.n

    out <- .data_frame(
      Value = names(model),
      Proportion = rel.frq,
      SE = suppressWarnings(sqrt(rel.frq * (1 - rel.frq) / total.n))
    )
  } else {
    out <- NA
  }

  out
}


#' @export
standard_error.xtabs <- standard_error.table


#' @export
standard_error.parameters_standardized <- function(model, verbose = TRUE, ...) {
  se <- attr(model, "standard_error")

  if (is.null(se)) {
    if (isTRUE(verbose)) {
      insight::print_color("\nCould not extract standard errors of standardized coefficients.\n", "red")
    }
    return(NULL)
  }

  # for "refit" method
  if (is.data.frame(se) && "SE" %in% colnames(se)) {
    se <- se$SE
  }

  out <- .data_frame(
    Parameter = model$Parameter,
    SE = as.vector(se)
  )

  insight::text_remove_backticks(out, verbose = FALSE)
}




# p-Values from standard classes ---------------------------------------------

#' @export
p_value.numeric <- function(model, null = 0, ...) {
  # k_lt0 <- sum(model <= 0)
  # k_gt0 <- sum(model >= 0)
  # k <- 2 * min(k_lt0, k_gt0)
  # N <- length(model)

  # https://blogs.sas.com/content/iml/2011/11/02/how-to-compute-p-values-for-a-bootstrap-distribution.html
  # https://stats.stackexchange.com/a/28725/293056
  x <- stats::na.omit(model)
  xM <- mean(x)
  x0 <- x - xM
  k <- sum(abs(x0) > abs(xM - null)) # two tailed p-value
  N <- length(x)
  (k + 1) / (N + 1)
}


#' @export
p_value.data.frame <- function(model, ...) {
  model_data <- model[vapply(model, is.numeric, TRUE)]
  .data_frame(
    Parameter = names(model_data),
    p = vapply(model_data, p_value, 1)
  )
}


#' @export
p_value.list <- function(model, method = NULL, verbose = TRUE, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    p_value(model, method = method)
  } else if (isTRUE(verbose)) {
    insight::format_warning("Could not extract p-values from model object.")
  }
}
