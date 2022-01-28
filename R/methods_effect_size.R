#' @export
ci.effectsize_std_params <- function(x, ci = .95, verbose = TRUE, ...) {
  se <- attr(x, "standard_error")

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

  # check if we have model. if so, use df from model
  model <- .get_object(x)
  if (!is.null(model)) {
    df <- degrees_of_freedom(model, method = "any")
    if (!is.null(df)) {
      if (length(df) > 1 && length(df) != nrow(x)) {
        df <- Inf
      }
    } else {
      df <- Inf
    }
  } else {
    df <- Inf
  }

  out <- lapply(ci, function(i) {
    alpha <- (1 + i) / 2
    fac <- stats::qt(alpha, df = df)
    data.frame(
      Parameter = x$Parameter,
      CI = i,
      CI_low = x$Std_Coefficient - se * fac,
      CI_high = x$Std_Coefficient + se * fac,
      stringsAsFactors = FALSE
    )
  })

  insight::text_remove_backticks(do.call(rbind, out))
}


#' @export
ci.effectsize_table <- ci.effectsize_std_params


#' @export
standard_error.effectsize_table <- standard_error.effectsize_std_params
