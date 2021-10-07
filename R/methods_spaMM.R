
#' @export
model_parameters.HLfit <- model_parameters.default


#' @export
ci.HLfit <- function(x,
                     ci = 0.95,
                     method = c("wald", "ml1", "betwithin", "profile", "boot"),
                     iterations = 100,
                     ...) {
  method <- match.arg(tolower(method))

  # Wald approx
  if (method == "wald") {
    out <- .ci_generic(model = x, ci = ci, dof = Inf)

    # ml1 approx
  } else if (method == "ml1") {
    out <- ci_ml1(x, ci)

    # betwithin approx
  } else if (method == "betwithin") {
    out <- ci_betwithin(x, ci)

    # profiled
  } else if (method == "profile") {
    nparms <- n_parameters(x)
    conf <- stats::confint(x, parm = 1:nparms, level = ci, verbose = FALSE, boot_args = NULL)
    if (nparms == 1) {
      out <- as.data.frame(t(conf$interval))
    } else {
      out <- as.data.frame(do.call(rbind, lapply(conf, function(i) i$interval)))
    }
    colnames(out) <- c("CI_low", "CI_high")
    out$Parameter <- insight::find_parameters(x, effects = "fixed", flatten = TRUE)
    out$CI <- ci
    out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  }

  #   # bootstrapping
  # } else if (method == "boot") {
  #   out <- stats::confint(x, parm = n_parameters(x), level = ci, verbose = FALSE, boot_args = list(nsim = iterations, showpbar = FALSE))
  # }

  out
}


#' @export
standard_error.HLfit <- function(model, method = NULL, ...) {
  if (is.null(method)) method <- "wald"

  utils::capture.output(se <- summary(model)$beta_table[, 2])
  .data_frame(
    Parameter = insight::find_parameters(model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    SE = as.vector(se)
  )
}


#' @export
p_value.HLfit <- p_value.cpglmm
