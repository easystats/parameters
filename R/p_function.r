#' @title p or consonance function
#' @name p_function
#'
#' @description Compute p-values and compatibility (confidence) intervals for
#' statistical models, at different levels. This function is also called
#' consonance function. It allows to see which estimates are compatible with
#' the model at various compatibility levels. Use `plot()` to generate plots
#' of the _p_ resp. _consonance_ function and compatibility intervals at
#' different levels.
#'
#' @param ci_levels Vector of scalars, indicating the different levels at which
#' compatibility intervals should be printed or plotted.
#'
#' @inheritParams model_parameters
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.glmmTMB
#'
#' @return A data frame with p-values and compatibility intervals.
#'
#' @references
#' - Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
#'   science: Replace confidence and significance by compatibility and surprise.
#'   BMC Medical Research Methodology. 2020;20(1):244. \doi{10.1186/s12874-020-01105-9}
#'
#' - Fraser DAS. The P-value function and statistical inference. The American
#'   Statistician. 2019;73(sup1):135-147. \doi{10.1080/00031305.2018.1556735}
#'
#' - Schweder T, Hjort NL. Confidence and Likelihood. Scandinavian Journal of
#'   Statistics. 2002;29(2):309-332. \doi{10.1111/1467-9469.00285}
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' p_function(model, keep = "Speciesversicolor")
#' @export
p_function <- function(model,
                       ci_levels = c(.25, .5, .75, .95),
                       effects = "fixed",
                       component = "all",
                       keep = NULL,
                       drop = NULL,
                       verbose = TRUE,
                       ...) {
  # degrees of freedom
  dof <- insight::get_df(model, type = "wald")

  # standard errors
  se <- standard_error(
    model,
    effects = effects,
    component = component
  )$SE

  if (is.null(dof) || length(dof) == 0 || .is_chi2_model(model, dof)) {
    dof <- Inf
  }

  x <- do.call(rbind, lapply(seq(0, 1, 0.01), function(i) {
    suppressMessages(.ci_dof(
      model,
      ci = i,
      dof,
      effects,
      component,
      method = "wald",
      se = se,
      vcov = NULL,
      vcov_args = NULL,
      verbose = TRUE
    ))
  }))

  # data for plotting
  out <- x[!is.infinite(x$CI_low) & !is.infinite(x$CI_high), ]
  out$CI <- round(out$CI, 2)

  # most plausible value (point estimate)
  point_estimate <- out$CI_low[which.min(out$CI)]

  if (!is.null(keep) || !is.null(drop)) {
    out <- .filter_parameters(out,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  # transform non-Gaussian
  info <- insight::model_info(model)
  if (info$is_linear) {
    delta <- 0
  } else {
    delta <- 1
    out$CI_low <- exp(out$CI_low)
    out$CI_high <- exp(out$CI_high)
  }

  # data for vertical CI level lines
  out <- out[out$CI %in% ci_levels, ]
  out$group <- 1
  out$group[out$CI == 0.95] <- 2

  # data for p_function ribbon
  data_ribbon <- datawizard::data_to_long(
    out,
    select = c("CI_low", "CI_high"),
    values_to = "x"
  )
  attr(out, "data") <- data_ribbon
  attr(out, "point_estimate") <- point_estimate

  class(out) <- c("parameters_p_function", "see_p_function", "data.frame")
  out
}

#' @rdname p_function
#' @export
consonance_function <- p_function



# methods ----------------------


#' @export
plot.parameters_p_function <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
format.parameters_p_function <- function(x,
                                         digits = 2,
                                         format = NULL,
                                         ci_width = NULL,
                                         ci_brackets = TRUE,
                                        ...) {
  # print
  dat <- lapply(split(x, x$CI), function(i) {
    ci <- as.character(i$CI)[1]
    out <- datawizard::data_rename(
      i,
      pattern = c("CI_low", "CI_high"),
      replacement = c(sprintf("CI_low_%s", ci), sprintf("CI_high_%s", ci))
    )
    out$CI <- NULL
    out$group <- NULL
    out
  })

  insight::format_table(
    do.call(datawizard::data_merge, list(dat, by = "Parameter")),
    digits = digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    format = format
  )
}


#' @export
print.parameters_p_function <- function(x,
                                        digits = 2,
                                        format = NULL,
                                        ci_width = NULL,
                                        ci_brackets = TRUE,
                                        ...) {
  formatted_table <- format(
    x,
    digits = digits,
    format = "text",
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    ...
  )

  cat(insight::export_table(
    formatted_table,
    format = "text",
    caption = "Consonance Function",
    ...
  ))
}