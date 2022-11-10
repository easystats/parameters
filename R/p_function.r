#' @title p-value or consonance function
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
#' compatibility intervals should be printed or plotted. In plots, these levels
#' are highlighted by vertical lines. It is possible to increase thickness for
#' one or more of these lines by providing a names vector, where the to be
#' highlighted values should be named `"emph"`, e.g
#' `ci_levels = c(0.25, 0.5, emph = 0.95)`.
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
#' p_function(model)
#' @export
p_function <- function(model,
                       ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
                       exponentiate = FALSE,
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
  if (isTRUE(exponentiate)) {
    out$CI_low <- exp(out$CI_low)
    out$CI_high <- exp(out$CI_high)
  }

  # data for p_function ribbon
  data_ribbon <- datawizard::data_to_long(
    out,
    select = c("CI_low", "CI_high"),
    values_to = "x"
  )

  # data for vertical CI level lines
  out <- out[out$CI %in% ci_levels, ]
  out$group <- 1

  # emphasize focal hypothesis line
  emphasize <- which(names(ci_levels) == "emph")
  if (length(emphasize)) {
    out$group[out$CI == ci_levels[emphasize]] <- 2
  }

  attr(out, "data") <- data_ribbon
  attr(out, "point_estimate") <- point_estimate
  attr(out, "pretty_names") <- suppressWarnings(format_parameters(model, ...))

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
                                         pretty_names = TRUE,
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

  out <- do.call(datawizard::data_merge, list(dat, by = "Parameter"))
  attr(out, "pretty_names") <- attributes(x)$pretty_names

  insight::format_table(
    out,
    digits = digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    format = format,
    pretty_names = pretty_names
  )
}


#' @export
print.parameters_p_function <- function(x,
                                        digits = 2,
                                        ci_width = "auto",
                                        ci_brackets = TRUE,
                                        pretty_names = TRUE,
                                        ...) {
  cat(.print_p_function(
    x,
    digits,
    ci_width,
    ci_brackets,
    pretty_names = pretty_names,
    format = "text",
    ...
  ))
}


#' @export
print_md.parameters_p_function <- function(x,
                                           digits = 2,
                                           ci_width = "auto",
                                           ci_brackets = c("(", ")"),
                                           pretty_names = TRUE,
                                           ...) {
  .print_p_function(x, digits, ci_width, ci_brackets, pretty_names, format = "markdown", ...)
}


#' @export
print_html.parameters_p_function <- function(x,
                                             digits = 2,
                                             ci_width = "auto",
                                             ci_brackets = c("(", ")"),
                                             pretty_names = TRUE,
                                             ...) {
  .print_p_function(x, digits, ci_width, ci_brackets, pretty_names, format = "html", ...)
}



# helper ----------

.print_p_function <- function(x,
                              digits = 2,
                              ci_width = "auto",
                              ci_brackets = c("(", ")"),
                              pretty_names = TRUE,
                              format = "html",
                              ...) {
  formatted_table <- format(
    x,
    digits = digits,
    format = format,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    pretty_names = pretty_names,
    ...
  )

  insight::export_table(
    formatted_table,
    format = format,
    caption = "Consonance Function",
    ...
  )
}

# model <- lm(Sepal.Length ~ Species, data = iris)
# for later use: highlight p-value for secific parameter estimate values
# stat <- insight::get_statistic(model)
# se <- parameters::standard_error(model)
# estimate to test against - compute p-value for specific estimate
# null_estimate <- 1.5
# p <- 2 * stats::pt(abs(stat$Statistic[3]) - (null_estimate / se$SE[3]), df = 147, lower.tail = FALSE)
# bayestestR::p_to_pd(p)
