#' Correlations and t-test Parameters
#'
#' Parameters of h-tests (correlations, t-tests).
#'
#' @param model Object of class \code{htest}.
#' @param bootstrap Should estimates be bootsrapped?
#' @param ... Arguments passed to or from other methods (e.g., to \link[=standardize.lm]{standardize}).
#'
#' @examples
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model)
#' @export
model_parameters.htest <- function(model, bootstrap = FALSE, ...) {
  if (bootstrap) {
    stop("Bootstrapped h-tests are not yet implemented.")
  } else {
    return(.extract_parameters_htest(model))
  }
}



#' @keywords internal
.extract_parameters_htest <- function(model) {
  if (insight::model_info(model)$is_correlation) {
    names <- unlist(strsplit(model$data.name, " and "))
    out <- data.frame(
      "Parameter1" = names[1],
      "Parameter2" = names[2]
    )

    if (grepl("Pearson", model$method)) {
      out$r <- model$estimate
      out$t <- model$statistic
      out$DoF <- model$parameter
      out$p <- model$p.value
      out$CI_low <- model$conf.int[1]
      out$CI_high <- model$conf.int[2]
      out$CI <- attributes(model$conf.int)$conf.level
      out$Method <- "Pearson"
    } else if (grepl("Spearman", model$method)) {
      out$rho <- model$estimate
      out$S <- model$statistic
      out$DoF <- model$parameter
      out$p <- model$p.value
      out$Method <- "Spearman"
    } else {
      out$tau <- model$estimate
      out$z <- model$statistic
      out$DoF <- model$parameter
      out$p <- model$p.value
      out$Method <- "Kendall"
    }
  } else if (insight::model_info(model)$is_ttest) {
    if (grepl(" and ", model$data.name)) {
      names <- unlist(strsplit(model$data.name, " and "))
      out <- data.frame(
        "Parameter1" = names[1],
        "Parameter2" = names[2],
        "Mean_Parameter1" = model$estimate[1],
        "Mean_Parameter2" = model$estimate[2],
        "Difference" = model$estimate[2] - model$estimate[1],
        "t" = model$statistic,
        "DoF" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "CI" = attributes(model$conf.int)$conf.level,
        "Method" = model$method
      )
    } else if (grepl(" by ", model$data.name)) {
      names <- unlist(strsplit(model$data.name, " by "))
      out <- data.frame(
        "Parameter" = names[1],
        "Group" = names[2],
        "Mean_Group1" = model$estimate[1],
        "Mean_Group2" = model$estimate[2],
        "Difference" = model$estimate[2] - model$estimate[1],
        "t" = model$statistic,
        "DoF" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "CI" = attributes(model$conf.int)$conf.level,
        "Method" = model$method
      )
    } else {
      out <- data.frame(
        "Parameter" = model$data.name,
        "Mean" = model$estimate,
        "mu" = model$null.value,
        "Difference" = model$estimate - model$null.value,
        "t" = model$statistic,
        "DoF" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "CI" = attributes(model$conf.int)$conf.level,
        "Method" = model$method
      )
    }
  } else {
    stop("model_parameters not implemented for such h-tests yet.")
  }

  row.names(out) <- NULL
  return(out)
}
