#' Parameters from Correlations and t-tests
#'
#' Parameters of h-tests (correlations, t-tests).
#'
#' @param model Object of class \code{htest} or \code{pairwise.htest}.
#' @param bootstrap Should estimates be bootstrapped?
#' @param ci Level of confidence intervals, when test statistic can be included.
#'   Currently only applies to objects from \code{chisq.test()}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model_parameters(model)
#'
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model)
#'
#' data(airquality)
#' airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
#' model <- pairwise.t.test(airquality$Ozone, airquality$Month)
#' model_parameters(model)
#'
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' model <- pairwise.prop.test(smokers, patients)
#' model_parameters(model)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.htest <- function(model, ci = 0.95, bootstrap = FALSE, ...) {
  if (bootstrap) {
    stop("Bootstrapped h-tests are not yet implemented.")
  } else {
    parameters <- .extract_parameters_htest(model, ci = ci)
  }

  attr(parameters, "ci") <- ci
  attr(parameters, "ci_test") <- attributes(model$conf.int)$conf.level
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}






#' @keywords internal
.extract_parameters_htest <- function(model, ci = 0.95) {
  m_info <- insight::model_info(model)

  if (m_info$is_correlation) {
    out <- .extract_htest_correlation(model)
  } else if (m_info$is_ttest) {
    out <- .extract_htest_ttest(model)
  } else if (m_info$is_onewaytest) {
    out <- .extract_htest_oneway(model)
  } else if (m_info$is_chi2test) {
    out <- .extract_htest_chi2(model)
    out <- .add_effectsize_chi2(model, out, ci = ci)
  } else if (m_info$is_proptest) {
    out <- .extract_htest_prop(model)
  } else if (m_info$is_binomtest) {
    out <- .extract_htest_binom(model)
  } else {
    stop("model_parameters not implemented for such h-tests yet.")
  }

  row.names(out) <- NULL
  out
}





# extract htest correlation ----------------------

.extract_htest_correlation <- function(model) {
  names <- unlist(strsplit(model$data.name, " and "))
  out <- data.frame(
    "Parameter1" = names[1],
    "Parameter2" = names[2],
    stringsAsFactors = FALSE
  )

  if (model$method == "Pearson's Chi-squared test") {
    out$Chi2 <- model$statistic
    out$df <- model$parameter
    out$p <- model$p.value
    out$Method <- "Pearson"
  } else if (grepl("Pearson", model$method)) {
    out$r <- model$estimate
    out$t <- model$statistic
    out$df <- model$parameter
    out$p <- model$p.value
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    out$Method <- "Pearson"
  } else if (grepl("Spearman", model$method)) {
    out$rho <- model$estimate
    out$S <- model$statistic
    out$df <- model$parameter
    out$p <- model$p.value
    out$Method <- "Spearman"
  } else {
    out$tau <- model$estimate
    out$z <- model$statistic
    out$df <- model$parameter
    out$p <- model$p.value
    out$Method <- "Kendall"
  }
  out
}


# extract htest ttest ----------------------

.extract_htest_ttest <- function(model) {
  if (grepl(" and ", model$data.name)) {
    names <- unlist(strsplit(model$data.name, " and "))
    out <- data.frame(
      "Parameter1" = names[1],
      "Parameter2" = names[2],
      "Mean_Parameter1" = model$estimate[1],
      "Mean_Parameter2" = model$estimate[2],
      "Difference" = model$estimate[1] - model$estimate[2],
      "t" = model$statistic,
      "df" = model$parameter,
      "p" = model$p.value,
      "CI_low" = model$conf.int[1],
      "CI_high" = model$conf.int[2],
      "Method" = model$method,
      stringsAsFactors = FALSE
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
      "df" = model$parameter,
      "p" = model$p.value,
      "CI_low" = model$conf.int[1],
      "CI_high" = model$conf.int[2],
      "Method" = model$method,
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      "Parameter" = model$data.name,
      "Mean" = model$estimate,
      "mu" = model$null.value,
      "Difference" = model$estimate - model$null.value,
      "t" = model$statistic,
      "df" = model$parameter,
      "p" = model$p.value,
      "CI_low" = model$conf.int[1],
      "CI_high" = model$conf.int[2],
      "Method" = model$method,
      stringsAsFactors = FALSE
    )
  }
  out
}


# extract htest oneway ----------------------

.extract_htest_oneway <- function(model) {
  data.frame(
    "F" = model$statistic,
    "df_num" = model$parameter[1],
    "df_denom" = model$parameter[2],
    "p" = model$p.value,
    "Method" = model$method,
    stringsAsFactors = FALSE
  )
}


# extract htest chi2 ----------------------

.extract_htest_chi2 <- function(model) {
  data.frame(
    "Chi2" = model$statistic,
    "df" = model$parameter,
    "p" = model$p.value,
    "Method" = model$method,
    stringsAsFactors = FALSE
  )
}


# extract htest prop ----------------------

.extract_htest_prop <- function(model) {
  if (is.null(model$conf.int)) {
    out <- data.frame(
      "Parameter" = model$estimate,
      "Chi2" = model$statistic,
      "df" = model$parameter[1],
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      "Parameter" = model$estimate,
      "CI_low" = model$conf.int[1],
      "CI_high" = model$conf.int[2],
      "Chi2" = model$statistic,
      "df" = model$parameter[1],
      stringsAsFactors = FALSE
    )
  }
  out$Null_value <- model$null.value
  out$p <- model$p.value
  out$Method <- model$method
  out
}


# extract htest binom ----------------------

.extract_htest_binom <- function(model) {
  out <- data.frame(
    "Parameter" = model$estimate,
    "CI_low" = model$conf.int[1],
    "CI_high" = model$conf.int[2],
    "Success" = model$statistic,
    "Trials" = model$parameter,
    stringsAsFactors = FALSE
  )
  out$Null_value <- model$null.value
  out$p <- model$p.value
  out$Method <- model$method
  out
}




# effectsizes ---------------------

.add_effectsize_chi2 <- function(model, out, ci = .95) {
  if (requireNamespace("effectsize", quietly = TRUE)) {
    # Cramers V
    es <- effectsize::cramers_v(model$observed, ci = ci)
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("Cramers_", names(es)[ci_cols])
    out <- cbind(out, es)

    # Phi
    es <- effectsize::phi(model$observed, ci = ci)
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("Phi_", names(es)[ci_cols])
    out <- cbind(out, es)

    # reorder
    col_order <- c("Chi2", "df", "Cramers_v", "Cramers_CI_low", "Cramers_CI_high",
                   "Phi", "Phi_CI_low", "Phi_CI_high", "p", "method")
    out <- out[col_order[col_order %in% names(out)]]
  }
  out
}
