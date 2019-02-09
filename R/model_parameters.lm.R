#' Linear Model Parameters
#'
#' Parameters of linear models.
#'
#' @param model Object of class \link{lm}.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters.
#' @param ... Arguments passed to or from other methods (e.g., to \code{standardize}).
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' model_parameters(model, standardize = TRUE)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @import dplyr
#' @export
model_parameters.lm <- function(model, ci = .95, standardize = FALSE, ...) {

  # Processing
  parameters <- cbind(broom::tidy(model), broom::confint_tidy(model, conf.level = ci)) %>%
    dplyr::rename_(
      "Parameter" = "term",
      "beta" = "estimate",
      "SE" = "std.error",
      "t" = "statistic",
      "p" = "p.value",
      "CI_low" = "conf.low",
      "CI_high" = "conf.high"
    )

  parameters$CI <- ci
  parameters$DoF_residual <- model$df.residual


  # Standardized
  if (standardize) {
    data <- insight::get_data(model)
    std_model <- standardize(model, data = data, robust = FALSE)
    std_table <- cbind(broom::tidy(std_model), broom::confint_tidy(std_model, ci)) %>%
      rename_(
        "Std_Parameter" = "term",
        "Std_beta" = "estimate",
        "Std_SE" = "std.error",
        "Std_t" = "statistic",
        "Std_p" = "p.value",
        "Std_CI_low" = "conf.low",
        "Std_CI_high" = "conf.high"
      )
    std_table <- select(std_table, one_of("Std_beta", "Std_SE", "Std_CI_low", "Std_CI_high"))
    parameters <- cbind(parameters, std_table)
  }



  return(parameters)
}
