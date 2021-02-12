#' @title Compare model parameters of multiple models
#' @name compare_paramerers
#'
#' @description Compute and extract model parameters of multiple regression models.
#'   See \code{\link{model_parameters}} for further details.
#'
#' @param ... One or more regression model objects. Regression models may be
#' of different model types.
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
compare_paramerers <- function(...) {
  merge(merge(mp1, mp2, by = "Parameter", no.dups = TRUE), mp1, by = "Parameter", no.dups = TRUE)



  out <- Reduce(function(x, y) merge(x, y, by = "Parameter", all = TRUE, sort = FALSE),
                list(format(mp1, select = "minimal"), format(mp2, select = "minimal"), format(mp3, select = "minimal")))

  cat(export_table(out))

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
  mp2 <- model_parameters(model)
}
