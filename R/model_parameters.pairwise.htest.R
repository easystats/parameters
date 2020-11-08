#' Parameters from Pairwise tests
#'
#' Parameters of pairwise h-tests (Wilcox tests, t-tests, etc.).
#'
#' @param model Object of class \code{pairwise.htest}.
#' @param ... Arguments passed to or from other methods.
#'
#' @importFrom stats na.omit
#'
#' @examples
#' # t-test
#' data(airquality)
#' Month <- factor(Month, labels = month.abb[5:9])
#' x <- pairwise.t.test(Ozone, Month)
#' model_parameters(x)
#'
#' # proportion test
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' p <- pairwise.prop.test(smokers, patients)
#' model_parameters(p)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.pairwise.htest <- function(model, ...) {
  m <- model$p.value
  parameters <-
    data.frame(
      group1 = rep(rownames(m), each = ncol(m)),
      group2 = rep(colnames(m), times = nrow(m)),
      p.value = as.numeric(t(m)),
      p.adjust.method = model$p.adjust.method,
      stringsAsFactors = FALSE
    )

  parameters <- stats::na.omit(parameters)

  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}
