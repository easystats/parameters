#' Machine learning model trained to classify distributions
#'
#' Mean accuracy and Kappa of 0.96.
#'
#'
"classify_distribution"





#' Classify the variable's distribution using machine learning
#'
#'
#' This function uses an internal random forest model to classify the variable's distribution.
#'
#' @param x A numeric vector.
#' @param probabilities Return a dataframe containing the probabilities of belonging to each distribution type.
#'
#' @examples
#' find_distribution(rnorm(100))
#' find_distribution(rpois(100, lambda = 4))
#' find_distribution(runif(100))
#' @importFrom bayestestR map_estimate
#' @importFrom stats IQR density predict
#' @export
find_distribution <- function(x, probabilities = FALSE) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package `caret` required for distribution classification. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package `randomForest` required for distribution classification. Please install it.", call. = FALSE)
  }


  # Extract features
  density_Z <- parameters::normalize(stats::density(x, n = 100)$y)

  # Extract features
  data <- data.frame(
    "Mean" = mean(x),
    "SD" = stats::sd(x),
    "Median" = stats::median(x),
    "MAD" = mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - stats::median(x),
    "Mean_Mode_Distance" = mean(x) - bayestestR::map_estimate(x),
    "SD_MAD_Distance" = stats::sd(x) - stats::mad(x, constant = 1),
    "Mode" = bayestestR::map_estimate(x),
    "Range" = diff(range(x)) / stats::sd(x),
    "IQR" = stats::IQR(x),
    "Skewness" = skewness(x),
    "Kurtosis" = kurtosis(x),
    "Smoothness_Cor_1" = smoothness(density_Z, method = "cor", lag = 1),
    "Smoothness_Diff_1" = smoothness(density_Z, method = "diff", lag = 1),
    "Smoothness_Cor_5" = smoothness(density_Z, method = "cor", lag = 5),
    "Smoothness_Diff_5" = smoothness(density_Z, method = "diff", lag = 5)
  )


  # Predict
  if (probabilities) {
    return(stats::predict(classify_distribution, data, type = "prob"))
  } else {
    return(as.character(stats::predict(classify_distribution, data)))
  }
}
