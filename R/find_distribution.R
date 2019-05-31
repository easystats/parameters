#' Machine learning model trained to classify distributions
#'
#' Mean accuracy and Kappa of 0.86 and 0.85, repsectively. The code to train this model is available
#' \href{https://easystats.github.io/circus/articles/classify_distribution.html}{here}.
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
#' @importFrom stats IQR density predict sd mad
#' @export
find_distribution <- function(x, probabilities = FALSE) {

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package `randomForest` required for distribution classification. Please install it by running `install.packages(randomForest).", call. = FALSE)
  }


  # Extract features
  data <- data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - median(x),
    "Mean_Mode_Distance" = mean(x) - as.numeric(bayestestR::map_estimate(x, bw = "nrd0")),
    "SD_MAD_Distance" = stats::sd(x) - stats::mad(x, constant = 1),
    "Range" = diff(range(x)) / stats::sd(x),
    "IQR" = stats::IQR(x),
    "Skewness" = skewness(x),
    "Kurtosis" = kurtosis(x),
    "Uniques" = length(unique(x)) / length(x)
  )


  # Predict
  if (probabilities) {
    return(as.data.frame(stats::predict(classify_distribution, data, type = "prob")))
  } else {
    return(as.character(stats::predict(classify_distribution, data)))
  }
}
