#' Machine learning model trained to classify distributions
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
find_distribution <- function(x, probabilities=FALSE){

  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package `caret` required for distribution classification.", call. = FALSE)
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package `randomForest` required for distribution classification.", call. = FALSE)
  }


  # Extract features
  data <- data.frame(
    "Mean" = mean(x),
    "SD" = sd(x),
    "Median" = median(x),
    "MAD" = mad(x, constant=1),
    "Mean_Median_Distance" = mean(x) - median(x),
    "Mean_Mode_Distance" = mean(x) - bayestestR::map_estimate(x),
    "SD_MAD_Distance" = sd(x) - mad(x, constant=1),
    "Mode" = bayestestR::map_estimate(x),
    "Range" = diff(range(x)) / sd(x),
    "IQR" = stats::IQR(x),
    "Skewness" = skewness(x),
    "Kurtosis" = kurtosis(x),
    "Smoothness_Cor" = parameters::smoothness(density(x)$y, method="cor"),
    "Smoothness_Diff" = parameters::smoothness(density(x)$y, method="cor")
  )

  density_df <- as.data.frame(t(parameters::normalize(density(x, n=20)$y)))
  names(density_df) <- paste0("Density_", 1:ncol(density_df))

  data <- cbind(data, density_df)

  # Predict
  # data("classify_distribution", envir=environment())
  # data("classify_distribution")
  # get("classify_distribution")
  if(probabilities){
    return(predict(classify_distribution, data, type="prob"))
  } else{
    return(as.character(predict(classify_distribution, data)))
  }
}