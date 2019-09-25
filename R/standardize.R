#' Standardization (Z-scoring)
#'
#' Performs a standardization of data (Z-scoring), i.e., centred and scaled, so that the data is expressed in terms of standard deviation (i.e., mean = 0, SD = 1) or Median Absolute Deviance (median = 0, MAD = 1). When applied to a statistical model, this function extracts the dataset, standardizes it, and refits the model with this standardized version of the dataset. The \code{\link{normalize}} function can also be used to scale all numeric variables within the 0 - 1 range.
#'
#' @param x A dataframe, a vector or a statistical model.
#' @param robust Logical, if \code{TRUE}, centering is done by substracting the
#'   median from the variables and divide it by the median absolute deviation
#'   (MAD). If \code{FALSE}, variables are standardized by substracting the
#'   mean and divide it by the standard deviation (SD).
#' @param method The method of standardization. For data.frames, can be \code{"default"} (variables are centred by mean or median, and divided by SD or MAD, depending on \code{robust}) or \code{"2sd"}, in which case they are divided by two times the deviation (SD or MAD, again depending on \code{robust}).
#' @param verbose Toggle warnings on or off.
#' @param force Logical, if \code{TRUE}, forces standardization of factors as
#'   well. Factors are converted to numerical values, with the lowest level
#'   being the value \code{1} (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param select For a data frame, character vector of column names to be
#'   standardized. If \code{NULL} (the default), all variables will be
#'   standardized.
#' @param exclude For a data frame, character vector of column names to
#'   be excluded from standardization.
#' @param include_response For a model, if \code{TRUE} (default), the response value
#'   will also be standardized. If \code{FALSE}, only the predictors will be standardized.
#'   Note that for certain models (logistic regression, count models, ...), the
#'   response value will never be standardized, to make re-fitting the model work.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The standardized object (either a standardize dataframe or a statistical model fitted on standardized data).
#'
#' @seealso \code{\link{normalize}} \code{\link{parameters_standardize}}
#' @examples
#' # Dataframes
#' summary(standardize(iris))
#'
#' # Models
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @export
standardize <- function(x, ...) {
  UseMethod("standardize")
}
