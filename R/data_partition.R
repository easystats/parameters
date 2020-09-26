#' Partition data into a test and a training set
#'
#' Creates a training and a test set based on a dataframe. Can also be stratified (i.e., evenly spread a given factor) using the \code{group} argument.
#'
#' @param x A data frame, or an object that can be coerced to a data frame.
#' @param training_proportion The proportion (between 0 and 1) of the training set. The remaining part will be used for the test set.
#' @param group A character vector indicating the name(s) of the column(s) used for stratified partitioning.
#'
#' @return A list of two data frames, named \code{test} and \code{training}.
#'
#' @examples
#' df <- iris
#' df$Smell <- rep(c("Strong", "Light"), 75)
#'
#' head(data_partition(df))
#' head(data_partition(df, group = "Species"))
#' head(data_partition(df, group = c("Species", "Smell")))
#' @export
data_partition <- function(x, training_proportion = 0.7, group = NULL) {
  if (!is.data.frame(x)) {
    x <- tryCatch(
      expr = {
        as.data.frame(x)
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(x)) {
      stop("`x` needs to be a data frame, or an object that can be coerced to a data frame.")
    }
  }

  training <- data.frame()
  test <- data.frame()

  if (!is.null(group)) {
    for (i in split(x, x[group])) {
      out <- .data_partition(i, training_proportion)
      training <- rbind(training, i[out$training, ])
      test <- rbind(test, i[out$test, ])
    }
  } else {
    out <- .data_partition(x, training_proportion)
    training <- rbind(training, x[out$training, ])
    test <- rbind(test, x[out$test, ])
  }

  list(
    training = training,
    test = test
  )
}


#' @keywords internal
.data_partition <- function(x, training_proportion = 0.8) {
  set.seed(333)
  training_indices <- sample(1:nrow(x), size = training_proportion * nrow(x))
  test_indices <- (1:nrow(x))[-training_indices]

  list(
    training = training_indices,
    test = test_indices
  )
}
