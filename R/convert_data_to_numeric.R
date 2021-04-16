#' Convert data to numeric
#'
#' Convert data to numeric by converting characters to factors and factors to either numeric levels or dummy variables.
#'
#' @param x A data frame or a vector.
#' @param dummy_factors Transform factors to dummy factors (all factor levels as different columns filled with a binary 0-1 value).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' head(convert_data_to_numeric(iris))
#' @return A data frame of numeric variables.
#' @export
convert_data_to_numeric <- function(x, dummy_factors = TRUE, ...) {
  UseMethod("convert_data_to_numeric")
}

#' @rdname convert_data_to_numeric
#' @export
data_to_numeric <- convert_data_to_numeric


#' @export
convert_data_to_numeric.data.frame <- function(x, dummy_factors = TRUE, ...) {
  out <- sapply(x, convert_data_to_numeric, dummy_factors = dummy_factors, simplify = FALSE)
  as.data.frame(do.call(cbind, out))
}


#' @export
convert_data_to_numeric.numeric <- function(x, ...) {
  as.numeric(x)
}

#' @export
convert_data_to_numeric.double <- convert_data_to_numeric.numeric

#' @export
convert_data_to_numeric.logical <- convert_data_to_numeric.numeric


#' @export
convert_data_to_numeric.factor <- function(x, dummy_factors = TRUE, ...) {
  if (dummy_factors) {
    out <- as.data.frame(stats::model.matrix(~x, contrasts.arg = list(x = "contr.treatment")))
    out[1] <- as.numeric(rowSums(out[2:ncol(out)]) == 0)
    names(out) <- levels(x)
  } else {
    out <- as.numeric(x)
  }
  out
}





#' @export
convert_data_to_numeric.character <- function(x, dummy_factors = FALSE, ...) {
  nums <- grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", x)
  if (all(nums)) {
    out <- as.numeric(nums)
  } else {
    out <- convert_data_to_numeric(as.factor(nums), dummy_factors = dummy_factors)
  }
  out
}
