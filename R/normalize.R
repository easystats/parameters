#' Normalization
#'
#' Performs a normalization of data. This scales all numeric
#' variables in the range 0 - 1.
#'
#' @inheritParams standardize.data.frame
#'
#' @param x Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return Standardized object.
#' @export
normalize <- function(x, ...) {
  UseMethod("normalize")
}






#' @rdname normalize
#' @importFrom stats median mad
#' @export
normalize.numeric <- function(x, verbose = TRUE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }


  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only one unique value and will not be normalized."))
    }
    return(x)
  }


  # Warning if logical vector
  if (length(unique(x)) == 2) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
    }
  }


  as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))
}








#' @export
normalize.factor <- function(x, ...) {
  x
}




#' @rdname normalize
#' @export
normalize.grouped_df <- function(x, select = NULL, exclude = NULL, ...) {
  info <- attributes(x)
  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- normalize(
      x[rows, ],
      select = select,
      exclude = exclude,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname normalize
#' @export
normalize.data.frame <- function(x, select = NULL, exclude = NULL, ...) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  x[select] <- lapply(x[select], normalize)
  x
}
