#' Centering (Grand-Mean Centering)
#'
#' Performs a grand-mean centering of data.
#'
#' @param x A data frame, a (numeric or character) vector or a factor.
#' @param select Character vector of column names. If \code{NULL} (the default), all
#'   variables will be selected.
#' @param exclude Character vector of column names to be excluded from selection.
#' @param force Logical, if \code{TRUE}, forces centering of factors as
#'   well. Factors are converted to numerical values, with the lowest level
#'   being the value \code{1} (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param append Logical, if \code{TRUE} and \code{x} is a data frame, standardized
#'   variables will be added as additional columns; if \code{FALSE},
#'   existing variables are overwritten.
#' @param suffix Character value, will be appended to variable (column) names of
#'   \code{x}, if \code{x} is a data frame and \code{append = TRUE}.
#' @param robust Logical, if \code{TRUE}, centering is done by subtracting the
#'   median from the variables. If \code{FALSE}, variables are centered by
#'   subtracting the mean.
#' @param weights Can be \code{NULL} (for no weighting), or:
#' \itemize{
#'   \item For data frames: a numeric vector of weights, or a character of the name of a column in the \code{data.frame} that contains the weights.
#'   \item For numeric vectors: a numeric vector of weights.
#' }
#' @param ... Currently not used.
#' @inheritParams p_value
#'
#' @seealso If centering within-clusters (instead of grand-mean centering)
#'   is required, see \code{\link{demean}}.
#'
#' @return The centered variables.
#'
#' @examples
#' data(iris)
#' head(iris$Sepal.Width)
#' head(center(iris$Sepal.Width))
#' head(center(iris))
#' head(center(iris, force = TRUE))
#' @export
center <- function(x, ...) {
  UseMethod("center")
}


#' @rdname center
#' @export
center.numeric <- function(x, weights = NULL, robust = FALSE, verbose = TRUE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  if (.are_weights(weights)) {
    valid_x <- !is.na(x) & !is.na(weights)
    x <- x[valid_x]
    weights <- weights[valid_x]
  } else {
    valid_x <- !is.na(x)
    x <- x[valid_x]
  }
  centered_x <- rep(NA, length(x))


  # Sanity checks
  check <- .check_center_numeric(x, name = NULL, verbose = verbose)
  if (is.null(check)) {
    return(x)
  }

  if (robust) {
    center <- .median(x, weights, verbose)
  } else {
    center <- .mean(x, weights, verbose)
  }

  x <- as.vector(x - center)

  centered_x[valid_x] <- x
  centered_x
}


#' @export
center.factor <- function(x, weights = NULL, robust = FALSE, verbose = TRUE, ...) {
  center(.factor_to_numeric(x), weights = weights, robust = robust, verbose = verbose, ...)
}


#' @export
center.character <- function(x, weights = NULL, robust = FALSE, verbose = TRUE, ...) {
  center(.factor_to_numeric(x), weights = weights, robust = robust, verbose = verbose, ...)
}


#' @rdname center
#' @export
center.data.frame <- function(x, select = NULL, exclude = NULL, weights = NULL, robust = FALSE, force = FALSE, append = FALSE, suffix = "_c", verbose = TRUE, ...) {
  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  if (!is.null(weights) && is.character(weights)) {
    if (weights %in% colnames(x)) {
      exclude <- c(exclude, weights)
    } else {
      warning("Could not find weighting column '", weights, "'. Weighting not carried out.")
      weights <- NULL
    }
  }

  select <- .select_c_variables(x, select, exclude, force)
  if (!is.null(weights) && is.character(weights)) weights <- x[[weights]]

  if (append) {
    new_variables <- x[select]
    if (!is.null(suffix)) {
      colnames(new_variables) <- paste0(colnames(new_variables), suffix)
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
  }

  x[select] <- lapply(
    x[select],
    center,
    weights = weights,
    robust = robust,
    verbose = FALSE
  )
  x
}







# helper ------------------------


.select_c_variables <- function(x, select, exclude, force) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  if (!force) {
    factors <- sapply(x[select], function(i) is.factor(i) | is.character(i))
    select <- select[!factors]
  }

  select
}




#' @keywords internal
.check_center_numeric <- function(x, name = NULL, verbose = TRUE) {
  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only one unique value and will not be standardized.")
      } else {
        message(paste0("The variable `", name, "` contains only one unique value and will not be standardized."))
      }
    }
    return(NULL)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2 && !is.factor(x) && !is.character(x)) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only two different values. Consider converting it to a factor.")
      } else {
        message(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
      }
    }
  }
  x
}



.are_weights <- function(w) {
  !is.null(w) && length(w) && !all(w == 1) && !all(w == w[1])
}



#' @importFrom stats na.omit
.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}



#' @importFrom stats weighted.mean
.mean <- function(x, weights = NULL, verbose = TRUE) {
  if (!.are_weights(weights)) {
    return(mean(x, na.rm = TRUE))
  }

  if (!all(weights > 0, na.rm = TRUE)) {
    if (isTRUE(verbose)) {
      warning("Some weights were negative. Weighting not carried out.", call. = FALSE)
    }
    return(mean(x, na.rm = TRUE))
  }

  stats::weighted.mean(x, weights, na.rm = TRUE)
}



#' @importFrom stats median
.median <- function(x, weights = NULL, verbose = TRUE) {
  # From spatstat + wiki
  if (!.are_weights(weights)) {
    return(stats::median(x, na.rm = TRUE))
  }

  if (!all(weights > 0, na.rm = TRUE)) {
    if (isTRUE(verbose)) {
      warning("Some weights were negative. Weighting not carried out.", call. = FALSE)
    }
    return(stats::median(x, na.rm = TRUE))
  }

  oo <- order(x)
  x <- x[oo]
  weights <- weights[oo]
  Fx <- cumsum(weights) / sum(weights)

  lefties <- which(Fx <= 0.5)
  left <- max(lefties)
  if (length(lefties) == 0) {
    result <- x[1]
  } else if (left == length(x)) {
    result <- x[length(x)]
  } else {
    result <- x[left]

    if (!(Fx[left - 1] < 0.5 && 1 - Fx[left] < 0.5)) {
      right <- left + 1
      y <- x[left] * Fx[left] + x[right] * Fx[right]
      if (is.finite(y)) result <- y
    }
  }

  result
}
