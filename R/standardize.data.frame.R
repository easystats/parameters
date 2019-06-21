#' @rdname standardize
#' @importFrom stats median mad na.omit
#' @export
standardize.numeric <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {
  method <- match.arg(method, choices = c("default", "refit", "2sd", "full", "partial", "classic"))

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  x <- stats::na.omit(x)

  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only one unique value and will not be standardized."))
    }
    return(x)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2 && !is.factor(x) && !is.character(x)) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
    }
  }

  if (is.factor(x) || is.character(x)) {
    x <- .factor_to_numeric(x)
  }

  if (robust) {
    center <- stats::median(x)
    scale <- stats::mad(x)
  } else {
    center <- mean(x)
    scale <- stats::sd(x)
  }

  if (method %in% c("default", "classic", "refit", "full")) {
    x <- as.vector((x - center) / scale)
  } else {
    x <- as.vector((x - center) / 2 * scale)
  }

  attr(x, "center") <- center
  attr(x, "scale") <- scale
  x
}









#' @rdname standardize
#' @inherit standardize
#' @export
standardize.factor <- function(x, force = FALSE, ...) {
  if (force) {
    standardize(as.numeric(x), ...)
  } else {
    x
  }
}


#' @export
standardize.character <- standardize.factor




#' @inheritParams standardize
#' @export
standardize.grouped_df <- function(x, robust = FALSE, method = "default", select = NULL, exclude = NULL, verbose = TRUE, force = FALSE, ...) {
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
    x[rows, ] <- standardize(
      x[rows, ],
      select = select,
      exclude = exclude,
      robust = robust,
      method = method,
      verbose = verbose,
      force = force,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' Data Standardization (Z-scores)
#'
#' Standardize (centering and scaling, Z-score) the data so that the
#'   values are expressed in terms of standard deviation (i.e., mean = 0, SD = 1)
#'   or Median Absolute Deviance (median = 0, MAD = 1). A \code{normalization}
#'   scales all numeric variables in the 0 - 1 range.
#'
#' @inheritParams standardize
#' @param select For a data frame, character vector of column names to be
#'   standardized. If \code{NULL} (the default), all variables will be
#'   standardized.
#' @param exclude For a data frame, character vector of column names to
#'   be excluded from standardization.
#'
#' @examples
#' summary(standardize(iris))
#' @export
standardize.data.frame <- function(x, robust = FALSE, method = "default", select = NULL, exclude = NULL, verbose = TRUE, force = FALSE, ...) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  x[select] <- lapply(x[select], standardize, robust = robust, method = method, verbose = verbose, force = force)

  attr(x, "center") <- sapply(x[select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[select], function(z) attributes(z)$scale)
  x
}
