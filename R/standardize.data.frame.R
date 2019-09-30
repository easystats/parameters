#' @rdname standardize
#' @importFrom stats median mad na.omit
#' @export
standardize.numeric <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {
  method <- match.arg(method, choices = c("default", "refit", "2sd", "smart", "partial", "classic"))

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  x <- stats::na.omit(x)

  # Sanity checks
  check <- .check_standardize_numeric(x, name = NULL, verbose = verbose)
  if (is.null(check)) {
    return(x)
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

  if (method %in% c("default", "classic", "refit", "smart")) {
    x <- as.vector((x - center) / scale)
  } else {
    x <- as.vector((x - center) / 2 * scale)
  }

  attr(x, "center") <- center
  attr(x, "scale") <- scale
  x
}



#' @keywords internal
.check_standardize_numeric <- function(x, name = NULL, verbose = TRUE) {
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


#' @export
standardize.logical <- standardize.factor


#' @export
standardize.Surv <- function(x, ...) {
  insight::print_color("'Surv' objects cannot be standardized.\n", "red")
  x
}


#' @export
standardize.AsIs <- standardize.numeric



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



#' @rdname standardize
#' @export
standardize.data.frame <- function(x, robust = FALSE, method = "default", select = NULL, exclude = NULL, verbose = TRUE, force = FALSE, ...) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  for (i in 1:length(select)) {
    .check_standardize_numeric(x[[select[i]]], name = select[i], verbose = verbose)
  }

  x[select] <- lapply(x[select], standardize, robust = robust, method = method, verbose = FALSE, force = force)

  attr(x, "center") <- sapply(x[select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[select], function(z) attributes(z)$scale)
  x
}
