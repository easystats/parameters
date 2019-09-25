#' Conversion between (log)odds and probabilities
#'
#' Enables a conversion between (log)odds and probabilities.
#'
#' @param x Odds or probs values in vector or dataframe.
#' @param log Are these Log odds (such as in logistic models)?
#' @param ... Arguments passed to or from other methods.
#'
#' @return Converted index.
#' @examples
#' odds_to_probs(-1.45)
#' odds_to_probs(3.22)
#' @export
odds_to_probs <- function(x, log = FALSE, ...) {
  UseMethod("odds_to_probs")
}

#' @export
odds_to_probs.numeric <- function(x, log = FALSE, ...) {
  .odds_to_probs(x, log = log)
}

#' @export
odds_to_probs.double <- odds_to_probs.numeric



#' @param select Character or list of of column names to be
#' transformed.
#' @param exclude Character or list of column names to be excluded
#' from transformation.
#' @rdname odds_to_probs
#' @export
odds_to_probs.data.frame <- function(x, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(odds = x, log = log, select = select, exclude = exclude, ...)
}


#' @rdname odds_to_probs
#' @export
probs_to_odds <- function(x, log = FALSE, ...) {
  UseMethod("probs_to_odds")
}

#' @export
probs_to_odds.numeric <- function(x, log = FALSE, ...) {
  .probs_to_odds(x, log = log)
}

#' @export
probs_to_odds.double <- probs_to_odds.numeric

#' @export
probs_to_odds.data.frame <- function(x, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(probs = x, log = log, select = select, exclude = exclude, ...)
}

#' @rdname odds_to_probs
#' @export
convert_odds_to_probs <- odds_to_probs


#' @rdname odds_to_probs
#' @export
convert_probs_to_odds <- probs_to_odds













#' @keywords internal
.odds_to_probs_df <- function(odds = NULL, probs = NULL, log = FALSE, select = NULL, exclude = NULL, ...) {

  # If vector
  if (!is.null(odds)) {
    df <- odds
  } else {
    df <- probs
  }

  # Variable order
  var_order <- names(df)

  # Keep subset
  if (!is.null(select) && select %in% names(df)) {
    to_keep <- as.data.frame(df[!names(df) %in% c(select)])
    df <- df[names(df) %in% c(select)]
  } else {
    to_keep <- NULL
  }

  # Remove exceptions
  if (!is.null(exclude) && exclude %in% names(df)) {
    if (is.null(to_keep)) {
      to_keep <- as.data.frame(df[exclude])
    } else {
      to_keep <- cbind(to_keep, as.data.frame(df[exclude]))
    }

    df <- df[!names(df) %in% c(exclude)]
  }

  # Remove non-numerics
  dfother <- df[!sapply(df, is.numeric, simplify = TRUE)]
  dfnum <- df[sapply(df, is.numeric, simplify = TRUE)]

  # Tranform
  if (!is.null(odds)) {
    dfnum <- .odds_to_probs(dfnum, log = log)
  } else {
    dfnum <- .probs_to_odds(dfnum, log = log)
  }

  # Add non-numerics
  if (is.null(ncol(dfother))) {
    df <- dfnum
  } else {
    df <- cbind(dfother, dfnum)
  }

  # Add exceptions
  if (!is.null(select) | !is.null(exclude) && exists("to_keep")) {
    df <- cbind(df, to_keep)
  }

  # Reorder
  df <- df[var_order]

  return(df)
}


#' @keywords internal
.odds_to_probs <- function(odds, log = TRUE) {
  if (log == TRUE) {
    odds <- exp(odds)
  }
  probs <- odds / (1 + odds)
  return(probs)
}

#' @keywords internal
.probs_to_odds <- function(probs, log = TRUE) {
  odds <- probs / (1 - probs)
  if (log == TRUE) {
    odds <- log(odds)
  }
  return(odds)
}
