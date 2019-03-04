#' Convert (log)odds to probabilies.
#'
#' @param odds Odds values in vector or dataframe.
#' @param select Character or list of of column names to be
#' transformed.
#' @param exclude Character or list of column names to be excluded
#' from transformation.
#' @param log Are these Log odds (such as in logistic models)?
#'
#' @examples
#' odds_to_probs(-1.45)
#' @export
odds_to_probs <- function(odds, select = NULL, exclude = NULL, log = FALSE) {

  # If vector
  if (ncol(as.matrix(odds)) == 1) {
    return(.odds_to_probs(odds, log = log))
  } else {
    df <- odds
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
  dfnum <- .odds_to_probs(dfnum, log = log)

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
