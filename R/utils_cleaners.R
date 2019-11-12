#' @keywords internal
.clean_parameter_names <- function(x, full = FALSE) {
  # return if x is empty
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  pattern <- if (full) {
    c(
      "as.factor", "as.numeric", "factor", "offset", "log1p", "log10", "log2", "log", "lag",
      "diff", "pspline", "poly", "catg", "asis", "matrx", "pol", "strata", "strat",
      "scale", "scored", "interaction", "sqrt", "lsp", "rcs", "pb", "lo", "bs",
      "ns", "t2", "te", "ti", "tt", "mi", "mo", "gp", "s", "I"
    )
  } else {
    c("as.factor", "as.numeric", "factor", "catg", "asis", "interaction", "I")
  }

  for (j in 1:length(pattern)) {
    # remove possible  namespace
    x <- sub("(.*)::(.*)", "\\2", x)
    if (pattern[j] == "offset") {
      x <- trimws(sub("offset\\(([^-+ )]*)\\)(.*)", "\\1\\2", x))
    } else if (pattern[j] == "I") {
      if (full) {
        x <- trimws(sub("I\\(((\\w|\\.)*).*", "\\1", x))
      } else {
        x <- trimws(sub("I\\((.*)\\)(.*)", "\\1", x))
      }
    } else {
      p <- paste0(pattern[j], "\\(((\\w|\\.)*)\\)(.*)")
      x <- trimws(sub(p, "\\1\\3", x))
    }
  }

  gsub("`", "", x, fixed = TRUE)
}




#' @keywords internal
.clean_confint <- function(ci) {
  estimate_row <- grep(pattern = "^estimate", x = rownames(ci), ignore.case = TRUE)
  if (length(estimate_row)) {
    ci <- ci[-estimate_row, ]
  }

  zi_col <- grep(pattern = "^zi\\.", x = colnames(ci), ignore.case = TRUE)
  if (length(zi_col)) {
    ci <- ci[, -zi_col, drop = FALSE]
  }

  colnames(ci) <- gsub("cond.", "", colnames(ci), fixed = TRUE)
  ci
}






#' @keywords internal
.remove_backticks_from_string <- function(x) {
  if (is.character(x)) {
    x <- gsub("`", "", x, fixed = TRUE)
  }
  x
}

#' @keywords internal
.remove_backticks_from_parameter_names <- function(x) {
  if (is.data.frame(x) && "Parameter" %in% colnames(x)) {
    x$Parameter <- gsub("`", "", x$Parameter, fixed = TRUE)
  }
  x
}