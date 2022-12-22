#' @keywords internal
.clean_parameter_names <- function(x, full = FALSE) {
  # return if x is empty
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  # here we need to capture only those patterns that we do *not* want to format
  # in a particular style. However, these patterns will not be shown in the output
  # from "model_parameters()". If certain patterns contain useful information,
  # remove them here and clean/prepare them in ".parameters_type_basic()".
  # for formatting / printing, refer to ".format_parameter()".

  pattern <- if (full) {
    c(
      "as.factor", "as.numeric", "factor", "offset", "lag", "diff", "catg",
      "matrx", "pol", "strata", "strat", "scale", "scored", "interaction",
      "lsp", "pb", "lo", "t2", "te", "ti", "tt", "mi", "mo", "gp"
    )
  } else {
    c("as.factor", "as.numeric", "factor", "catg", "interaction")
  }

  for (j in seq_along(pattern)) {
    # remove possible  namespace
    if (any(grepl("::", x, fixed = TRUE))) {
      x <- sub("(.*)::(.*)", "\\2", x)
    }
    if (pattern[j] == "offset" && any(grepl("offset(", x, fixed = TRUE))) {
      x <- insight::trim_ws(sub("offset\\(([^-+ )]*)\\)(.*)", "\\1\\2", x))
      # some exceptions here...
    } else if (full && pattern[j] == "scale" && any(grepl("scale(", x, fixed = TRUE))) {
      x[grepl("scale(", x, fixed = TRUE)] <- insight::clean_names(grep("scale(", x, fixed = TRUE, value = TRUE))
    } else if (any(grepl(pattern[j], x, fixed = TRUE))) {
      p <- paste0(pattern[j], "\\(((\\w|\\.)*)\\)(.*)")
      x <- insight::trim_ws(sub(p, "\\1\\3", x))
    }
  }

  gsub("`", "", x, fixed = TRUE)
}




#' @keywords internal
.remove_backticks_from_string <- function(x) {
  if (is.character(x)) {
    x <- gsub("`", "", x, fixed = TRUE)
  }
  x
}

#' @keywords internal
.intercepts <- function() {
  c(
    "(intercept)_zi",
    "intercept (zero-inflated)",
    "intercept (zero-inflation)",
    "intercept",
    "zi_intercept",
    "(intercept)",
    "b_intercept",
    "b_zi_intercept"
  )
}

#' @keywords internal
.in_intercepts <- function(x) {
  tolower(x) %in% .intercepts() | startsWith(tolower(x), "intercept")
}
