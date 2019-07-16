#' Dataframe and Tables Pretty Formatting
#'
#' @param x A dataframe.
#' @param sep Column separator.
#'
#' @examples
#' library(parameters)
#'
#' x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#' cat(format_table(x))
#'
#' @export
format_table <- function(x, sep = " | ") {
  df <- as.data.frame(sapply(x, format_value, digits = 2), stringsAsFactors = FALSE)

  # Add colnames as row
  df <- rbind(Parameter = colnames(df), df)

  # Align
  aligned <- format(df, justify = "right")

  # Centre first row
  first_row <- as.character(aligned[1, ])
  for (i in 1:length(first_row)) {
    aligned[1, i] <- format(trimws(first_row[i]), width = nchar(first_row[i]), justify = "right")
  }

  final <- as.matrix(aligned)

  # left-align first column
  final[, 1] <- format(trimws(final[, 1]), justify = "left")

  # Transform to character
  rows <- c()
  for (row in 1:nrow(final)) {
    final_row <- paste0(final[row, ], collapse = sep)
    rows <- paste0(rows, final_row, sep = "\n")

    # First row separation
    if (row == 1) {
      rows <- paste0(rows, paste0(rep_len("-", nchar(final_row)), collapse = ""), sep = "\n")
    }
  }
  rows
}

