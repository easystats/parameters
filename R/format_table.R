#' Dataframe and Tables Pretty Formatting
#'
#' @param x A dataframe.
#' @param sep Column separator.
#'
#' @return A data.frame in character format.
#' @examples
#' library(parameters)
#'
#' cat(format_table(iris))
#'
#' @export
format_table <- function(x, sep = " | ") {

  # Convert to character
  col_names <- names(x)
  df <- as.data.frame(sapply(x, as.character, simplify = FALSE), stringsAsFactors = FALSE)
  names(df) <- col_names

  # Add colnames as row
  df <- rbind(colnames(df), df)

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

