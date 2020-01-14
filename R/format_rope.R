#' Percentage in ROPE formatting
#'
#' @param rope_percentage Value or vector of percentages in ROPE.
#' @inheritParams format_p
#'
#' @return A formatted string.
#'
#' @examples
#' format_rope(c(0.02, 0.12, 0.357, 0))
#' format_rope(c(0.02, 0.12, 0.357, 0), name = NULL)
#' @importFrom insight format_value
#' @export
format_rope <- function(rope_percentage, name = "in ROPE") {
  text <- ifelse(rope_percentage == 0, "0%",
    ifelse(rope_percentage == 1, "100%",
      paste0(insight::format_value(rope_percentage * 100), "%")
    )
  )
  if (!is.null(name)) {
    text <- paste(text, name)
  }
  text
}
