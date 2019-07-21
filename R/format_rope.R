#' Percentage in ROPE Formatting
#'
#' @param rope_percentage Value or vector of percentages in ROPE.
#' @inheritParams format_p
#'
#' @examples
#' format_rope(c(0.02, 0.12, 0.357, 0))
#' format_rope(c(0.02, 0.12, 0.357, 0), name = NULL)
#' @export
format_rope <- function(rope_percentage, name = "ROPE") {
  text <- ifelse(rope_percentage == 0, "= 0%",
                        ifelse(rope_percentage == 1, "= 100%",
                               paste0("= ", format_value(rope_percentage * 100), "%")))
  .add_prefix_and_remove_stars(text, stars = FALSE, stars_only = FALSE, name)
}
