#' Probability of direction (pd) Formatting
#'
#' @param pd Probability of direction (pd).
#' @inheritParams format_p
#'
#' @return A formatted string.
#' @examples
#' format_pd(0.12)
#' format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), name = NULL)
#' format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), stars = TRUE)
#' @export
format_pd <- function(pd, stars = FALSE, stars_only = FALSE, name = "pd") {
  text <- ifelse(pd >= 1, "= 100%***",
                 ifelse(pd > 0.999, paste0("= ", format_value(pd * 100), "%***"),
                        ifelse(pd > 0.99, paste0("= ", format_value(pd * 100), "%**"),
                               ifelse(pd > 0.97, paste0("= ", format_value(pd * 100), "%*"),
                                      paste0("= ", format_value(pd * 100), "%")))))

  .add_prefix_and_remove_stars(text, stars, stars_only, name)
}
