#' Bayes Factor Formatting
#'
#' @param bf Bayes Factor.
#' @inheritParams format_p
#'
#' @return A formatted string.
#'
#' @examples
#' format_bf(1.20)
#' format_bf(c(1.20, 1557, 3.5, 12), stars = TRUE)
#' format_bf(c(1.20, 1557, 3.5, 12), name = NULL)
#' @export
format_bf <- function(bf, stars = FALSE, stars_only = FALSE, name = "BF") {
  text <- ifelse(bf > 999, "> 999***",
    paste0(
      "= ",
      ifelse(bf > 30, paste0(format_value(bf), "***"),
        ifelse(bf > 10, paste0(format_value(bf), "**"),
          ifelse(bf > 3, paste0(format_value(bf), "*"),
            paste0(format_value(bf))
          )
        )
      )
    )
  )

  .add_prefix_and_remove_stars(text, stars, stars_only, name)
}
