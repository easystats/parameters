#' p-values formatting
#'
#' Format p-values.
#'
#' @param p value or vector of p-values.
#' @param stars Add significance stars (e.g., p < .001***).
#' @param stars_only Return only significance stars.
#' @param name Name prefixing the text. Can be \code{NULL}.
#' @inherit insight::format_value
#'
#' @return A formatted string.
#' @examples
#' format_p(c(.02, .065, 0, .23))
#' format_p(c(.02, .065, 0, .23), name = NULL)
#' format_p(c(.02, .065, 0, .23), stars_only = TRUE)
#' @importFrom insight format_value
#' @export
format_p <- function(p, stars = FALSE, stars_only = FALSE, name = "p", missing = "") {
  text <- ifelse(is.na(p), NA,
    ifelse(p < 0.001, "< .001***",
      ifelse(p < 0.01, "< .01**",
        ifelse(p < 0.05, "< .05*",
          ifelse(p < 0.1, paste0("= ", insight::format_value(p, 2)),
            "> .1"
          )
        )
      )
    )
  )

  .add_prefix_and_remove_stars(text, stars, stars_only, name, missing)
}


#' @keywords internal
.add_prefix_and_remove_stars <- function(text, stars, stars_only, name, missing = "") {
  missing_index <- is.na(text)

  if (is.null(name)) {
    text <- gsub("= ", "", text)
  } else {
    text <- paste(name, text)
  }

  if (stars_only == TRUE) {
    text <- gsub("[^\\*]", "", text)
  } else if (stars == FALSE) {
    text <- gsub("\\*", "", text)
  }

  text[missing_index] <- missing
  text
}
