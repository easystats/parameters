#' p-values formatting
#'
#' Format p-values.
#'
#' @param p value or vector of p-values.
#' @param stars Add significance stars (e.g., p < .001***).
#' @param stars_only Return only significance stars.
#' @param name Name prefixing the text. Can be \code{NULL}.
#' @param digits Number of significant digits. May also be \code{"scientific"} to return exact p-values in scientific notation, or \code{"apa"} to use an APA-style for p-values.
#' @param ... Arguments from other methods.
#' @inherit insight::format_value
#'
#' @return A formatted string.
#' @examples
#' format_p(c(.02, .065, 0, .23))
#' format_p(c(.02, .065, 0, .23), name = NULL)
#' format_p(c(.02, .065, 0, .23), stars_only = TRUE)
#'
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' format_p(p_value(model)$p, digits = "scientific")
#'
#' @importFrom insight format_value
#' @export
format_p <- function(p, stars = FALSE, stars_only = FALSE, name = "p", missing = "", digits = 3, ...) {
  if (digits == "apa") {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, "< .001***",
        ifelse(p < 0.01, "< .01**",
          ifelse(p < 0.05, "< .05*",
            paste0("= ", insight::format_value(p, digits))
          )
        )
      )
    )
  } else if (digits == "scientific") {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, sprintf("= %.5e***", p),
        ifelse(p < 0.01, sprintf("= %.5e**", p),
          ifelse(p < 0.05, sprintf("= %.5e*", p),
            sprintf("= %.5e", p)
          )
        )
      )
    )
  } else if (digits <= 3) {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, "< .001***",
        ifelse(p < 0.01, paste0("= ", insight::format_value(p, digits), "**"),
          ifelse(p < 0.05, paste0("= ", insight::format_value(p, digits), "*"),
            paste0("= ", insight::format_value(p, digits))
          )
        )
      )
    )
  } else {
    text <- ifelse(is.na(p), NA,
      ifelse(p < 0.001, paste0("= ", insight::format_value(p, digits), "***"),
        ifelse(p < 0.01, paste0("= ", insight::format_value(p, digits), "**"),
          ifelse(p < 0.05, paste0("= ", insight::format_value(p, digits), "*"),
            paste0("= ", insight::format_value(p, digits))
          )
        )
      )
    )
  }

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
