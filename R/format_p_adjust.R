#' Format the name of the p-value adjustment methods
#'
#' Format the name of the p-value adjustment methods.
#'
#' @param method Name of the method.
#'
#' @examples
#' library(parameters)
#'
#' format_p_adjust("holm")
#' format_p_adjust("bonferroni")
#' @return A string with the full surname(s) of the author(s), including year of publication, for the adjustment-method.
#' @export
format_p_adjust <- function(method) {
  method <- tolower(method)

  switch(
    method,
    "holm" = "Holm (1979)",
    "hochberg" = "Hochberg (1988)",
    "hommel" = "Hommel (1988)",
    "bonferroni" = "Bonferroni",
    "fdr" = "Benjamini & Hochberg (1995)",
    "bh" = "Benjamini & Hochberg (1995)",
    "by" = "Benjamini & Yekutieli (2001)",
    "tukey" = "Tukey",
    method
  )
}





#' @importFrom stats ptukey p.adjust.methods p.adjust
.p_adjust <- function(params, p_adjust) {
  if (!is.null(p_adjust) && "p" %in% colnames(params)) {
    if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
      params$p <- stats::p.adjust(params$p, method = p_adjust)
    } else if (tolower(p_adjust) == "tukey") {
      stat_column <- stats::na.omit(match(c("t", "Statistic"), colnames(params)))
      if ("df" %in% colnames(params) && length(stat_column) > 0) {
        params$p <- stats::ptukey(sqrt(2) * abs(params[[stat_column]]), nrow(params), params$df, lower.tail = FALSE)
      }
    }
  }
  params
}
