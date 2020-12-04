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
    "by" = " Benjamini & Yekutieli (2001)",
    method
  )
}
