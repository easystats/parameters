#' Format the name of the degrees-of-freedom adjustment methods
#'
#' Format the name of the degrees-of-freedom adjustment methods.
#'
#' @param method Name of the method.
#' @param approx_string Suffix added to the name of the method in the returned string.
#'
#' @examples
#' library(parameters)
#'
#' format_df_adjust("kenward")
#' format_df_adjust("kenward", approx_string = "", dof_string = " DoF")
#' @return A formatted string.
#' @export
format_df_adjust <- function(method,
                             approx_string = "-approximated",
                             dof_string = " degrees of freedom") {
  method <- tolower(method)

  out <- switch(method,
         "kr" = ,
         "kenward-roger" = ,
         "kenward" = "Kenward-Roger",
         "ml1" = "m-l-1",
         "betwithin" = ,
         "bw" = "Between-within",
         "fit" = "Residual",
         "boot" = "Bootstrapped",
         .capitalize(method)
  )

  paste0(out, approx_string, dof_string)
}
